library(data.table)
library(arrow)
library(fixest)
library(ggplot2)
library(timeDate) # einmalig: 

dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)
dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)

# ==============================================================================
# 1. PANEL LADEN & NYSE-HANDELSKALENDER + DST-BEREINIGUNG (America/New_York)
# ==============================================================================
panel_path <- "data/processed/panel_30m_utc.parquet"
if (!file.exists(panel_path)) {
  stop("Panel nicht gefunden. Zuerst scripts/02_build_30m_panel.R ausführen.")
}
panel <- as.data.table(read_parquet(panel_path))
n_coins <- uniqueN(panel$symbol)
n_days  <- uniqueN(panel$date)

cat(sprintf("Geladen: %d Coins, %d Tage (%s bis %s UTC)\n",
            n_coins, n_days, min(panel$date), max(panel$date)))

yrs <- seq(as.integer(format(min(panel$date), "%Y")),
           as.integer(format(max(panel$date), "%Y")))

# Vollständige NYSE-Feiertage + Sicherheitsnetz für neuere/einmalige Schließtage
nyse_holidays <- unique(c(
  as.Date(holidayNYSE(yrs)),
  as.Date(c(
    "2022-06-20", "2023-06-19", "2024-06-19", "2025-06-19", "2026-06-19", # Juneteenth
    "2025-01-09"                                                          # National Day of Mourning (Jimmy Carter)
  ))
))

# NYSE-Early-Closes (13:00 ET statt 16:00 ET): Black Friday, 3. Juli, 24. Dez.
# (Fällt 3. Juli / 24. Dez. auf ein Wochenende oder einen Vollfeiertag, greift der Filter ohnehin)
nyse_early_closes <- unique(c(
  as.Date(USThanksgivingDay(yrs)) + 1L,
  as.Date(sprintf("%d-07-03", yrs)),
  as.Date(sprintf("%d-12-24", yrs))
))

# Schnelles Zeitzonen-Mapping auf eindeutigen Zeitstempeln
ts_map <- data.table(ts_30m = unique(panel$ts_30m))
lt_ny  <- as.POSIXlt(ts_map$ts_30m, tz = "America/New_York")

ts_map[, `:=`(
  date_ny        = as.Date(sprintf("%04d-%02d-%02d", lt_ny$year + 1900L, lt_ny$mon + 1L, lt_ny$mday)),
  interval_id_ny = lt_ny$hour * 2L + (lt_ny$min %/% 30L) + 1L,
  wday_ny        = lt_ny$wday,
  isdst_ny       = lt_ny$isdst
)]

# DST-Umstellungstage (2 Sonntage/Jahr) erkennen -> verhindert doppelte 01:00-02:00-Slots
# Hinweis: US-Open (09:30 NY) und US-Close (15:30 NY) am Montag danach liegen >24 UTC-Stunden
# hinter dem Umstellungszeitpunkt (So 02:00 NY) und sind von shift(48) nicht betroffen.
dst_dates <- ts_map[, .(n_dst = uniqueN(isdst_ny)), by = date_ny][n_dst > 1L, date_ny]

ts_map[, ny_regime := fcase(
  date_ny %in% dst_dates,                                                 "DST_Transition",
  wday_ny %in% 1:5 & !(date_ny %in% c(nyse_holidays, nyse_early_closes)), "US_Trading_Day",
  wday_ny %in% c(0L, 6L),                                                 "Weekend",
  default = "US_Holiday_or_EarlyClose"
)]

# Diagnose-Ausgabe der Tagesklassifikation (NY-Kalender)
regime_counts <- ts_map[, .(n_days_ny = uniqueN(date_ny)), by = ny_regime]
cat("\nNY-Kalender-Klassifikation:\n")
print(regime_counts)
cat("\n")

panel[ts_map, on = "ts_30m", `:=`(
  date_ny        = i.date_ny,
  interval_id_ny = i.interval_id_ny,
  ny_regime      = i.ny_regime
)]
rm(ts_map, lt_ny)

# ==============================================================================
# 2. HILFSFUNKTION: 48 PANEL-REGRESSIONEN (NORMALISIERTE GEWICHTE & DOUBLE-CLUSTERING)
# ==============================================================================
make_label <- function(k) {
  h1 <- (k - 1L) %/% 2L
  m1 <- ((k - 1L) %% 2L) * 30L
  h2 <- (k %% 48L) %/% 2L
  m2 <- (k %% 2L) * 30L
  sprintf("%02d:%02d-%02d:%02d", h1, m1, h2, m2)
}

run_48_scan <- function(dt, predictor_col, weight_col = NULL,
                        id_col = "interval_id", fe_time_col = "ts_30m",
                        cluster_time_col = "date") {
  res_list <- vector("list", 48L)
  fml <- as.formula(sprintf("ret_30m ~ %s | symbol + %s", predictor_col, fe_time_col))
  cl_vars <- c("symbol", cluster_time_col)

  for (k in 1:48) {
    sub <- dt[get(id_col) == k]
    if (nrow(sub) == 0L) next

    mod <- if (!is.null(weight_col)) {
      feols(fml, data = sub, weights = sub[[weight_col]], cluster = cl_vars, notes = FALSE)
    } else {
      feols(fml, data = sub, cluster = cl_vars, notes = FALSE)
    }

    beta <- unname(coef(mod)[1]) * 100  # Skalierung x100 wie bei Baltussen et al.
    se_v <- unname(se(mod)[1]) * 100
    t_v  <- unname(tstat(mod)[1])

    res_list[[k]] <- data.table(
      interval_id = k,
      label       = make_label(k),
      beta        = beta,
      se          = se_v,
      ci_low      = beta - 1.96 * se_v,
      ci_high     = beta + 1.96 * se_v,
      t_stat      = t_v,
      n_obs       = nobs(mod)
    )
  }
  rbindlist(res_list)
}

# ==============================================================================
# 3. TEIL A: UTC 48-INTERVALL-SCANS (CAPPED-VW, NORMALIZED RAW-VW & EW)
# ==============================================================================
cat("1/6 UTC-Scan: 30m-Skip, 5%-Capped-VW (Hauptspezifikation)...\n")
res_skip_cap5 <- run_48_scan(panel, "ret_prev_skip", weight_col = "w_vw_cap5")

cat("2/6 UTC-Scan: 30m-Skip, Normalized Raw-VW...\n")
res_skip_vw <- run_48_scan(panel, "ret_prev_skip", weight_col = "w_vw_raw")

cat("3/6 UTC-Scan: No Skip, 5%-Capped-VW (Analog Figure A.1)...\n")
res_noskip_cap5 <- run_48_scan(panel, "ret_prev_noskip", weight_col = "w_vw_cap5")

cat("4/6 UTC-Scan: No Skip, Normalized Raw-VW...\n")
res_noskip_vw <- run_48_scan(panel, "ret_prev_noskip", weight_col = "w_vw_raw")

cat("5/6 UTC-Scan: 30m-Skip, Equal-Weighted...\n")
res_skip_ew <- run_48_scan(panel, "ret_prev_skip", weight_col = NULL)

fwrite(res_skip_cap5,   "output/tables/scan48_utc_skip_cap5.csv")
fwrite(res_skip_vw,     "output/tables/scan48_utc_skip_vw_norm.csv")
fwrite(res_noskip_cap5, "output/tables/scan48_utc_noskip_cap5.csv")
fwrite(res_noskip_vw,   "output/tables/scan48_utc_noskip_vw_norm.csv")
fwrite(res_skip_ew,     "output/tables/scan48_utc_skip_ew.csv")

# ==============================================================================
# 4. TEIL B: US-ZEITZONE (America/New_York) — NYSE-HANDELSTAGE VS. WOCHENENDE
# ==============================================================================
cat("6/6 NY-Time-Scan (ex-Feiertage, ex-Early-Closes, ex-DST): NYSE-Handelstage vs. Wochenende...\n")

lbl_trd <- "US Trading Days (NYSE Full Days)"
lbl_wkd <- "Weekends (Sat-Sun, ex-DST)"

res_ny_trd_cap5 <- run_48_scan(
  panel[ny_regime == "US_Trading_Day"], "ret_prev_skip", weight_col = "w_vw_cap5",
  id_col = "interval_id_ny", fe_time_col = "ts_30m", cluster_time_col = "date_ny"
)[, regime := lbl_trd]

res_ny_wkd_cap5 <- run_48_scan(
  panel[ny_regime == "Weekend"], "ret_prev_skip", weight_col = "w_vw_cap5",
  id_col = "interval_id_ny", fe_time_col = "ts_30m", cluster_time_col = "date_ny"
)[, regime := lbl_wkd]

res_ny_cap5 <- rbindlist(list(res_ny_trd_cap5, res_ny_wkd_cap5))
fwrite(res_ny_cap5, "output/tables/scan48_ny_tradingday_vs_weekend_cap5.csv")

# Robustheit: Normalized Raw-VW & Equal-Weighted für NY-Time
res_ny_trd_vw <- run_48_scan(
  panel[ny_regime == "US_Trading_Day"], "ret_prev_skip", weight_col = "w_vw_raw",
  id_col = "interval_id_ny", fe_time_col = "ts_30m", cluster_time_col = "date_ny"
)[, regime := lbl_trd]

res_ny_wkd_vw <- run_48_scan(
  panel[ny_regime == "Weekend"], "ret_prev_skip", weight_col = "w_vw_raw",
  id_col = "interval_id_ny", fe_time_col = "ts_30m", cluster_time_col = "date_ny"
)[, regime := lbl_wkd]

res_ny_vw <- rbindlist(list(res_ny_trd_vw, res_ny_wkd_vw))
fwrite(res_ny_vw, "output/tables/scan48_ny_tradingday_vs_weekend_vw_norm.csv")

res_ny_trd_ew <- run_48_scan(
  panel[ny_regime == "US_Trading_Day"], "ret_prev_skip", weight_col = NULL,
  id_col = "interval_id_ny", fe_time_col = "ts_30m", cluster_time_col = "date_ny"
)[, regime := lbl_trd]

res_ny_wkd_ew <- run_48_scan(
  panel[ny_regime == "Weekend"], "ret_prev_skip", weight_col = NULL,
  id_col = "interval_id_ny", fe_time_col = "ts_30m", cluster_time_col = "date_ny"
)[, regime := lbl_wkd]

res_ny_ew <- rbindlist(list(res_ny_trd_ew, res_ny_wkd_ew))
fwrite(res_ny_ew, "output/tables/scan48_ny_tradingday_vs_weekend_ew.csv")

# ==============================================================================
# 5. PLOTS IM STIL VON BALTUSSEN, DA & SOEBHAG (2026)
# ==============================================================================
plot_scan_utc <- function(res, title_txt, subtitle_txt) {
  d <- copy(res)
  d[, label := factor(label, levels = label)]

  # Exakte getrennte Schattierung (OHNE Slot 41 = 20:00-20:30 UTC!):
  # Slot 40 (19:30-20:00 UTC) = 15:30-16:00 EDT (US-Close Sommerzeit)
  # Slot 42 (20:30-21:00 UTC) = 15:30-16:00 EST (US-Close Winterzeit)
  # Slot 48 (23:30-00:00 UTC) = Binance Daily Candle Close (00:00 UTC Anker)
  ggplot(d, aes(x = label, y = beta)) +
    annotate("rect", xmin = 39.5, xmax = 40.5, ymin = -Inf, ymax = Inf,
             fill = "#2C7BB6", alpha = 0.16) +
    annotate("rect", xmin = 41.5, xmax = 42.5, ymin = -Inf, ymax = Inf,
             fill = "#2C7BB6", alpha = 0.16) +
    annotate("rect", xmin = 47.5, xmax = 48.5, ymin = -Inf, ymax = Inf,
             fill = "#D7191C", alpha = 0.16) +
    geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.6, color = "black") +
    geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.35, linewidth = 0.45) +
    geom_point(shape = 21, fill = "white", color = "black", size = 1.7, stroke = 0.7) +
    scale_x_discrete(breaks = as.character(d$label)[seq(1, 48, by = 2)]) +
    labs(
      title    = title_txt,
      subtitle = subtitle_txt,
      x        = "30-Minuten-Intervall (UTC)",
      y        = "Slope Coefficient (x 100)"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title       = element_text(face = "bold", size = 12),
      plot.subtitle    = element_text(size = 9.5, color = "grey30"),
      axis.text.x      = element_text(angle = 45, hjust = 1, size = 8.5),
      panel.grid.minor = element_blank(),
      panel.border     = element_rect(color = "grey40", fill = NA, linewidth = 0.5)
    )
}

plot_scan_ny_facet <- function(res_ny, title_txt, subtitle_txt) {
  d <- copy(res_ny)
  lbls <- make_label(1:48)
  d[, label := factor(label, levels = lbls)]
  d[, regime := factor(regime, levels = c(lbl_trd, lbl_wkd))]

  # k = 20 (09:30-10:00 NY) = US Equity Open (FH)
  # k = 32 (15:30-16:00 NY) = US Equity Close (LH, direkter Paper-Test)
  ggplot(d, aes(x = label, y = beta)) +
    annotate("rect", xmin = 19.5, xmax = 20.5, ymin = -Inf, ymax = Inf,
             fill = "#1A9641", alpha = 0.15) +
    annotate("rect", xmin = 31.5, xmax = 32.5, ymin = -Inf, ymax = Inf,
             fill = "#2C7BB6", alpha = 0.18) +
    geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.6, color = "black") +
    geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.35, linewidth = 0.45) +
    geom_point(shape = 21, fill = "white", color = "black", size = 1.6, stroke = 0.65) +
    facet_wrap(~regime, ncol = 1, scales = "fixed") +
    scale_x_discrete(breaks = lbls[seq(1, 48, by = 2)]) +
    labs(
      title    = title_txt,
      subtitle = subtitle_txt,
      x        = "30-Minuten-Intervall (America/New_York, DST-bereinigt)",
      y        = "Slope Coefficient (x 100)"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title       = element_text(face = "bold", size = 12),
      plot.subtitle    = element_text(size = 9.5, color = "grey30"),
      strip.text       = element_text(face = "bold", size = 10.5),
      axis.text.x      = element_text(angle = 45, hjust = 1, size = 8.5),
      panel.grid.minor = element_blank(),
      panel.border     = element_rect(color = "grey40", fill = NA, linewidth = 0.5)
    )
}

sub_utc <- sprintf(
  "Binance Spot (%d Coins, %s bis %s) | Blau: US-Close (19:30-20:00 EDT / 20:30-21:00 EST) | Rot: 00:00 UTC Tageskerzen-Anker",
  n_coins, min(panel$date), max(panel$date)
)
sub_ny <- sprintf(
  "Binance Spot (%d Coins, %s bis %s, ohne NYSE-Feiertage, Early Closes & DST-Sonntage) | Grün: US-Open (09:30-10:00) | Blau: US-Close (15:30-16:00 NY)",
  n_coins, min(panel$date), max(panel$date)
)

p_skip_cap5   <- plot_scan_utc(res_skip_cap5,   "Crypto 48-Interval Scan in UTC (5%-Capped-VW, 30-Min Skip)", sub_utc)
p_skip_vw     <- plot_scan_utc(res_skip_vw,     "Crypto 48-Interval Scan in UTC (Normalized Raw-VW, 30-Min Skip)", sub_utc)
p_noskip_cap5 <- plot_scan_utc(res_noskip_cap5, "Crypto 48-Interval Scan in UTC (5%-Capped-VW, No Skip) - Analog Figure A.1", sub_utc)
p_skip_ew     <- plot_scan_utc(res_skip_ew,     "Crypto 48-Interval Scan in UTC (Equal-Weighted, 30-Min Skip)", sub_utc)
p_ny_facet    <- plot_scan_ny_facet(res_ny_cap5, "Crypto 48-Interval Scan in US Eastern Time: NYSE Trading Days vs. Weekends (5%-Capped-VW)", sub_ny)
p_ny_ew       <- plot_scan_ny_facet(res_ny_ew,   "Crypto 48-Interval Scan in US Eastern Time: NYSE Trading Days vs. Weekends (Equal-Weighted)", sub_ny)

save_both <- function(plot_obj, stem, w = 12, h = 5) {
  ggsave(paste0("output/figures/", stem, ".pdf"), plot_obj, width = w, height = h)
  ggsave(paste0("output/figures/", stem, ".png"), plot_obj, width = w, height = h, dpi = 200)
}

save_both(p_skip_cap5,   "fig1_crypto_48scan_skip_cap5")
save_both(p_skip_vw,     "fig1_crypto_48scan_skip_vw_norm")
save_both(p_noskip_cap5, "figA1_crypto_48scan_noskip_cap5")
save_both(p_skip_ew,     "fig1b_crypto_48scan_skip_ew")
save_both(p_ny_facet,    "fig1c_crypto_48scan_ny_tradingday_vs_weekend_cap5", w = 12, h = 8)
save_both(p_ny_ew,       "fig1d_crypto_48scan_ny_tradingday_vs_weekend_ew",   w = 12, h = 8)

# ==============================================================================
# 6. AUSFÜHRLICHE KONSOLENAUSGABE (KERNINTERVALLE + TOP-5 EXTREM-SLOTS)
# ==============================================================================
fmt_out <- function(dt) {
  dt[, .(interval_id, label, beta = round(beta, 3), t_stat = round(t_stat, 2), n_obs)]
}

cat("\n================ KERNINTERVALLE UTC (30m SKIP, 5%-CAPPED-VW) ================\n")
print(fmt_out(res_skip_cap5[interval_id %in% c(1, 16, 32, 40, 41, 42, 47, 48)]))

cat("\n================ TOP-5 EXTREM-SLOTS UTC (5%-CAPPED-VW, nach |t|) ============\n")
print(fmt_out(res_skip_cap5[order(-abs(t_stat))][1:5]))

cat("\n================ TOP-5 EXTREM-SLOTS UTC (EQUAL-WEIGHTED, nach |t|) ==========\n")
print(fmt_out(res_skip_ew[order(-abs(t_stat))][1:5]))

cat("\n================ KERNINTERVALLE NY-TIME (5%-CAPPED-VW: OPEN & CLOSE) ========\n")
print(res_ny_cap5[interval_id %in% c(20, 31, 32, 33),
                  .(regime, interval_id, label, beta = round(beta, 3), t_stat = round(t_stat, 2), n_obs)])

cat("\n================ KERNINTERVALLE NY-TIME (EQUAL-WEIGHTED: OPEN & CLOSE) ======\n")
print(res_ny_ew[interval_id %in% c(20, 31, 32, 33),
                .(regime, interval_id, label, beta = round(beta, 3), t_stat = round(t_stat, 2), n_obs)])

cat("\nFertig! Alle Tabellen & Grafiken gespeichert in output/tables/ und output/figures/\n")