library(data.table)
library(arrow)
library(fixest)
library(ggplot2)
library(timeDate)

dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)

# ==============================================================================
# 1. PANEL LADEN & NYSE-HANDELSKALENDER (IDENTISCH ZU SKRIPT 03 & 05)
# ==============================================================================
panel_path <- "data/processed/panel_30m_utc.parquet"
if (!file.exists(panel_path)) {
  stop("Panel nicht gefunden. Zuerst scripts/02_build_30m_panel.R ausführen.")
}
panel <- as.data.table(read_parquet(panel_path))

n_coins_total <- uniqueN(panel$symbol)
start_lbl     <- format(min(panel$date), "%b %Y")
end_lbl       <- format(max(panel$date), "%b %Y")
ETF_DATE      <- as.Date("2024-01-11") # Start US Spot Bitcoin ETFs

yrs <- seq(as.integer(format(min(panel$date), "%Y")),
           as.integer(format(max(panel$date), "%Y")))

nyse_holidays <- unique(c(
  as.Date(holidayNYSE(yrs)),
  as.Date(c(
    "2022-06-20", "2023-06-19", "2024-06-19", "2025-06-19", "2026-06-19",
    "2025-01-09"
  ))
))
nyse_early_closes <- unique(c(
  as.Date(USThanksgivingDay(yrs)) + 1L,
  as.Date(sprintf("%d-07-03", yrs)),
  as.Date(sprintf("%d-12-24", yrs))
))

ts_map <- data.table(ts_30m = unique(panel$ts_30m))
lt_ny  <- as.POSIXlt(ts_map$ts_30m, tz = "America/New_York")
ts_map[, `:=`(
  date_ny        = as.Date(sprintf("%04d-%02d-%02d", lt_ny$year + 1900L, lt_ny$mon + 1L, lt_ny$mday)),
  interval_id_ny = lt_ny$hour * 2L + (lt_ny$min %/% 30L) + 1L,
  wday_ny        = lt_ny$wday,
  isdst_ny       = lt_ny$isdst
)]
dst_dates <- ts_map[, .(n_dst = uniqueN(isdst_ny)), by = date_ny][n_dst > 1L, date_ny]
ts_map[, ny_regime := fcase(
  date_ny %in% dst_dates,                                                 "DST_Transition",
  wday_ny %in% 1:5 & !(date_ny %in% c(nyse_holidays, nyse_early_closes)), "US_Trading_Day",
  wday_ny %in% c(0L, 6L),                                                 "Weekend",
  default = "US_Holiday_or_EarlyClose"
)]

panel[ts_map, on = "ts_30m", `:=`(
  date_ny        = i.date_ny,
  interval_id_ny = i.interval_id_ny,
  ny_regime      = i.ny_regime
)]
rm(ts_map, lt_ny)

# Beide Ziel-Subsets extrahieren
dt_utc <- panel[interval_id == 48L,
                .(symbol, dt_day = date, ret_30m, ret_prev_skip, w_vw_cap5)]
dt_ny  <- panel[interval_id_ny == 32L & ny_regime == "US_Trading_Day",
                .(symbol, dt_day = date_ny, ret_30m, ret_prev_skip, w_vw_cap5)]

# ==============================================================================
# 2. HILFSFUNKTION: 1-JAHRES-ROLLING PANEL-REGRESSION (ANALOG FIGURE 2)
# ==============================================================================
run_rolling_panel <- function(dt, anchor_label, window_days = 365L, step_days = 14L) {
  d <- dt[is.finite(ret_30m) & is.finite(ret_prev_skip) & is.finite(w_vw_cap5) & w_vw_cap5 > 0]
  setkey(d, dt_day)

  min_d <- min(d$dt_day)
  max_d <- max(d$dt_day)
  eval_dates <- unique(c(seq(min_d + window_days, max_d, by = step_days), max_d))

  res_list <- vector("list", length(eval_dates))
  for (i in seq_along(eval_dates)) {
    end_d   <- eval_dates[i]
    start_d <- end_d - window_days + 1L
    sub     <- d[dt_day >= start_d & dt_day <= end_d]

    mod <- feols(ret_30m ~ ret_prev_skip | symbol + dt_day,
                 data = sub, weights = ~w_vw_cap5,
                 cluster = ~symbol + dt_day, notes = FALSE)

    b <- unname(coef(mod)[1]) * 100
    s <- unname(se(mod)[1]) * 100
    t_val <- unname(tstat(mod)[1])

    res_list[[i]] <- data.table(
      anchor   = anchor_label,
      end_date = end_d,
      beta     = b,
      se       = s,
      t_stat   = t_val,
      n_days   = uniqueN(sub$dt_day),
      n_obs    = nobs(mod)
    )
  }
  rbindlist(res_list)
}

cat("Schätze 1-Jahres-Rolling Panel-Regressionen (365 Tage, 5%-Capped-VW)...\n")
roll_utc <- run_rolling_panel(dt_utc, "00:00 UTC Close (23:30-00:00 UTC)")
roll_ny  <- run_rolling_panel(dt_ny,  "US Equity Close (15:30-16:00 NY, NYSE Days)")
roll_all <- rbindlist(list(roll_utc, roll_ny))

fwrite(roll_all, "output/tables/figure2_rolling_1y_cap5.csv")

# ==============================================================================
# 3. PLOT: FIGURE 2 (ROLLING SLOPE & T-STATISTIK, INKL. SPOT-BTC-ETF-LINIE)
# ==============================================================================
plot_long <- melt(
  roll_all,
  id.vars       = c("anchor", "end_date"),
  measure.vars  = c("beta", "t_stat"),
  variable.name = "metric",
  value.name    = "val"
)

metric_lvls <- c("Slope Coefficient (x 100)", "t-Statistic (2-Way Clustered)")
plot_long[, metric := factor(
  fifelse(metric == "beta", metric_lvls[1], metric_lvls[2]),
  levels = metric_lvls
)]
plot_long[, anchor := factor(anchor, levels = c(
  "00:00 UTC Close (23:30-00:00 UTC)",
  "US Equity Close (15:30-16:00 NY, NYSE Days)"
))]

# Referenzlinien pro Facet-Zeile (0 überall; +-1.96 im t-Statistik-Panel)
hline_dt <- data.table(
  metric = factor(c(metric_lvls[1], metric_lvls[2], metric_lvls[2], metric_lvls[2]),
                  levels = metric_lvls),
  yint   = c(0, 0, -1.96, 1.96),
  ltype  = c("dashed", "dashed", "dotted", "dotted"),
  col    = c("grey30", "grey30", "#D7191C", "#D7191C")
)

p_roll <- ggplot(plot_long, aes(x = end_date, y = val)) +
  # Schattierte Übergangsphase, bis das 1-Jahres-Fenster zu 100% Post-ETF ist
  annotate("rect", xmin = ETF_DATE, xmax = ETF_DATE + 365L,
           ymin = -Inf, ymax = Inf, fill = "#2C7BB6", alpha = 0.08) +
  geom_vline(xintercept = ETF_DATE, linetype = "dashed", color = "#2C7BB6", linewidth = 0.65) +
  geom_hline(data = hline_dt, aes(yintercept = yint, linetype = ltype, color = col),
             linewidth = 0.5, show.legend = FALSE) +
  geom_line(color = "black", linewidth = 0.75) +
  facet_grid(metric ~ anchor, scales = "free_y") +
  scale_linetype_identity() +
  scale_color_identity() +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(
    title    = "Time-Series Persistence: 1-Year Rolling Panel Regressions (Analog Figure 2)",
    subtitle = sprintf(
      "Binance Spot (%d Coins, %s bis %s | 5%%-Capped-VW, 30m Skip) | Blau gestrichelt: US Spot BTC-ETF-Start (11. Jan 2024); rot punktiert: t = +-1,96",
      n_coins_total, start_lbl, end_lbl
    ),
    x        = "Ende des 365-Tage-Rolling-Fensters",
    y        = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title       = element_text(face = "bold", size = 12),
    plot.subtitle    = element_text(size = 9, color = "grey30"),
    strip.text       = element_text(face = "bold", size = 10),
    panel.grid.minor = element_blank(),
    panel.border     = element_rect(color = "grey40", fill = NA, linewidth = 0.5)
  )

ggsave("output/figures/fig2_rolling_persistence_cap5.pdf", p_roll, width = 12, height = 6.8)
ggsave("output/figures/fig2_rolling_persistence_cap5.png", p_roll, width = 12, height = 6.8, dpi = 200)

# ==============================================================================
# 4. ZUSATZDIAGNOSE: PRE- VS. POST-SPOT-BTC-ETF & KALENDERJAHRE (2020-2026)
# ==============================================================================
run_regime_tests <- function(dt, anchor_label) {
  d <- copy(dt[is.finite(ret_30m) & is.finite(ret_prev_skip) & is.finite(w_vw_cap5) & w_vw_cap5 > 0])
  d[, post_etf := as.integer(dt_day >= ETF_DATE)]
  d[, year     := as.integer(format(dt_day, "%Y"))]

  # (A) Separate Subsample-Schätzungen: Pre-ETF vs. Post-ETF
  m_pre  <- feols(ret_30m ~ ret_prev_skip | symbol + dt_day,
                  data = d[post_etf == 0L], weights = ~w_vw_cap5,
                  cluster = ~symbol + dt_day, notes = FALSE)
  m_post <- feols(ret_30m ~ ret_prev_skip | symbol + dt_day,
                  data = d[post_etf == 1L], weights = ~w_vw_cap5,
                  cluster = ~symbol + dt_day, notes = FALSE)

  # (B) Gepoolter Interaktionstest auf Strukturbruch (Post minus Pre)
  # (Haupteffekt von post_etf wird durch dt_day-FE absorbiert)
  m_diff <- feols(ret_30m ~ ret_prev_skip + ret_prev_skip:post_etf | symbol + dt_day,
                  data = d, weights = ~w_vw_cap5,
                  cluster = ~symbol + dt_day, notes = FALSE)

  etf_tab <- data.table(
    anchor = anchor_label,
    period = c("Pre-ETF (< 2024-01-11)", "Post-ETF (>= 2024-01-11)", "Diff (Post - Pre)"),
    beta   = c(unname(coef(m_pre)[1]), unname(coef(m_post)[1]), unname(coef(m_diff)[2])) * 100,
    t_stat = c(unname(tstat(m_pre)[1]), unname(tstat(m_post)[1]), unname(tstat(m_diff)[2])),
    n_days = c(uniqueN(d[post_etf == 0L, dt_day]), uniqueN(d[post_etf == 1L, dt_day]), uniqueN(d$dt_day))
  )

  # (C) Einzelne Kalenderjahre (2020-2026)
  yr_list <- lapply(sort(unique(d$year)), function(y) {
    sub <- d[year == y]
    m <- feols(ret_30m ~ ret_prev_skip | symbol + dt_day,
               data = sub, weights = ~w_vw_cap5,
               cluster = ~symbol + dt_day, notes = FALSE)
    data.table(
      anchor = anchor_label,
      year   = y,
      beta   = unname(coef(m)[1]) * 100,
      se     = unname(se(m)[1]) * 100,
      t_stat = unname(tstat(m)[1]),
      n_days = uniqueN(sub$dt_day)
    )
  })

  list(etf = etf_tab, yearly = rbindlist(yr_list))
}

diag_utc <- run_regime_tests(dt_utc, "00:00 UTC Close")
diag_ny  <- run_regime_tests(dt_ny,  "US Equity Close")

etf_all    <- rbindlist(list(diag_utc$etf, diag_ny$etf))
yearly_all <- rbindlist(list(diag_utc$yearly, diag_ny$yearly))

fwrite(etf_all,    "output/tables/regime_pre_post_etf_cap5.csv")
fwrite(yearly_all, "output/tables/yearly_coefficients_cap5.csv")

cat("\n================ STRUKTURBRUCH-TEST: PRE- VS. POST-SPOT-BTC-ETF ================\n")
print(etf_all[, .(anchor, period, beta = round(beta, 3), t_stat = round(t_stat, 2), n_days)])

cat("\n================ KALENDERJAHRE (2020-2026, 5%-CAPPED-VW) =======================\n")
print(yearly_all[, .(anchor, year, beta = round(beta, 3), t_stat = round(t_stat, 2), n_days)])

cat("\nFertig! Gespeichert:\n")
cat("  - output/figures/fig2_rolling_persistence_cap5.pdf & .png\n")
cat("  - output/tables/figure2_rolling_1y_cap5.csv\n")
cat("  - output/tables/regime_pre_post_etf_cap5.csv & yearly_coefficients_cap5.csv\n")