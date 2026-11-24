library(data.table)
library(arrow)
library(ggplot2)
library(timeDate)

dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)

# ==============================================================================
# 1. PANEL LADEN & NYSE-HANDELSKALENDER (IDENTISCH ZU SKRIPT 03)
# ==============================================================================
panel_path <- "data/processed/panel_30m_utc.parquet"
if (!file.exists(panel_path)) {
  stop("Panel nicht gefunden. Zuerst scripts/02_build_30m_panel.R ausführen.")
}
panel <- as.data.table(read_parquet(panel_path))

n_coins_total <- uniqueN(panel$symbol)
start_lbl     <- format(min(panel$date), "%b %Y")
end_lbl       <- format(max(panel$date), "%b %Y")

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

# ==============================================================================
# 2. HILFSFUNKTION: NEWEY-WEST (HAC) OLS (IDENTISCH ZU sandwich::NeweyWest)
# ==============================================================================
nw_ols <- function(y, x = NULL, lag = NULL) {
  ok  <- is.finite(y) & (if (is.null(x)) TRUE else is.finite(x))
  y   <- y[ok]
  T_n <- length(y)
  X   <- if (is.null(x)) matrix(1, nrow = T_n, ncol = 1L) else cbind(1, x[ok])
  k   <- ncol(X)

  # Newey-West (1994) Plug-in-Regel: floor(4 * (T/100)^(2/9)) -> ca. 7-8 Lags
  if (is.null(lag)) lag <- floor(4 * (T_n / 100)^(2 / 9))

  XtX_inv <- solve(crossprod(X))
  beta    <- as.numeric(XtX_inv %*% crossprod(X, y))
  e       <- y - as.numeric(X %*% beta)
  U       <- X * e

  S <- crossprod(U)
  if (lag > 0L) {
    for (l in seq_len(lag)) {
      w_l <- 1 - l / (lag + 1)
      G_l <- crossprod(U[(l + 1L):T_n, , drop = FALSE],
                       U[1L:(T_n - l), , drop = FALSE])
      S   <- S + w_l * (G_l + t(G_l))
    }
  }
  V    <- (T_n / (T_n - k)) * (XtX_inv %*% S %*% XtX_inv)
  se   <- sqrt(diag(V))
  tval <- beta / se

  list(est = beta[1L], se = se[1L], t = tval[1L], lag = lag, n = T_n)
}

stars_fn <- function(t_val) {
  fcase(
    abs(t_val) >= 2.576, "$^{***}$",
    abs(t_val) >= 1.960, "$^{**}$",
    abs(t_val) >= 1.645, "$^{*}$",
    default = ""
  )
}

# ==============================================================================
# 3. KERNFUNKTION: UNIVARIATE QUINTIL-SORTS (L, 2, 3, 4, H, L-H)
# ==============================================================================
run_univariate_sort <- function(dt_anchor, anchor_name,
                                ret_col = "ret_30m_raw",
                                sort_col = "ret_prev_skip_raw",
                                date_col = "date",
                                ann_factor = 365) {

  d <- dt_anchor[is.finite(get(ret_col)) & is.finite(get(sort_col)) & vol_lag1d > 0,
                 .(symbol,
                   dt_day = get(date_col),
                   r_hold = get(ret_col),
                   r_sort = get(sort_col),
                   vol_lag1d,
                   w_vw_cap5)]

  # Defensiver Check: mind. 25 Coins pro Tag (5 pro Quintil)
  d[, n_day := .N, by = dt_day]
  d <- d[n_day >= 25L]

  # Tägliche Quintil-Zuordnung per Querschnittsrang (1 = L / Losers, 5 = H / Winners)
  d[, q := pmin(pmax(as.integer(ceiling(frank(r_sort, ties.method = "average") * 5L / .N)), 1L), 5L),
    by = dt_day]

  # Zeitgleicher 30m-Krypto-Marktreturn pro Tag & Gewichtung (für CAPM-Alpha)
  mkt_dt <- d[, .(
    mkt_cap5 = sum(w_vw_cap5 * r_hold) / sum(w_vw_cap5),
    mkt_vw   = sum(vol_lag1d * r_hold) / sum(vol_lag1d),
    mkt_ew   = mean(r_hold)
  ), by = dt_day]

  # Tägliche Quintil-Renditen (Gewichte pro Quintil sauber auf 1 normiert)
  q_daily <- d[, .(
    ret_cap5 = sum(w_vw_cap5 * r_hold) / sum(w_vw_cap5),
    ret_vw   = sum(vol_lag1d * r_hold) / sum(vol_lag1d),
    ret_ew   = mean(r_hold),
    n_coins  = .N
  ), by = .(dt_day, q)]

  # Long-Short Spread: L - H (Quintil 1 minus Quintil 5, exakt wie Table 2)
  q_wide <- dcast(q_daily, dt_day ~ q, value.var = c("ret_cap5", "ret_vw", "ret_ew"))
  ls_daily <- q_wide[, .(
    dt_day   = dt_day,
    q        = 6L, # 6L = "L-H"
    ret_cap5 = ret_cap5_1 - ret_cap5_5,
    ret_vw   = ret_vw_1   - ret_vw_5,
    ret_ew   = ret_ew_1   - ret_ew_5,
    n_coins  = NA_integer_
  )]

  all_daily <- rbindlist(list(q_daily, ls_daily), use.names = TRUE)
  all_daily <- merge(all_daily, mkt_dt, by = "dt_day", all.x = TRUE)
  setorder(all_daily, q, dt_day)

  q_labels <- c("L", "2", "3", "4", "H", "L-H")
  all_daily[, portfolio := factor(q_labels[q], levels = q_labels)]
  all_daily[, anchor := anchor_name]

  # Zeitreihen-Statistiken (Mean R in bps/Tag, CAPM-Alpha in bps/Tag, NW t-Stats, Sharpe)
  schemes <- list(
    list(name = "Capped-VW (5%)", r_col = "ret_cap5", m_col = "mkt_cap5"),
    list(name = "Raw-VW",         r_col = "ret_vw",   m_col = "mkt_vw"),
    list(name = "Equal-Weighted", r_col = "ret_ew",   m_col = "mkt_ew")
  )

  stats_list <- list()
  for (sc in schemes) {
    for (p_lbl in q_labels) {
      sub <- all_daily[portfolio == p_lbl]
      r_bps <- sub[[sc$r_col]] * 1e4  # Basispunkte pro Tag (wie Table 2 im Paper)
      m_bps <- sub[[sc$m_col]] * 1e4

      res_mean <- nw_ols(r_bps)
      res_capm <- nw_ols(r_bps, m_bps)
      sr_ann   <- (mean(r_bps, na.rm = TRUE) / sd(r_bps, na.rm = TRUE)) * sqrt(ann_factor)

      stats_list[[length(stats_list) + 1L]] <- data.table(
        anchor    = anchor_name,
        scheme    = sc$name,
        portfolio = p_lbl,
        R_bps     = res_mean$est,
        se_R      = res_mean$se,
        t_R       = res_mean$t,
        Alpha_bps = res_capm$est,
        se_Alpha  = res_capm$se,
        t_Alpha   = res_capm$t,
        SR_ann    = sr_ann,
        n_days    = res_mean$n,
        nw_lag    = res_mean$lag
      )
    }
  }

  list(summary = rbindlist(stats_list), daily = all_daily)
}

# ==============================================================================
# 4. BERECHNUNG FÜR BEIDE ANKER (HAUPT: ROH-RENDITEN + ROBUSTHEIT: WINSORIZED)
# ==============================================================================
cat("Berechne Univariate Quintil-Sorts: 00:00 UTC Close (23:30-00:00 UTC)...\n")
res_utc_raw <- run_univariate_sort(
  panel[interval_id == 48L],
  anchor_name = "00:00 UTC Close (All Days)",
  ret_col     = "ret_30m_raw",
  sort_col    = "ret_prev_skip_raw",
  date_col    = "date",
  ann_factor  = 365
)

cat("Berechne Univariate Quintil-Sorts: US Equity Close (15:30-16:00 NY, NYSE-Handelstage)...\n")
res_ny_raw <- run_univariate_sort(
  panel[interval_id_ny == 32L & ny_regime == "US_Trading_Day"],
  anchor_name = "US Equity Close (15:30-16:00 NY, NYSE Days)",
  ret_col     = "ret_30m_raw",
  sort_col    = "ret_prev_skip_raw",
  date_col    = "date_ny",
  ann_factor  = 252
)

cat("Berechne Robustheitscheck mit 1%/99%-winsorisierten Renditen (00:00 UTC)...\n")
res_utc_win <- run_univariate_sort(
  panel[interval_id == 48L],
  anchor_name = "00:00 UTC Close (Winsorized Check)",
  ret_col     = "ret_30m",
  sort_col    = "ret_prev_skip",
  date_col    = "date",
  ann_factor  = 365
)

summary_all <- rbindlist(list(res_utc_raw$summary, res_ny_raw$summary, res_utc_win$summary))
daily_all   <- rbindlist(list(res_utc_raw$daily, res_ny_raw$daily))

fwrite(summary_all, "output/tables/table2_univariate_sorts.csv")
fwrite(daily_all,   "output/tables/univariate_sorts_daily.csv")

# ==============================================================================
# 5. LATEX-EXPORT: TABLE 2 (ANALOG BALTUSSEN, DA & SOEBHAG 2026)
# ==============================================================================
build_panel_tex <- function(sum_dt, panel_title) {
  q_order <- c("L", "2", "3", "4", "H", "L-H")
  sc_order <- c("Capped-VW (5%)", "Raw-VW", "Equal-Weighted")
  lines <- c(
    sprintf("\\multicolumn{7}{l}{\\textit{%s}} \\\\", panel_title),
    "\\midrule"
  )

  for (p in q_order) {
    if (p == "L-H") lines <- c(lines, "\\midrule")
    vals_est <- character(0)
    vals_t   <- character(0)

    for (sc in sc_order) {
      row <- sum_dt[scheme == sc & portfolio == p]
      vals_est <- c(
        vals_est,
        sprintf("%.2f%s", row$R_bps, stars_fn(row$t_R)),
        sprintf("%.2f%s", row$Alpha_bps, stars_fn(row$t_Alpha))
      )
      vals_t <- c(
        vals_t,
        sprintf("(%.2f)", row$t_R),
        sprintf("(%.2f)", row$t_Alpha)
      )
    }

    p_disp <- if (p == "L-H") "\\textbf{L -- H}" else p
    lines <- c(
      lines,
      paste0(p_disp, " & ", paste(vals_est, collapse = " & "), " \\\\"),
      paste0(" & ", paste(vals_t, collapse = " & "), " \\\\")
    )
  }

  # Annualisierte Sharpe Ratio für L-H als letzte Zeile im Panel
  sr_cells <- vapply(sc_order, function(sc) {
    sr <- sum_dt[scheme == sc & portfolio == "L-H", SR_ann]
    sprintf("\\multicolumn{2}{c}{%.2f}", sr)
  }, character(1))
  lines <- c(
    lines,
    paste0("L -- H Sharpe (ann.) & ", paste(sr_cells, collapse = " & "), " \\\\")
  )
  lines
}

tex_tab2 <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  sprintf("\\caption{Univariate Portfolio Sorts on Prior 23-Hour Returns (ROD3, %s -- %s)}", start_lbl, end_lbl),
  "\\label{tab:univariate_sorts}",
  "\\resizebox{\\textwidth}{!}{%",
  "\\begin{tabular}{lcccccc}",
  "\\toprule",
  " & \\multicolumn{2}{c}{5\\%-Capped-VW} & \\multicolumn{2}{c}{Raw-VW} & \\multicolumn{2}{c}{Equal-Weighted} \\\\",
  "\\cmidrule(lr){2-3} \\cmidrule(lr){4-5} \\cmidrule(lr){6-7}",
  "Quintile & $R$ (bps) & $\\alpha_{\\text{MKT}}$ (bps) & $R$ (bps) & $\\alpha_{\\text{MKT}}$ (bps) & $R$ (bps) & $\\alpha_{\\text{MKT}}$ (bps) \\\\",
  "\\midrule",
  build_panel_tex(res_utc_raw$summary,
                  sprintf("Panel A: Crypto-Native 00:00 UTC Close (23:30--00:00 UTC, N = %d days)",
                          res_utc_raw$summary$n_days[1])),
  "\\midrule",
  build_panel_tex(res_ny_raw$summary,
                  sprintf("Panel B: US Equity Close (15:30--16:00 NY, NYSE Trading Days, N = %d days)",
                          res_ny_raw$summary$n_days[1])),
  "\\bottomrule",
  "\\end{tabular}%",
  "}",
  "\\vspace{0.3em}",
  sprintf(
    "\\footnotesize{\\textit{Notes:} Daily cross-sectional quintile sorts on prior 23-hour returns with a 30-minute skip (ROD3). Portfolio returns are measured in basis points per day using unwinsorized 30-minute returns across %d Binance Spot USDT pairs (%s -- %s). $\\alpha_{\\text{MKT}}$ is the intercept from regressing portfolio returns on the contemporaneous 30-minute crypto market return. Newey-West $t$-statistics are reported in parentheses. $^{***}$, $^{**}$, and $^{*}$ denote significance at the 1\\%%, 5\\%%, and 10\\%% levels.}",
    n_coins_total, start_lbl, end_lbl
  ),
  "\\end{table}"
)
writeLines(tex_tab2, "output/tables/table2_univariate_sorts.tex")

# ==============================================================================
# 6. PRÄSENTATIONSGRAFIK: QUINTIL-RENDITEN (5%-CAPPED-VW & EW)
# ==============================================================================
plot_dt <- rbindlist(list(res_utc_raw$summary, res_ny_raw$summary))[
  portfolio != "L-H" & scheme %in% c("Capped-VW (5%)", "Equal-Weighted")
]
plot_dt[, `:=`(
  portfolio = factor(portfolio, levels = c("L", "2", "3", "4", "H")),
  ci_low    = R_bps - 1.96 * se_R,
  ci_high   = R_bps + 1.96 * se_R
)]

p_bars <- ggplot(plot_dt, aes(x = portfolio, y = R_bps, fill = portfolio)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey30", linewidth = 0.5) +
  geom_col(width = 0.65, alpha = 0.88, color = "black", linewidth = 0.3) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.2, linewidth = 0.45) +
  facet_grid(scheme ~ anchor, scales = "free_y") +
  scale_fill_manual(values = c("L" = "#D7191C", "2" = "#FDAE61", "3" = "#D9D9D9",
                               "4" = "#ABD9E9", "H" = "#2C7BB6"), guide = "none") +
  labs(
    title    = "Univariate Quintile Sorts: Mean 30-Minute Returns by Prior 23h Performance (ROD3)",
    subtitle = sprintf("Binance Spot (%d Coins, %s bis %s) | Roh-Renditen in bps/Tag mit 95%% Newey-West-KI",
                       n_coins_total, start_lbl, end_lbl),
    x        = "ROD3 Quintil (L = Tagesverlierer, H = Tagesgewinner)",
    y        = "Mittlere 30m-Rendite (bps / Tag)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title       = element_text(face = "bold", size = 12),
    plot.subtitle    = element_text(size = 9.5, color = "grey30"),
    strip.text       = element_text(face = "bold", size = 10),
    panel.grid.minor = element_blank(),
    panel.border     = element_rect(color = "grey40", fill = NA, linewidth = 0.5)
  )

ggsave("output/figures/fig2_univariate_sorts_bars.pdf", p_bars, width = 11, height = 6.5)
ggsave("output/figures/fig2_univariate_sorts_bars.png", p_bars, width = 11, height = 6.5, dpi = 200)

# ==============================================================================
# 7. KONSOLENAUSGABE
# ==============================================================================
cat("\n================ TABLE 2: 00:00 UTC CLOSE (RAW RETURNS, bps/Tag) ================\n")
print(res_utc_raw$summary[, .(
  scheme, portfolio,
  R_bps = round(R_bps, 2), t_R = round(t_R, 2),
  Alpha_bps = round(Alpha_bps, 2), t_Alpha = round(t_Alpha, 2),
  SR_ann = round(SR_ann, 2)
)])

cat("\n================ TABLE 2: US EQUITY CLOSE (NYSE DAYS, bps/Tag) ==================\n")
print(res_ny_raw$summary[, .(
  scheme, portfolio,
  R_bps = round(R_bps, 2), t_R = round(t_R, 2),
  Alpha_bps = round(Alpha_bps, 2), t_Alpha = round(t_Alpha, 2),
  SR_ann = round(SR_ann, 2)
)])

cat("\n================ ROBUSTHEIT: 00:00 UTC CLOSE (WINSORIZED, bps/Tag) ==============\n")
print(res_utc_win$summary[portfolio == "L-H", .(
  scheme, portfolio,
  R_bps = round(R_bps, 2), t_R = round(t_R, 2),
  Alpha_bps = round(Alpha_bps, 2), t_Alpha = round(t_Alpha, 2)
)])

cat("\nFertig! Gespeichert:\n")
cat("  - output/tables/table2_univariate_sorts.tex & .csv\n")
cat("  - output/figures/fig2_univariate_sorts_bars.pdf & .png\n")