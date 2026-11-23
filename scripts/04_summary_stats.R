library(data.table)
library(arrow)

dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)

# ==============================================================================
# 1. PANEL LADEN & METADATEN DYNAMISCH AUSLESEN
# ==============================================================================
panel_path <- "data/processed/panel_30m_utc.parquet"
if (!file.exists(panel_path)) {
  stop("Panel nicht gefunden. Zuerst scripts/02_build_30m_panel.R ausführen.")
}
panel <- as.data.table(read_parquet(panel_path))

n_coins_total <- uniqueN(panel$symbol)
n_days_total  <- uniqueN(panel$date)
start_lbl     <- format(min(panel$date), "%b %Y")
end_lbl       <- format(max(panel$date), "%b %Y")

cat(sprintf("Geladen: %d Coins, %d Tage (%s bis %s UTC)\n",
            n_coins_total, n_days_total, min(panel$date), max(panel$date)))

# ==============================================================================
# 2. HILFSFUNKTION: ZEITREIHEN-DURCHSCHNITT DER QUERSCHNITTSSTATISTIKEN (TABLE 1)
# ==============================================================================
cs_summary <- function(dt, var_col, label_tex, label_txt, by_col = "date", scale_factor = 1) {
  valid <- dt[!is.na(get(var_col)) & is.finite(get(var_col))]
  x_dt  <- valid[, .(val = get(var_col) * scale_factor, grp = get(by_col))]

  cs <- x_dt[, .(
    m   = mean(val),
    sd  = sd(val),
    p10 = quantile(val, 0.10, names = FALSE),
    p50 = median(val),
    p90 = quantile(val, 0.90, names = FALSE)
  ), by = grp]

  data.table(
    Variable     = label_txt,
    Variable_tex = label_tex,
    Mean         = mean(cs$m, na.rm = TRUE),
    Std_Dev      = mean(cs$sd, na.rm = TRUE),
    P10          = mean(cs$p10, na.rm = TRUE),
    Median       = mean(cs$p50, na.rm = TRUE),
    P90          = mean(cs$p90, na.rm = TRUE),
    Days         = uniqueN(valid$date),
    N_Obs        = nrow(x_dt)
  )
}

# ==============================================================================
# 3. VARIABLEN FÜR TABLE 1 (UTC-ANKER 23:30-00:00 & GESAMTPANEL)
# ==============================================================================
dt_lh  <- panel[interval_id == 48L]
dt_slh <- panel[interval_id == 47L]

dt_lh[, tb_share := fifelse(quote_vol_30m > 0, (tb_qvol_30m / quote_vol_30m) * 100, NA_real_)]

tab1 <- rbindlist(list(
  cs_summary(dt_lh,  "ret_30m",         "LH (23:30--00:00 UTC, \\%)",            "LH (23:30-00:00 UTC, %)",            by_col = "date",   scale_factor = 100),
  cs_summary(dt_lh,  "ret_25m",         "LH ex-last-5m (23:30--23:55 UTC, \\%)", "LH ex-last-5m (23:30-23:55 UTC, %)", by_col = "date",   scale_factor = 100),
  cs_summary(dt_lh,  "ret_prev_skip",   "ROD3 (00:00--23:00 UTC, \\%)",          "ROD3 (00:00-23:00 UTC, %)",          by_col = "date",   scale_factor = 100),
  cs_summary(dt_lh,  "ret_prev_noskip", "ROD (No Skip, \\%)",                    "ROD (No Skip, %)",                   by_col = "date",   scale_factor = 100),
  cs_summary(dt_slh, "ret_30m",         "SLH (23:00--23:30 UTC, \\%)",           "SLH (23:00-23:30 UTC, %)",           by_col = "date",   scale_factor = 100),
  cs_summary(panel,  "ret_30m",         "All 30m Returns (\\%)",                 "All 30m Returns (%)",                by_col = "ts_30m", scale_factor = 100),
  cs_summary(dt_lh,  "rv_lag1d",        "Realized Variance$_{t-1}$ ($\\%^2$)",    "Realized Variance t-1 (%^2)",        by_col = "date",   scale_factor = 1e4),
  cs_summary(dt_lh,  "vol_lag1d",       "Daily Volume$_{t-1}$ (Mio. USDT)",      "Daily Volume t-1 (Mio. USDT)",       by_col = "date",   scale_factor = 1e-6),
  cs_summary(dt_lh,  "tb_share",        "LH Taker-Buy Share (\\%)",              "LH Taker-Buy Share (%)",             by_col = "date",   scale_factor = 1),
  cs_summary(dt_lh,  "n_trades_30m",    "LH Trades (in 1,000)",                  "LH Trades (in 1,000)",               by_col = "date",   scale_factor = 1e-3)
))

fwrite(tab1[, .(Variable, Mean, Std_Dev, P10, Median, P90, Days, N_Obs)],
       "output/tables/table1_summary_stats.csv")

# ==============================================================================
# 4. LATEX-EXPORT TABLE 1 (BOOKTABS-FORMAT, VOLLSTÄNDIG DYNAMISCH)
# ==============================================================================
fmt_num <- function(x) sprintf("%.2f", x)
fmt_int <- function(x) format(round(x), big.mark = ",", trim = TRUE, scientific = FALSE)

tex_lines <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  sprintf("\\caption{Summary Statistics: Binance Spot 30-Minute Crypto Panel (%s -- %s)}", start_lbl, end_lbl),
  "\\label{tab:summary_stats}",
  "\\resizebox{\\textwidth}{!}{%",
  "\\begin{tabular}{lrrrrrrr}",
  "\\toprule",
  "Variable & Mean & Std. Dev. & 10th & Median & 90th & Days & Obs. \\\\",
  "\\midrule"
)

for (i in seq_len(nrow(tab1))) {
  r <- tab1[i]
  tex_lines <- c(tex_lines, sprintf(
    "%s & %s & %s & %s & %s & %s & %s & %s \\\\",
    r$Variable_tex, fmt_num(r$Mean), fmt_num(r$Std_Dev),
    fmt_num(r$P10), fmt_num(r$Median), fmt_num(r$P90),
    fmt_int(r$Days), fmt_int(r$N_Obs)
  ))
}

tex_lines <- c(
  tex_lines,
  "\\bottomrule",
  "\\end{tabular}%",
  "}",
  "\\vspace{0.3em}",
  sprintf(
    "\\footnotesize{\\textit{Notes:} Time-series averages of cross-sectional summary statistics (analogous to Table 1 in Baltussen, Da \\& Soebhag, 2026). Sample: %d Binance Spot non-stablecoin USDT pairs (including delistings, excluding wrapped/staked peg duplicates) across %s days (%s -- %s), dynamic lagged volume filter ($\\ge 500$k USDT and above daily 10th percentile), 1\\%%/99\\%% cross-sectional winsorization. Realized Variance$_{t-1}$ is the daily sum of squared 5-minute returns scaled by $10^4$ ($\\%%^2$).}",
    n_coins_total, fmt_int(n_days_total), start_lbl, end_lbl
  ),
  "\\end{table}"
)
writeLines(tex_lines, "output/tables/table1_summary_stats.tex")

# ==============================================================================
# 5. ZUSATZTABELLE: GEWICHTUNGS- & KONZENTRATIONSDIAGNOSE (LATEX)
# ==============================================================================
conc_path <- "output/tables/weight_concentration_daily.csv"
if (file.exists(conc_path)) {
  dc <- fread(conc_path)
  conc_tab <- data.table(
    Metric_tex = c(
      "Active Coins per Day ($N_t$)",
      "Effective $N$ (Raw-VW, $1/\\sum w_i^2$)",
      "Effective $N$ (5\\%-Capped-VW)",
      "BTC Weight in Raw-VW (\\%)",
      "ETH Weight in Raw-VW (\\%)",
      "Top-2 Coins Weight in Raw-VW (\\%)",
      "Top-5 Coins Weight in Raw-VW (\\%)",
      "Top-5 Coins Weight in 5\\%-Capped-VW (\\%)"
    ),
    Mean   = c(mean(dc$n_coins), mean(dc$neff_raw), mean(dc$neff_cap5),
               mean(dc$btc_raw_pct), mean(dc$eth_raw_pct),
               mean(dc$top2_raw_pct), mean(dc$top5_raw_pct), mean(dc$top5_cap5_pct)),
    Median = c(median(dc$n_coins), median(dc$neff_raw), median(dc$neff_cap5),
               median(dc$btc_raw_pct), median(dc$eth_raw_pct),
               median(dc$top2_raw_pct), median(dc$top5_raw_pct), median(dc$top5_cap5_pct)),
    P90    = c(quantile(dc$n_coins, 0.90), quantile(dc$neff_raw, 0.90), quantile(dc$neff_cap5, 0.90),
               quantile(dc$btc_raw_pct, 0.90), quantile(dc$eth_raw_pct, 0.90),
               quantile(dc$top2_raw_pct, 0.90), quantile(dc$top5_raw_pct, 0.90), quantile(dc$top5_cap5_pct, 0.90)),
    Max    = c(max(dc$n_coins), max(dc$neff_raw), max(dc$neff_cap5),
               max(dc$btc_raw_pct), max(dc$eth_raw_pct),
               max(dc$top2_raw_pct), max(dc$top5_raw_pct), max(dc$top5_cap5_pct))
  )

  ctex <- c(
    "\\begin{table}[htbp]",
    "\\centering",
    sprintf("\\caption{Cross-Sectional Concentration under Raw vs. 5\\%%-Capped Volume Weighting (%s -- %s)}", start_lbl, end_lbl),
    "\\label{tab:weight_concentration}",
    "\\begin{tabular}{lrrrr}",
    "\\toprule",
    "Metric & Mean & Median & 90th & Max \\\\",
    "\\midrule"
  )
  for (i in seq_len(nrow(conc_tab))) {
    r <- conc_tab[i]
    ctex <- c(ctex, sprintf("%s & %.1f & %.1f & %.1f & %.1f \\\\",
                            r$Metric_tex, r$Mean, r$Median, r$P90, r$Max))
  }
  ctex <- c(
    ctex,
    "\\bottomrule",
    "\\end{tabular}",
    "\\vspace{0.3em}",
    sprintf(
      "\\footnotesize{\\textit{Notes:} Daily cross-sectional concentration across %s active trading days (%d unique coins) based on lagged daily USDT volume. 5\\%%-Capped-VW applies iterative water-filling capping so that no single asset exceeds 5\\%% weight while daily weights sum to 1.}",
      fmt_int(nrow(dc)), n_coins_total
    ),
    "\\end{table}"
  )
  writeLines(ctex, "output/tables/table1b_weight_concentration.tex")
} else {
  warning("weight_concentration_daily.csv nicht gefunden -> zuerst 02_build_30m_panel.R ausführen.")
}

# ==============================================================================
# 6. KONSOLENAUSGABE
# ==============================================================================
cat("\n======================== TABLE 1: SUMMARY STATISTICS ========================\n")
print(tab1[, .(
  Variable,
  Mean    = round(Mean, 2),
  Std_Dev = round(Std_Dev, 2),
  P10     = round(P10, 2),
  Median  = round(Median, 2),
  P90     = round(P90, 2),
  Days,
  N_Obs
)])
cat("=============================================================================\n")
cat("LaTeX-Dateien gespeichert unter:\n")
cat("  - output/tables/table1_summary_stats.tex\n")
cat("  - output/tables/table1b_weight_concentration.tex\n")