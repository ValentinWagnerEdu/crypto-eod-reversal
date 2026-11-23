library(data.table)
library(arrow)

dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)

# ==============================================================================
# 1. PARAMETER FÜR DYNAMISCHEN FILTER (STRENG LAG t-1 -> KEIN LOOK-AHEAD-BIAS)
# ==============================================================================
MIN_DAYS_COIN     <- 30L      # Coin muss mind. 30 Tage gehandelt worden sein
MIN_VOL_USDT      <- 500000   # Mindest-Vortagesvolumen: 500.000 USDT
DROP_BOTTOM_PCT   <- 0.10     # Täglich unterste 10% nach Vortagesvolumen raus (analog NYSE 10%)
MIN_COINS_PER_DAY <- 25L      # Mind. 25 liquide Coins pro Tag für saubere Querschnitte/Quintile
CAP_WEIGHT        <- 0.05     # 5%-Cap für konzentrationsbereinigte VW-Gewichte

# Zusätzlicher Sicherheitsfilter für lokal bereits heruntergeladene Wrapped/Staked-Tokens
EXCLUDED_WRAPPED <- c(
  "WBTC", "BTCB", "RENBTC", "WBETH", "BETH", "WETH", "STETH", "WSTETH",
  "CBETH", "EZETH", "BNSOL", "MSOL", "JITOSOL"
)

files <- list.files("data/processed/coins_5m", pattern = "\\.parquet$", full.names = TRUE)
if (length(files) == 0L) {
  stop("Keine Parquet-Dateien in data/processed/coins_5m/ gefunden.")
}
cat("Verarbeite", length(files), "Coins aus data/processed/coins_5m/ ...\n")

# Exaktes iteratives Capping (Water-Filling): kein Coin > cap, Summe exakt = 1
cap_weights <- function(vol, cap = 0.05) {
  n <- length(vol)
  if (n == 0L) return(numeric(0))
  if (n * cap <= 1.0) return(rep(1 / n, n))
  w <- vol / sum(vol)
  fixed <- rep(FALSE, n)
  repeat {
    newly_over <- (!fixed) & (w > cap + 1e-12)
    if (!any(newly_over)) break
    fixed[newly_over] <- TRUE
    w[fixed] <- cap
    rem <- 1.0 - sum(w[fixed])
    free <- !fixed
    if (!any(free) || sum(vol[free]) <= 0) break
    w[free] <- rem * (vol[free] / sum(vol[free]))
  }
  w / sum(w)
}

# ==============================================================================
# 2. AGGREGATION 5m -> LÜCKENLOSES 30m-PANEL PRO COIN (INKL. 5m-RV & 25m-CLOSE)
# ==============================================================================
build_coin_30m <- function(f) {
  dt <- as.data.table(read_parquet(f))
  if (nrow(dt) < 288L * MIN_DAYS_COIN) return(NULL)
  stopifnot(uniqueN(dt$symbol) == 1L)

  sym_name <- dt$symbol[1L]
  base_sym <- sub("USDT$", "", sym_name)
  if (base_sym %in% EXCLUDED_WRAPPED) return(NULL)

  setorder(dt, open_time_utc)

  # --- A) 5m-Renditen & tägliche Realized Variance (strikt auf 300s-Abständen) ---
  ts_num <- as.numeric(dt$open_time_utc)
  dt[, `:=`(
    dt_sec    = ts_num - shift(ts_num, 1L),
    p_5m_lag1 = shift(close, 1L),
    date_5m   = as.Date(open_time_utc, tz = "UTC"),
    ts_30m    = as.POSIXct(floor(ts_num / 1800) * 1800, origin = "1970-01-01", tz = "UTC"),
    slot_5m   = as.integer((ts_num %% 1800) %/% 300L)
  )]
  dt[, ret_5m := fifelse(dt_sec == 300 & p_5m_lag1 > 0, (close / p_5m_lag1) - 1, NA_real_)]

  # Lückenloses Tagesraster -> shift(..., 1L) ist garantiert Kalendertag t-1
  daily_grid <- data.table(
    symbol = sym_name,
    date   = seq(min(dt$date_5m), max(dt$date_5m), by = "1 day")
  )
  daily_stats <- dt[, .(
    daily_qvol = sum(quote_volume, na.rm = TRUE),
    rv_daily   = if (sum(is.finite(ret_5m)) >= 200L) sum(ret_5m^2, na.rm = TRUE) else NA_real_
  ), by = .(symbol, date = date_5m)]

  daily_stats <- merge(daily_grid, daily_stats, by = c("symbol", "date"), all.x = TRUE)
  setorder(daily_stats, symbol, date)
  daily_stats[, `:=`(
    vol_lag1d = shift(daily_qvol, 1L),
    rv_lag1d  = shift(rv_daily, 1L)
  ), by = symbol]

  # --- B) 30m-Aggregation: nur vollständige 6x5m-Intervalle akzeptieren ---
  # slot_5m == 4L -> 5. Kerze (:20/:50, schließt :25/:55) für ret_25m (Table 4 Spalte 5)
  # slot_5m == 5L -> 6. Kerze (:25/:55, schließt :30/:00) für ret_30m
  dt30 <- dt[, {
    complete <- (.N == 6L && slot_5m[6L] == 5L && slot_5m[5L] == 4L)
    .(
      close_30m     = if (complete) close[6L] else NA_real_,
      close_25m     = if (complete) close[5L] else NA_real_,
      quote_vol_30m = if (complete) sum(quote_volume, na.rm = TRUE) else NA_real_,
      tb_qvol_30m   = if (complete) sum(taker_buy_quote_volume, na.rm = TRUE) else NA_real_,
      n_trades_30m  = if (complete) sum(n_trades, na.rm = TRUE) else NA_integer_
    )
  }, by = .(symbol, ts_30m)]

  # Lückenloses 30m-Zeitraster -> shift() überspringt niemals Ausfall-/Wartungszeiten
  grid <- data.table(
    symbol = sym_name,
    ts_30m = seq(min(dt30$ts_30m), max(dt30$ts_30m), by = "30 mins")
  )
  dt30 <- merge(grid, dt30, by = c("symbol", "ts_30m"), all.x = TRUE)
  setorder(dt30, ts_30m)

  dt30[, `:=`(
    p_lag1  = shift(close_30m, 1L),
    p_lag2  = shift(close_30m, 2L),
    p_lag48 = shift(close_30m, 48L)
  )]

  dt30[, `:=`(
    ret_30m_raw         = (close_30m / p_lag1) - 1,
    ret_25m_raw         = (close_25m / p_lag1) - 1,   # Ohne letzte 5 Min. (Table 4 Col. 5)
    ret_prev_skip_raw   = (p_lag2 / p_lag48) - 1,     # 23h + 30m Skip (Figure 1)
    ret_prev_noskip_raw = (p_lag1 / p_lag48) - 1      # 23.5h ohne Skip (Figure A.1)
  )]

  dt30[, date := as.Date(ts_30m, tz = "UTC")]
  dt30[, interval_id := as.integer((as.numeric(ts_30m) %% 86400) %/% 1800L) + 1L]

  dt30 <- merge(
    dt30,
    daily_stats[, .(symbol, date, vol_lag1d, rv_lag1d)],
    by = c("symbol", "date"),
    all.x = TRUE
  )

  dt30[
    is.finite(ret_30m_raw) &
    is.finite(ret_25m_raw) &
    is.finite(ret_prev_skip_raw) &
    is.finite(ret_prev_noskip_raw) &
    !is.na(vol_lag1d) & vol_lag1d > 0,
    .(
      symbol, ts_30m, date, interval_id,
      ret_30m_raw, ret_25m_raw, ret_prev_skip_raw, ret_prev_noskip_raw,
      vol_lag1d, rv_lag1d, quote_vol_30m, tb_qvol_30m, n_trades_30m
    )
  ]
}

panel_list <- vector("list", length(files))
for (i in seq_along(files)) {
  panel_list[[i]] <- build_coin_30m(files[i])
  if (i %% 100L == 0L) {
    cat(sprintf("  ... %d / %d Coins aggregiert\n", i, length(files)))
    invisible(gc(FALSE))
  }
}

panel <- rbindlist(Filter(Negate(is.null), panel_list), use.names = TRUE)
rm(panel_list)
invisible(gc(FALSE))

cat(sprintf("Roh-Panel vor dynamischem Filter: %s Zeilen (%d Coins)\n",
            format(nrow(panel), big.mark = ","), uniqueN(panel$symbol)))

# ==============================================================================
# 3. DYNAMISCHER VORTAGES-VOLUMENFILTER (VOR DEM WINSORIZING!)
# ==============================================================================
daily_univ <- unique(panel[, .(symbol, date, vol_lag1d)])
daily_univ[, vol_p10 := quantile(vol_lag1d, probs = DROP_BOTTOM_PCT, na.rm = TRUE, names = FALSE), by = date]
daily_univ <- daily_univ[vol_lag1d >= MIN_VOL_USDT & vol_lag1d >= vol_p10]

day_counts <- daily_univ[, .(n_coins_day = .N), by = date]
valid_days <- day_counts[n_coins_day >= MIN_COINS_PER_DAY, .(date)]
daily_univ <- daily_univ[valid_days, on = "date", nomatch = 0L]

# Konzentrationsdiagnose pro Tag berechnen & speichern (für Table 1b)
cat("Berechne tägliche Konzentrationsdiagnose (Raw-VW vs. 5%-Capped-VW)...\n")
conc_daily <- daily_univ[, {
  w_raw   <- vol_lag1d / sum(vol_lag1d)
  w_cap   <- cap_weights(vol_lag1d, cap = CAP_WEIGHT)
  w_raw_s <- sort(w_raw, decreasing = TRUE)
  w_cap_s <- sort(w_cap, decreasing = TRUE)
  .(
    n_coins       = .N,
    neff_raw      = 1 / sum(w_raw^2),
    neff_cap5     = 1 / sum(w_cap^2),
    btc_raw_pct   = sum(w_raw[symbol == "BTCUSDT"]) * 100,
    eth_raw_pct   = sum(w_raw[symbol == "ETHUSDT"]) * 100,
    top2_raw_pct  = sum(w_raw_s[seq_len(min(2L, .N))]) * 100,
    top5_raw_pct  = sum(w_raw_s[seq_len(min(5L, .N))]) * 100,
    top5_cap5_pct = sum(w_cap_s[seq_len(min(5L, .N))]) * 100
  )
}, by = date]
setorder(conc_daily, date)
fwrite(conc_daily, "output/tables/weight_concentration_daily.csv")

# Panel auf zulässige (symbol, date)-Kombinationen einschränken
setkey(daily_univ, symbol, date)
setkey(panel, symbol, date)
panel <- panel[daily_univ[, .(symbol, date)], nomatch = 0L]
rm(daily_univ, day_counts, valid_days, conc_daily)
invisible(gc(FALSE))

# ==============================================================================
# 4. NORMALISIERTE GEWICHTE (SUMME = 1 PRO ZEITSTEMPEL -> KEIN BULL/BEAR-BIAS)
# ==============================================================================
cat("Berechne zeitstempel-normalisierte Gewichte (w_vw_raw & w_vw_cap5)...\n")
panel[, `:=`(
  w_vw_raw  = vol_lag1d / sum(vol_lag1d),
  w_vw_cap5 = cap_weights(vol_lag1d, cap = CAP_WEIGHT)
), by = ts_30m]

# ==============================================================================
# 5. QUERSCHNITTSWEISES WINSORIZING BEI 1% / 99% (ROH-RENDITEN BLEIBEN ERHALTEN)
# ==============================================================================
winsorize_vec <- function(x, p = c(0.01, 0.99)) {
  if (sum(!is.na(x)) < 5L) return(x)
  qs <- quantile(x, probs = p, na.rm = TRUE, names = FALSE)
  pmin(pmax(x, qs[1]), qs[2])
}

cat("Führe querschnittsweises Winsorizing (1% / 99%) durch (Roh-Renditen bleiben als *_raw)...\n")
panel[, `:=`(
  ret_30m         = winsorize_vec(ret_30m_raw),
  ret_25m         = winsorize_vec(ret_25m_raw),
  ret_prev_skip   = winsorize_vec(ret_prev_skip_raw),
  ret_prev_noskip = winsorize_vec(ret_prev_noskip_raw)
), by = ts_30m]

setorder(panel, symbol, ts_30m)

# ==============================================================================
# 6. SPEICHERN & AUSFÜHRLICHER PLAUSIBILITÄTS-CHECK
# ==============================================================================
out_path <- "data/processed/panel_30m_utc.parquet"
write_parquet(panel, out_path)

coins_per_day <- panel[, .(n_coins = uniqueN(symbol)), by = date]
delisted_check <- intersect(c("LUNAUSDT", "FTTUSDT", "WAVESUSDT", "SRMUSDT", "OMGUSDT"), unique(panel$symbol))
wrapped_check  <- intersect(paste0(EXCLUDED_WRAPPED, "USDT"), unique(panel$symbol))

cat("\n================ PLAUSIBILITÄTS-CHECK (VOLL-SAMPLE) ================\n")
cat("Gespeichert unter:     ", out_path, "\n")
cat("Unique Coins gesamt:   ", uniqueN(panel$symbol), "\n")
cat("Aktive Coins pro Tag:  ",
    sprintf("Min: %d | Median: %.0f | Max: %d\n",
            min(coins_per_day$n_coins), median(coins_per_day$n_coins), max(coins_per_day$n_coins)))
cat("Delisting-Check aktiv: ",
    ifelse(length(delisted_check) > 0L, paste(delisted_check, collapse = ", "), "keine gefunden"), "\n")
cat("Wrapped-Check (0=OK):  ", length(wrapped_check), "\n")
cat("Anzahl Tage:           ", uniqueN(panel$date), "\n")
cat("Zeitraum (UTC):        ", as.character(min(panel$date)), "bis", as.character(max(panel$date)), "\n")
cat("Zeilen gesamt:         ", format(nrow(panel), big.mark = ","), "\n")
cat("Intervall-IDs:         ", min(panel$interval_id), "bis", max(panel$interval_id),
    "(Unique:", uniqueN(panel$interval_id), ")\n")
cat(sprintf("Max w_vw_cap5:         %.4f (Soll <= %.2f)\n", max(panel$w_vw_cap5), CAP_WEIGHT))
cat(sprintf("ret_30m       Mean: %+.4f%% | SD: %.4f%%\n",
            mean(panel$ret_30m) * 100, sd(panel$ret_30m) * 100))
cat(sprintf("ret_25m       Mean: %+.4f%% | SD: %.4f%%\n",
            mean(panel$ret_25m) * 100, sd(panel$ret_25m) * 100))
cat(sprintf("ret_prev_skip Mean: %+.4f%% | SD: %.4f%%\n",
            mean(panel$ret_prev_skip) * 100, sd(panel$ret_prev_skip) * 100))
cat("====================================================================\n")