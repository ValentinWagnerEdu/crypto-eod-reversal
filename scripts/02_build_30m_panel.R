library(data.table)
library(arrow)

# ==============================================================================
# 1. PARAMETER FÜR DYNAMISCHEN FILTER (STRENG LAG t-1 -> KEIN LOOK-AHEAD-BIAS)
# ==============================================================================
MIN_DAYS_COIN     <- 30L      # Coin muss mind. 30 Tage gehandelt worden sein
MIN_VOL_USDT      <- 500000   # Mindest-Vortagesvolumen: 500.000 USDT
DROP_BOTTOM_PCT   <- 0.10     # Täglich unterste 10% nach Vortagesvolumen raus (analog NYSE 10%)
MIN_COINS_PER_DAY <- 25L      # Mind. 25 liquide Coins pro Tag für saubere Querschnitte/Quintile

files <- list.files("data/processed/coins_5m", pattern = "\\.parquet$", full.names = TRUE)
if (length(files) == 0L) {
  stop("Keine Parquet-Dateien in data/processed/coins_5m/ gefunden.")
}
cat("Verarbeite", length(files), "Coins aus data/processed/coins_5m/ ...\n")

# ==============================================================================
# 2. AGGREGATION 5m -> LÜCKENLOSES 30m-PANEL PRO COIN (UTC)
# ==============================================================================
build_coin_30m <- function(f) {
  dt <- as.data.table(read_parquet(f))
  if (nrow(dt) < 288L * MIN_DAYS_COIN) return(NULL)

  dt[, ts_30m := as.POSIXct(
    floor(as.numeric(open_time_utc) / 1800) * 1800,
    origin = "1970-01-01", tz = "UTC"
  )]
  setorder(dt, open_time_utc)

  dt30 <- dt[, .(
    close_30m     = close[.N],
    quote_vol_30m = sum(quote_volume, na.rm = TRUE),
    tb_qvol_30m   = sum(taker_buy_quote_volume, na.rm = TRUE),
    n_trades_30m  = sum(n_trades, na.rm = TRUE)
  ), by = .(symbol, ts_30m)]

  # Lückenloses 30m-Zeitraster -> shift() überspringt niemals Ausfall-/Wartungszeiten
  grid <- data.table(
    symbol = dt30$symbol[1],
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
    ret_30m         = (close_30m / p_lag1) - 1,
    ret_prev_skip   = (p_lag2 / p_lag48) - 1,   # 23h + 30m Skip (Figure 1)
    ret_prev_noskip = (p_lag1 / p_lag48) - 1    # 23.5h ohne Skip (Figure A.1)
  )]

  dt30[, date := as.Date(ts_30m, tz = "UTC")]
  dt30[, interval_id := as.integer((as.numeric(ts_30m) %% 86400) %/% 1800L) + 1L]

  # Streng gelaggtes Vortages-USDT-Volumen (t-1)
  daily_vol <- dt30[, .(daily_qvol = sum(quote_vol_30m, na.rm = TRUE)), by = .(symbol, date)]
  setorder(daily_vol, date)
  daily_vol[, vol_lag1d := shift(daily_qvol, 1L)]

  dt30 <- merge(dt30, daily_vol[, .(symbol, date, vol_lag1d)], by = c("symbol", "date"), all.x = TRUE)

  dt30[
    is.finite(ret_30m) &
    is.finite(ret_prev_skip) &
    is.finite(ret_prev_noskip) &
    !is.na(vol_lag1d) & vol_lag1d > 0,
    .(
      symbol, ts_30m, date, interval_id,
      ret_30m, ret_prev_skip, ret_prev_noskip,
      vol_lag1d, quote_vol_30m, tb_qvol_30m, n_trades_30m
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

# Nur Tage mit ausreichend breitem Querschnitt behalten
day_counts <- daily_univ[, .(n_coins_day = .N), by = date]
valid_days <- day_counts[n_coins_day >= MIN_COINS_PER_DAY, .(date)]
daily_univ <- daily_univ[valid_days, on = "date", nomatch = 0L]

# Panel auf zulässige (symbol, date)-Kombinationen einschränken
setkey(daily_univ, symbol, date)
setkey(panel, symbol, date)
panel <- panel[daily_univ[, .(symbol, date)], nomatch = 0L]
rm(daily_univ, day_counts, valid_days)
invisible(gc(FALSE))

# ==============================================================================
# 4. QUERSCHNITTSWEISES WINSORIZING BEI 1% / 99% PRO ZEITSTEMPEL
# ==============================================================================
winsorize_vec <- function(x, p = c(0.01, 0.99)) {
  if (sum(!is.na(x)) < 5L) return(x)
  qs <- quantile(x, probs = p, na.rm = TRUE, names = FALSE)
  pmin(pmax(x, qs[1]), qs[2])
}

cat("Führe querschnittsweises Winsorizing (1% / 99%) durch...\n")
panel[, `:=`(
  ret_30m         = winsorize_vec(ret_30m),
  ret_prev_skip   = winsorize_vec(ret_prev_skip),
  ret_prev_noskip = winsorize_vec(ret_prev_noskip)
), by = ts_30m]

setorder(panel, symbol, ts_30m)

# ==============================================================================
# 5. SPEICHERN & AUSFÜHRLICHER PLAUSIBILITÄTS-CHECK
# ==============================================================================
out_path <- "data/processed/panel_30m_utc.parquet"
write_parquet(panel, out_path)

coins_per_day <- panel[, .(n_coins = uniqueN(symbol)), by = date]
delisted_check <- intersect(c("LUNAUSDT", "FTTUSDT", "WAVESUSDT", "SRMUSDT", "OMGUSDT"), unique(panel$symbol))

cat("\n================ PLAUSIBILITÄTS-CHECK (VOLL-SAMPLE) ================\n")
cat("Gespeichert unter:     ", out_path, "\n")
cat("Unique Coins gesamt:   ", uniqueN(panel$symbol), "\n")
cat("Aktive Coins pro Tag:  ",
    sprintf("Min: %d | Median: %.0f | Max: %d\n",
            min(coins_per_day$n_coins), median(coins_per_day$n_coins), max(coins_per_day$n_coins)))
cat("Delisting-Check aktiv: ",
    ifelse(length(delisted_check) > 0L, paste(delisted_check, collapse = ", "), "keine gefunden"), "\n")
cat("Anzahl Tage:           ", uniqueN(panel$date), "\n")
cat("Zeitraum (UTC):        ", as.character(min(panel$date)), "bis", as.character(max(panel$date)), "\n")
cat("Zeilen gesamt:         ", format(nrow(panel), big.mark = ","), "\n")
cat("Intervall-IDs:         ", min(panel$interval_id), "bis", max(panel$interval_id),
    "(Unique:", uniqueN(panel$interval_id), ")\n")
cat(sprintf("ret_30m       Mean: %+.4f%% | SD: %.4f%%\n",
            mean(panel$ret_30m) * 100, sd(panel$ret_30m) * 100))
cat(sprintf("ret_prev_skip Mean: %+.4f%% | SD: %.4f%%\n",
            mean(panel$ret_prev_skip) * 100, sd(panel$ret_prev_skip) * 100))
cat("====================================================================\n")