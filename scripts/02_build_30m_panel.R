library(data.table)
library(arrow)

# ==============================================================================
# 1. EINGABEDATEIEN PRÜFEN
# ==============================================================================
files <- list.files("data/processed/coins_5m", pattern = "\\.parquet$", full.names = TRUE)
if (length(files) == 0L) {
  stop("Keine Parquet-Dateien in data/processed/coins_5m/ gefunden. Zuerst 01_download_binance.R ausführen.")
}
cat("Verarbeite", length(files), "Coins aus data/processed/coins_5m/ ...\n")

# ==============================================================================
# 2. AGGREGATION 5m -> LÜCKENLOSES 30m-PANEL PRO COIN (UTC)
# ==============================================================================
build_coin_30m <- function(f) {
  dt <- as.data.table(read_parquet(f))
  if (nrow(dt) < 288L * 30L) return(NULL) # Mindestens 30 Handelstage verlangen

  # Schnelle & zeitzonensichere Rundung auf 30-Minuten-Blöcke (UTC, 1800 Sekunden)
  dt[, ts_30m := as.POSIXct(
    floor(as.numeric(open_time_utc) / 1800) * 1800,
    origin = "1970-01-01", tz = "UTC"
  )]
  setorder(dt, open_time_utc)

  # 5m-Candles zu 30m-Blöcken aggregieren (inkl. Taker Buy Volume für spätere Retail-Analyse)
  dt30 <- dt[, .(
    close_30m     = close[.N],
    quote_vol_30m = sum(quote_volume, na.rm = TRUE),
    tb_qvol_30m   = sum(taker_buy_quote_volume, na.rm = TRUE),
    n_trades_30m  = sum(n_trades, na.rm = TRUE),
    n_candles     = .N
  ), by = .(symbol, ts_30m)]

  # Lückenloses 30m-Raster erzeugen -> shift() springt nie über Börsen-Wartungen hinweg
  grid <- data.table(
    symbol = dt30$symbol[1],
    ts_30m = seq(min(dt30$ts_30m), max(dt30$ts_30m), by = "30 mins")
  )
  dt30 <- merge(grid, dt30, by = c("symbol", "ts_30m"), all.x = TRUE)
  setorder(dt30, ts_30m)

  # Gelaggte Preise:
  # close_30m = Ende Zielintervall k      (z. B. 00:00 UTC)
  # p_lag1    = Anfang Zielintervall k    (z. B. 23:30 UTC)
  # p_lag2    = Ende Prädiktor mit Skip   (z. B. 23:00 UTC -> 30m Puffer gegen Bid-Ask-Bounce)
  # p_lag48   = Start vor exakt 24 Stunden(z. B. 00:00 UTC Vortag)
  dt30[, `:=`(
    p_lag1  = shift(close_30m, 1L),
    p_lag2  = shift(close_30m, 2L),
    p_lag48 = shift(close_30m, 48L)
  )]

  dt30[, `:=`(
    ret_30m         = (close_30m / p_lag1) - 1, # Zielvariable (LH-Analog)
    ret_prev_skip   = (p_lag2 / p_lag48) - 1,   # 23h-Return + 30m Skip (ROD3 / Figure 1)
    ret_prev_noskip = (p_lag1 / p_lag48) - 1    # 23.5h-Return ohne Skip (Figure A.1)
  )]

  # UTC-Datum & Intervall-ID (1 = 00:00-00:30 UTC ... 48 = 23:30-00:00 UTC)
  dt30[, date := as.Date(ts_30m, tz = "UTC")]
  dt30[, interval_id := as.integer((as.numeric(ts_30m) %% 86400) %/% 1800L) + 1L]

  # Vortages-USDT-Volumen (streng gelagged auf t-1 -> kein Look-Ahead-Bias)
  daily_vol <- dt30[, .(daily_qvol = sum(quote_vol_30m, na.rm = TRUE)), by = .(symbol, date)]
  setorder(daily_vol, date)
  daily_vol[, vol_lag1d := shift(daily_qvol, 1L)]

  dt30 <- merge(dt30, daily_vol[, .(symbol, date, vol_lag1d)], by = c("symbol", "date"), all.x = TRUE)

  # Nur vollständig berechenbare Beobachtungen mit positivem Vortagesvolumen behalten
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

panel_list <- lapply(files, build_coin_30m)
panel <- rbindlist(panel_list, use.names = TRUE)

# ==============================================================================
# 3. QUERSCHNITTSWEISES WINSORIZING BEI 1% / 99% PRO ZEITSTEMPEL
#    (Bei 5 Test-Coins primär technischer Check; greift voll bei 50-100 Coins)
# ==============================================================================
winsorize_vec <- function(x, p = c(0.01, 0.99)) {
  if (sum(!is.na(x)) < 2L) return(x)
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
# 4. SPEICHERN & PLAUSIBILITÄTS-CHECK
# ==============================================================================
out_path <- "data/processed/panel_30m_utc.parquet"
write_parquet(panel, out_path)

cat("\n================ PLAUSIBILITÄTS-CHECK ================\n")
cat("Gespeichert unter:  ", out_path, "\n")
cat("Anzahl Coins:       ", uniqueN(panel$symbol), "\n")
cat("Anzahl Tage:        ", uniqueN(panel$date), "\n")
cat("Zeitraum (UTC):     ", as.character(min(panel$date)), "bis", as.character(max(panel$date)), "\n")
cat("Zeilen gesamt:      ", format(nrow(panel), big.mark = "."), "\n")
cat("Intervall-IDs:      ", min(panel$interval_id), "bis", max(panel$interval_id),
    "(Unique:", uniqueN(panel$interval_id), ")\n")
cat(sprintf("ret_30m       Mean: %+.4f%% | SD: %.4f%%\n",
            mean(panel$ret_30m) * 100, sd(panel$ret_30m) * 100))
cat(sprintf("ret_prev_skip Mean: %+.4f%% | SD: %.4f%%\n",
            mean(panel$ret_prev_skip) * 100, sd(panel$ret_prev_skip) * 100))
cat("======================================================\n")