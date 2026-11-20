library(data.table)
library(arrow)
library(jsonlite)
library(curl)

dir.create("data/raw/zips", recursive = TRUE, showWarnings = FALSE)
dir.create("data/processed/coins_5m", recursive = TRUE, showWarnings = FALSE)

# ==============================================================================
# 1. PARAMETER (Mini-Testlauf: 5 Coins, Jan 2025 - Okt 2026)
#    Später für das volle Sample einfach hochdrehen (z. B. 50 Coins ab 2021-01)
# ==============================================================================
TOP_N_COINS <- 5
START_DATE  <- as.Date("2025-01-01")
END_DATE    <- as.Date("2026-10-01") # letzter abgeschlossener Monat

# Stablecoins, Fiat-Pegs & Gold-Tokens ausschließen
EXCLUDED_BASES <- c(
  "USDC", "BUSD", "TUSD", "USDP", "FDUSD", "DAI", "USD1", "USDE", "EURI",
  "RLUSD", "XUSD", "BFUSD", "EUR", "GBP", "AUD", "BRL", "TRY", "RUB",
  "AEUR", "PAXG", "XAUT", "UST", "USTC"
)

# ==============================================================================
# 2. LIQUIDESTE USDT-PAARE ABRUFEN (öffentlicher Endpoint, kein API-Key nötig)
# ==============================================================================
cat("Rufe aktuelle 24h-Volumen von Binance ab...\n")
tickers <- as.data.table(fromJSON("https://data-api.binance.vision/api/v3/ticker/24hr"))
tickers[, quoteVolume := as.numeric(quoteVolume)]

usdt_pairs <- tickers[grepl("USDT$", symbol)]
usdt_pairs[, base := sub("USDT$", "", symbol)]
usdt_pairs <- usdt_pairs[!(base %in% EXCLUDED_BASES)]
setorder(usdt_pairs, -quoteVolume)

symbols <- head(usdt_pairs$symbol, TOP_N_COINS)
cat("Ausgewählte Coins:", paste(symbols, collapse = ", "), "\n\n")

# ==============================================================================
# 3. MONATLICHE 5m-CANDLES LADEN & ALS PARQUET SPEICHERN
# ==============================================================================
months_seq <- format(seq(START_DATE, END_DATE, by = "month"), "%Y-%m")

col_names <- c(
  "open_time", "open", "high", "low", "close", "volume",
  "close_time", "quote_volume", "n_trades",
  "taker_buy_volume", "taker_buy_quote_volume", "ignore"
)

for (sym in symbols) {
  out_file <- file.path("data/processed/coins_5m", paste0(sym, ".parquet"))
  if (file.exists(out_file)) {
    cat("[SKIP]", sym, "existiert bereits.\n")
    next
  }

  cat("[DOWNLOAD]", sym, "...")
  sym_list <- list()

  for (ym in months_seq) {
    url <- sprintf(
      "https://data.binance.vision/data/spot/monthly/klines/%s/5m/%s-5m-%s.zip",
      sym, sym, ym
    )
    zip_path <- file.path("data/raw/zips", sprintf("%s-5m-%s.zip", sym, ym))

    ok <- tryCatch({
      curl_download(url, destfile = zip_path, quiet = TRUE)
      TRUE
    }, error = function(e) FALSE)

    if (!ok) next # Coin war in diesem Monat noch nicht gelistet

    csv_name <- unzip(zip_path, list = TRUE)$Name[1]
    unzip(zip_path, files = csv_name, exdir = tempdir(), overwrite = TRUE)
    csv_path <- file.path(tempdir(), csv_name)

    dt <- fread(csv_path, header = FALSE, col.names = col_names, showProgress = FALSE)
    unlink(csv_path)
    unlink(zip_path) # ZIP direkt löschen -> spart Festplattenplatz

    # Schutz, falls eine CSV unerwartet eine Header-Zeile enthält
    if (is.na(suppressWarnings(as.numeric(dt$open_time[1])))) {
      dt <- dt[-1]
    }

    # KRITISCH: Explizit als numeric (verhindert integer64-Fehler),
    # dann dynamisch Mikrosekunden (ab 2025, 16 Stellen) vs. Millisekunden (13 Stellen) umrechnen
    open_time_num <- as.numeric(dt$open_time)
    open_time_sec <- fifelse(open_time_num > 1e14, open_time_num / 1e6, open_time_num / 1e3)

    sym_list[[ym]] <- data.table(
      symbol                 = sym,
      open_time_utc          = as.POSIXct(open_time_sec, origin = "1970-01-01", tz = "UTC"),
      open                   = as.numeric(dt$open),
      high                   = as.numeric(dt$high),
      low                    = as.numeric(dt$low),
      close                  = as.numeric(dt$close),
      quote_volume           = as.numeric(dt$quote_volume),
      taker_buy_quote_volume = as.numeric(dt$taker_buy_quote_volume),
      n_trades               = as.integer(dt$n_trades)
    )
  }

  if (length(sym_list) > 0) {
    dt_coin <- rbindlist(sym_list, use.names = TRUE)
    dt_coin <- unique(dt_coin, by = "open_time_utc")
    setorder(dt_coin, open_time_utc)
    write_parquet(dt_coin, out_file)

    cat(sprintf(
      " fertig! %s Candles (%s bis %s UTC)\n",
      format(nrow(dt_coin), big.mark = "."),
      format(min(dt_coin$open_time_utc), "%Y-%m-%d"),
      format(max(dt_coin$open_time_utc), "%Y-%m-%d")
    ))
  } else {
    cat(" keine Daten gefunden.\n")
  }
}

cat("\nDownload abgeschlossen. Gespeicherte Dateien in data/processed/coins_5m/:\n")
print(list.files("data/processed/coins_5m", pattern = "\\.parquet$"))