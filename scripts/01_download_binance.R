library(data.table)
library(arrow)
library(curl)

dir.create("data/processed/coins_5m", recursive = TRUE, showWarnings = FALSE)
dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)

# ==============================================================================
# 1. PARAMETER (Survivorship-Bias-freies Voll-Sample: Jan 2020 - Okt 2026)
# ==============================================================================
START_YM <- "2020-01"
END_YM   <- "2026-10"
S3_BASE  <- "https://s3-ap-northeast-1.amazonaws.com/data.binance.vision"

# Stablecoins, Fiat-Pegs & Gold-Tokens ausschließen
EXCLUDED_BASES <- c(
  "USDC", "BUSD", "TUSD", "USDP", "FDUSD", "DAI", "USD1", "USDE", "EURI",
  "RLUSD", "XUSD", "BFUSD", "PAX", "SUSD", "GUSD", "UST", "USTC", "USDS",
  "USDJ", "USDN", "HUSD", "VAI", "AEUR", "EUR", "GBP", "AUD", "BRL", "TRY",
  "RUB", "BIDR", "IDRT", "NGN", "UAH", "ZAR", "PLN", "RON", "ARS", "COP",
  "MXN", "CZK", "PAXG", "XAUT", "BKRW", "BVND"
)

# Ehemalige Binance Leveraged Tokens (BLVT / FTX-Leveraged) gezielt ausschließen
# (Bewusst exakte Basisliste statt Regex "UP$", damit echte Coins wie JUP/SYRUP bleiben!)
BLVT_UNDERLYINGS <- c(
  "BTC", "ETH", "BNB", "ADA", "LINK", "DOT", "XRP", "EOS", "TRX", "LTC",
  "BCH", "XTZ", "SXP", "UNI", "SUSHI", "XLM", "AAVE", "YFI", "FIL", "1INCH"
)
EXCLUDED_LEVERAGED <- c(
  paste0(BLVT_UNDERLYINGS, "UP"),
  paste0(BLVT_UNDERLYINGS, "DOWN"),
  "BULL", "BEAR", "ETHBULL", "ETHBEAR", "BNBBULL", "BNBBEAR",
  "EOSBULL", "EOSBEAR", "XRPBULL", "XRPBEAR", "TRXBULL", "TRXBEAR"
)
EXCLUDED_ALL <- unique(c(EXCLUDED_BASES, EXCLUDED_LEVERAGED))

# ==============================================================================
# 2. HILFSFUNKTIONEN FÜR DAS ÖFFENTLICHE BINANCE-S3-ARCHIV (OHNE EXTRA-PAKETE)
# ==============================================================================
extract_xml_tag <- function(xml_txt, tag) {
  pat <- sprintf("<%s>([^<]+)</%s>", tag, tag)
  m <- gregexpr(pat, xml_txt, perl = TRUE)
  hits <- regmatches(xml_txt, m)[[1]]
  if (length(hits) == 0L) return(character(0))
  sub(pat, "\\1", hits, perl = TRUE)
}

s3_fetch_xml <- function(query_url) {
  con <- curl_fetch_memory(query_url)
  if (con$status_code != 200L) return(NULL)
  rawToChar(con$content)
}

# Alle jemals gelisteten Spot-Symbole direkt aus S3 auslesen (inkl. Delistings)
list_all_s3_usdt_symbols <- function() {
  all_prefixes <- character(0)
  token <- NULL

  repeat {
    url <- paste0(S3_BASE, "?list-type=2&prefix=data/spot/monthly/klines/&delimiter=/")
    if (!is.null(token)) {
      url <- paste0(url, "&continuation-token=", curl_escape(token))
    }
    xml_txt <- s3_fetch_xml(url)
    if (is.null(xml_txt)) stop("S3-Abruf der Symbolliste fehlgeschlagen.")

    prefixes <- extract_xml_tag(xml_txt, "Prefix")
    all_prefixes <- c(all_prefixes, prefixes)

    trunc_flag <- extract_xml_tag(xml_txt, "IsTruncated")
    if (length(trunc_flag) > 0L && tolower(trunc_flag[1]) == "true") {
      tok <- extract_xml_tag(xml_txt, "NextContinuationToken")[1]
      token <- gsub("&amp;", "&", tok, fixed = TRUE)
    } else {
      break
    }
  }

  # Nur echte Symbol-Ordner der Form ".../klines/{SYMBOL}/" extrahieren
  m <- regexec("^data/spot/monthly/klines/([A-Z0-9]+)/$", all_prefixes)
  syms <- unique(vapply(regmatches(all_prefixes, m), function(x) {
    if (length(x) >= 2L) x[2] else NA_character_
  }, character(1)))
  syms <- sort(syms[!is.na(syms)])

  # Nur USDT-Paare, bereinigt um Stablecoins, Fiat & Leveraged Tokens
  usdt_syms <- syms[grepl("USDT$", syms)]
  bases <- sub("USDT$", "", usdt_syms)
  usdt_syms[!(bases %in% EXCLUDED_ALL)]
}

# Exakte Liste der vorhandenen 5m-ZIPs für einen Coin im Zeitraum holen (null 404s)
list_coin_5m_zips <- function(sym, start_ym, end_ym) {
  url <- sprintf("%s?list-type=2&prefix=data/spot/monthly/klines/%s/5m/", S3_BASE, sym)
  xml_txt <- s3_fetch_xml(url)
  if (is.null(xml_txt)) return(character(0))

  keys <- extract_xml_tag(xml_txt, "Key")
  zips <- keys[grepl("-5m-[0-9]{4}-[0-9]{2}\\.zip$", keys)]
  if (length(zips) == 0L) return(character(0))

  yms <- sub("^.*-5m-([0-9]{4}-[0-9]{2})\\.zip$", "\\1", zips)
  zips[yms >= start_ym & yms <= end_ym]
}

# ==============================================================================
# 3. SYMBOL-UNIVERSUM HOLEN & PARALLEL PRO COIN LADEN
# ==============================================================================
cat("Lese vollständiges historisches Binance-Spot-Universum aus S3 aus (inkl. Delistings)...\n")
symbols <- list_all_s3_usdt_symbols()
cat(sprintf("Gefundene Non-Stablecoin-USDT-Paare (inkl. Delistings): %d\n\n", length(symbols)))

col_names <- c(
  "open_time", "open", "high", "low", "close", "volume",
  "close_time", "quote_volume", "n_trades",
  "taker_buy_volume", "taker_buy_quote_volume", "ignore"
)

# Lokaler Temp-Ordner auf C:\ (schont Google Drive Sync massiv!)
work_tmp <- file.path(tempdir(), "binance_5m_tmp")
dir.create(work_tmp, recursive = TRUE, showWarnings = FALSE)

t_start <- Sys.time()
n_total <- length(symbols)

for (idx in seq_along(symbols)) {
  sym <- symbols[idx]
  out_file <- file.path("data/processed/coins_5m", paste0(sym, ".parquet"))

  if (file.exists(out_file)) {
    if (idx %% 50L == 0L) cat(sprintf("[%d/%d] Bereits lokal vorhanden...\n", idx, n_total))
    next
  }

  zip_keys <- tryCatch(list_coin_5m_zips(sym, START_YM, END_YM), error = function(e) character(0))
  if (length(zip_keys) == 0L) next # Coin war vor 2020 delistet oder hat keine 5m-Daten im Fenster

  urls      <- paste0("https://data.binance.vision/", zip_keys)
  destfiles <- file.path(work_tmp, basename(zip_keys))

  # Alle Monats-ZIPs dieses Coins parallel herunterladen
  dl <- tryCatch(
    multi_download(urls = urls, destfiles = destfiles, progress = FALSE, multiplex = TRUE),
    error = function(e) NULL
  )
  if (is.null(dl)) {
    unlink(destfiles)
    next
  }

  ok_files <- dl$destfile[dl$success & dl$status_code == 200L & file.exists(dl$destfile)]
  if (length(ok_files) == 0L) {
    unlink(destfiles)
    next
  }

  sym_list <- vector("list", length(ok_files))
  for (j in seq_along(ok_files)) {
    zf <- ok_files[j]
    dt <- tryCatch({
      csv_name <- unzip(zf, list = TRUE)$Name[1]
      unzip(zf, files = csv_name, exdir = work_tmp, overwrite = TRUE)
      csv_path <- file.path(work_tmp, csv_name)
      d <- fread(csv_path, header = FALSE, col.names = col_names, showProgress = FALSE)
      unlink(csv_path)
      d
    }, error = function(e) NULL)

    if (is.null(dt) || nrow(dt) == 0L) next
    if (is.na(suppressWarnings(as.numeric(dt$open_time[1])))) dt <- dt[-1]
    if (nrow(dt) == 0L) next

    # KRITISCH: Mikrosekunden ab 01.01.2025 (16 Stellen) vs. Millisekunden (13 Stellen)
    open_time_num <- as.numeric(dt$open_time)
    open_time_sec <- fifelse(open_time_num > 1e14, open_time_num / 1e6, open_time_num / 1e3)

    sym_list[[j]] <- data.table(
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

  unlink(destfiles) # Lokale Temp-ZIPs sofort löschen

  sym_list <- Filter(Negate(is.null), sym_list)
  if (length(sym_list) > 0L) {
    dt_coin <- rbindlist(sym_list, use.names = TRUE)
    dt_coin <- unique(dt_coin, by = "open_time_utc")
    setorder(dt_coin, open_time_utc)
    write_parquet(dt_coin, out_file)

    elapsed <- as.numeric(difftime(Sys.time(), t_start, units = "mins"))
    cat(sprintf(
      "[%3d/%d | %4.1f Min] %-12s -> %2d Monate | %s bis %s\n",
      idx, n_total, elapsed, sym, length(sym_list),
      format(min(dt_coin$open_time_utc), "%Y-%m-%d"),
      format(max(dt_coin$open_time_utc), "%Y-%m-%d")
    ))
  }
}

unlink(work_tmp, recursive = TRUE)
n_saved <- length(list.files("data/processed/coins_5m", pattern = "\\.parquet$"))
cat(sprintf("\nFertig! Insgesamt %d Coins als 5m-Parquet in data/processed/coins_5m/ gespeichert.\n", n_saved))