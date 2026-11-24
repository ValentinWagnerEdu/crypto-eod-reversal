library(data.table)

# ==============================================================================
# KONFIGURATION
# ==============================================================================
FIG_DIR  <- "output/figures"
TAB_DIR  <- "output/tables"
OUT_HTML <- "output/report_zwischenpraesi.html"
MAX_ROWS_FULL <- 100L   # Tabellen bis 100 Zeilen komplett zeigen
PREVIEW_ROWS  <- 25L    # Große Tages-CSVs als aufklappbare 25-Zeilen-Vorschau

# ==============================================================================
# HILFSFUNKTIONEN (OHNE ZUSÄTZLICHE PAKETE)
# ==============================================================================
encode_base64 <- function(file_path) {
  raw_bytes <- readBin(file_path, what = "raw", n = file.info(file_path)$size)
  if (requireNamespace("base64enc", quietly = TRUE)) {
    return(base64enc::base64encode(raw_bytes))
  }
  if (requireNamespace("jsonlite", quietly = TRUE)) {
    return(gsub("[\r\n]", "", jsonlite::base64_enc(raw_bytes)))
  }
  # Base-R Fallback
  alphabet <- c(LETTERS, letters, 0:9, "+", "/")
  n   <- length(raw_bytes)
  pad <- (3L - (n %% 3L)) %% 3L
  if (pad > 0L) raw_bytes <- c(raw_bytes, as.raw(rep(0L, pad)))
  m   <- matrix(as.numeric(raw_bytes), nrow = 3L)
  v   <- m[1L, ] * 65536 + m[2L, ] * 256 + m[3L, ]
  idx <- rbind(
    (v %/% 262144) %% 64,
    (v %/% 4096) %% 64,
    (v %/% 64) %% 64,
    v %% 64
  ) + 1
  out <- alphabet[as.integer(idx)]
  if (pad > 0L) out[(length(out) - pad + 1L):length(out)] <- "="
  paste0(out, collapse = "")
}

html_escape <- function(x) {
  x <- as.character(x)
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x
}

fmt_col <- function(col) {
  if (is.integer(col)) {
    format(col, big.mark = ",", trim = TRUE, scientific = FALSE)
  } else if (is.numeric(col)) {
    ifelse(is.na(col), "", sprintf("%.3f", col))
  } else {
    ifelse(is.na(col), "", as.character(col))
  }
}

dt_to_html_table <- function(dt) {
  headers <- paste0("<th>", html_escape(names(dt)), "</th>", collapse = "")
  formatted <- lapply(dt, fmt_col)
  n_rows <- nrow(dt)
  n_cols <- ncol(dt)

  rows <- vapply(seq_len(n_rows), function(i) {
    cells <- vapply(seq_len(n_cols), function(j) {
      cls <- if (is.numeric(dt[[j]])) ' class="num"' else ""
      paste0("<td", cls, ">", html_escape(formatted[[j]][i]), "</td>")
    }, character(1))
    paste0("<tr>", paste0(cells, collapse = ""), "</tr>")
  }, character(1))

  paste0(
    '<div class="table-wrap"><table><thead><tr>', headers,
    "</tr></thead><tbody>", paste0(rows, collapse = "\n"),
    "</tbody></table></div>"
  )
}

# ==============================================================================
# 1. DATEIEN EINSAMMELN
# ==============================================================================
png_files <- sort(list.files(FIG_DIR, pattern = "\\.png$", full.names = TRUE, ignore.case = TRUE))
csv_files <- sort(list.files(TAB_DIR, pattern = "\\.csv$", full.names = TRUE, ignore.case = TRUE))

cat(sprintf("Gefunden: %d PNG-Grafiken und %d CSV-Tabellen.\n", length(png_files), length(csv_files)))

# ==============================================================================
# 2. HTML-SECTIONS BAUEN
# ==============================================================================
toc_figs <- character(0)
sec_figs <- character(0)

for (i in seq_along(png_files)) {
  f     <- png_files[i]
  fname <- basename(f)
  id    <- sprintf("fig-%02d", i)
  b64   <- encode_base64(f)

  toc_figs <- c(toc_figs, sprintf('<li><a href="#%s">%s</a></li>', id, html_escape(fname)))
  sec_figs <- c(sec_figs, sprintf(
    '<div class="card" id="%s">
       <div class="card-header"><span>%s</span><a class="top-link" href="#top">Nach oben</a></div>
       <img src="data:image/png;base64,%s" alt="%s" />
     </div>',
    id, html_escape(fname), b64, html_escape(fname)
  ))
}

toc_tabs <- character(0)
sec_tabs <- character(0)

for (i in seq_along(csv_files)) {
  f     <- csv_files[i]
  fname <- basename(f)
  id    <- sprintf("tab-%02d", i)
  dt    <- tryCatch(fread(f), error = function(e) NULL)
  if (is.null(dt) || nrow(dt) == 0L) next

  n_tot <- nrow(dt)
  toc_tabs <- c(toc_tabs, sprintf(
    '<li><a href="#%s">%s</a> <span class="badge">%s Zeilen</span></li>',
    id, html_escape(fname), format(n_tot, big.mark = ",")
  ))

  if (n_tot <= MAX_ROWS_FULL) {
    body_html <- dt_to_html_table(dt)
  } else {
    note <- sprintf(
      '<p class="note">Große Datei (%s Zeilen &times; %d Spalten) &ndash; zeige erste %d Zeilen als Vorschau.</p>',
      format(n_tot, big.mark = ","), ncol(dt), PREVIEW_ROWS
    )
    body_html <- paste0(
      "<details><summary>Vorschau aufklappen (", format(n_tot, big.mark = ","), " Zeilen)</summary>",
      note, dt_to_html_table(dt[seq_len(PREVIEW_ROWS)]), "</details>"
    )
  }

  sec_tabs <- c(sec_tabs, sprintf(
    '<div class="card" id="%s">
       <div class="card-header"><span>%s <small>(%s Zeilen &times; %d Spalten)</small></span><a class="top-link" href="#top">Nach oben</a></div>
       %s
     </div>',
    id, html_escape(fname), format(n_tot, big.mark = ","), ncol(dt), body_html
  ))
}

# ==============================================================================
# 3. GESAMT-HTML SCHREIBEN
# ==============================================================================
html_doc <- paste0('<!DOCTYPE html>
<html lang="de">
<head>
<meta charset="UTF-8">
<title>Crypto EOD Reversal - Ergebnis-Dashboard</title>
<style>
  :root { --bg: #f6f7f9; --card: #ffffff; --border: #dde2e8; --accent: #1d4ed8; --text: #1f2937; --muted: #6b7280; }
  body { font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif; background: var(--bg); color: var(--text); margin: 0; padding: 24px 32px; line-height: 1.45; }
  h1 { margin: 0 0 6px 0; font-size: 1.65rem; }
  .subtitle { color: var(--muted); margin-bottom: 24px; font-size: 0.92rem; }
  h2 { margin: 32px 0 14px 0; padding-bottom: 6px; border-bottom: 2px solid var(--border); font-size: 1.25rem; }
  .toc { display: grid; grid-template-columns: 1fr 1fr; gap: 20px; background: var(--card); border: 1px solid var(--border); border-radius: 10px; padding: 18px 22px; margin-bottom: 28px; }
  .toc h3 { margin: 0 0 10px 0; font-size: 1rem; color: var(--accent); }
  .toc ul { margin: 0; padding-left: 18px; font-size: 0.88rem; }
  .toc li { margin-bottom: 4px; }
  .toc a { color: var(--text); text-decoration: none; }
  .toc a:hover { color: var(--accent); text-decoration: underline; }
  .badge { background: #eef2ff; color: var(--accent); padding: 1px 6px; border-radius: 999px; font-size: 0.76rem; margin-left: 4px; }
  .card { background: var(--card); border: 1px solid var(--border); border-radius: 10px; padding: 16px 20px; margin-bottom: 22px; box-shadow: 0 1px 2px rgba(0,0,0,0.03); }
  .card-header { display: flex; justify-content: space-between; align-items: center; font-weight: 600; font-size: 0.98rem; margin-bottom: 12px; padding-bottom: 8px; border-bottom: 1px solid #eef0f3; }
  .card-header small { font-weight: 400; color: var(--muted); margin-left: 6px; }
  .top-link { font-size: 0.78rem; font-weight: 400; color: var(--muted); text-decoration: none; }
  .top-link:hover { color: var(--accent); }
  img { width: 100%; height: auto; display: block; border-radius: 4px; }
  .table-wrap { overflow-x: auto; }
  table { border-collapse: collapse; width: 100%; font-size: 0.84rem; }
  th, td { padding: 6px 10px; border-bottom: 1px solid #edf0f4; text-align: left; white-space: nowrap; }
  th { background: #f8fafc; font-weight: 600; position: sticky; top: 0; border-bottom: 2px solid var(--border); }
  td.num { text-align: right; font-variant-numeric: tabular-nums; }
  tr:hover td { background: #f9fbff; }
  details summary { cursor: pointer; color: var(--accent); font-weight: 500; font-size: 0.88rem; }
  .note { font-size: 0.82rem; color: var(--muted); margin: 8px 0; }
  @media print {
    body { background: #fff; padding: 0; }
    .top-link { display: none; }
    .card { break-inside: avoid; box-shadow: none; border: 1px solid #ccc; }
  }
</style>
</head>
<body id="top">
  <h1>Crypto End-of-Day Reversal &ndash; Ergebnis-Dashboard</h1>
  <div class="subtitle">Automatisch generiert am ', format(Sys.time(), "%d.%m.%Y um %H:%M Uhr"), ' | ', length(png_files), ' Grafiken &amp; ', length(csv_files), ' Tabellen</div>

  <div class="toc">
    <div><h3>Grafiken (', length(png_files), ')</h3><ul>', paste0(toc_figs, collapse = "\n"), '</ul></div>
    <div><h3>Tabellen (', length(csv_files), ')</h3><ul>', paste0(toc_tabs, collapse = "\n"), '</ul></div>
  </div>

  <h2>1. Abbildungen (output/figures)</h2>
  ', paste0(sec_figs, collapse = "\n"), '

  <h2>2. Ergebnistabellen (output/tables)</h2>
  ', paste0(sec_tabs, collapse = "\n"), '
</body>
</html>')

writeLines(html_doc, OUT_HTML, useBytes = TRUE)
cat("Fertig! Einzeldatei gespeichert unter:", OUT_HTML, "\n")