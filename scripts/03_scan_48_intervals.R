library(data.table)
library(arrow)
library(fixest)
library(ggplot2)

dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)
dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)

# ==============================================================================
# 1. PANEL LADEN
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

# ==============================================================================
# 2. HILFSFUNKTION: 48 PANEL-REGRESSIONEN (ANALOG FIGURE 1 & FIGURE A.1)
# ==============================================================================
make_label <- function(k) {
  h1 <- (k - 1L) %/% 2L
  m1 <- ((k - 1L) %% 2L) * 30L
  h2 <- (k %% 48L) %/% 2L
  m2 <- (k %% 2L) * 30L
  sprintf("%02d:%02d-%02d:%02d", h1, m1, h2, m2)
}

run_48_scan <- function(dt, predictor_col, weighted = TRUE) {
  res_list <- vector("list", 48L)

  for (k in 1:48) {
    sub <- dt[interval_id == k]
    fml <- as.formula(paste("ret_30m ~", predictor_col, "| symbol + date"))

    mod <- if (weighted) {
      feols(fml, data = sub, weights = ~vol_lag1d, cluster = ~symbol + date, notes = FALSE)
    } else {
      feols(fml, data = sub, cluster = ~symbol + date, notes = FALSE)
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

cat("Schätze 48 Panel-Regressionen MIT 30m-Skip (Volume-Weighted, Analog Figure 1)...\n")
res_skip_vw <- run_48_scan(panel, "ret_prev_skip", weighted = TRUE)

cat("Schätze 48 Panel-Regressionen OHNE Skip (Volume-Weighted, Analog Figure A.1)...\n")
res_noskip_vw <- run_48_scan(panel, "ret_prev_noskip", weighted = TRUE)

cat("Schätze 48 Panel-Regressionen MIT 30m-Skip (Equal-Weighted, Robustheit)...\n")
res_skip_ew <- run_48_scan(panel, "ret_prev_skip", weighted = FALSE)

# Ergebnisse zusätzlich als CSV speichern (für spätere Checks)
fwrite(res_skip_vw,   "output/tables/scan48_utc_skip_vw.csv")
fwrite(res_noskip_vw, "output/tables/scan48_utc_noskip_vw.csv")
fwrite(res_skip_ew,   "output/tables/scan48_utc_skip_ew.csv")

# ==============================================================================
# 3. PLOTS IM STIL VON BALTUSSEN, DA & SOEBHAG (2026)
# ==============================================================================
plot_scan <- function(res, title_txt, subtitle_txt) {
  d <- copy(res)
  d[, label := factor(label, levels = label)]

  # Markierungen:
  # k = 40 (19:30-20:00 UTC) -> 15:30-16:00 EDT (US-Sommerzeit)
  # k = 42 (20:30-21:00 UTC) -> 15:30-16:00 EST (US-Winterzeit)
  # k = 48 (23:30-00:00 UTC) -> Krypto-Tageskerzen-Schluss (00:00 UTC)
  ggplot(d, aes(x = label, y = beta)) +
    annotate("rect", xmin = 39.5, xmax = 42.5, ymin = -Inf, ymax = Inf,
             fill = "#2C7BB6", alpha = 0.12) +
    annotate("rect", xmin = 47.5, xmax = 48.5, ymin = -Inf, ymax = Inf,
             fill = "#D7191C", alpha = 0.15) +
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

sub_info <- sprintf(
  "Binance Spot (%d Coins, %s bis %s) | Blau: US-Close-Fenster (19:30-21:00 UTC) | Rot: 00:00 UTC Close",
  n_coins, min(panel$date), max(panel$date)
)

p_skip_vw <- plot_scan(
  res_skip_vw,
  "Crypto 48-Interval Scan (Volume-Weighted, 30-Min Skip) - Analog Figure 1",
  sub_info
)
p_noskip_vw <- plot_scan(
  res_noskip_vw,
  "Crypto 48-Interval Scan (Volume-Weighted, No Skip) - Analog Figure A.1",
  sub_info
)
p_skip_ew <- plot_scan(
  res_skip_ew,
  "Crypto 48-Interval Scan (Equal-Weighted, 30-Min Skip)",
  sub_info
)

# Als PDF (für LaTeX) und PNG (für schnelle Vorschau in Cursor) speichern
ggsave("output/figures/fig1_crypto_48scan_skip_vw.pdf",   p_skip_vw,   width = 12, height = 5)
ggsave("output/figures/fig1_crypto_48scan_skip_vw.png",   p_skip_vw,   width = 12, height = 5, dpi = 200)
ggsave("output/figures/figA1_crypto_48scan_noskip_vw.pdf", p_noskip_vw, width = 12, height = 5)
ggsave("output/figures/figA1_crypto_48scan_noskip_vw.png", p_noskip_vw, width = 12, height = 5, dpi = 200)
ggsave("output/figures/fig1b_crypto_48scan_skip_ew.pdf",  p_skip_ew,   width = 12, height = 5)
ggsave("output/figures/fig1b_crypto_48scan_skip_ew.png",  p_skip_ew,   width = 12, height = 5, dpi = 200)

# ==============================================================================
# 4. KERNINTERVALLE IM TERMINAL AUSGEBEN
# ==============================================================================
cat("\n================ KERNINTERVALLE (WITH 30m SKIP, VW) ================\n")
print(res_skip_vw[interval_id %in% c(40, 41, 42, 47, 48),
                  .(interval_id, label, beta = round(beta, 3), t_stat = round(t_stat, 2), n_obs)])

cat("\n================ KERNINTERVALLE (NO SKIP, VW) ======================\n")
print(res_noskip_vw[interval_id %in% c(40, 41, 42, 47, 48),
                    .(interval_id, label, beta = round(beta, 3), t_stat = round(t_stat, 2), n_obs)])

cat("\nFertig! Grafiken gespeichert in output/figures/\n")