library(data.table)
library(arrow)
library(fixest)
library(ggplot2)

panel <- as.data.table(read_parquet("data/processed/panel_30m_utc.parquet"))

make_label <- function(k) {
  sprintf("%02d:%02d-%02d:%02d", (k - 1L) %/% 2L, ((k - 1L) %% 2L) * 30L,
          (k %% 48L) %/% 2L, (k %% 2L) * 30L)
}

run_asym_scan <- function(dt, sign_label) {
  res <- vector("list", 48L)
  for (k in 1:48) {
    sub <- dt[interval_id == k]
    mod <- feols(ret_30m ~ ret_prev_skip | symbol + ts_30m,
                 data = sub, weights = sub$w_vw_cap5,
                 cluster = ~symbol + date, notes = FALSE)
    b <- unname(coef(mod)[1]) * 100
    s <- unname(se(mod)[1]) * 100
    res[[k]] <- data.table(
      interval_id = k, label = make_label(k), sign = sign_label,
      beta = b, se = s, ci_low = b - 1.96 * s, ci_high = b + 1.96 * s,
      t_stat = unname(tstat(mod)[1])
    )
  }
  rbindlist(res)
}

cat("Schätze Figure 3 (Asymmetrie: ROD3 > 0 vs. ROD3 < 0, 5%-Capped-VW)...\n")
res_pos <- run_asym_scan(panel[ret_prev_skip > 0], "Positive Prior 23h Return (Winners: ROD3 > 0)")
res_neg <- run_asym_scan(panel[ret_prev_skip < 0], "Negative Prior 23h Return (Losers: ROD3 < 0)")
res_asym <- rbindlist(list(res_pos, res_neg))

fwrite(res_asym, "output/tables/figure3_asymmetry_utc_cap5.csv")

lbls <- make_label(1:48)
res_asym[, label := factor(label, levels = lbls)]
res_asym[, sign  := factor(sign, levels = c(
  "Positive Prior 23h Return (Winners: ROD3 > 0)",
  "Negative Prior 23h Return (Losers: ROD3 < 0)"
))]

p_asym <- ggplot(res_asym, aes(x = label, y = beta)) +
  annotate("rect", xmin = 39.5, xmax = 40.5, ymin = -Inf, ymax = Inf, fill = "#2C7BB6", alpha = 0.14) +
  annotate("rect", xmin = 41.5, xmax = 42.5, ymin = -Inf, ymax = Inf, fill = "#2C7BB6", alpha = 0.14) +
  annotate("rect", xmin = 47.5, xmax = 48.5, ymin = -Inf, ymax = Inf, fill = "#D7191C", alpha = 0.16) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.6) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.35, linewidth = 0.45) +
  geom_point(shape = 21, fill = "white", color = "black", size = 1.6, stroke = 0.65) +
  facet_wrap(~sign, ncol = 1, scales = "fixed") +
  scale_x_discrete(breaks = lbls[seq(1, 48, by = 2)]) +
  labs(
    title    = "Asymmetric Intraday Predictability in Crypto (Analog Figure 3)",
    subtitle = "Binance Spot (5%-Capped-VW, 30m Skip) | Blau: US-Close (EDT/EST) | Rot: 00:00 UTC Close",
    x        = "30-Minuten-Intervall (UTC)",
    y        = "Slope Coefficient (x 100)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title       = element_text(face = "bold", size = 12),
    strip.text       = element_text(face = "bold", size = 10.5),
    axis.text.x      = element_text(angle = 45, hjust = 1, size = 8.5),
    panel.grid.minor = element_blank(),
    panel.border     = element_rect(color = "grey40", fill = NA, linewidth = 0.5)
  )

ggsave("output/figures/fig3_crypto_asymmetry_cap5.pdf", p_asym, width = 12, height = 7.5)
ggsave("output/figures/fig3_crypto_asymmetry_cap5.png", p_asym, width = 12, height = 7.5, dpi = 200)

cat("\nKernintervalle Asymmetrie (Slot 48 = 23:30-00:00 UTC):\n")
print(res_asym[interval_id %in% c(40, 41, 42, 48),
               .(sign, interval_id, label, beta = round(beta, 3), t_stat = round(t_stat, 2))])