library(readr)
library(ggplot2)
library(MASS)
library(ggrepel)
library(cowplot)

breakpoints_data <- read_delim("aakomp_fig1.tsv", 
                               delim = "\t", escape_double = FALSE, 
                               trim_ws = TRUE)
gs_corr <- breakpoints_data
gs_corr$recip_amn <- gs_corr$aaKomp

keep_labels <- seq(0, 90000, by = 10000)

cor_test_busco_recip_amn <- cor.test(gs_corr$Busco, gs_corr$recip_amn, use = "complete.obs")
cor_busco_recip_amn <- signif(cor_test_busco_recip_amn$estimate, 4)
ci_busco_recip_amn <- signif(cor_test_busco_recip_amn$conf.int, 4)
fit4 <- rlm(recip_amn ~ Busco, data = gs_corr)
coef4 <- coef(fit4)

cor_test_compleasm_recip_amn <- cor.test(gs_corr$Compleasm, gs_corr$recip_amn, use = "complete.obs")
cor_compleasm_recip_amn <- signif(cor_test_compleasm_recip_amn$estimate, 4)
ci_compleasm_recip_amn <- signif(cor_test_compleasm_recip_amn$conf.int, 4)
fit3 <- rlm(recip_amn ~ Compleasm, data = gs_corr)
coef3 <- coef(fit3)

p4 <- ggplot(gs_corr, aes(x = Busco, y = recip_amn)) +
  geom_point(aes(size = ifelse(breakpoints %in% keep_labels, 5, 2)), color = "blue") +
  scale_size_identity() +
  geom_abline(intercept = coef4[1], slope = coef4[2], color = "black", size = 1.1) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed", size = 1.1) +
  annotate("text", 
           x = -Inf, y = Inf, 
           label = paste0("y = ", round(coef4[2], 4), "x + ", round(coef4[1], 4)),
           hjust = -0.1, vjust = 1.5, size = 9) +
  annotate("text", 
           x = Inf, y = -Inf,
           label = paste0("r = ", cor_busco_recip_amn, 
                          " \n95% CI: [", ci_busco_recip_amn[1], ", ", ci_busco_recip_amn[2], "] "),
           hjust = 1, vjust = -0.7, size = 9) +
  labs(x = "BUSCO completeness (%)", 
       y = "aaKomp Score") +
  scale_x_continuous(expand = expansion(mult = c(0.12, 0.12))) + 
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.05))) +
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA),
    axis.ticks = element_line(color = "black"),
    axis.text = element_text(size = 30, color = "black"),
    axis.title = element_text(size = 34)
  ) +
  geom_text_repel(
    aes(label = ifelse(breakpoints %in% keep_labels, as.character(breakpoints), "")),
    size = 7,
    nudge_x = ifelse(gs_corr$breakpoints == 90000, 5, 
                     ifelse(gs_corr$breakpoints == 0, -7, -5)),
    direction = "x",
    hjust = ifelse(gs_corr$breakpoints == 90000, 0, 1),
    min.segment.length = 0,
    segment.color = "grey50",
    segment.size = 0.5,
    box.padding = 0.5,
    force = 0
  )

p3 <- ggplot(gs_corr, aes(x = Compleasm, y = recip_amn)) +
  geom_point(aes(size = ifelse(breakpoints %in% keep_labels, 5, 2)), color = "blue") +
  scale_size_identity() +
  geom_abline(intercept = coef3[1], slope = coef3[2], color = "black", size = 1.1) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed", size = 1.1) +
  annotate("text", 
           x = -Inf, y = Inf, 
           label = paste0("y = ", round(coef3[2], 4), "x + ", round(coef3[1], 4)),
           hjust = -0.1, vjust = 1.5, size = 9) +
  annotate("text", 
           x = Inf, y = -Inf,
           label = paste0("r = ", cor_compleasm_recip_amn, 
                          " \n95% CI: [", ci_compleasm_recip_amn[1], ", ", ci_compleasm_recip_amn[2], "] "),
           hjust = 1, vjust = -0.7, size = 9) +
  labs(x = "compleasm completeness (%)", 
       y = "") +
  scale_x_continuous(expand = expansion(mult = c(0.12, 0.12))) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.05))) +
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA),
    axis.ticks = element_line(color = "black"),
    axis.text = element_text(size = 30, color = "black"),
    axis.title = element_text(size = 34)
  ) +
  geom_text_repel(
    aes(label = ifelse(breakpoints %in% keep_labels, as.character(breakpoints), "")),
    size = 7,
    nudge_x = ifelse(gs_corr$breakpoints == 90000, 5, 
                     ifelse(gs_corr$breakpoints == 0, -7, -5)),
    direction = "x",
    hjust = ifelse(gs_corr$breakpoints == 90000, 0, 1),
    min.segment.length = 0,
    segment.color = "grey50",
    segment.size = 0.5,
    box.padding = 0.5,
    force = 0
  )

fig1 <- plot_grid(p4, p3, labels = c("A", "B"), ncol = 2, label_size = 30)

print(fig1)