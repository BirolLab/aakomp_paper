library(readr)
library(ggplot2)
library(MASS)
library(cowplot) 


breakpoints <- read_delim("aakomp_sup_fig3.tsv", 
                          delim = "\t", escape_double = FALSE, 
                          trim_ws = TRUE)
breakpoints <- head(breakpoints, -1)
gs_corr <- breakpoints

gs_corr$recip_amn <- gs_corr$aaKomp



fit4 <- rlm(recip_amn ~ Busco, data = gs_corr)
gs_corr$resid_busco <- resid(fit4)


fit3 <- rlm(recip_amn ~ Compleasm, data = gs_corr)
gs_corr$resid_compleasm <- resid(fit3)

y_lim <- range(c(gs_corr$resid_busco, gs_corr$resid_compleasm), na.rm = TRUE)


p_resid_busco <- ggplot(gs_corr, aes(x = Busco, y = resid_busco)) +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed", size = 1.1) +
  geom_point(aes(color = breakpoints), size = 5) +
  scale_color_gradient(low = "dodgerblue", high = "firebrick", name = "Breakpoints", limits = c(0, 100000)) +
  labs(x = "BUSCO completeness (%)", 
       y = "Residuals (aaKomp score - predicted)") +
  scale_x_continuous(expand = expansion(mult = c(0.05, 0.05))) +
  scale_y_continuous(limits = y_lim, expand = expansion(mult = c(0.05, 0.05))) +
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA),
    axis.ticks = element_line(color = "black"),
    axis.text = element_text(size = 30, color = "black"),
    axis.title = element_text(size = 34),
    legend.position = "none" 
  )


p_resid_compleasm <- ggplot(gs_corr, aes(x = Compleasm, y = resid_compleasm)) +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed", size = 1.1) +
  geom_point(aes(color = breakpoints), size = 5) +
  scale_color_gradient(low = "dodgerblue", high = "firebrick", name = "Breakpoints", limits = c(0, 100000)) +
  labs(x = "compleasm completeness (%)", 
       y = "Residuals (aaKomp score - predicted)") +
  scale_x_continuous(expand = expansion(mult = c(0.05, 0.05))) +
  scale_y_continuous(limits = y_lim, expand = expansion(mult = c(0.05, 0.05))) +
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA),
    axis.ticks.x = element_line(color = "black"),
    axis.text.x = element_text(size = 30, color = "black"),
    axis.title.x = element_text(size = 34),
    
    axis.title.y = element_text(size = 34, color = "transparent"),
    axis.text.y = element_text(size = 30, color = "transparent"),
    axis.ticks.y = element_line(color = "transparent"),
    
    legend.title = element_text(size = 28, color = "black", margin = margin(b = 15)),
    legend.text = element_text(size = 22, color = "black"),
    legend.key.height = unit(2.5, "cm"),
    legend.key.width = unit(1, "cm"),
    
    legend.box.margin = margin(r = 20) 
  )


shared_legend <- get_legend(p_resid_compleasm)

p_resid_compleasm <- p_resid_compleasm + theme(legend.position = "none")

fig_ab <- plot_grid(p_resid_busco, p_resid_compleasm, 
                    labels = c("A", "B"), 
                    ncol = 2, 
                    label_size = 30, 
                    align = "hv",
                    axis = "tblr")

fig_residuals <- plot_grid(fig_ab, shared_legend, 
                           ncol = 2, 
                           rel_widths = c(1, 0.12))

print(fig_residuals)