library(readr)
library(ggplot2)
library(cowplot)
library(scales) 

rep_data <- read_delim("aakomp_sup_fig9.tsv", 
                       delim = "\t", escape_double = FALSE, 
                       trim_ws = TRUE)

rep_data$time_min <- pmin(rep_data$time_rep1_min, rep_data$time_rep2_min, rep_data$time_rep3_min, na.rm = TRUE)
rep_data$time_max <- pmax(rep_data$time_rep1_min, rep_data$time_rep2_min, rep_data$time_rep3_min, na.rm = TRUE)

rep_data$rss_min <- pmin(rep_data$rss_rep1_gb, rep_data$rss_rep2_gb, rep_data$rss_rep3_gb, na.rm = TRUE)
rep_data$rss_max <- pmax(rep_data$rss_rep1_gb, rep_data$rss_rep2_gb, rep_data$rss_rep3_gb, na.rm = TRUE)

base_theme <- theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1.2),
    axis.ticks = element_line(color = "black", linewidth = 0.8),
    axis.text = element_text(size = 26, color = "black"),
    axis.title = element_text(size = 30, color = "black"),
    plot.margin = margin(t = 10, r = 15, b = 10, l = 15)
  )

p_time <- ggplot(rep_data, aes(x = breakpoints, y = time_mean_min)) +
  geom_line( linewidth = 1.2) +
  geom_errorbar(aes(ymin = time_min, ymax = time_max), 
                width = 1500, linewidth = 0.8) +
  geom_point(size = 3.5) +
  scale_x_continuous(breaks = seq(0, 100000, by = 20000), 
                     labels = comma,
                     limits = c(-2000, 102000), 
                     expand = expansion(mult = c(0.01, 0.02))) +
  scale_y_continuous(breaks = seq(0, 2.0, by = 0.5), 
                     limits = c(0, 2.1), 
                     expand = expansion(mult = c(0.02, 0.05))) +
  labs(y = "Run time (min)", x = NULL) +   
  base_theme +
  theme(axis.title.x = element_blank())

p_ram <- ggplot(rep_data, aes(x = breakpoints, y = rss_mean_gb)) +
  geom_line(linewidth = 1.2) +
  geom_errorbar(aes(ymin = rss_min, ymax = rss_max), 
                width = 1500, linewidth = 0.8) +
  geom_point(, size = 3.5) +
  scale_x_continuous(breaks = seq(0, 100000, by = 20000), 
                     labels = comma,
                     limits = c(-2000, 102000), 
                     expand = expansion(mult = c(0.01, 0.02))) +
  scale_y_continuous(breaks = seq(0, 18, by = 3), 
                     limits = c(0, 18), 
                     expand = expansion(mult = c(0.02, 0.05))) +
  labs(y = "Peak Memory (GB)", x = "Simulated Breakpoints") +
  base_theme

final_fig_s9 <- plot_grid(
  p_time, 
  p_ram, 
  labels = c("A", "B"), 
  ncol = 1, 
  label_size = 32,
  align = "v"
)

print(final_fig_s9)


