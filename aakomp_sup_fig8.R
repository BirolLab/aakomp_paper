library(readr)
library(ggplot2)
library(cowplot)
library(ggrepel)
library(grid)

scaling_data <- read_delim("aakomp_sup_fig8.tsv", 
                           delim = "\t", escape_double = FALSE, 
                           trim_ws = TRUE)

scaling_data$speedup <- as.numeric(gsub("x", "", scaling_data$speedup_vs_4t))
scaling_data$cpu_util <- as.numeric(gsub("%", "", scaling_data$cpu_util_pct))

bp_levels <- c(0, 24000, 48000, 72000, 96000)
scaling_data$Assembly <- factor(
  ifelse(scaling_data$breakpoints == 0, "0 bp (Intact)", paste0(format(scaling_data$breakpoints, big.mark = ","), " bp")),
  levels = c("0 bp (Intact)", paste0(format(bp_levels[-1], big.mark = ","), " bp"))
)

bp_colors <- c(
  "0 bp (Intact)" = "#D55E00",   # Highlighted orange-red for chromosome-scale assembly
  "24,000 bp"     = "#0072B2",   # Blue
  "48,000 bp"     = "#009E73",   # Green
  "72,000 bp"     = "#CC79A7",   # Purple
  "96,000 bp"     = "#56B4E9"    # Sky blue
)

ideal_line <- data.frame(
  threads = c(4, 12, 24, 48),
  speedup = c(1, 3, 6, 12)
)

base_theme <- theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1.2),
    axis.ticks = element_line(color = "black", linewidth = 0.8),
    axis.text = element_text(size = 26, color = "black"),
    axis.title.y = element_text(size = 30, color = "black"),
    axis.title.x = element_blank(),   # Removed from subplots
    legend.position = "none"          # Subplots have NO individual legends
  )

p_speedup <- ggplot(scaling_data, aes(x = threads, y = speedup, color = Assembly, group = Assembly)) +
  geom_line(data = ideal_line, aes(x = threads, y = speedup), 
            color = "gray50", linetype = "dashed", linewidth = 1.2, inherit.aes = FALSE) +
  geom_line(linewidth = 1.4) +
  geom_point(size = 4.5) +
  scale_color_manual(values = bp_colors) +
  scale_x_continuous(breaks = c(4, 12, 24, 48), limits = c(3, 50), expand = expansion(mult = c(0.02, 0.05))) +
  scale_y_continuous(breaks = seq(0, 12, by = 2), limits = c(0, 13), expand = expansion(mult = c(0.02, 0.05))) +
  labs(y = "Speedup (relative to 4T)") +
  base_theme

p_time <- ggplot(scaling_data, aes(x = threads, y = wall_time_min, color = Assembly, group = Assembly)) +
  geom_line(linewidth = 1.4) +
  geom_point(size = 4.5) +
  scale_color_manual(values = bp_colors) +
  scale_x_continuous(breaks = c(4, 12, 24, 48), limits = c(3, 50), expand = expansion(mult = c(0.02, 0.05))) +
  scale_y_continuous(breaks = seq(0, 7, by = 1), limits = c(0, 7), expand = expansion(mult = c(0.02, 0.05))) +
  labs(y = "Run time (min)") +
  base_theme

p_ram <- ggplot(scaling_data, aes(x = threads, y = max_rss_gb, color = Assembly, group = Assembly)) +
  geom_line(linewidth = 1.4) +
  geom_point(size = 4.5) +
  scale_color_manual(values = bp_colors) +
  scale_x_continuous(breaks = c(4, 12, 24, 48), limits = c(3, 50), expand = expansion(mult = c(0.02, 0.05))) +
  scale_y_continuous(breaks = seq(0, 18, by = 3), limits = c(0, 18), expand = expansion(mult = c(0.02, 0.05))) +
  labs(y = "Peak Memory (GB)") +
  base_theme

p_cpu <- ggplot(scaling_data, aes(x = threads, y = cpu_util, color = Assembly, group = Assembly)) +
  geom_line(linewidth = 1.4) +
  geom_point(size = 4.5) +
  scale_color_manual(values = bp_colors) +
  scale_x_continuous(breaks = c(4, 12, 24, 48), limits = c(3, 50), expand = expansion(mult = c(0.02, 0.05))) +
  scale_y_continuous(breaks = seq(0, 4500, by = 1000), limits = c(0, 4600), expand = expansion(mult = c(0.02, 0.05))) +
  labs(y = "CPU Utilization (%)") +
  base_theme

plot_grid_combined <- plot_grid(
  p_speedup, p_time, p_ram, p_cpu,
  labels = c("A", "B", "C", "D"),
  ncol = 2,
  label_size = 32,
  align = "hv",
  axis = "tblr"
)

x_axis_title <- ggdraw() + 
  draw_label("Allocated CPU Threads", size = 32, fontface = "bold")

legend_plot <- ggplot(scaling_data, aes(x = threads, y = speedup, color = Assembly)) +
  geom_point(size = 5) +
  geom_line(linewidth = 1.5) +
  scale_color_manual(values = bp_colors) +
  guides(color = guide_legend(
    title = "Assembly:",
    direction = "horizontal",
    nrow = 1,
    byrow = TRUE,
    title.position = "left",
    title.vjust = 0.5,
    keywidth = unit(2.5, "lines")
  )) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "horizontal",
    legend.justification = "center",
    legend.text = element_text(size = 22),
    legend.title = element_text(size = 24, face = "bold")
  )

extract_legend <- function(p) {
  leg <- tryCatch(cowplot::get_plot_component(p, "guide-box-bottom", return_all = FALSE), error = function(e) NULL)
  if (!is.null(leg) && !inherits(leg, "zeroGrob")) return(leg)
  
  leg <- tryCatch(cowplot::get_legend(p), error = function(e) NULL)
  if (!is.null(leg) && !inherits(leg, "zeroGrob")) return(leg)
  
  g <- ggplotGrob(p)
  guide_idx <- grep("guide-box", g$layout$name)
  if (length(guide_idx) > 0) {
    for (i in guide_idx) {
      candidate <- g$grobs[[i]]
      if (!inherits(candidate, "zeroGrob")) return(candidate)
    }
  }
  return(NULL)
}

shared_legend <- extract_legend(legend_plot)

# Combine into Final Figure
final_fig_s8 <- plot_grid(
  plot_grid_combined, 
  x_axis_title, 
  shared_legend, 
  ncol = 1, 
  rel_heights = c(1, 0.05, 0.07)
)
print(final_fig_s8)