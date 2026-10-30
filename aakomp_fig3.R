library(ggplot2)
library(tidyr)
library(dplyr)
library(ggpubr)
library(cowplot)
library(readr)

hap1_hap2 <- read_delim("aakomp_fig3.tsv", 
                        delim = "\t", escape_double = FALSE, 
                        trim_ws = TRUE)
hap1_hap2$N50 <- as.numeric(hap1_hap2$N50)
hap1_hap2$sample_id <- seq_len(nrow(hap1_hap2))

df_subset <- hap1_hap2[, c("sample_id", "RAM_GB", "Time_Minutes", "aaKomp_score", "N50")]
df_long <- pivot_longer(df_subset, cols = c("RAM_GB", "Time_Minutes", "aaKomp_score", "N50"),
                        names_to = "Metric", values_to = "Value")

df_long <- df_long %>%
  left_join(hap1_hap2[, c("sample_id", "N50")], by = "sample_id") %>%
  rename(N50_value = N50)


df_summary <- df_long %>%
  group_by(Metric) %>%
  summarise(
    min = min(Value, na.rm = TRUE),
    q1 = quantile(Value, 0.25, na.rm = TRUE),
    mean = mean(Value, na.rm = TRUE),
    sd = sd(Value, na.rm = TRUE),
    q3 = quantile(Value, 0.75, na.rm = TRUE),
    max = max(Value, na.rm = TRUE)
  ) %>%
  mutate(
    y_pos = max + 0.05 * (max - min),
    stats_label = paste0("Min: ", round(min, 2), "\n",
                         "Q1: ", round(q1, 2), "\n",
                         "Mean: ", round(mean, 2), "\n",
                         "SD: ", round(sd, 2), "\n",
                         "Q3: ", round(q3, 2), "\n",
                         "Max: ", round(max, 2))
  )

create_boxplot <- function(metric_name, show_legend = FALSE) {
  df_metric <- df_long %>% filter(Metric == metric_name)
  df_summary_metric <- df_summary %>% filter(Metric == metric_name)
  
  ggplot(df_metric, aes(x = Metric, y = Value, color = N50_value)) +
    geom_boxplot(color = "black", fill = "white", outlier.shape = NA, size = 1.2) +
    geom_jitter(width = 0.2,height = 0, size = 3, shape = 16, alpha = 0.7) +  # larger dots
    labs(x = "", y = metric_name) +
    scale_color_gradient(low = "blue", high = "orange", name = "N50 (Mbp)") +
    theme_minimal() +
    theme(
      legend.position = if (show_legend) "left" else "none",
      axis.title = element_text(size = 34),
      axis.text = element_text(size = 30, color = "black"),
      panel.border = element_rect(color = "black", fill = NA, size = 1),
      axis.ticks = element_line(size = 1, color = "black"),
      plot.margin = margin(10, 10, 10, 10),
      legend.text = element_text(size = 14),
      legend.title = element_text(size = 16)
    ) +
    coord_cartesian(clip = "off")
}

custom_sec_axis <- function(min_val, max_val) {
  scale_y_continuous(
    limits = c(min_val, max_val),
    sec.axis = sec_axis(
      trans = ~ ./min_val,
      name = "Relative Scale"
    )
  )
}

range_n50 <- range(df_long$Value[df_long$Metric == "N50"], na.rm = TRUE)
range_aakomp <- range(df_long$Value[df_long$Metric == "aaKomp_score"], na.rm = TRUE)

p1 <- create_boxplot("RAM_GB") +
  theme(legend.position = "none", axis.text.x = element_blank()) +
  labs(x = NULL, y = "Peak Memory (GB)")

p2 <- create_boxplot("Time_Minutes") +
  theme(legend.position = "none", axis.text.x = element_blank()) +
  labs(x = NULL, y = "Run time (mins)")

p3 <- create_boxplot("aaKomp_score", show_legend = TRUE) +  # only this one has legend
  custom_sec_axis(range_aakomp[1], range_aakomp[2]) +
  theme(axis.text.x = element_blank()
) +
  labs(x = NULL, y = "aaKomp score")

p4 <- create_boxplot("N50") +
  custom_sec_axis(range_n50[1], range_n50[2]) +
  theme(axis.text.x = element_blank()) +
  labs(x = NULL, y = "N50 (Mbp)")

final_plot <- plot_grid(
  plot_grid(p3 + theme(legend.position = "none"),
            p4, p1, p2,
            ncol = 4,
            rel_widths = c(1.6, 1.35, 1, 1),
            labels = c("A", "B", "C", "D"),
            label_size = 30),
  rel_widths = c(0.25, 4),  # adjust to give space for legend
  nrow = 1
)

print(final_plot)
