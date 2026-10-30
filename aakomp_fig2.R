library(ggplot2)
library(tidyr)
library(dplyr)
library(cowplot)
library(scales)

run_time <- read.delim("aakomp_fig2a.tsv") %>%
  rename(compleasm = Compleasm)


data_time <- run_time




data_long_time <- pivot_longer(data_time, cols = everything(),
                               names_to = "Metric", values_to = "Value")


p_run_time <- ggplot(data_long_time, aes(x = Metric, y = Value)) +
  geom_boxplot(color = "black", fill = "white", outlier.shape = NA, size = 1.2) +
  geom_jitter(width = 0.2, size = 3, alpha = 0.7) +
  labs(x = "", y = "Run Time (mins)") +
  scale_x_discrete(expand = expansion(add = c(0.5, 1))) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.title = element_text(size = 34),
    axis.text = element_text(size = 30, color = "black"),
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    axis.ticks = element_line(size = 1, color = "black"),
    plot.margin = margin(10, 50, 10, 40),
    axis.title.y = element_text(size = 34)
  ) +
  coord_cartesian(clip = "off") 


run_ram <- read.delim("aakomp_fig2a.tsv") %>%
  rename(compleasm = Compleasm)
data_ram <- run_ram

data_long_ram <- pivot_longer(data_ram, cols = everything(),
                              names_to = "Metric", values_to = "Value")


p_run_ram <- ggplot(data_long_ram, aes(x = Metric, y = Value)) +
  geom_boxplot(color = "black", fill = "white", outlier.shape = NA, size = 1.2) +
  geom_jitter(width = 0.2, size = 3, alpha = 0.7) +
  labs(x = "", y = "RAM usage (GB)") +
  scale_x_discrete(expand = expansion(add = c(0.5, 1))) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.title = element_text(size = 34),
    axis.text = element_text(size = 30, color = "black"),
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    axis.ticks = element_line(size = 1, color = "black"),
    plot.margin = margin(10, 50, 10, 40),
  ) +
  coord_cartesian(clip = "off") 


final_plot <- plot_grid(p_run_time, p_run_ram, ncol = 1, labels = c("A", "B"), label_size = 30)
print(final_plot)
