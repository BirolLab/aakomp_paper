library(readr)
library(ggplot2)
library(dplyr)
library(cowplot)

df <- read_delim("aakomp_sup_fig5.tsv", 
                 delim = "\t", escape_double = FALSE, 
                 trim_ws = TRUE)

target_line <- 0.7

vals_A_0 <- df %>%
  select(ratio = order1_0bp) %>%
  filter(!is.na(ratio)) %>%
  arrange(ratio) %>%
  mutate(cdf = row_number() / n(), dataset = "0 breakpoints")

vals_A_20k <- df %>%
  select(ratio = order1_20kbp) %>%
  filter(!is.na(ratio)) %>%
  arrange(ratio) %>%
  mutate(cdf = row_number() / n(), dataset = "20000 breakpoints")

vals_A <- bind_rows(vals_A_0, vals_A_20k)
vals_A$dataset <- factor(vals_A$dataset, levels = c("0 breakpoints", "20000 breakpoints"))

vals_B_0 <- df %>%
  select(ratio = order2_0bp) %>%
  filter(!is.na(ratio)) %>%
  arrange(ratio) %>%
  mutate(cdf = row_number() / n(), dataset = "0 breakpoints")

vals_B_20k <- df %>%
  select(ratio = order2_20kbp) %>%
  filter(!is.na(ratio)) %>%
  arrange(ratio) %>%
  mutate(cdf = row_number() / n(), dataset = "20000 breakpoints")

vals_B <- bind_rows(vals_B_0, vals_B_20k)
vals_B$dataset <- factor(vals_B$dataset, levels = c("0 breakpoints", "20000 breakpoints"))

color_map <- c("0 breakpoints" = "black", "20000 breakpoints" = "dodgerblue")

base_theme <- theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1.2),
    axis.ticks = element_line(color = "black", linewidth = 0.8),
    axis.title = element_text(size = 28, face = "plain"),
    axis.text = element_text(size = 22, color = "black"),
    plot.title = element_text(size = 32, face = "bold")
  )

plot_A <- ggplot(vals_A, aes(x = ratio, y = cdf, color = dataset)) +
  geom_step(direction = "hv", linewidth = 2) +
  geom_vline(xintercept = target_line, linetype = "dotted", color = "red", linewidth = 1) +
  scale_color_manual(values = color_map) +
  labs(
    x = expression(italic(k)*"-mer completeness ratio"),
    y = "CDF",
    title = "A"
  ) +
  base_theme +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.18, 0.88),
    legend.title = element_blank(),
    legend.text = element_text(size = 20),
    legend.key = element_blank(),
    legend.background = element_rect(fill = "transparent")
  )

plot_B <- ggplot(vals_B, aes(x = ratio, y = cdf, color = dataset)) +
  geom_step(direction = "hv", linewidth = 2) +
  geom_vline(xintercept = target_line, linetype = "dotted", color = "red", linewidth = 1) +
  scale_color_manual(values = color_map) +
  labs(
    x = expression(italic(k)*"-mer completeness ratio"),
    y = "CDF",
    title = "B"
  ) +
  base_theme +
  theme(legend.position = "none")

combined_plot <- plot_grid(plot_A, plot_B, ncol = 1)

print(combined_plot)