library(ggplot2)
library(readr)

calibration_data <- read_delim("aakomp_sup_fig4.tsv", 
                               delim = "\t", 
                               escape_double = FALSE, 
                               trim_ws = TRUE)
fit_cal <- lm(aakomp_score ~ intact_pct, data = calibration_data)
coef_cal <- coef(fit_cal)
r_cal <- cor(calibration_data$intact_pct, calibration_data$aakomp_score)

p_calibration <- ggplot(calibration_data, aes(x = intact_pct, y = aakomp_score)) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed", size = 1.1) +
  geom_smooth(method = "lm", se = FALSE, color = "black", size = 1.1) +
  geom_point(color = "dodgerblue", size = 5) +
  
  annotate("text", 
           x = -Inf, y = Inf, 
           label = paste0("y = ", round(coef_cal[2], 4), "x + ", round(coef_cal[1], 4),
                          "\n"),
           hjust = -0.1, vjust = 1.2, size = 9, color = "black") +
  
  # Set strict identical limits so the 1:1 line is a true 45-degree angle
  scale_x_continuous(limits = c(70, 100), expand = expansion(mult = c(0.05, 0.05))) +
  scale_y_continuous(limits = c(70, 100), expand = expansion(mult = c(0.05, 0.05))) +
  labs(
    x = "Ground truth intact genes (%)",
    y = "aaKomp score"
  ) +
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA),
    axis.ticks = element_line(color = "black"),
    axis.text = element_text(size = 30, color = "black"),
    axis.title = element_text(size = 34)
  )

print(p_calibration)
