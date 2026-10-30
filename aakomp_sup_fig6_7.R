library(readr)
library(ggplot2)
library(cowplot)
library(ggrepel)

df <- read_delim("aakomp_sup_fig6_7.tsv", 
                 delim = "\t", escape_double = FALSE, 
                 trim_ws = TRUE)

df$species_label <- sub("^([A-Z])[a-z]+ ", "\\1. ", df$species)

prim_levels <- c("Hominidae", "Hylobatidae", "Cercopithecidae", "Cebidae", "Tarsiidae", "Cheirogaleidae")
prim_colors <- c(
  "Hominidae"       = "#D55E00",
  "Hylobatidae"     = "#0072B2",
  "Cercopithecidae" = "#009E73",
  "Cebidae"         = "#E69F00",
  "Tarsiidae"       = "#CC79A7",
  "Cheirogaleidae"  = "#56B4E9"
)

eel_levels <- c("Anguilliformes", "Albuliformes", "Elopiformes", "Cypriniformes", "Osteoglossiformes")
eel_colors <- c(
  "Anguilliformes"    = "#0072B2",
  "Albuliformes"      = "#009E73",
  "Elopiformes"       = "#E69F00",
  "Cypriniformes"     = "#D55E00",
  "Osteoglossiformes" = "#CC79A7"
)

bee_levels <- c("Bombus", "Other Apidae", "Halictidae", "Megachilidae", "Vespidae", "Formicidae")
bee_colors <- c(
  "Bombus"       = "#D55E00",
  "Other Apidae" = "#E69F00",
  "Halictidae"   = "#0072B2",
  "Megachilidae" = "#009E73",
  "Vespidae"     = "#CC79A7",
  "Formicidae"   = "#56B4E9"
)

rice_levels <- c("Oryza", "Leersia", "Pooideae", "Panicoideae", "Bromeliaceae")
rice_colors <- c(
  "Oryza"        = "#009E73",
  "Leersia"      = "#56B4E9",
  "Pooideae"     = "#0072B2",
  "Panicoideae"  = "#E69F00",
  "Bromeliaceae" = "#D55E00"
)

base_theme <- theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1.2),
    axis.ticks = element_line(color = "black", linewidth = 0.8),
    axis.text = element_text(size = 20, color = "black"),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    legend.position = "none",
    plot.margin = margin(t = 28, r = 8, b = 6, l = 8)
  )

extract_legend <- function(p) {
  leg <- tryCatch(cowplot::get_plot_component(p, "guide-box-right", return_all = FALSE), error = function(e) NULL)
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

create_organism_plots <- function(df_sub, org_name, clade_levels, clade_colors,
                                  mya_breaks, mya_limits, mash_breaks, mash_limits,
                                  y_breaks, y_limits, legend_title = "Taxonomic Clade:") {
  
  df_org <- subset(df_sub, organism == org_name)
  df_org$clade <- factor(df_org$clade, levels = clade_levels)
  
  p_mya <- ggplot(df_org, aes(x = divergence_mya, y = aakomp_score)) +
    geom_point(aes(color = clade), size = 5.5) +
    geom_text_repel(
      aes(label = species_label),
      size = 8.5,
      fontface = "italic",
      color = "black",
      seed = 42,
      max.overlaps = Inf,
      box.padding = 0.65,
      point.padding = 0.55,
      force = 8,
      force_pull = 0.4,
      max.time = 5,
      max.iter = 50000,
      segment.color = "grey65",
      segment.size = 0.4,
      min.segment.length = 0
    ) +
    scale_color_manual(values = clade_colors) +
    scale_x_continuous(breaks = mya_breaks, limits = mya_limits, expand = expansion(mult = c(0.06, 0.10))) +
    scale_y_continuous(breaks = y_breaks, limits = y_limits, expand = expansion(mult = c(0.06, 0.08))) +
    base_theme
  
  df_org_mash <- df_org[order(df_org$mash_distance), ]
  p_mash <- ggplot(df_org_mash, aes(x = mash_distance, y = aakomp_score)) +
    geom_point(aes(color = clade), size = 5.5) +
    geom_text_repel(
      aes(label = species_label),
      size = 8.5,
      fontface = "italic",
      color = "black",
      seed = 42,
      max.overlaps = Inf,
      box.padding = 0.65,
      point.padding = 0.55,
      force = 8,
      force_pull = 0.4,
      max.time = 5,
      max.iter = 50000,
      segment.color = "grey65",
      segment.size = 0.4,
      min.segment.length = 0
    ) +
    scale_color_manual(values = clade_colors) +
    scale_x_continuous(breaks = mash_breaks, limits = mash_limits, expand = expansion(mult = c(0.06, 0.10))) +
    scale_y_continuous(breaks = y_breaks, limits = y_limits, expand = expansion(mult = c(0.06, 0.08))) +
    base_theme
  
  legend_plot <- ggplot(df_org, aes(x = divergence_mya, y = aakomp_score, color = clade)) +
    geom_point(size = 5.5) +
    scale_color_manual(values = clade_colors, name = legend_title) +
    theme_minimal() +
    theme(
      legend.position = "right",
      legend.direction = "vertical",
      legend.text = element_text(size = 20, face = "plain"),
      legend.title = element_text(size = 22, face = "plain"),
      plot.margin = margin(t = 2, r = 15, b = 2, l = 0)
    )
  
  leg_grob <- extract_legend(legend_plot)
  legend_top <- ggdraw() + draw_grob(leg_grob, x = 0.02, y = 0.89, hjust = 0, vjust = 1)
  
  return(list(mya = p_mya, mash = p_mash, legend = legend_top))
}

generate_8panel_figure <- function(df_sub, is_proteome = TRUE) {
  
  unified_title <- "Taxonomic Clade:"
  
  if (is_proteome) {
    prim_y_lim <- c(60, 100); prim_y_brk <- seq(60, 100, by = 20)
    eel_y_lim  <- c(25, 100); eel_y_brk  <- seq(30, 100, by = 20)
    bee_y_lim  <- c(25, 100); bee_y_brk  <- seq(30, 100, by = 20)
    rice_y_lim <- c(0, 100);  rice_y_brk <- seq(0, 100, by = 20)
  } else {
    prim_y_lim <- c(75, 100); prim_y_brk <- seq(75, 100, by = 5)
    eel_y_lim  <- c(35, 65);  eel_y_brk  <- seq(35, 65, by = 10)
    bee_y_lim  <- c(50, 75);  bee_y_brk  <- seq(50, 75, by = 5)
    rice_y_lim <- c(22, 80);  rice_y_brk <- seq(30, 70, by = 10)
  }
  
  r1 <- create_organism_plots(
    df_sub, "Primates", prim_levels, prim_colors,
    mya_breaks = seq(0, 80, by = 20), mya_limits = c(0, 80),
    mash_breaks = seq(0, 0.20, by = 0.05), mash_limits = c(0, 0.20),
    y_breaks = prim_y_brk, y_limits = prim_y_lim,
    legend_title = unified_title
  )
  
  r2 <- create_organism_plots(
    df_sub, "European eel", eel_levels, eel_colors,
    mya_breaks = seq(0, 300, by = 50), mya_limits = c(0, 300),
    mash_breaks = seq(0, 0.30, by = 0.10), mash_limits = c(0, 0.32),
    y_breaks = eel_y_brk, y_limits = eel_y_lim,
    legend_title = unified_title
  )
  
  r3 <- create_organism_plots(
    df_sub, "Bumble bee", bee_levels, bee_colors,
    mya_breaks = seq(0, 120, by = 30), mya_limits = c(0, 130),
    mash_breaks = seq(0, 0.30, by = 0.10), mash_limits = c(0, 0.30),
    y_breaks = bee_y_brk, y_limits = bee_y_lim,
    legend_title = unified_title
  )
  
  r4 <- create_organism_plots(
    df_sub, "Asian cultivated rice", rice_levels, rice_colors,
    mya_breaks = seq(0, 120, by = 20), mya_limits = c(0, 120),
    mash_breaks = seq(0, 0.30, by = 0.10), mash_limits = c(0, 0.32),
    y_breaks = rice_y_brk, y_limits = rice_y_lim,
    legend_title = unified_title
  )
  
  aligned_plots <- align_plots(
    r1$mya, r1$mash,
    r2$mya, r2$mash,
    r3$mya, r3$mash,
    r4$mya, r4$mash,
    align = "hv",
    axis = "tblr"
  )
  
  grid_4rows <- plot_grid(
    aligned_plots[[1]], aligned_plots[[2]], r1$legend,
    aligned_plots[[3]], aligned_plots[[4]], r2$legend,
    aligned_plots[[5]], aligned_plots[[6]], r3$legend,
    aligned_plots[[7]], aligned_plots[[8]], r4$legend,
    labels = c("A", "B", "", "C", "D", "", "E", "F", "", "G", "H", ""),
    ncol = 3,
    rel_widths = c(1, 1, 0.42),
    label_size = 22,
    label_fontface = "plain",
    label_x = 0.02,
    label_y = 0.98,
    hjust = 0,
    vjust = 1
  )
  
  y_axis_title <- ggdraw() + 
    draw_label("aaKomp Score (%)", angle = 90, size = 32, fontface = "plain")
  
  body_with_y <- plot_grid(
    y_axis_title,
    grid_4rows,
    ncol = 2,
    rel_widths = c(0.035, 1)
  )
  
  x_axis_titles <- plot_grid(
    NULL,
    ggdraw() + draw_label("Evolutionary Divergence Time (MYA)", size = 32, fontface = "plain"),
    ggdraw() + draw_label("Mash Distance", size = 32, fontface = "plain"),
    NULL,
    ncol = 4,
    rel_widths = c(0.035, 1, 1, 0.42)
  )
  
  final_fig <- plot_grid(
    body_with_y,
    x_axis_titles,
    ncol = 1,
    rel_heights = c(1, 0.035)
  )
  
  return(final_fig)
}

df_lineage <- subset(df, database_type == "OrthoDB Lineage")
fig_lineage <- generate_8panel_figure(df_lineage, is_proteome = FALSE)
print(fig_lineage)

df_proteome <- subset(df, database_type == "Species Proteome")
fig_proteome <- generate_8panel_figure(df_proteome, is_proteome = TRUE)
print(fig_proteome)