rm(list = ls())

# Notes -------------------------------------------------------------------


# Library -----------------------------------------------------------------

  # Install packages
  # install.packages("tidyverse")
  # install.packages("ggtext")
  # install.packages("tidybayes")
  # install.packages("patchwork")

  # Load packages
  library(tidyverse)
  library(patchwork)
  library(ggtext)
  library(tidybayes)

  # Functions
  inv_logit <- function(a) exp(a) / ( 1 + exp(a) )


# Prepare -----------------------------------------------------------------

  # Import results (preregistered analyses)
  d_prereg <- read_rds("Experiment 1/results/results_pregistered_analyses.rds")
  
  # Import results (exploratory analyses)
  d_explor <- read_rds("Experiment 1/results/results_exploratory_analyses.rds")
  

# Figure 3 (top) ----------------------------------------------------------

  # Make Figure 3a (z)
  f_3a_z <- ggplot(d_prereg, aes(x = z, y = actor)) +
    stat_pointinterval(
      aes(shape = ingroup),
      .width = 0.95,
      fill = "white",
      size = 0.455,
      point_size = 0.455 * 3
    ) +
    scale_x_continuous(minor_breaks = NULL) +
    scale_shape_manual(values = c("Ingroup" = 21, "Outgroup" = 19)) +
    coord_cartesian(xlim = c(-0.5, 0.5)) +
    facet_grid(observer ~ .) +
    theme_grey(base_size = 10) +
    theme(
      legend.position = "none",
      # legend.position = c(0.95, 0.95),
      # legend.justification = c(1, 1),
      # legend.title = element_blank(),
      # legend.spacing = unit(0, "pt"),
      strip.background = element_blank(),
      strip.text = element_blank(),
      strip.placement = "outside",
      axis.line.x = element_line(size = rel(0.5)),
      axis.title = element_text(size = rel(0.8)),
      axis.text = element_text(colour = "black"),
      axis.text.y = element_text(colour = "black"),
      plot.margin = margin(0, 5, 0, 5),
      plot.tag = element_text(face = "bold")
    ) +
    labs(
      tag = "A",
      x = expression(theta[(italic(z))]),
      y = "Protesters"
    )
  
  # Make Figure 3a (p)
  f_3a_p <- ggplot(d_prereg, aes(x = p, y = actor)) +
    stat_pointinterval(
      aes(shape = ingroup),
      .width = 0.95,
      fill = "white",
      size = 0.455,
      point_size = 0.455 * 3
    ) +
    scale_x_continuous(
      minor_breaks = NULL,
      labels = ~numform::f_num(., digits = 2)
    ) +
    scale_shape_manual(values = c("Ingroup" = 21, "Outgroup" = 19)) +
    coord_cartesian(xlim = c(0.584, 0.750)) +
    facet_grid(observer ~ .) +
    theme_grey(base_size = 10) +
    labs(
      x = expression(italic(p)),
      y = "Protesters"
    ) +
    theme(
      legend.position = "none",
      # legend.position = "bottom",
      # legend.title = element_blank(),
      # legend.spacing = unit(0, "pt"),
      # legend.margin = margin(0, 0, 0, 0),
      strip.background = element_blank(),
      strip.text = element_blank(),
      strip.placement = "outside",
      axis.line.x = element_line(size = rel(0.5)),
      axis.title = element_text(size = rel(0.8)),
      axis.title.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text = element_text(colour = "black"),
      axis.text.y = element_blank(),
      plot.margin = margin(0, 5, 0, 2)
    )
  
  # Make Figure 3a (m)
  f_3a_n <- ggplot(d_prereg, aes(x = n, y = actor)) +
    stat_pointinterval(
      aes(shape = ingroup),
      .width = 0.95,
      fill = "white",
      size = 0.455,
      point_size = 0.455 * 3
    ) +
    scale_y_discrete(position = "right") +
    scale_x_continuous(minor_breaks = NULL) +
    scale_shape_manual(values = c("Ingroup" = 21, "Outgroup" = 19)) +
    coord_cartesian(xlim = c(14.61, 18.754)) +
    facet_grid(observer ~ .) +
    theme_grey(base_size = 10) +
    labs(
      x = expression(italic(n)),
      y = "Participant"
    ) +
    theme(
      # legend.position = "none",
      legend.position = c(1, 1.135),
      legend.justification = c(1, 1),
      legend.direction = "horizontal",
      legend.title = element_blank(),
      legend.spacing = unit(0, "pt"),
      legend.margin = margin(0, 0, 0, 0),
      legend.key.size = unit(12, "pt"),
      strip.background = element_blank(),
      strip.placement = "outside",
      axis.line.x = element_line(size = rel(0.5)),
      axis.title = element_text(size = rel(0.8)),
      axis.ticks.y = element_blank(),
      axis.text = element_text(colour = "black"),
      axis.text.y = element_blank(),
      plot.margin = margin(0, 5, 0, 5),
      plot.tag = element_text(face = "bold")
    )
  
  
# Figure 3 (bottom) -------------------------------------------------------

  # Rename predictors
  d_explor <- d_explor %>% 
    mutate(
      outcome = recode_factor(
        outcome,
        "System Justification" = "System Justification<sub>(*z*)</sub>",
        "Social Dominance" = "Social Dominance<sub>(*z*)</sub>",
        "Political Orientation" = "Political Orientation<sub>(*z*)</sub>" 
      )
    )
  
  # Make figure
  f_3b <- ggplot(d_explor, aes(x = x, y = z)) +
    stat_lineribbon(
      .width = c(.99),
      size = 0.455,
      alpha = 1.0,
      fill = "white"
    ) +
    # geom_hline(
    #   data = d_explor %>% 
    #     filter(x == 0) %>% 
    #     group_by(outcome) %>% 
    #     summarise(z = median(z)),
    #   aes(yintercept = z),
    # size = 0.455,
    #   colour = "grey20",
    #   linetype = "dashed"
    # ) +
    geom_hline(
      yintercept = 0,
      size = 0.455,
      colour = "grey20",
      linetype = "dashed"
    ) +
    stat_lineribbon(
      .width = c(.99, .95, .8, .5),
      size = 0.455,
      alpha = 0.6
    ) +
    geom_vline(xintercept = c(-2.5, 2.5), size = 0.455) +
    geom_hline(yintercept = c(-1 - 2 * 0.05, 1 + 2 * 0.05), size = 0.455) +
    scale_x_continuous(minor_breaks = NULL, expand = c(0, 0)) +
    scale_y_continuous(minor_breaks = NULL) + 
    scale_fill_brewer(type = "seq", palette = "Greys", direction = -1) +
    coord_cartesian(ylim = c(-1, 1)) +
    facet_grid(. ~ outcome, switch = "x") +
    labs(
      tag = "B",
      x = expression(italic(z)),
      y = expression(theta[(italic(z))])
    ) +
    theme_grey(base_size = 10) +
    theme(
      legend.position = "none",
      strip.text = element_markdown(size = rel(0.8)),
      strip.background = element_blank(),
      strip.placement = "outside",
      axis.title.x = element_blank(),
      axis.title.y = element_text(size = rel(0.8)),
      axis.text = element_text(colour = "black"),
      panel.spacing = unit(10, "pt"),
      plot.margin = margin(5, 5, 0, 5),
      plot.tag = element_text(face = "bold")
    )
  
  
# Combine -----------------------------------------------------------------

  # Assemble subfigures
  f_3a_z + f_3a_p + f_3a_n + f_3b + 
    plot_layout(
      design = "
      ABC
      DDD
      "
    )


# Export ------------------------------------------------------------------
  
  # Export as .png
  ggsave(
    "Experiment 1/figures/figure-3.png",
    width = 6.5, height = 6.5/3*2, units = "in", dpi = 600,
    type = "cairo"
  )
  
  # Export as .pdf
  ggsave(
    "Experiment 1/figures/figure-3.pdf",
    width = 6.5, height = 6.5/3*2, units = "in", dpi = 600,
    device = cairo_pdf
  )  
