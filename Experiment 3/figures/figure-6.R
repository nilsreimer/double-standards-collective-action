rm(list = ls())

# Notes -------------------------------------------------------------------


# Library -----------------------------------------------------------------

  # Load packages
  library(tidyverse)
  library(patchwork)
  library(cmdstanr)
  library(posterior)
  library(tidybayes)
  
  # Functions
  inv_logit <- function(a) exp(a) / ( 1 + exp(a) )


# Prepare -----------------------------------------------------------------

  # Load results
  m1_post <- read_rds("Experiment 3/results/m1_post.rds")
  
  # Prepare data for figure
  d_fig <- m1_post %>% mutate(order = kk)
  

# Figure 6 ----------------------------------------------------------------

  # Make table
  f_6_t <- d_fig %>% 
    distinct(order, participant_ideology, participant_gender, protesters_cause) %>% 
    rename(Gender = participant_gender, Ideology = participant_ideology, Cause = protesters_cause) %>% 
    pivot_longer(
      c(Gender, Ideology, Cause),
      names_to = "column",
      values_to = "text"
    ) %>% 
    mutate(
      column = factor(column, levels = c("Ideology", "Gender", "Cause"))
    ) %>% 
    ggplot(., aes(x = 0, y = order)) +
    geom_text(
      aes(label = text),
      size = 10/.pt,
      hjust = 0
    ) +
    geom_hline(yintercept = c(0.5, 8.5), size = 0.455) +
    scale_y_reverse(expand = c(0, 0)) +
    xlim(0, 1) +
    facet_grid(. ~ column) +
    theme_grey(base_size = 10) +
    theme(
      legend.position = "none",
      strip.background = element_blank(),
      strip.text.x = element_text(size = 10, colour = "black", face = "bold"),
      strip.text.y = element_blank(),
      axis.text.x = element_text(colour = NA),
      axis.ticks.x = element_line(colour = NA),
      axis.title.x = element_text(colour = NA),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.title.y = element_blank(),
      panel.background = element_blank(),
      plot.margin = margin(2, 1, 2, 0)
    )
  
  # Make plot
  f_6_p <- d_fig %>% 
    ggplot(., aes(x = z, y = order)) +
    geom_ribbon(
      data = d_fig %>% group_by(order, participant_ideology, protesters_cause) %>% median_qi(z),
      aes(group = interaction(protesters_cause, participant_ideology), xmin = .lower, xmax = .upper, fill = participant_ideology),
      alpha = 0.4
    ) +
    stat_pointinterval(
      aes(group = order, colour = participant_ideology, shape = participant_ideology),
      .width = 0.95,
      fill = "white",
      size = 0.455,
      point_size = 3*0.455
    ) +
    geom_hline(yintercept = c(0.5, 8.5), size = 0.455) +
    geom_hline(yintercept = c(4.5), size = 3*0.455, colour = "white") +
    scale_x_continuous(minor_breaks = NULL) +
    scale_y_reverse(expand = c(0, 0)) +
    scale_shape_manual(values = c("Conservative" = 21, "Liberal" = 19)) +
    scale_colour_manual(values = c("Conservative" = "#DC267F", "Liberal" = "#648FFF")) +
    scale_fill_manual(values = c("Conservative" = "#DC267F", "Liberal" = "#648FFF")) +
    coord_cartesian(xlim = c(-0.65, 1.15)) +
    facet_grid(. ~ "") +
    theme_grey(base_size = 10) +
    theme(
      axis.text.x = element_text(colour = "black"),
      axis.ticks.x = element_line(colour = "black"),
      axis.title.x = element_text(size = rel(0.8)),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.title.y = element_blank(),
      legend.position = "none",
      strip.background = element_blank(),
      strip.text.x = element_text(size = 10, colour = "black", face = "bold"),
      plot.margin = margin(2, 0, 2, 2)
    ) +
    labs(
      x = expression(theta[(italic(z))])
    )
  

# Combine -----------------------------------------------------------------
  
  # Assemble subfigures
  f_6_t + f_6_p + plot_layout(widths = c(2, 1))


# Export ------------------------------------------------------------------
  
  # Export as .png
  ggsave(
    "Experiment 3/figures/figure-6.png",
    width = 6.5/4*3, height = 6.5/5*2, units = "in", dpi = 600
  )
  
  # Export as .pdf
  ggsave(
    "Experiment 3/figures/figure-6.pdf",
    width = 6.5/4*3, height = 6.5/5*2, units = "in"
  )    
  