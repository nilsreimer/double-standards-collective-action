rm(list = ls())

# Notes -------------------------------------------------------------------


# Library -----------------------------------------------------------------

  # Install packages
  # install.packages("tidyverse")
  # install.packages("ggtext")
  
  # Load packages
  library(tidyverse)
  library(ggtext)


# Functions ---------------------------------------------------------------

  # Link function
  inv_logit <- function(x) exp(x) / ( 1 + exp(x) )
  
  # IRT equation
  irt <- function(theta, alpha, beta) {
    inv_logit(alpha*(theta + beta))
  }
  irt <- Vectorize(irt)
  

# Visualize ---------------------------------------------------------------
  
  # Make figure
  ggplot() +
    geom_vline(xintercept = 0.0, colour = "grey92", size = 0.455) +
    geom_hline(yintercept = 0.5, colour = "grey92", size = 0.455) +
    stat_function(
      aes(colour = "Action 1"),
      fun = irt, 
      args = list(
        alpha = 1.00,
        beta  = 0.00
      ),
      n = 10001, size = 0.455
    ) +
    stat_function(
      aes(colour = "Action 2"),
      fun = irt, 
      args = list(
        alpha = 1.00,
        beta  = qnorm(0.9)
      ),
      n = 10001, size = 0.455
    ) +
    stat_function(
      aes(colour = "Action 3"),
      fun = irt, 
      args = list(
        alpha = 3.60,
        beta  = 0.00
      ),
      n = 10001, size = 0.455
    ) +
    annotate(
      geom = "rich_text",
      x = c(rep(qnorm(0.1), 3), rep(qnorm(0.9), 3)),
      y = c(
        irt(qnorm(0.1), 1.00, 0.00),
        irt(qnorm(0.1), 1.00, qnorm(0.9)),
        irt(qnorm(0.1), 3.60, 0.00),
        irt(qnorm(0.9), 1.00, 0.00),
        irt(qnorm(0.9), 1.00, qnorm(0.9)),
        irt(qnorm(0.9), 3.60, 0.00)
      ),
      label = rep(1:3, 2),
      colour = rep(c("#648FFF", "#DC267F", "#FFB000"), 2),
      label.colour = NA,
      label.padding = unit(0.05, "lines"),
      label.r = unit(0.05, "lines"),
      size = 8/.pt
    ) + 
    annotate(
      geom = "rich_text",
      x = qnorm(0.995), y = 0,
      label = "<span style='color:#648FFF'>&alpha;<sub>1</sub> = 1.00, &beta;<sub>1</sub> = 0.00</span><br><span style='color:#DC267F'>&alpha;<sub>2</sub> = 1.00, &beta;<sub>2</sub> = 1.28</span><br><span style='color:#FFB000'>&alpha;<sub>3</sub> = 3.60, &beta;<sub>3</sub> = 0.00</span>",
      vjust = 0, hjust = 1,
      label.colour = NA,
      label.padding = unit(0, "lines"),
      size = 8/.pt
    ) +
    scale_x_continuous(
      limits = qnorm(c(0.0025, 0.9975)),
      breaks = qnorm(c(0.005, 0.1, 0.5, 0.9, 0.995)),
      minor_breaks = NULL,
      labels = scales::number(qnorm(c(0.005, 0.1, 0.5, 0.9, 0.995)), accuracy = 0.01),
      sec.axis = dup_axis(
        name = NULL,
        breaks = qnorm(c(0.005, 0.1, 0.5, 0.9, 0.995)),
        labels = c("<1%", "10%", "50%", "90%", ">99%")
      ),
      expand = c(0, 0)
    ) +
    scale_y_continuous(
      minor_breaks = NULL
    ) +
    scale_colour_manual(
      values = c("#648FFF", "#DC267F", "#FFB000")
    ) +
    coord_fixed(dist(qnorm(c(0.0025, 0.9975)))/1) +
    theme_classic(base_size = 10) +
    theme(
      legend.position = "none",
      strip.background = element_blank(),
      axis.line = element_blank(),
      axis.text = element_text(colour = "black"),
      axis.title = element_text(size = 10, colour = "black"),
      panel.background = element_rect(colour = "black", fill = NA),
      panel.ontop = TRUE,
      plot.margin = margin(0, 0, 0, 0)
    ) +
    labs(
      x = expression(theta[(italic(z))]),
      y = expression(Pr(italic(y[ij])==1)),
      colour = NULL
    )
  

# Export ------------------------------------------------------------------

  # Export as .png
  ggsave(
    "Scale Development/figures/figure-1.png",
    width = (6.5 - 18/72.27)/2, height = (6.5 - 18/72.27)/2, units = "in", 
    dpi = 600, type = "cairo"
  )
  
  # Export as .pdf
  ggsave(
    "Scale Development/figures/figure-1.pdf",
    width = (6.5 - 18/72.27)/2, height = (6.5 - 18/72.27)/2, units = "in",
    device = cairo_pdf
  )
  