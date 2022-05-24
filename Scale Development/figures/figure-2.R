rm(list = ls())

# Notes -------------------------------------------------------------------


# Library -----------------------------------------------------------------

  # Install packages
  # install.packages("tidyverse")
  # install.packages("ggtext")
  
  # Load packages
  library(tidyverse)
  library(ggtext)


# Prepare -----------------------------------------------------------------

  # Import data
  dl <- read_rds("Scale Development/data/dl_ps2.rds")
  
  # Exclude
  dl <- dl %>% filter(assigned == 1)
  
  # Summarize
  ds <- dl %>% 
    group_by(action, action_text) %>% 
    summarize(p_sometimes = mean(q2 >= 3L)) %>% 
    ungroup()
  

# Visualize ---------------------------------------------------------------
  
  # Prepare data
  d_fig <- ds %>% 
    filter(action %in% c(1L, 23L, 31L, 38L, 46L, 52L, 56L, 60L)) %>% 
    mutate(
      action_text = case_when(
        str_detect(action_text, "\\(e.g.,") ~ str_replace(action_text, " \\(e.g.,", "\n(e.g.,"),
        TRUE ~ str_wrap(action_text, width = 40)
      ),
      included = (action %in% c(23L, 38L, 46L, 56L))
    ) %>% 
    arrange(p_sometimes)
  
  # Make figure
  d_fig %>% 
    mutate(action_text = factor(action_text, levels = d_fig$action_text)) %>% 
    ggplot(., aes(x = p_sometimes, y = action_text)) +
    geom_col(
      aes(fill = included),
      width = 0.8
    ) +
    geom_vline(xintercept = 0, size = 0.455) +
    geom_richtext(
      aes(
        label = numform::f_prop2percent(p_sometimes, digits = 0),
        hjust = if_else(p_sometimes > 0.25, 1, 0),
        colour = if_else(p_sometimes > 0.25, "white", "black")
      ),
      size = 10*0.8/.pt,
      fill = NA,
      label.colour = NA
    ) +
    scale_x_continuous(labels = scales::percent) +
    scale_colour_identity() +
    scale_fill_manual(values = c("grey70", "black")) +
    coord_cartesian(xlim = c(-0.008, 1.008), ylim = c(0.5, 8.5), expand = FALSE) +
    theme_classic(base_size = 10) +
    theme(
      axis.line.y = element_blank(), 
      axis.text = element_text(size = rel(0.8), colour = "black"),
      axis.ticks = element_line(colour = "black"),
      axis.ticks.y = element_blank(),
      axis.title.x = element_markdown(size = rel(0.8)),
      legend.position = "none",
      panel.grid = element_blank(),
      plot.margin = margin(4, 8, 4, 4)
    ) +
    labs(
      x = "Is this action at least *sometimes* an acceptable means of protest?",
      y = NULL
    )
  

# Export ------------------------------------------------------------------

  # Export as .png
  ggsave(
    "Scale Development/figures/figure-2.png",
    width = 6.5, height = 6.5/7*3, units = "in", 
    dpi = 600, type = "cairo"
  )
  
  # Export as .pdf
  ggsave(
    "Scale Development/figures/figure-2.pdf",
    width = 6.5, height = 6.5/7*3, units = "in", 
    device = cairo_pdf
  )
  