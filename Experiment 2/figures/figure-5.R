rm(list = ls())

# Notes -------------------------------------------------------------------


# Library -----------------------------------------------------------------

  # Install packages
  # install.packages("tidyverse")
  # install.packages("tidybayes")
  # install.packages("patchwork")
  # install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
  # install.packages("posterior", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
  # install_cmdstan() 
  
  # Load packages
  library(tidyverse)
  library(patchwork)
  library(cmdstanr)
  library(posterior)
  library(tidybayes)
  
  # Functions
  inv_logit <- function(a) exp(a) / ( 1 + exp(a) )
  

# Prepare -----------------------------------------------------------------

  # Import data
  dl <- read_rds("Experiment 2/data/dl.rds")
  dw <- read_rds("Experiment 2/data/dw.rds")
  
  # Exclude participants
  dl <- dl %>% filter(!exclude) %>% select(-exclude)
  dw <- dw %>% filter(!exclude) %>% select(-exclude)
  
  # Calculate factor scores (system-justifying beliefs)
  source("Experiment 2/scripts/run_factor_analyses.R")
  
  # Code predictors
  dl <- dw %>%
    transmute(
      id,
      x_sjb = (sjb - mean(sjb))/sd(sjb),
      x_sup = (q5 - mean(q5))/sd(q5),
      x_pol = (q19_1 - mean(q19_1))/sd(q19_1)
    ) %>% 
    left_join(dl, ., by = "id") %>% 
    mutate(
      ii = action,
      jj = as.integer(factor(id)),
      kk = case_when(
        observers == "Black" & protesters == "Black" & protest == "for"     ~ 1L, 
        observers == "Black" & protesters == "Black" & protest == "against" ~ 2L, 
        observers == "Black" & protesters == "White" & protest == "for"     ~ 3L, 
        observers == "Black" & protesters == "White" & protest == "against" ~ 4L, 
        observers == "White" & protesters == "Black" & protest == "for"     ~ 5L, 
        observers == "White" & protesters == "Black" & protest == "against" ~ 6L, 
        observers == "White" & protesters == "White" & protest == "for"     ~ 7L, 
        observers == "White" & protesters == "White" & protest == "against" ~ 8L
      ),
      y  = response
    ) %>% 
    mutate(
      order = case_when(
        observers == "Black" & protesters == "White" & protest == "against" ~ 1L, 
        observers == "Black" & protesters == "Black" & protest == "against" ~ 2L, 
        observers == "Black" & protesters == "White" & protest == "for"     ~ 3L, 
        observers == "Black" & protesters == "Black" & protest == "for"     ~ 4L, 
        observers == "White" & protesters == "White" & protest == "against" ~ 5L, 
        observers == "White" & protesters == "Black" & protest == "against" ~ 6L, 
        observers == "White" & protesters == "White" & protest == "for"     ~ 7L, 
        observers == "White" & protesters == "Black" & protest == "for"     ~ 8L, 
      )
    ) %>% 
    mutate(
      protest = recode_factor(protest, "for" = "For", "against" = "Against"),
    ) %>% 
    rename(Participant = observers, Protesters = protesters, Cause = protest)
  
  
# Predict -----------------------------------------------------------------

  # Import results
  m2_fit <- read_rds("Experiment 2/results/m2_fit.rds")
  
  # Extract draws
  m2_ps <- m2_fit$draws() %>% as_draws_df()
  
  # Extract posterior draws
  m2_post_ii <- m2_ps %>% spread_draws(alpha[ii], beta[ii]) %>% ungroup()
  m2_post_jj <- m2_ps %>% spread_draws(theta[jj]) %>% ungroup()
  m2_post_kk <- m2_ps %>% spread_draws(delta[kk], b_sjb_kk[kk]) %>% ungroup()
  
  # Calculate effect sizes
  m2_post <- dl %>% 
    distinct(jj, kk, x_sjb) %>% 
    left_join(m2_post_kk, by = c("kk")) %>% 
    left_join(m2_post_jj, by = c("jj", ".draw", ".chain", ".iteration")) %>% 
    group_by(.draw) %>% 
    summarize(
      mean = mean(theta + delta + b_sjb_kk * x_sjb),
      sd = sd(theta + delta + b_sjb_kk * x_sjb)
    ) %>% 
    crossing(
      kk = 1:8, 
      x_sjb = c(-1, 1)
    ) %>% 
    left_join(m2_post_kk, by = c(".draw", "kk")) %>% 
    mutate(
      z = ((delta + b_sjb_kk * x_sjb) - mean)/sd
    ) %>% 
    left_join(dl %>% distinct(kk, order, Participant, Protesters, Cause), by = "kk") %>% 
    select(.draw, kk, x_sjb, z, order, Participant, Protesters, Cause)
  
  # Import results
  m4_fit <- read_rds("Experiment 2/results/m4_fit.rds")
  
  # Extract draws
  m4_ps <- m4_fit$draws() %>% as_draws_df()
  
  # Extract posterior draws
  m4_post_ii <- m4_ps %>% spread_draws(alpha[ii], beta[ii]) %>% ungroup()
  m4_post_jj <- m4_ps %>% spread_draws(theta[jj]) %>% ungroup()
  m4_post_kk <- m4_ps %>% spread_draws(delta[kk], b_pol_kk[kk]) %>% ungroup()
  
  # Calculate effect sizes
  m4_post <- dl %>% 
    distinct(jj, kk, x_pol) %>% 
    left_join(m4_post_kk, by = c("kk")) %>% 
    left_join(m4_post_jj, by = c("jj", ".draw", ".chain", ".iteration")) %>% 
    group_by(.draw) %>% 
    summarize(
      mean = mean(theta + delta + b_pol_kk * x_pol),
      sd = sd(theta + delta + b_pol_kk * x_pol)
    ) %>% 
    crossing(
      kk = 1:8, 
      x_pol = c(-1, 1)
    ) %>% 
    left_join(m4_post_kk, by = c(".draw", "kk")) %>% 
    mutate(
      z = ((delta + b_pol_kk * x_pol) - mean)/sd
    ) %>% 
    left_join(dl %>% distinct(kk, order, Participant, Protesters, Cause), by = "kk") %>% 
    select(.draw, kk, x_pol, z, order, Participant, Protesters, Cause)


# Merge -------------------------------------------------------------------

  # Combine predictions
  d_fig <- bind_rows(
    m2_post %>% transmute(
      .draw, 
      Participant, 
      Protesters, 
      Cause,
      order, 
      kk, 
      predictor = "System Justification", 
      x = case_when(x_sjb == -1 ~ "-1SD", x_sjb == 1 ~ "+1SD"),
      z
    ),
    m4_post %>% transmute(
      .draw, 
      Participant, 
      Protesters, 
      Cause,
      order, 
      kk, 
      predictor = "Political Orientation", 
      x = case_when(x_pol == -1 ~ "-1SD", x_pol == 1 ~ "+1SD"),
      z
    )
  ) %>% 
    mutate(
      predictor = factor(
        predictor, 
        levels = c("System Justification", "Political Orientation")
      )
    )
  
  
# Figure 5 ----------------------------------------------------------------

  # Make table
  f_5_t <- dl %>% 
    distinct(order, Participant, Protesters, Cause) %>% 
    pivot_longer(
      c(Participant, Protesters, Cause),
      names_to = "column",
      values_to = "text"
    ) %>% 
    mutate(
      column = factor(column, levels = c("Participant", "Protesters", "Cause"))
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
  f_5_p <- ggplot(d_fig, aes(x = z, y = fct_rev(factor(order)), group = x)) + 
    geom_ribbon(
      data = d_fig %>% group_by(order, x, predictor) %>% median_qi(z),
      aes(xmin = .lower, xmax = .upper, fill = x),
      alpha = 0.4
    ) +
    stat_pointinterval(
      aes(shape = x, colour = x, group = x),
      fill = "white",
      size = 0.455,
      point_size = 3*0.455
    ) +
    geom_hline(yintercept = c(0.5, 8.5), size = 0.455) +
    geom_hline(yintercept = c(4.5), size = 3*0.455, colour = "white") +
    scale_x_continuous(minor_breaks = NULL) +
    scale_y_discrete(expand = c(0, 0)) +
    scale_shape_manual(values = c("+1SD" = 19, "-1SD" = 21)) +
    scale_colour_manual(values = c("+1SD" = "#648FFF", "-1SD" = "#DC267F")) +
    scale_fill_manual(values = c("+1SD" = "#648FFF", "-1SD" = "#DC267F")) +
    facet_grid(. ~ predictor) +
    coord_cartesian(ylim = c(0.5, 8.5)) +
    theme_grey(base_size = 10) +
    theme(
      axis.text.x = element_text(colour = "black"),
      axis.ticks.x = element_line(colour = "black"),
      axis.title.x = element_text(size = rel(0.8)),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.title.y = element_blank(),
      # legend.justification = c(1, 1),
      # legend.position = c(0.99, 0.97),
      legend.title = element_blank(),
      legend.spacing = unit(0, "pt"),
      legend.margin = margin(0, 0, 4, 0),
      legend.key.size = unit(12, "pt"),
      legend.key = element_blank(),
      strip.background = element_blank(),
      strip.text.x = element_text(size = 10, colour = "black", face = "bold"),
      plot.margin = margin(2, 0, 2, 2)
    ) +
    labs(
      x = expression(theta[(italic(z))])
    )
  
  
# Combine -----------------------------------------------------------------

  # Assemble subfigures
  f_5_t + f_5_p + plot_layout(widths = c(3, 4))


# Export ------------------------------------------------------------------
  
  # Export as .png
  ggsave(
    "Experiment 2/figures/figure-5.png",
    width = 6.5, height = 6.5/5*2, units = "in", dpi = 600,
    type = "cairo"
  )
  
  # Export as .pdf
  ggsave(
    "Experiment 2/figures/figure-5.pdf",
    width = 6.5, height = 6.5/5*2, units = "in",
    device = cairo_pdf
  )  
  