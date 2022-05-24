rm(list = ls())

# Notes -------------------------------------------------------------------

  # CHECK FIGURE M1

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
      x_sup = case_when(
        protest == "for" ~ q5,
        protest == "against" ~ 6L - q5
      )
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
  m1_fit <- read_rds("Experiment 2/results/m1_fit.rds")
  
  # Extract draws
  m1_ps <- m1_fit$draws() %>% as_draws_df()
  
  # Extract posterior draws
  m1_post_ii <- m1_ps %>% spread_draws(alpha[ii], beta[ii]) %>% ungroup()
  m1_post_jj <- m1_ps %>% spread_draws(theta[jj]) %>% ungroup()
  m1_post_kk <- m1_ps %>% spread_draws(delta[kk]) %>% ungroup()
  
  # Calculate effect sizes
  m1_post <- dl %>% 
    distinct(jj, kk) %>% 
    left_join(m1_post_kk, by = c("kk")) %>% 
    left_join(m1_post_jj, by = c("jj", ".draw", ".chain", ".iteration")) %>% 
    group_by(.draw) %>% 
    mutate(
      z = (delta - mean(theta + delta))/sd(theta + delta)
    ) %>% 
    ungroup() %>% 
    select(-jj, -theta) %>% 
    distinct() %>% 
    crossing(ii = 1:25) %>% 
    left_join(m1_post_ii, by = c(".draw", ".chain", ".iteration", "ii")) %>% 
    mutate(
      p = inv_logit(alpha * (0 + beta + delta))
    ) %>% 
    group_by(.draw, .chain, .iteration, kk) %>%
    summarize(
      z = unique(z),
      n = sum(p),
      p = mean(p)
    ) %>% 
    ungroup() %>% 
    right_join(
      dl %>% distinct(order, kk, Participant, Protesters, Cause), 
      by = "kk"
    )
    

# Figure 4a ---------------------------------------------------------------

  # Make table
  f_4a_t <- dl %>% 
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
      plot.margin = margin(2, 1, 2, 0),
      plot.tag = element_text(face = "bold")
    ) +
    labs(
      tag = "M1"
    )

  # Make plot
  f_4a_p <- m1_post %>% 
    mutate(ingroup = if_else(Participant == Protesters, "Ingroup", "Outgroup")) %>% 
    ggplot(., aes(x = z, y = fct_rev(factor(order)))) + 
    stat_pointinterval(
      aes(shape = ingroup),
      fill = "white",
      size = 0.455,
      point_size = 3*0.455
    ) +
    geom_hline(yintercept = c(0.5, 8.5), size = 0.455) +
    geom_hline(yintercept = c(4.5), size = 3*0.455, colour = "white") +
    scale_x_continuous(minor_breaks = NULL) +
    scale_y_discrete(expand = c(0, 0)) +
    scale_shape_manual(values = c("Ingroup" = 21, "Outgroup" = 19)) +
    coord_cartesian(xlim = c(-0.5, 0.5), ylim = c(0.5, 8.5)) +
    theme_grey(base_size = 10) +
    theme(
      axis.text.x = element_text(colour = "black"),
      axis.ticks.x = element_line(colour = "black"),
      axis.title.x = element_text(size = rel(0.8)),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.title.y = element_blank(),
      # legend.position = c(0.975, 0.050),
      # legend.justification = c(1, 0),
      # legend.title = element_blank(),
      # legend.spacing = unit(0, "pt"),
      # legend.margin = margin(1, 5, 5, 5),
      # legend.key.size = unit(12, "pt"),
      legend.position = c(1, 1.150),
      legend.justification = c(1, 1),
      legend.direction = "horizontal",
      legend.title = element_blank(),
      legend.spacing = unit(0, "pt"),
      legend.margin = margin(0, 0, 0, 0),
      legend.key.size = unit(12, "pt"),
      plot.margin = margin(2, 0, 2, 2)
    ) +
    labs(
      x = expression(theta[(italic(z))])
    )
    

# Predict -----------------------------------------------------------------

  # Import results
  m2_fit <- read_rds("Experiment 2/results/m2_fit.rds")
  
  # Extract draws
  m2_ps <- m2_fit$draws() %>% as_draws_df()
  
  # Extract posterior draws
  m2_post_ii <- m2_ps %>% spread_draws(alpha[ii], beta[ii]) %>% ungroup()
  m2_post_jj <- m2_ps %>% spread_draws(theta[jj]) %>% ungroup()
  m2_post_kk <- m2_ps %>% spread_draws(delta[kk], b_sjb_kk[kk]) %>% ungroup()

  # Make posterior predictions
  m2_post <- dl %>% 
    left_join(
      tibble(
        kk = 1:8,
        x_for = c(1L, 0L, 1L, 0L, 1L, 0L, 1L, 0L)
      ),
      by = "kk"
    ) %>% 
    distinct(kk, x_for) %>% 
    left_join(
      m2_post_kk,
      by = "kk"
    ) %>% 
    group_by(.chain, .iteration, .draw, x_for) %>% 
    summarize(delta = mean(delta)) %>% 
    ungroup() %>% 
    left_join(
      m2_ps %>% spread_draws(b_sjb_for, b_sjb_against),
      by = c(".chain", ".iteration", ".draw")
    ) %>% 
    crossing(
      x_sjb = seq(-2.5, 2.5, 0.1)
    ) %>% 
    transmute(
      .chain, .iteration, .draw, x_for, x_sjb, 
      eta = delta + b_sjb_for * x_for * x_sjb + b_sjb_against * (1 - x_for) * x_sjb
    )
  
  # Calculate effect sizes
  m2_post <- dl %>% 
    distinct(jj, kk, x_sjb) %>% 
    crossing(.draw = 1:4000) %>% 
    left_join(m2_post_kk, by = c(".draw", "kk")) %>% 
    left_join(m2_post_jj, by = c("jj", ".draw", ".chain", ".iteration")) %>% 
    group_by(.draw) %>% 
    summarize(
      mean = mean(theta + delta + x_sjb * b_sjb_kk),
      sd = sd(theta + delta + x_sjb * b_sjb_kk)
    ) %>% 
    left_join(m2_post, ., by = ".draw") %>% 
    mutate(z = (eta - mean)/sd) %>% 
    select(.chain, .iteration, .draw, x_for, x_sjb, z) %>% 
    mutate(
      x_for = recode_factor(
        x_for,
        "0" = "Against",
        "1" = "For"
      )
    )
  

# Figure 4b ---------------------------------------------------------------

  # Make Figure 4b
  f_4b <- ggplot(m2_post, aes(x = x_sjb, y = z)) +
    stat_lineribbon(
      .width = c(.99),
      size = 0.455,
      alpha = 1.0,
      fill = "white"
    ) +
    geom_hline(
      data = m2_post %>% filter(x_sjb == 0) %>% group_by(x_for, x_sjb) %>% summarize(z = median(z)),
      aes(yintercept = z),
      colour = "grey20",
      linetype = "dashed",
      size = 0.455
    ) +
    stat_lineribbon(
      .width = c(.99, .95, .8, .5),
      size = 0.455,
      alpha = 0.6
    ) +
    geom_vline(xintercept = c(-2.5, 2.5), size = 0.455) +
    geom_hline(yintercept = c(-2 - 4 * 0.05, 2 + 4 * 0.05), size = 0.455) +
    scale_x_continuous(minor_breaks = NULL, expand = c(0, 0)) +
    scale_y_continuous(minor_breaks = NULL) + 
    scale_fill_brewer(type = "seq", palette = "Greys", direction = -1) +
    coord_cartesian(ylim = c(-2, 2)) +
    facet_grid(. ~ x_for) +
    labs(
      tag = "M2",
      x = expression(System~Justification[(italic(z))]),
      y = expression(theta[(italic(z))])
    ) +
    theme_grey(base_size = 10) +
    theme(
      legend.position = "none",
      strip.background = element_blank(),
      axis.text = element_text(colour = "black"),
      axis.title = element_text(size = rel(0.8)),
      plot.margin = margin(2, 2, 2, 2),
      plot.tag = element_text(face = "bold")
    )  
  
  
# Predict -----------------------------------------------------------------
  
  # Import results
  m3_fit <- read_rds("Experiment 2/results/m3_fit.rds")
  
  # Extract draws
  m3_ps <- m3_fit$draws() %>% as_draws_df()
  
  # Extract posterior draws
  m3_post_ii <- m3_ps %>% spread_draws(alpha[ii], beta[ii]) %>% ungroup()
  m3_post_jj <- m3_ps %>% spread_draws(theta[jj]) %>% ungroup()
  m3_post_kk <- m3_ps %>% spread_draws(delta[kk], b_sup_kk[kk]) %>% ungroup()
  m3_post_x  <- m3_ps %>% 
    select(.chain, .iteration, .draw, starts_with("z_sup")) %>% 
    crossing(x_sup = 1:5) %>% 
    mutate(
      mo_sup = case_when(
        x_sup == 1L ~ 0,
        x_sup == 2L ~ 1 * (`z_sup[1]`),
        x_sup == 3L ~ 2 * (`z_sup[1]` + `z_sup[2]`),
        x_sup == 4L ~ 3 * (`z_sup[1]` + `z_sup[2]` + `z_sup[3]`),
        x_sup == 5L ~ 4 * (`z_sup[1]` + `z_sup[2]` + `z_sup[3]` + `z_sup[4]`),
        TRUE ~ NA_real_
      )
    ) %>% 
    select(-starts_with("z_sup"))
  
  # Make posterior predictions
  m3_post <- dl %>% 
    distinct(jj, kk, x_sup) %>% 
    crossing(.draw = 1:4000) %>% 
    left_join(m3_post_kk, by = c(".draw", "kk")) %>% 
    left_join(m3_post_jj, by = c("jj", ".draw", ".chain", ".iteration")) %>% 
    left_join(m3_post_x, by = c(".chain", ".iteration", ".draw", "x_sup")) %>% 
    group_by(.draw) %>% 
    summarize(
      mean = mean(theta + delta + b_sup_kk * mo_sup),
      sd = sd(theta + delta + b_sup_kk * mo_sup)
    ) %>% 
    crossing(x_sup = 1:5) %>% 
    left_join(m3_ps %>% spread_draws(b_sup), by = c(".draw")) %>% 
    left_join(m3_post_x, by = c(".chain", ".iteration", ".draw", "x_sup")) %>% 
    mutate(z = (b_sup * mo_sup - mean)/sd) %>% 
    select(.chain, .iteration, .draw, x_sup, z)


# Figure 4c ---------------------------------------------------------------

  # Make Figure 4b
  f_4c <- ggplot(m3_post, aes(x = x_sup, y = z)) +
    stat_lineribbon(
      .width = c(.99),
      size = 0.455,
      alpha = 1.0,
      fill = "white"
    ) +
    geom_hline(
      yintercept = 0,
      colour = "grey20",
      linetype = "dashed",
      size = 0.455
    ) +
    stat_lineribbon(
      .width = c(.99, .95, .8, .5),
      size = 0.455,
      alpha = 0.6
    ) +
    geom_vline(xintercept = c(1, 5), size = 0.455) +
    geom_hline(yintercept = c(-2 - 4 * 0.05, 2 + 4 * 0.05), size = 0.455) +
    scale_x_continuous(minor_breaks = NULL, expand = c(0, 0)) +
    scale_y_continuous(minor_breaks = NULL) + 
    scale_fill_brewer(type = "seq", palette = "Greys", direction = -1) +
    coord_cartesian(xlim = c(c(0.984, 5.016)), ylim = c(-2, 2)) +
    labs(
      tag = "M3",
      x = "Support for Protesters' Cause",
      y = expression(theta[(italic(z))])
    ) +
    theme_grey(base_size = 10) +
    theme(
      legend.position = "none",
      strip.background = element_blank(),
      axis.text = element_text(colour = "black"),
      axis.title = element_text(size = rel(0.8)),
      plot.margin = margin(2, 2, 2, 2),
      plot.tag = element_text(face = "bold")
    ); f_4c
  
  
# Combine -----------------------------------------------------------------

  # Assemble subfigures
  f_4a_t + f_4a_p + f_4b + f_4c + plot_layout(
    heights = c(3, 4),
    design = "
    AAABB#
    CCCCDD
    "
  )


# Export ------------------------------------------------------------------
  
  # Export as .png
  ggsave(
    "Experiment 2/figures/figure-4.png",
    width = 6.5, height = 6.5/4*3, units = "in", dpi = 600,
    type = "cairo"
  )
  
  # Export as .pdf
  ggsave(
    "Experiment 2/figures/figure-4.pdf",
    width = 6.5, height = 6.5/4*3, units = "in",
    device = cairo_pdf
  )  
  