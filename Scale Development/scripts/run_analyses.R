rm(list = ls())

# Notes -------------------------------------------------------------------

  # An ideal item should be (1) ambigious in that neither "never" nor 
  # "always" gets all the votes, and (2) polarising in that an item should
  # be prefered if not-"sometimes" responses fall on either side of the 
  # mid-point.

# Library -----------------------------------------------------------------

  # Install packages
  # install.packages("tidyverse")
  # install.packages("tidybayes")
  # install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
  # install.packages("posterior", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
  # install_cmdstan() 
  
  # Load packages
  library(tidyverse)
  library(tidybayes)
  library(cmdstanr)
  library(posterior)
  
  # Functions
  inv_logit <- function(a) exp(a) / ( 1 + exp(a) )
  
  
# Prepare -----------------------------------------------------------------
  
  # Import data
  dl <- read_rds("Scale Development/data/dl_ps2.rds")
  
  # Exclude observations
  dl <- dl %>% filter(assigned == 1)
  
  # Exclude actions
  actions <- dl %>%
    group_by(action, action_text) %>%
    count(q2) %>%
    spread(q2, n) %>%
    ungroup() %>%
    mutate_all(~replace_na(., 0L)) %>%
    # filter(`1` + `2` > 0, `4` + `5` > 0) %>%
    pull(action)
  
  # Prepare data frame
  df <- dl %>%
    select(id, sample, q2, action, action_text) %>%
    filter(action %in% actions) %>%
    transmute(
      id, sample, action, action_text,
      ii = as.integer(factor(action)),
      jj = id,
      y = q2
    ) %>%
    arrange(jj, ii)
  
  # Prepare data list
  dlist <- with(df, list(
    N  = length(y),
    I  = max(ii),
    J  = max(jj),
    K  = max(y),
    ii = ii,
    jj = jj,
    y  = y
  ))
  

# Estimate ----------------------------------------------------------------
  
  # Compile model
  model <- cmdstan_model("Scale Development/models/2pl_graded_response_model.stan")
  
  # Run model
  fit <- model$sample(
    data = dlist,
    seed = 6915778,
    chains = 8,
    parallel_chains = parallel::detectCores(),
    iter_sampling = 500,
    iter_warmup = 1000,
    adapt_delta = 0.99
  )
  
  # Save results as .rds
  fit$save_object("Scale Development/results/fit_scale_development.rds")


# Process -----------------------------------------------------------------

  # Load results
  fit <- read_rds("Scale Development/results/fit_scale_development.rds")
  
  # Extract posterior draws
  post <- fit %>% 
    spread_draws(beta[ii,kk], alpha[ii]) %>% 
    spread(kk, beta) %>% 
    rename_at(vars(`1`, `2`, `3`, `4`), ~paste0("beta_", .))
  
  # Extract theta
  m_theta <- fit %>% 
    spread_draws(theta[jj]) %>%
    group_by(.draw) %>%
    summarise_at(vars(theta), list("mean" = mean, "sd" = sd)) %>%
    ungroup() %>%
    median_qi()
  
  # Summarise
  post <- post %>% summarise(across(c(alpha, starts_with("beta_")), median))
  
  # Probability
  p_ik <- function(theta, i, k, standardized = FALSE) {
    if (standardized == TRUE) theta <- theta * m_theta$sd + m_theta$mean
    if (k == 1) {
      p <- with(post, 1 - inv_logit(alpha[ii == i]*theta - beta_1[ii == i]))
    } else if (k == 2) {
      p <- with(post, inv_logit(alpha[ii == i]*theta - beta_1[ii == i]) - inv_logit(alpha[ii == i]*theta - beta_2[ii == i]))
    } else if (k == 3) {
      p <- with(post, inv_logit(alpha[ii == i]*theta - beta_2[ii == i]) - inv_logit(alpha[ii == i]*theta - beta_3[ii == i]))
    } else if (k == 4) {
      p <- with(post, inv_logit(alpha[ii == i]*theta - beta_3[ii == i]) - inv_logit(alpha[ii == i]*theta - beta_4[ii == i]))
    } else if (k == 5) {
      p <- with(post, inv_logit(alpha[ii == i]*theta - beta_4[ii == i]))
    } else {
      p <- NA_real_
    }
    return(p)
  }
  
  # Option Information Function
  I_ik <- function(theta, i, k, standardized = FALSE) {
    if (standardized == TRUE) theta <- theta * m_theta$sd + m_theta$mean
    with(post, alpha[ii == i]^2) * p_ik(theta, i, k) * (1 - p_ik(theta, i, k))
  }
  
  # Item Information Function
  I_i <- function(theta, i, standardized = FALSE) {
    if (standardized == TRUE) theta <- theta * m_theta$sd + m_theta$mean
    sum(map_dbl(1:5, ~I_ik(theta, i, .)))
  }
  
  # Total Information Function
  I <- function(theta, ii = 1:72, standardized = FALSE) {
    if (standardized == TRUE) theta <- theta * m_theta$sd + m_theta$mean
    map_dbl(theta, \(theta) sum(map_dbl(ii, \(ii) I_i(theta, ii))))
  }
  
  # Vectorize
  p_ik <- Vectorize(p_ik)
  I_ik <- Vectorize(I_ik)
  I_i  <- Vectorize(I_i)

  # Summarize
  results <- post %>%
    mutate(
      I  = map_dbl(ii, ~integrate(f = function(x) I_i(x, i = ., standardized = F), -99, 99)$value)
    ) %>%
    left_join(
      df %>% 
        group_by(ii, action_text) %>% 
        count(y) %>% 
        mutate(p = n/sum(n)) %>% 
        select(-n) %>% 
        pivot_wider(
          names_from = y,
          values_from = p,
          values_fill = 0L
        ) %>% 
        select(ii, action_text, `1`, `2`, `3`, `4`, `5`), 
      by = "ii"
    )
  

# Export ------------------------------------------------------------------

  # Save as .rds
  write_rds(results, "Scale Development/results/results_scale_development.rds")

  
# Show --------------------------------------------------------------------

  # Plot Total Information Curves
  p <- ggplot()
  for (n_i in c(1:24, 24:72)) {
    p <- p + stat_function(
      fun = I,
      args = list(ii = pull(slice_max(results, n = n_i, I), ii), standardized = T),
      color = grey.colors(72)[73 - n_i],
      linewidth = 0.455
    )
  }
  p <- p + stat_function(
    fun = I,
    args = list(ii = pull(slice_max(results, n = 25, I), ii), standardized = T),
    color = "red",
    linewidth = 0.455 * 2
  ) 
  p + scale_x_continuous(
    limits = c(-5, 5),
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
    theme_classic(base_size = 10) +
    theme(
      legend.position = "right",
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
      y = "Information",
      colour = NULL
    )


# Export ------------------------------------------------------------------

  # Export as .png
  ggsave(
    "Scale Development/figures/figure-s1.png",
    width = 6.5, height = 6.5/2, units = "in", dpi = 600
  )
  
  # Export as .pdf
  ggsave(
    "Scale Development/figures/figure-s1.pdf",
    width = 6.5, height = 6.5/2, units = "in", dpi = 600
  )  
