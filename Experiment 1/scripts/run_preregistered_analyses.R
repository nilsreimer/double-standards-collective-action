rm(list = ls())

# Notes -------------------------------------------------------------------


# Library -----------------------------------------------------------------

  # Install packages
  # install.packages("tidyverse")
  # install.packages("tidybayes")
  # install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
  # install.packages("posterior", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
  # install_cmdstan() 

  # Load packages
  library(tidyverse)
  library(cmdstanr)
  library(posterior)
  library(tidybayes)

  # Functions
  inv_logit <- function(a) exp(a) / ( 1 + exp(a) )


# Prepare -----------------------------------------------------------------

  # Import data
  dl <- read_rds("Experiment 1/data/dl.rds")
  
  # Exclude participants
  dl <- dl %>% filter(!exclude) %>% select(-exclude)
  
  # Code predictors
  dl <- dl %>%
    mutate(
      ii = action,
      jj = as.integer(factor(id)),
      x_1 = if_else(target == "PJ", 1L, 0L),
      x_2 = if_else(target == sample, 1L, 0L),
      x_3 = if_else(target == "PJ" & sample == "PJ", 1L, 0L),
      y  = response
    )
  
  # Compile data list
  dlist <- with(dl, list(
    N = length(ii),
    I = max(ii),
    J = max(jj),
    K = 3L,
    ii = ii,
    jj = jj,
    X = cbind(x_1, x_2, x_3),
    y = y
  ))


# Estimate ----------------------------------------------------------------

  # Compile model
  model <- cmdstan_model("Experiment 1/models/s1_2pl_model.stan")

  # Run model
  fit <- model$sample(
    data = dlist,
    seed = 36939726,
    chains = 8,
    parallel_chains = parallel::detectCores(),
    iter_sampling = 1000,
    iter_warmup = 1000
  )

  # Save results as .rds
  fit$save_object("Experiment 1/results/fit_pregistered_analyses.rds")


# Predict -----------------------------------------------------------------
  
  # Import results
  fit <- read_rds("Experiment 1/results/fit_pregistered_analyses.rds")
  
  # Extract draws
  ps <- fit$draws() %>% as_draws_df()
  
  # Extract posterior draws
  post_ii <- ps %>% spread_draws(alpha[ii], beta[ii]) %>% ungroup()
  post_jj <- ps %>% spread_draws(theta[jj]) %>% ungroup()
  post_kk <- ps %>% 
    gather_draws(delta[kk]) %>% 
    pivot_wider(
      names_from = c(".variable", "kk"), 
      names_sep = "_", 
      values_from = ".value"
    )
  
  # Calculate effect sizes
  post <- dl %>% 
    select(sample, target, jj, x_1, x_2, x_3) %>% 
    distinct() %>% 
    crossing(.draw = 1:max(post_ii$.draw)) %>% 
    left_join(post_kk, by = ".draw") %>% 
    mutate(
      delta = x_1*delta_1 + x_2*delta_2 + x_3*delta_3
    ) %>% 
    select(-x_1:-x_3, -delta_1:-delta_3) %>% 
    left_join(post_jj, by = c("jj", ".draw", ".chain", ".iteration")) %>% 
    group_by(.draw) %>% 
    mutate(
      z = (delta - mean(theta + delta))/sd(theta + delta)
    ) %>% 
    ungroup() %>% 
    select(-jj, -theta) %>% 
    distinct() %>% 
    crossing(ii = 1:25) %>% 
    left_join(post_ii, by = c(".draw", ".chain", ".iteration", "ii")) %>% 
    mutate(
      p = inv_logit(alpha * (0 - beta + delta))
    ) %>% 
    group_by(.draw, .chain, .iteration, sample, target) %>%
    summarize(
      z = unique(z),
      n = sum(p),
      p = mean(p)
    ) %>% 
    ungroup()
  
  # Rename
  post <- post %>% 
    mutate(
      actor = if_else(target == "WJ", "LS", "HS"),
      observer = if_else(sample == "WJ", "LS", "HS"),
      across(c(actor, observer), ~ordered(., levels = c("LS", "HS"))),
      ingroup = if_else(actor == observer, "Ingroup", "Outgroup")
    )
  
  
# Export ------------------------------------------------------------------

  # Save sample as .rds
  write_rds(post, "Experiment 1/results/results_pregistered_analyses.rds")

 
# Compare -----------------------------------------------------------------

  # By ingroup
  d_fig %>%
    group_by(.draw, .chain, .iteration, ingroup) %>%
    summarize(
      z = mean(z)
    ) %>%
    ungroup() %>%
    pivot_wider(names_from = ingroup, values_from = z) %>%
    # summarize(p = mean(ingroup > outgroup))
    median_qi(d = ingroup - outgroup)

  # By actor
  d_fig %>%
    group_by(.draw, .chain, .iteration, actor) %>%
    summarize(
      z = mean(z)
    ) %>%
    ungroup() %>%
    pivot_wider(names_from = actor, values_from = z) %>%
    # summarize(p = mean(HS > LS))
    median_qi(d = HS - LS)

  # By actor/observer
  d_fig %>%
    select(-sample, -target, -n, -p, -ingroup) %>%
    pivot_wider(names_from = observer, values_from = z) %>%
    group_by(actor) %>%
    # summarize(p = mean(HS > LS))
    median_qi(d = HS - LS)
  