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
  dl <- read_rds("Experiment 1/data/dl.rds")
  dw <- read_rds("Experiment 1/data/dw.rds")
  ds <- read_rds("Experiment 1/data/ds.rds")
  
  # Exclude participants
  # dl <- dl %>% filter(!exclude) %>% select(-exclude)
  # dw <- dw %>% filter(!exclude) %>% select(-exclude)
  
  # Remove duplicates
  dw <- dw |> group_by(id) |> filter(n() == 1) |> ungroup()
  dl <- dl |> filter(id %in% dw$id)
  ds <- ds |> filter(id %in% dw$id)
  
  # Code predictors (preregistered analyses)
  dl <- dl %>%
    mutate(
      ii = action,
      jj = as.integer(factor(id)),
      x_1 = if_else(target == "PJ", 1L, 0L),
      x_2 = if_else(target == sample, 1L, 0L),
      x_3 = if_else(target == "PJ" & sample == "PJ", 1L, 0L),
      y  = response
    )
  
  # Code predictors (exploratory analyses)
  dw <- dw %>% 
    left_join(
      ds %>% select(id, political_orientation = q10),
      by = "id"
    ) %>% 
    rename_with(~str_replace(., "q11", "sdo")) %>% 
    rename_with(~str_replace(., "q12", "sjb")) %>% 
    mutate(
      across(c(sdo_3, sdo_4, sdo_7, sdo_8, sjb_3, sjb_7), ~8L - .)
    )
  
  # Calculate factor scores
  source("Experiment 1/scripts/run_factor_analyses.R")
  
  # Exclude observations with missing data
  dw <- dw %>% 
    drop_na(sdo, sjb) |> 
    transmute(
      id, 
      x_4 = political_orientation,
      x_5 = sdo, 
      x_6 = sjb,
      across(x_4:x_6, ~(. - mean(.))/sd(.))
    )
  
  # Merge predictors
  dl <- dl |> 
    filter(
      id %in% dw$id
    ) |> 
    left_join(dw, by = "id")
  
  # Compile data list
  dlist <- with(dl, list(
    N = length(ii),
    I = max(ii),
    J = max(jj),
    K = 6L,
    ii = ii,
    jj = jj,
    X = cbind(x_1, x_2, x_3, x_4, x_5, x_6),
    y = y
  ))


# Estimate ----------------------------------------------------------------

  # Compile model
  model <- cmdstan_model("Experiment 1/models/s1_2pl_model.stan")

  # Run model
  fit <- model$sample(
    data = dlist,
    seed = 6466031,
    chains = 8,
    parallel_chains = parallel::detectCores(),
    iter_sampling = 1000,
    iter_warmup = 1000
  )

  # Save results
  fit$save_object("Experiment 1/results/fit_exploratory_analyses_without_exclusions.rds")
  

# Predict -----------------------------------------------------------------

  # Import results
  fit <- read_rds("Experiment 1/results/fit_exploratory_analyses_without_exclusions.rds")
  
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
  
  # Merge posterior draws
  post <- tibble(
    outcome = rep(c("x_4", "x_5", "x_6"), each = 51L),
    x_1 = 0.50,
    x_2 = 0.50,
    x_3 = 0.25,
    x_4 = c(seq(-2.5, 2.5, 0.1), rep(0, 51L), rep(0, 51L)),
    x_5 = c(rep(0, 51L), seq(-2.5, 2.5, 0.1), rep(0, 51L)),
    x_6 = c(rep(0, 51L), rep(0, 51L), seq(-2.5, 2.5, 0.1))
  ) %>%
    crossing(.draw = 1:max(post_ii$.draw)) %>% 
    left_join(
      dl %>% 
        distinct(jj, x_1, x_2, x_3, x_4, x_5, x_6) %>% 
        left_join(post_jj, by = "jj") %>% 
        left_join(post_kk, by = c(".chain", ".iteration", ".draw")) %>% 
        mutate(
          delta = x_1*delta_1 + x_2*delta_2 + x_3*delta_3 + x_4*delta_4 + x_5*delta_5 + x_6*delta_6
        ) %>% 
        group_by(.chain, .iteration, .draw) %>% 
        summarize(
          mean = mean(delta + theta),
          sd = sd(delta + theta)
        ) %>% 
        ungroup(),
      by = ".draw"
    ) %>% 
    left_join(post_kk, by = c(".draw", ".chain", ".iteration")) %>% 
    mutate(
      delta = x_1*delta_1 + x_2*delta_2 + x_3*delta_3 + x_4*delta_4 + x_5*delta_5 + x_6*delta_6,
      z = (delta - mean)/sd
    ) %>% 
    select(-delta_1:-delta_6) %>% 
    crossing(ii = 1:25) %>% 
    left_join(post_ii, by = c("ii", ".draw", ".chain", ".iteration"))
  
  # Make posterior predictions
  post <- post %>% 
    mutate(
      p = inv_logit(alpha * (0 - beta + delta))
    ) %>% 
    group_by(outcome, x_1, x_2, x_3, x_4, x_5, x_6, .draw, .chain, .iteration, z) %>% 
    summarize(
      n = sum(p),
      p = mean(p)
    ) %>% 
    ungroup()
  
  # Rename
  post <- post %>% 
    mutate(
      x = case_when(
        outcome == "x_4" ~ x_4,
        outcome == "x_5" ~ x_5,
        outcome == "x_6" ~ x_6
      ),
      outcome = recode_factor(
        outcome,
        "x_4" = "Political Orientation",
        "x_5" = "Social Dominance",
        "x_6" = "System Justification",
        .ordered = TRUE
      )
    )
  
  
# Export ------------------------------------------------------------------
  
  # Save sample as .rds
  write_rds(post, "Experiment 1/results/results_exploratory_analyses_without_exclusions.rds")


# Compare -----------------------------------------------------------------

  # Calculate standardized coefficients
  dl %>% 
    distinct(jj, x_1, x_2, x_3, x_4, x_5, x_6) %>% 
    left_join(post_jj, by = "jj") %>% 
    left_join(post_kk, by = c(".chain", ".iteration", ".draw")) %>% 
    mutate(
      delta = x_1*delta_1 + x_2*delta_2 + x_3*delta_3 + x_4*delta_4 + x_5*delta_5 + x_6*delta_6
    ) %>% 
    group_by(.chain, .iteration, .draw, delta_4, delta_5, delta_6) %>% 
    summarize(
      mean = mean(delta + theta),
      sd = sd(delta + theta)
    ) %>% 
    ungroup() %>% 
    pivot_longer(
      delta_4:delta_6,
      names_to = "predictor",
      values_to = "estimate"
    ) %>% 
    mutate(
      predictor = recode_factor(
        predictor,
        "delta_4" = "Political Orientation",
        "delta_5" = "Social Dominance",
        "delta_6" = "System Justificaiton"
      ),
      estimate = estimate/sd
    ) %>% 
    group_by(predictor) %>% 
    median_qi(estimate) %>% 
    mutate(across(where(is.numeric), round, 2))
