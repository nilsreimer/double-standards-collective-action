rm(list = ls())

# Notes -------------------------------------------------------------------


# Library -----------------------------------------------------------------
  
  # Install packages
  # install.packages("tidyverse")
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
  dl <- read_rds("Experiment 3/data/dl.rds")
  
  # Exclude participants
  dl <- dl %>% 
    filter(participant_gender %in% c("Man", "Woman"))
  
  # Code predictors
  dl <- dl %>%
    mutate(
      ii = action,
      jj = as.integer(factor(id)),
      kk = case_when(
        participant_ideology == "Conservative" & participant_gender == "Man"   & protesters_cause == "For"     ~ 1L, 
        participant_ideology == "Conservative" & participant_gender == "Woman" & protesters_cause == "For"     ~ 2L, 
        participant_ideology == "Conservative" & participant_gender == "Man"   & protesters_cause == "Against" ~ 3L, 
        participant_ideology == "Conservative" & participant_gender == "Woman" & protesters_cause == "Against" ~ 4L, 
        participant_ideology == "Liberal"      & participant_gender == "Man"   & protesters_cause == "For"     ~ 5L, 
        participant_ideology == "Liberal"      & participant_gender == "Woman" & protesters_cause == "For"     ~ 6L, 
        participant_ideology == "Liberal"      & participant_gender == "Man"   & protesters_cause == "Against" ~ 7L, 
        participant_ideology == "Liberal"      & participant_gender == "Woman" & protesters_cause == "Against" ~ 8L
      ),
      y  = response
    )
  
  # Compile data list
  dlist <- with(dl, list(
    N = length(ii),
    I = max(ii),
    J = max(jj),
    K = max(kk),
    ii = ii,
    jj = jj,
    kk = kk,
    y = y
  ))


# Estimate (Model 1) ------------------------------------------------------
  
  # Compile model
  m1_model <- cmdstan_model("Experiment 3/models/s3_m1_2pl_model.stan")

  # Run model
  m1_fit <- m1_model$sample(
    data = dlist,
    seed = 1134290,
    chains = 8,
    parallel_chains = parallel::detectCores(),
    iter_sampling = 500,
    iter_warmup = 1000
  )

  # Save results as .rds
  m1_fit$save_object("Experiment 3/results/m1_fit_without_exclusions.rds")
  

# Process (Model 1) -------------------------------------------------------

  # Import results
  m1_fit <- read_rds("Experiment 3/results/m1_fit_without_exclusions.rds")

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
      dl %>%
        distinct(
          kk,
          participant_ideology,
          participant_gender,
          protesters_cause
        ),
      by = "kk"
    )

  # Export results as .rds
  write_rds(m1_post, "Experiment 3/results/m1_post_without_exclusions.rds")
