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
  dw <- read_rds("Experiment 3/data/dw.rds")
  
  # Exclude participants
  dl <- dl %>% 
    filter(!exclude, participant_gender %in% c("Man", "Woman")) %>% 
    select(-exclude)
  dw <- dw %>% 
    filter(!exclude, participant_gender %in% c("Man", "Woman")) %>% 
    select(-exclude)
  
  # Calculate factor scores
  source("Experiment 3/scripts/run_factor_analyses.R")
  
  # Code predictors
  dl <- dw %>%
    mutate(
      ide = case_when(
        participant_gender == "Woman" ~ ide_women,
        participant_gender == "Man" ~ ide_men
      )
    ) %>% 
    transmute(
      id,
      across(
        c(ide, sjb),
        ~(. - mean(., na.rm = T))/sd(., na.rm = T),
        .names = "z_{.col}"
      )
    ) %>% 
    left_join(dl, ., by = "id") %>% 
    drop_na() %>% 
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
    y = y,
    x_for = c(1L, 1L, 0L, 0L, 1L, 1L, 0L, 0L),
    x_sjb = z_sjb,
    x_ide = z_ide
  ))
  

# Estimate (Gender Identification) ----------------------------------------

  # Compile model
  ide_model <- cmdstan_model("Experiment 3/models/s3_ide_2pl_model.stan")

  # Run model
  ide_fit <- ide_model$sample(
    data = dlist,
    seed = 6612039,
    chains = 8,
    parallel_chains = parallel::detectCores(),
    iter_sampling = 500,
    iter_warmup = 1000
  )

  # Save results as .rds
  ide_fit$save_object("Experiment 3/results/ide_fit.rds")
  

# Process (Gender Identification) -----------------------------------------

  # Import results
  ide_fit <- read_rds("Experiment 3/results/ide_fit.rds")
  
  # Extract draws
  ide_ps <- ide_fit$draws() %>% as_draws_df()
  
  # Extract posterior draws
  ide_post_ii <- ide_ps %>% spread_draws(alpha[ii], beta[ii]) %>% ungroup()
  ide_post_jj <- ide_ps %>% spread_draws(theta[jj]) %>% ungroup()
  ide_post_kk <- ide_ps %>% spread_draws(delta[kk], b_ide_kk[kk], b_ide) %>% ungroup()
  
  # Calculate effect sizes
  ide_post <- dl %>% 
    distinct(jj, kk, z_ide) %>% 
    left_join(ide_post_kk, by = c("kk")) %>% 
    left_join(ide_post_jj, by = c("jj", ".draw", ".chain", ".iteration")) %>% 
    group_by(.draw) %>% 
    mutate(
      b_ide = b_ide/sd(theta + delta + b_ide_kk*z_ide),
      b_ide_kk = b_ide_kk/sd(theta + delta + b_ide_kk*z_ide),
    ) %>% 
    ungroup() %>%
    distinct(.chain, .iteration, .draw, kk, b_ide, b_ide_kk) %>% 
      right_join(
        dl %>%
          select(kk, participant_ideology, participant_gender, protesters_cause) %>%
          distinct(),
        by = "kk"
      )
    
  # ide_post <- dl %>% 
  #   distinct(jj, kk, z_ide) %>% 
  #   left_join(ide_post_kk, by = c("kk")) %>% 
  #   left_join(ide_post_jj, by = c("jj", ".draw", ".chain", ".iteration")) %>% 
  #   group_by(.draw) %>% 
  #   summarize(
  #     mean = mean(theta + delta + b_ide_kk * z_ide),
  #     sd = sd(theta + delta + b_ide_kk * z_ide)
  #   ) %>% 
  #   crossing(
  #     kk = 1:8, 
  #     z_ide = -1:1
  #   ) %>% 
  #   left_join(ide_post_kk, by = c(".draw", "kk")) %>% 
  #   mutate(
  #     z = ((delta + b_ide_kk * z_ide) - mean)/sd
  #   ) %>% 
  #   right_join(
  #     dl %>% 
  #       select(kk, participant_ideology, participant_gender, protesters_cause) %>% 
  #       distinct(),
  #     by = "kk"
  #   ) %>% 
  #   select(.draw, kk, z_ide, z, participant_ideology, participant_gender, protesters_cause)
  
  # Export results as .rds
  write_rds(ide_post, "Experiment 3/results/ide_post.rds")
  

# Estimate (System Justification) -----------------------------------------

  # Compile model
  sjb_model <- cmdstan_model("Experiment 3/models/s3_sjb_2pl_model.stan")
  
  # Run model
  sjb_fit <- sjb_model$sample(
    data = dlist,
    seed = 3507678,
    chains = 8,
    parallel_chains = parallel::detectCores(),
    iter_sampling = 500,
    iter_warmup = 1000
  )
  
  # Save results as .rds
  sjb_fit$save_object("Experiment 3/results/sjb_fit.rds")


# Process (System Justification)  -----------------------------------------

  # Import results
  sjb_fit <- read_rds("Experiment 3/results/sjb_fit.rds")
  
  # Extract draws
  sjb_ps <- sjb_fit$draws() %>% as_draws_df()
  
  # Extract posterior draws
  sjb_post_ii <- sjb_ps %>% spread_draws(alpha[ii], beta[ii]) %>% ungroup()
  sjb_post_jj <- sjb_ps %>% spread_draws(theta[jj]) %>% ungroup()
  sjb_post_kk <- sjb_ps %>% spread_draws(delta[kk], b_sjb_kk[kk], b_sjb_for, b_sjb_against) %>% ungroup()
  
  # Calculate standardized coefficients
  sjb_post <- dl %>% 
    distinct(jj, kk, z_sjb) %>% 
    left_join(sjb_post_kk, by = c("kk")) %>% 
    left_join(sjb_post_jj, by = c("jj", ".draw", ".chain", ".iteration")) %>% 
    group_by(.draw) %>% 
    mutate(
      b_sjb_for = b_sjb_for/sd(theta + delta + b_sjb_kk*z_sjb),
      b_sjb_against = b_sjb_against/sd(theta + delta + b_sjb_kk*z_sjb),
    ) %>% 
    ungroup() %>% 
    select(.draw, b_sjb_for, b_sjb_against) %>% 
    distinct()
  
  # Export results as .rds
  write_rds(sjb_post, "Experiment 3/results/sjb_post.rds")
  