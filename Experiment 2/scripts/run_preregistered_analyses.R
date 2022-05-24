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
  m1_model <- cmdstan_model("Experiment 2/models/s2_m1_2pl_model.stan")

  # Run model
  m1_fit <- m1_model$sample(
    data = dlist,
    seed = 3336594,
    chains = 8,
    parallel_chains = parallel::detectCores(),
    iter_sampling = 500,
    iter_warmup = 1000
  )

  # Save results as .rds
  m1_fit$save_object("Experiment 2/results/m1_fit.rds")
  

# Estimate (Model 2) ------------------------------------------------------

  # Add predictor variable(s)
  m2_dlist <- dl %>%
    with(., list(
      x_sjb = x_sjb,
      x_for = c(1L, 0L, 1L, 0L, 1L, 0L, 1L, 0L)
    )) %>%
    append(dlist, .)

  # Compile model
  m2_model <- cmdstan_model("Experiment 2/models/s2_m2_2pl_model.stan")

  # Run model
  m2_fit <- m2_model$sample(
    data = m2_dlist,
    seed = 5233271,
    chains = 8,
    parallel_chains = parallel::detectCores(),
    iter_sampling = 500,
    iter_warmup = 1000
  )

  # Save results as .rds
  m2_fit$save_object("Experiment 2/results/m2_fit.rds")

  
# Estimate (Model 3) ------------------------------------------------------
  
  # Add predictor variable(s)
  m3_dlist <- with(dl, list(
      x_sup = x_sup
    )) %>%
    append(dlist, .)

  # Compile model
  m3_model <- cmdstan_model("Experiment 2/models/s2_m3_2pl_model.stan")

  # Run model
  m3_fit <- m3_model$sample(
    data = m3_dlist,
    seed = 4046327,
    chains = 8,
    parallel_chains = parallel::detectCores(),
    iter_sampling = 500,
    iter_warmup = 1000
  )

  # Save results as .rds
  m3_fit$save_object("Experiment 2/results/m3_fit.rds")
  

# Process (Model 1) -------------------------------------------------------

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
    distinct(kk, .chain, .iteration, .draw, z) %>% 
    right_join(
      dl %>% 
        transmute(
          kk, 
          Participant = observers, 
          Protesters = protesters, 
          Cause = str_to_title(protest)
        ) %>% 
        distinct(),
      by = "kk"
    )
  
  # Export results as .rds
  write_rds(m1_post, "Experiment 2/results/m1_post.rds")
  

# Process (Model 2) -------------------------------------------------------
  
  # Import results
  m2_fit <- read_rds("Experiment 2/results/m2_fit.rds")
  
  # Extract draws
  m2_ps <- m2_fit$draws() %>% as_draws_df()
  
  # Extract posterior draws
  m2_post_ii <- m2_ps %>% spread_draws(alpha[ii], beta[ii]) %>% ungroup()
  m2_post_jj <- m2_ps %>% spread_draws(theta[jj]) %>% ungroup()
  m2_post_kk <- m2_ps %>% spread_draws(delta[kk], b_sjb_kk[kk], b_sjb_for, b_sjb_against) %>% ungroup()
  
  # Calculate standardized coefficients
  m2_post <- dl %>% 
    distinct(jj, kk, x_sjb) %>% 
    left_join(m2_post_kk, by = c("kk")) %>% 
    left_join(m2_post_jj, by = c("jj", ".draw", ".chain", ".iteration")) %>% 
    group_by(.draw) %>% 
    mutate(
      b_sjb_for = b_sjb_for/sd(theta + delta + b_sjb_kk*x_sjb),
      b_sjb_against = b_sjb_against/sd(theta + delta + b_sjb_kk*x_sjb),
      b_sjb_kk = b_sjb_kk/sd(theta + delta + b_sjb_kk*x_sjb)
    ) %>% 
    ungroup() %>% 
    select(.draw, kk, b_sjb_kk, b_sjb_for, b_sjb_against) %>% 
    distinct() %>% 
    right_join(
      dl %>% 
        transmute(
          kk, 
          Participant = observers, 
          Protesters = protesters, 
          Cause = str_to_title(protest)
        ) %>% 
        distinct(),
      by = "kk"
    )
  
  # Export results as .rds
  write_rds(m2_post, "Experiment 2/results/m2_post.rds")
  

# Process (Model 3) -------------------------------------------------------
  
  # Import results
  m3_fit <- read_rds("Experiment 2/results/m3_fit.rds")
  
  # Extract draws
  m3_ps <- m3_fit$draws() %>% as_draws_df()
  
  # Extract posterior draws
  m3_post_ii <- m3_ps %>% spread_draws(alpha[ii], beta[ii]) %>% ungroup()
  m3_post_jj <- m3_ps %>% spread_draws(theta[jj]) %>% ungroup()
  m3_post_kk <- m3_ps %>% spread_draws(delta[kk], b_sup, b_sup_kk[kk]) %>% ungroup()
  
  # Calculate standardized coefficients
  m3_post <- dl %>% 
    distinct(jj, kk, x_sup) %>% 
    left_join(m3_post_kk, by = c("kk")) %>% 
    left_join(m3_post_jj, by = c("jj", ".draw", ".chain", ".iteration")) %>% 
    group_by(.draw) %>% 
    mutate(
      b_sup_for = b_sup/sd(theta + delta + b_sup_kk*x_sup),
      b_sup_kk = b_sup_kk/sd(theta + delta + b_sup_kk*x_sup)
    ) %>% 
    ungroup() %>% 
    select(.draw, kk, b_sup_kk, b_sup) %>% 
    distinct() %>% 
    right_join(
      dl %>% 
        transmute(
          kk, 
          Participant = observers, 
          Protesters = protesters, 
          Cause = str_to_title(protest)
        ) %>% 
        distinct(),
      by = "kk"
    )
  
  # Export results as .rds
  write_rds(m3_post, "Experiment 2/results/m3_post.rds")
