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
    x_for = c(1L, 0L, 1L, 0L, 1L, 0L, 1L, 0L),
    x_sjb = x_sjb,
    x_sup = x_sup,
    x_pol = x_pol
  ))


# Estimate (Model 4) ------------------------------------------------------
  
  # Compile model
  m4_model <- cmdstan_model("Experiment 2/models/s2_m4_2pl_model.stan")

  # Run model
  m4_fit <- m4_model$sample(
    data = dlist,
    seed = 4670233,
    chains = 8,
    parallel_chains = parallel::detectCores(),
    iter_sampling = 500,
    iter_warmup = 1000
  )

  # Save results as .rds
  m4_fit$save_object("Experiment 2/results/m4_fit.rds")
  

# Process (Model 4) -------------------------------------------------------
  
  # Import results
  m4_fit <- read_rds("Experiment 2/results/m4_fit.rds")
  
  # Extract draws
  m4_ps <- m4_fit$draws() %>% as_draws_df()
  
  # Extract posterior draws
  m4_post_ii <- m4_ps %>% spread_draws(alpha[ii], beta[ii]) %>% ungroup()
  m4_post_jj <- m4_ps %>% spread_draws(theta[jj]) %>% ungroup()
  m4_post_kk <- m4_ps %>% spread_draws(delta[kk], b_pol_kk[kk], b_pol_for, b_pol_against) %>% ungroup()
  
  # Calculate standardized coefficients
  m4_post <- dl %>% 
    distinct(jj, kk, x_pol) %>% 
    left_join(m4_post_kk, by = c("kk")) %>% 
    left_join(m4_post_jj, by = c("jj", ".draw", ".chain", ".iteration")) %>% 
    group_by(.draw) %>% 
    mutate(
      b_pol_for = b_pol_for/sd(theta + delta + b_pol_kk*x_pol),
      b_pol_against = b_pol_against/sd(theta + delta + b_pol_kk*x_pol),
      b_pol_kk = b_pol_kk/sd(theta + delta + b_pol_kk*x_pol)
    ) %>% 
    ungroup() %>% 
    select(.draw, kk, b_pol_kk, b_pol_for, b_pol_against) %>% 
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
  write_rds(m4_post, "Experiment 2/results/m4_post.rds")

