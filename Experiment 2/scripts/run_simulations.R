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
  
  # Exclude participants
  dl <- dl %>% filter(!exclude) %>% select(-exclude)
  
  # Code indices
  dl <- dl %>%
    mutate(
      ii = action,
      jj = as.integer(factor(id)),
      kk = case_when(
        sample == "WJ" & target == "PJ" ~ 1L,
        sample == "WJ" & target == "WJ" ~ 2L,
        sample == "PJ" & target == "PJ" ~ 3L,
        sample == "PJ" & target == "WJ" ~ 4L
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


# Estimate ----------------------------------------------------------------

  # Compile model
  model <- cmdstan_model("Experiment 2/models/s2_2pl_model.stan")

  # Run model
  fit <- model$sample(
    data = dlist,
    seed = 3909872,
    chains = 8,
    parallel_chains = parallel::detectCores(),
    iter_sampling = 500,
    iter_warmup = 500,
    adapt_delta = 0.95
  )


# Simulate ----------------------------------------------------------------
  
  # Extract draws
  ps <- fit$draws() %>% as_draws_df()
  
  # Extract estimates for simulation
  ps <- ps %>% spread_draws(mu_beta, sigma_alpha, sigma_beta, sigma_delta)
  
  # Simulate data list
  simulate_dlist <- function(I, J, K) {
    
    # Sample draws for simulation
    ps_sim <- ps %>% filter(.draw %in% sample.int(max(ps$.draw), 1L))
    
    # Simulate data
    ps_sim %>% 
      crossing(nesting(jj = 1:J, kk = rep(1:K, each = J/K))) %>% 
      mutate(
        theta = rnorm(n())
      ) %>% 
      select(.chain:.draw, jj, theta, kk) %>% 
      left_join(
        ps_sim %>% 
          crossing(kk = 1:K) %>% 
          mutate(
            delta = rnorm(n()) * sigma_delta
          ) %>% 
          select(.chain:.draw, kk, delta),
        by = c(".chain", ".iteration", ".draw", "kk")
      ) %>% 
      full_join(
        ps_sim %>% 
          crossing(ii = 1:I) %>% 
          mutate(
            alpha = exp(rnorm(n()) * sigma_alpha), 
            beta  = mu_beta + rnorm(n()) * sigma_beta
          ) %>% 
          select(.chain:.draw, ii, alpha, beta),
        .,
        by = c(".chain", ".iteration", ".draw")
      ) %>% 
      mutate(
        p = inv_logit(alpha * (theta + beta + delta)),
        y = rbinom(n(), 1, prob = p)
      ) %>% 
      with(., list(
        N = length(ii),
        I = max(ii),
        J = max(jj),
        K = max(kk),
        ii = ii,
        jj = jj,
        kk = kk,
        y = y
      )) %>% 
      return()
  }
  
  
# Estimate ----------------------------------------------------------------

  # Simulate data
  sim <- tibble(
      I = 25L, 
      J = rep(seq(1000L, 2000L, 200L), each = 10), 
      K = 8L
    ) %>% 
    mutate(
      dlist = pmap(list(I, J, K), simulate_dlist)
    ) %>% 
    rowid_to_column("run")
  
  # Measure time
  start_time <- Sys.time()
  
  # Run models
  for (i in sim$run) {
    
    # Show progress
    print(sim[i,])
    
    # Run model
    sim_fit <- model$sample(
      data = sim$dlist[[i]],
      chains = 8,
      parallel_chains = parallel::detectCores(),
      iter_sampling = 500,
      iter_warmup = 500
    )
    
    # Save result
    if (i == 1L) {
      fit <- list(sim_fit)
    } else {
      fit <- append(fit, list(sim_fit))
    }

  }
  
  # Measure time
  Sys.time() - start_time
  
  # Extract parameter estimates
  sim <- sim %>% 
    mutate(
      ps = map(fit, ~as_draws_df(.$draws())),
      ps_ii = map(ps, ~spread_draws(., alpha[ii], beta[ii])),
      ps_jj = map(ps, ~spread_draws(., theta[jj])),
      ps_kk = map(ps, ~spread_draws(., delta[kk]))
    ) %>% 
    select(-ps)
  

# Process -----------------------------------------------------------------

  # Calculate effect sizes
  sim <- sim %>% 
    mutate(
      z = map(dlist, ~with(., tibble(jj = jj, kk = kk)) %>% distinct()),
      z = map2(z, ps_jj, ~full_join(.x, .y)),
      z = map2(z, ps_kk, ~full_join(.x, .y)),
      z = map(
        z, 
        ~group_by(., .chain, .iteration, .draw) %>% 
          mutate(z = (delta - mean(theta + delta))/sd(theta + delta)) %>% 
          distinct(kk, z) %>% 
          ungroup() %>% 
          select(.chain, .iteration, .draw, kk, z)
      ),
      p = map(dlist, ~with(., tibble(ii = ii, kk = kk)) %>% distinct()),
      p = map2(p, ps_ii, ~full_join(.x, .y)),
      p = map2(p, ps_kk, ~full_join(.x, .y)),
      n = map(
        p, 
        ~group_by(., .chain, .iteration, .draw, kk) %>% 
          mutate(p = inv_logit(alpha * (0 + beta + delta))) %>% 
          summarize(n = sum(p)) %>% 
          ungroup() %>% 
          select(.chain, .iteration, .draw, kk, n)
      ),
      p = map(
        p, 
        ~group_by(., .chain, .iteration, .draw, kk) %>% 
          mutate(p = inv_logit(alpha * (0 + beta + delta))) %>% 
          summarize(p = mean(p)) %>% 
          ungroup() %>% 
          select(.chain, .iteration, .draw, kk, p)
      )
    )
  
  # Summarize
  sim <- sim %>%
    select(-dlist, -ps_ii, -ps_jj, -ps_kk) %>%
    mutate(
      z = map(z, ~rename(., estimate = z)),
      p = map(p, ~rename(., estimate = p)),
      n = map(n, ~rename(., estimate = n))
    ) %>%
    pivot_longer(
      z:n,
      names_to = "metric",
      values_to = "estimate"
    ) %>%
    unnest(estimate) %>% 
    group_by(run, I, J, K, metric, kk) %>%
    median_qi(estimate) %>% 
    mutate(interval_width = .upper - .lower) %>% 
    ungroup()
  
  # Visualize
  sim %>% 
    filter(metric == "z") %>% 
    group_by(J) %>% 
    mutate(run = as.integer(factor(run))) %>% 
    ungroup() %>% 
  ggplot(., aes(x = J, y = interval_width)) + 
    geom_point(
      aes(colour = factor(run)),
      size = 1
    ) +
    stat_summary(
      fun = "mean",
      geom = "line",
      colour = "darkred"
    ) +
    stat_summary(
      fun = "mean",
      geom = "point",
      colour = "darkred",
      shape = "diamond",
      size = 2
    ) +
    scale_x_continuous(
      breaks = seq(1000, 2000, 200),
      minor_breaks = NULL
    ) +
    scale_y_continuous(
      minor_breaks = NULL,
      sec.axis = sec_axis(~ ./2, name = "Interval Width / 2")
    ) + 
    scale_colour_grey(
      guide = "none"
    ) +
    coord_fixed(
      1000/0.42,
      xlim = c(1000, 2000),
      ylim = c(0.00, 0.42)
    ) +
    theme_bw(base_size = 10) +
    labs(
      x = expression(italic(J)),
      y = "Interval Width" 
    )


# Export ------------------------------------------------------------------

  # Export as .png
  ggsave(
    "Experiment 2/preregistration/figure-1.png",
    width = 12, height = 6, units = "cm",
    dpi = 600, type = "cairo"
  )
  
  # Export as .rds
  write_rds(sim, "Experiment 2/results/results_simulations.rds")
   