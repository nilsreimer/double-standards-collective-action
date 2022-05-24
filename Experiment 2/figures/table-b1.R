rm(list = ls())

# Notes -------------------------------------------------------------------


# Library -----------------------------------------------------------------

  # Install packages
  # install.packages("tidyverse")
  # install.packages("numform")
  
  # Load packages
  library(tidyverse)
  library(numform)


# Prepare -----------------------------------------------------------------

  # Import data
  dl <- read_rds("Experiment 2/data/dl.rds")
  
  # Exclude participants
  dl <- dl %>% filter(!exclude) %>% select(-exclude)
  
  # Summarize 
  ds <- dl %>% 
    group_by(action, action_text) %>% 
    summarize(
      Pr = mean(response)
    ) %>% 
    ungroup() %>% 
    arrange(desc(Pr)) %>% 
    transmute(
      `#` = row_number(),
      Action = action_text,
      Pr = f_prop2percent(Pr, digits = 0)
    )
    

# Export ------------------------------------------------------------------
  
  # Save as .rds
  write_rds(ds, "Experiment 2/figures/table-b1.rds")
