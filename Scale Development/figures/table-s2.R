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
  dl <- read_rds("Scale Development/data/dl_ps2.rds")
  
  # Import Table S2
  results <- read_rds("Scale Development/figures/table-s1.rds")
  
  # Exclude
  dl <- dl %>% filter(assigned == 1)
  
  # Summarize
  ds <- dl %>% 
    group_by(action, action_text) %>% 
    summarize(
      `Acce.` = mean(q2, na.rm = TRUE),
      `Disr.` = mean(q1_1, na.rm = TRUE),
      `Viol.` = mean(q1_2, na.rm = TRUE),
      `Extr.` = mean(q1_3, na.rm = TRUE),
      `Nega.` = mean(q3, na.rm = TRUE)
    ) %>% 
    ungroup() %>% 
    transmute(
      Action = action_text,
      across(
        `Acce.`:`Nega.`, 
        ~f_num(., digits = 2, retain.leading.zero = T)
      )
    ) %>% 
    left_join(results %>% distinct(`#`, Action), ., by = c("Action"))
  

  

# Export ------------------------------------------------------------------

  # Save as .rds
  write_rds(ds, "Scale Development/figures/table-s2.rds")
