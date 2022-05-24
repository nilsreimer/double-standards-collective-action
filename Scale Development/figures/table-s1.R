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

  # Import results 
  results <- read_rds("Scale Development/results/results_scale_development.rds")
  
  # Reformat
  results <- results %>% 
    arrange(desc(I)) %>% 
    transmute(
      `#` = row_number(),
      Action = action_text,
      Information = f_num(I, digits = 2, retain.leading.zero = T),
      alpha = f_num(alpha, digits = 2, retain.leading.zero = T),
      across(
        c(`1`:`5`),
        ~round(. * 100)
      )
    )
  

# Export ------------------------------------------------------------------

  # Save as .rds
  write_rds(results, "Scale Development/figures/table-s1.rds")
