rm(list = ls())

# Notes -------------------------------------------------------------------


# Library -----------------------------------------------------------------

  # Install packages
  # install.packages("tidyverse")
  # install.packages("numform")
  # install.packages("corrr")
  
  # Load packages
  library(tidyverse)
  library(numform)
  library(corrr)


# Prepare -----------------------------------------------------------------

  # Import data
  dl <- read_rds("Scale Development/data/dl_ps2.rds")
  
  # Exclude
  dl <- dl %>% filter(assigned == 1)
  
  # Summarize 
  ds <- dl %>% 
    select(
      Acceptable = q2,
      Disruptive = q1_1,
      Violent = q1_2,
      Extreme = q1_3,
      Negative = q3
    ) %>% 
    correlate() %>% 
    transmute(
      `#` = row_number(),
      Rating = term,
      across(c(-`#`, -Rating, -term), ~f_num(., digits = 2)),
      across(c(-`#`, -Rating, -term), ~replace_na(., ""))
    )
  

# Export ------------------------------------------------------------------

  # Save as .rds
  write_rds(ds, "Scale Development/figures/table-s3.rds")
