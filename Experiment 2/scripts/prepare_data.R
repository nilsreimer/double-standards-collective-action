rm(list = ls())

# Notes -------------------------------------------------------------------

  #########################################################################
  # All identifying information (e.g., Prolific IDs) have been removed in #
  # preprocessing and replaced with newly generated IDs.                  #
  #########################################################################

# Library -----------------------------------------------------------------

  # Install packages
  # install.packages("tidyverse")
  # install.packages("janitor")
  # devtools::install_github("ropenscilabs/gendercodeR")

  # Load packages
  library(tidyverse); library(janitor); library(gendercoder)


# Import ------------------------------------------------------------------

  # Import metadata from Prolific
  dp <- read_csv("Experiment 2/data/raw/prolific_export.csv")

  # Import data from Qualtrics (Experiment 2)
  dr <- read_csv("Experiment 2/data/raw/Study_2.csv")[-1:-2,]


# Prepare -----------------------------------------------------------------

  # Transform metadata (Prolific)
  dp <- dp %>% 
    clean_names() %>% 
    mutate(
      across(c(age), as.integer),
      across(where(is.character), ~if_else(. == "CONSENT REVOKED", NA_character_, .))
    )
  
  # Transform data (Experiment 2)
  dr <- dr %>% clean_names()
  
  # Remove duplicates
  dr <- dr %>% 
    mutate(
      recorded_date = lubridate::as_datetime(recorded_date)
    ) %>% 
    group_by(id) %>% 
    slice_min(recorded_date) %>% 
    ungroup()
  
  # Remove incomplete responses
  dr <- dr %>% filter(finished == "1")
  
  # Code gender (Experiment 2)
  dr <- dr %>% 
    mutate(
      gender = recode_gender(
        q20_1,
        dictionary = fewlevels_en,
        retain_unmatched = TRUE
      ),
      gender = case_when(
        gender == "fermale" ~ "woman",
        gender == "Genderfluid" ~ "sex and gender diverse",
        gender == "genderfluid" ~ "sex and gender diverse",
        gender == "Libramasculine" ~ "sex and gender diverse",
        gender == "cis gender female" ~ "woman",
        gender == "Nonbinary, thank you for making this a prompt and not a binary multiple choice." ~ "sex and gender diverse",
        gender == "Woman/Female" ~ "woman",
        TRUE ~ gender
      )
    )
  
  # Prepare long data (Experiment 2)
  dl <- dr %>% 
    select(id, observers, protesters, protest, starts_with("q12_")) %>% 
    pivot_longer(
      starts_with("q12_"),
      names_to = "action",
      names_pattern = ".*_([0-9]*)",
      names_transform = list(action = as.integer),
      values_to = "response",
      values_transform = list(response = as.integer)
    ) %>% 
    filter(!is.na(response))
  
  # Recode actions
  dl <- read_csv("Experiment 2/data/raw/Study_2.csv")[1,] %>% 
    clean_names() %>% 
    select(starts_with("q12_")) %>% 
    pivot_longer(
      starts_with("q12_"),
      names_to = "action",
      names_pattern = ".*_([0-9]*)",
      names_transform = list(action = as.integer),
      values_to = "action_text"
    ) %>%
    mutate(
      action_text = str_extract(action_text, "(?<= - ).*"),
      action_text = str_replace_all(action_text, "\\[Field-protest\\]", "[for/against]"),
      action_text = str_replace_all(action_text, "\\[Field-opposite\\]", "[against/for]")
    ) %>% 
    distinct(action, action_text) %>% 
    left_join(dl, ., by = "action")
  
  # Prepare wide data (Experiment 2)
  dw <- dr %>% 
    mutate(
      across(c(q4:q19_1, q20_2, q21:q25), as.integer),
      exclude = as.logical(exclude)
    ) %>% 
    mutate(
      sjb_1 = q13_1,
      sjb_2 = q13_2,
      sjb_3 = 8L - q13_3,
      sjb_4 = q13_4,
      sjb_5 = q13_5,
      sjb_6 = q13_6,
      sjb_7 = 8L - q13_7,
      sjb_8 = q13_8
    ) %>% 
    select(
      id, observers, protesters, protest,
      age = q20_2, gender, exclude,
      q5, starts_with("q12"), starts_with("q13"), sjb_1:sjb_8,
      q14:q19_1, q21, 
      attention_check_1 = q23,
      attention_check_2 = q24,
      attention_check_3 = q25
    )
  
  # Add exclusion criterion to long data
  dl <- dw %>% 
    distinct(id, exclude) %>% 
    left_join(dl, ., by = "id")


# Export ------------------------------------------------------------------

  # Export as .rds
  write_rds(dl, "Experiment 2/data/dl.rds")
  write_rds(dw, "Experiment 2/data/dw.rds")
  write_rds(dp, "Experiment 2/data/dp.rds")
  
  # Export as .csv
  write_csv(dl, "Experiment 2/data/csv/dl.csv")
  write_csv(dw, "Experiment 2/data/csv/dw.csv")
  write_csv(dp, "Experiment 2/data/csv/dp.csv")
  