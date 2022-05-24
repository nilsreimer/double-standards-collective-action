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
  library(tidyverse); library(janitor); library(gendercodeR)

  # Add to gender dictionary (gendercodeR)
  gender_dictionary <- list(
    email = "female",
    `my sex is female` = "female",
    makle = "male",
    femalew = "female",
    `female only` = "female"
  )


# Import ------------------------------------------------------------------

  # Import metadata from Prolific
  dp <- read_csv("Experiment 1/data/raw/prolific_export.csv")

  # Import data from Qualtrics (Screening Survey)
  ds <- read_csv("Experiment 1/data/raw/Screening_Survey.csv")[-1:-2,]
  
  # Import data from Qualtrics (Experiment 1)
  dr <- read_csv("Experiment 1/data/raw/Study_1.csv")[-1:-2,]


# Prepare -----------------------------------------------------------------

  # Transform metadata (Prolific)
  dp <- dp %>% 
    clean_names() %>% 
    mutate(
      across(c(age, socioeconomic_status), as.integer),
      across(where(is.character), ~if_else(. == "CONSENT REVOKED", NA_character_, .))
    )
  
  # Select variables (Prolific)
  dp <- dp %>% select(id, age, highest_education_level:student_status)
  
  # Transform data (Screening Survey)
  ds <- ds %>% 
    clean_names() %>% 
    select(id, sample, q2:q9, q10 = q10_1, q12_1:q13_3) %>%
    mutate(across(c(q3, q4, q5:q10, q12_1:q13_3), as.integer))
  
  # Code gender (Screening Survey)
  ds <- ds %>% 
    mutate(
      gender = recode_gender(
        q2,
        dictionary = c(narrow, gender_dictionary),
        fill = TRUE
      )
    ) %>% 
    select(id, sample, gender, everything())
  
  # Transform data (Experiment 1)
  dr <- dr %>% clean_names()
  
  # Prepare long data (Experiment 1)
  dl <- dr %>% 
    select(id, sample, matches("q3_|q5_")) %>% 
    pivot_longer(
      matches("q3_|q5_"),
      names_to = c("target", "action"),
      names_pattern = "(.*)_([0-9]*)",
      names_transform = list(target = as.character, action = as.integer),
      values_to = "response",
      values_transform = list(response = as.integer)
    ) %>% 
    mutate(
      target = recode(target, "q3" = "WJ", "q5" = "PJ")
    ) %>% 
    filter(!is.na(response))
  
  # Recode actions
  dl <- read_csv("Experiment 1/data/raw/Study_1.csv")[1,] %>% 
    clean_names() %>% 
    select(matches("q3_|q5_")) %>% 
    pivot_longer(
      matches("q3_|q5_"),
      names_to = "action",
      names_pattern = ".*_([0-9]*)",
      names_transform = list(action = as.integer),
      values_to = "action_text"
    ) %>%
    mutate(
      action_text = str_extract(action_text, "(?<= - ).*")
    ) %>% 
    distinct(action, action_text) %>% 
    left_join(dl, ., by = "action")
  
  # Prepare wide data (Experiment 1)
  dw <- dr %>% 
    left_join(dl %>% distinct(id, target), by = "id") %>% 
    select(
      id, sample, target,
      q6:q10, starts_with("q11_"), starts_with("q12_"), 
      attention_check = q13
    ) %>% 
    mutate(across(q6:q12_8, as.integer))


# Exclude -----------------------------------------------------------------

  # Code exclusion criteria (Screening Survey)
  ds <- ds %>%
    mutate(
      exclude = case_when(
        q3 >= 25L & q4 == 1L & q5 == 0L ~ 0L,
        TRUE ~ 1L
      ),
      job = if_else(is.na(q8), q9, q8),
      exclude = case_when(
        exclude == 1L ~ TRUE,
        (sample == "WJ" & job == 1L) | (sample == "PJ" & job == 2L) ~ FALSE,
        TRUE ~ TRUE
      )
    ) %>% 
    select(-job)
  
  # Code exclusion criteria (Experiment 1)
  dw <- dw %>%
    mutate(
      exclude = case_when(
        is.na(attention_check) ~ TRUE,
        attention_check == "" ~ TRUE,
        target == "WJ" & attention_check == "gorvernment" ~ TRUE,
        target == "WJ" & attention_check == "graduates" ~ TRUE,
        target == "WJ" & attention_check == "middle class" ~ TRUE,
        target == "WJ" & attention_check == "people from the middle to lowest paid" ~ TRUE,
        target == "WJ" & attention_check == "not sure" ~ TRUE,
        target == "WJ" & attention_check == "I cant write here" ~ TRUE,
        target == "WJ" & attention_check == "Middle class" ~ TRUE,
        target == "WJ" & attention_check == "all People with social conscience" ~ TRUE,
        target == "PJ" & attention_check == "People in lower social status" ~ TRUE,
        target == "PJ" & attention_check == "Normal people" ~ TRUE,
        target == "PJ" & attention_check == "Non Professionals" ~ TRUE,
        target == "PJ" & attention_check == "workers" ~ TRUE,
        target == "PJ" & attention_check == "honestly cannot remember" ~ TRUE,
        target == "PJ" & attention_check == "People" ~ TRUE,
        target == "PJ" & attention_check == "non professional workers" ~ TRUE,
        target == "PJ" & attention_check == "cannot remember" ~ TRUE,
        target == "PJ" & attention_check == "basic everyday people" ~ TRUE,
        target == "PJ" & attention_check == "public" ~ TRUE,
        target == "PJ" & attention_check == "Low income housholds" ~ TRUE,
        target == "PJ" & attention_check == "Employed people" ~ TRUE,
        target == "PJ" & attention_check == "Society" ~ TRUE,
        target == "PJ" & attention_check == "workers" ~ TRUE,
        target == "PJ" & attention_check == "Eu citizens" ~ TRUE,
        target == "PJ" & attention_check == "Everyone" ~ TRUE,
        target == "PJ" & attention_check == "Adults" ~ TRUE,
        target == "PJ" & attention_check == "the mentioned bill didnt display properly so cant answer this" ~ TRUE,
        target == "PJ" & attention_check == "unsure" ~ TRUE,
        target == "PJ" & attention_check == "the bottom half of society were most affected in terms of discriminatory action" ~ TRUE,
        target == "PJ" & attention_check == "Unsure" ~ TRUE,
        target == "PJ" & attention_check == "Poor" ~ TRUE,
        target == "PJ" & attention_check == "Not sure" ~ TRUE,
        target == "PJ" & attention_check == "working class" ~ TRUE,
        target == "PJ" & attention_check == "I have forgotten and wasnt able to go back a read the brief again" ~ TRUE,
        target == "PJ" & attention_check == "anyone with a job" ~ TRUE,
        target == "PJ" & attention_check == "Workers" ~ TRUE,
        target == "PJ" & attention_check == "everyone" ~ TRUE,
        target == "PJ" & attention_check == "disadvantaged people" ~ TRUE,
        target == "PJ" & attention_check == "The public" ~ TRUE,
        target == "PJ" & attention_check == "Working class" ~ TRUE,
        target == "PJ" & attention_check == "Public" ~ TRUE,
        target == "PJ" & attention_check == "Always the lower classes" ~ TRUE,
        target == "PJ" & attention_check == "not sure" ~ TRUE,
        target == "PJ" & attention_check == "Low income" ~ TRUE,
        target == "PJ" & attention_check == "non proffessionals " ~ TRUE,
        target == "PJ" & attention_check == "General public" ~ TRUE,
        target == "PJ" & attention_check == "not sure" ~ TRUE,
        target == "PJ" & attention_check == "Lower class " ~ TRUE,
        target == "PJ" & attention_check == "Working people " ~ TRUE,
        TRUE ~ FALSE
      )
    )
  dl <- dl %>% left_join(dw %>% distinct(id, exclude), by = "id")


# Export ------------------------------------------------------------------

  # Export as .rds
  write_rds(dl, "Experiment 1/data/dl.rds")
  write_rds(dw, "Experiment 1/data/dw.rds")
  write_rds(ds, "Experiment 1/data/ds.rds")
  write_rds(dp, "Experiment 1/data/dp.rds")
  
  # Export as .csv
  write_csv(dl, "Experiment 1/data/csv/dl.csv")
  write_csv(dw, "Experiment 1/data/csv/dw.csv")
  write_csv(ds, "Experiment 1/data/csv/ds.csv")
  write_csv(dp, "Experiment 1/data/csv/dp.csv")
  