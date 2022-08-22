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
  # devtools::install_github("ropenscilabs/gendercoder")
  
  # Load packages
  library(tidyverse); library(janitor); library(gendercoder)


# Import ------------------------------------------------------------------

  # Import metadata from Prolific
  dp <- read_csv("Experiment 3/data/raw/prolific_export.csv")
  
  # Import data from Qualtrics (Study 2)
  dr <- read_csv("Experiment 3/data/raw/qualtrics_export.csv")[-1:-2,]


# Prepare -----------------------------------------------------------------

  # Transform metadata (Prolific)
  dp <- dp %>% 
    clean_names() %>% 
    mutate(
      across(c(age), as.integer),
      across(
        where(is.character), 
        ~if_else(. %in% c("CONSENT REVOKED", "DATA EXPIRED"), NA_character_, .)
      )
    )

  # Transform data
  dr <- dr %>% clean_names()

  # Remove incomplete responses
  dr <- dr %>% filter(finished == "1")
  
  # Code demographics
  dr <- dr %>% 
    mutate(
      gender = recode_gender(
        q64_1,
        dictionary = fewlevels_en,
        retain_unmatched = FALSE
      ),
      gender = case_when(
        !is.na(gender) ~ gender,
        q64_1 == "Female non-binary" ~ "sex and gender diverse",
        q64_1 == "female/nonbinary" ~ "sex and gender diverse",
        q64_1 == "Female/Woman" ~ "woman",
        q64_1 == "Gender nonconforming female" ~ "sex and gender diverse",
        q64_1 == "genderfluid" ~ "sex and gender diverse",
        q64_1 == "hetrosexual male" ~ "man",
        q64_1 == "I am a woman" ~ "woman",
        q64_1 == "my sex is female" ~ "woman",
        q64_1 == "non-binary girl" ~ "sex and gender diverse",
        q64_1 == "Non-bindary" ~ "sex and gender diverse",
        q64_1 == "Questioning" ~ "sex and gender diverse",
        q64_1 == "white" ~ NA_character_,
        q64_1 == "Women" ~ "woman"
      ),
      age = as.integer(q64_2)
    )
  
  # Transform predictor and outcome variables
  dr <- dr %>% 
    mutate(
      across(
        c(q9, q10_1:q10_3, q12_1:q12_25, q16:q63),
        as.integer
      ),
      exclude = as.logical(exclude)
    ) %>% 
    mutate(
      supp = q9,
      mora_1 = q10_1,
      mora_2 = q10_2,
      mora_3 = q10_3,
      mfq_care_1 = q49,
      mfq_care_2 = q50,
      mfq_care_3 = q51,
      mfq_care_4 = q52,
      mfq_care_5 = q53,
      mfq_care_6 = q54,
      mfq_equa_1 = q43,
      mfq_equa_2 = q44,
      mfq_equa_3 = q45,
      mfq_equa_4 = q46,
      mfq_equa_5 = q47,
      mfq_equa_6 = q48,
      mfq_prop_1 = q37,
      mfq_prop_2 = q38,
      mfq_prop_3 = q39,
      mfq_prop_4 = q40,
      mfq_prop_5 = q41,
      mfq_prop_6 = q42,
      mfq_loya_1 = q31,
      mfq_loya_2 = q32,
      mfq_loya_3 = q33,
      mfq_loya_4 = q34,
      mfq_loya_5 = q35,
      mfq_loya_6 = q36,
      mfq_auth_1 = q25,
      mfq_auth_2 = q26,
      mfq_auth_3 = q27,
      mfq_auth_4 = q28,
      mfq_auth_5 = q29,
      mfq_auth_6 = q30,
      mfq_puri_1 = q19,
      mfq_puri_2 = q20,
      mfq_puri_3 = q21,
      mfq_puri_4 = q22,
      mfq_puri_5 = q23,
      mfq_puri_6 = q24,
      sjb_1 = q56,
      sjb_2 = q57,
      sjb_3 = 8L - q58,
      sjb_4 = q59,
      sjb_5 = q60,
      sjb_6 = q61,
      sjb_7 = 8L - q62,
      sjb_8 = q63,
      pol = q68_1,
      ide_women = q69,
      ide_men = q70
    )


# Exclude -----------------------------------------------------------------

  # Code exclusion criteria
  dr <- dr %>% 
    mutate(
      across(
        c(q16:q18, q72:q73),
        as.integer
      ),
      attention_check_1 = q16 == 3L,
      attention_check_2 = q17 == 5L,
      attention_check_3 = q18 == 2L,
      attention_check_4 = q72 == 1L,
      attention_check_5 = (q73 == 1L & cause == "for") | (q73 == 2L & cause == "against"),
      exclude = !attention_check_1 | !attention_check_2 | !attention_check_3 | !attention_check_4 | !attention_check_5
    ) %>% 
    mutate(
      exclude = exclude | is.na(exclude)
    )
    
  # Remove duplicates
  dr <- dr %>% 
    mutate(
      recorded_date = lubridate::as_datetime(recorded_date)
    ) %>% 
    group_by(id) %>% 
    slice_min(exclude) %>% 
    slice_min(recorded_date) %>% 
    ungroup()
  

# Transform ---------------------------------------------------------------

  # Code conditions
  dr <- dr %>% 
    mutate(
      participant_ideology = participant,
      participant_gender = str_to_sentence(gender),
      protesters_cause = str_to_sentence(cause)
    )
  
  # Prepare long data
  dl <- dr %>% 
    select(
      id, 
      participant_ideology, participant_gender, protesters_cause, 
      starts_with("q12_")
    ) %>% 
    pivot_longer(
      starts_with("q12_"),
      names_to = "action",
      names_pattern = ".*_([0-9]*)",
      names_transform = list(action = as.integer),
      values_to = "response",
      values_transform = list(response = as.integer)
    ) %>% 
    filter(!is.na(response))
  
  # Add action text
  dl <- read_csv("Experiment 3/data/raw/qualtrics_export.csv")[1,] %>% 
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

  # Prepare wide data
  dw <- dr %>% 
    mutate(
      across(
        c(q2, q6, q9, q10_1:q10_3, q13, q12_1:q12_25, q16:q63, 
          q65, q66, q67_1, q68_1, q69, q70, q72, q73),
        as.integer
      ),
      exclude = as.logical(exclude)
    ) %>% 
    mutate(
      sup = q9,
      mor_1 = q10_1,
      mor_2 = q10_2,
      mor_3 = q10_3,
      mfq_care_1 = q49,
      mfq_care_2 = q50,
      mfq_care_3 = q51,
      mfq_care_4 = q52,
      mfq_care_5 = q53,
      mfq_care_6 = q54,
      mfq_equa_1 = q43,
      mfq_equa_2 = q44,
      mfq_equa_3 = q45,
      mfq_equa_4 = q46,
      mfq_equa_5 = q47,
      mfq_equa_6 = q48,
      mfq_prop_1 = q37,
      mfq_prop_2 = q38,
      mfq_prop_3 = q39,
      mfq_prop_4 = q40,
      mfq_prop_5 = q41,
      mfq_prop_6 = q42,
      mfq_loya_1 = q31,
      mfq_loya_2 = q32,
      mfq_loya_3 = q33,
      mfq_loya_4 = q34,
      mfq_loya_5 = q35,
      mfq_loya_6 = q36,
      mfq_auth_1 = q25,
      mfq_auth_2 = q26,
      mfq_auth_3 = q27,
      mfq_auth_4 = q28,
      mfq_auth_5 = q29,
      mfq_auth_6 = q30,
      mfq_puri_1 = q19,
      mfq_puri_2 = q20,
      mfq_puri_3 = q21,
      mfq_puri_4 = q22,
      mfq_puri_5 = q23,
      mfq_puri_6 = q24,
      sjb_1 = q56,
      sjb_2 = q57,
      sjb_3 = 8L - q58,
      sjb_4 = q59,
      sjb_5 = q60,
      sjb_6 = q61,
      sjb_7 = 8L - q62,
      sjb_8 = q63,
      pol = q68_1,
      ide_women = q69,
      ide_men = q70
    ) %>% 
    select(
      id,
      participant_ideology, participant_gender, protesters_cause,
      age,
      sup, mor_1:mor_3,
      starts_with("mfq_"),
      starts_with("sjb_"),
      pol,
      starts_with("ide_"),
      starts_with("attention_check"),
      exclude
    )
  
  # Add exclusion criterion to long data
  dl <- dw %>% 
    distinct(id, exclude) %>% 
    left_join(dl, ., by = "id")


# Export ------------------------------------------------------------------

  # Export as .rds
  write_rds(dl, "Experiment 3/data/dl.rds")
  write_rds(dw, "Experiment 3/data/dw.rds")
  write_rds(dp, "Experiment 3/data/dp.rds")

  # Export as .csv
  write_csv(dl, "Experiment 3/data/csv/dl.csv")
  write_csv(dw, "Experiment 3/data/csv/dw.csv")
  write_csv(dp, "Experiment 3/data/csv/dp.csv")
