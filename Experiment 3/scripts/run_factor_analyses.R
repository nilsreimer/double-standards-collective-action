# Load packages -----------------------------------------------------------
library(lavaan)

# System-Justifying Beliefs (SJB) -----------------------------------------

  # Load model
  sjb_model <- '
        sjb =~ sjb_1 + sjb_2 + sjb_3 + sjb_4 + sjb_5 + sjb_6 + sjb_7 + sjb_8 
      '
  
  # Run models
  sjb_fit <- cfa(
    sjb_model,
    estimator = "mlr",
    missing = "fiml",
    std.lv = TRUE,
    data = dw
  )
  
  # Export factor scores
  dw <- predict(sjb_fit) %>%
    as.data.frame() %>%
    bind_cols(dw, .)
  
  

# Moral Foundations Questionnaire (MFQ-2) ---------------------------------

  # Load model
  mfq_model <- '
        mfq_care =~ mfq_care_1 + mfq_care_2 + mfq_care_3 + mfq_care_4 + mfq_care_5 + mfq_care_6 
        mfq_equa =~ mfq_equa_1 + mfq_equa_2 + mfq_equa_3 + mfq_equa_4 + mfq_equa_5 + mfq_equa_6 
        mfq_prop =~ mfq_prop_1 + mfq_prop_2 + mfq_prop_3 + mfq_prop_4 + mfq_prop_5 + mfq_prop_6 
        mfq_loya =~ mfq_loya_1 + mfq_loya_2 + mfq_loya_3 + mfq_loya_4 + mfq_loya_5 + mfq_loya_6 
        mfq_auth =~ mfq_auth_1 + mfq_auth_2 + mfq_auth_3 + mfq_auth_4 + mfq_auth_5 + mfq_auth_6 
        mfq_puri =~ mfq_puri_1 + mfq_puri_2 + mfq_puri_3 + mfq_puri_4 + mfq_puri_5 + mfq_puri_6 
      '
  
  # Run models
  mfq_fit <- cfa(
    mfq_model,
    estimator = "mlr",
    missing = "fiml",
    std.lv = TRUE,
    data = dw
  )
  
  # Export factor scores
  dw <- predict(mfq_fit) %>%
    as.data.frame() %>%
    bind_cols(dw, .)


# Remove ------------------------------------------------------------------
detach(package:lavaan)
rm("sjb_model", "sjb_fit", "mfq_model", "mfq_fit")
