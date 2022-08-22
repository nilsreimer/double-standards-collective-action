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


# Remove ------------------------------------------------------------------
detach(package:lavaan)
rm("sjb_model", "sjb_fit")