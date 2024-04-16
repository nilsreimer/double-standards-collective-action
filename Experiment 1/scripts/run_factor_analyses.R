# Load packages -----------------------------------------------------------
  library(lavaan)


# Social Dominance Orientation (SDO) --------------------------------------
  
  # Load model
  sdo_model <- '
    sdo =~ sdo_1 + sdo_2 + sdo_3 + sdo_4 + sdo_5 + sdo_6 + sdo_7 + sdo_8
  '

  # Run model
  sdo_fit <- cfa(
    sdo_model, 
    estimator = "mlr", 
    missing = "fiml",
    std.lv = TRUE,
    data = dw
  )

  # Export factor scores
  dw <- predict(sdo_fit) %>%
    as.data.frame() %>%
    bind_cols(dw, .)

  
# System-Justifying Beliefs (SJB) -----------------------------------------

  # Load models
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
  rm("sdo_model", "sdo_fit", "sjb_model", "sjb_fit")
  