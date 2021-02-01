
geo_type_selector <- function(geo_type, predictions_cards) {
  # tries to determine which geo_type we want and/or validate
  # our selection based on the content of predictions_cards
  # 
  # Wraps the function below for ease of testing (we test the validator,
  # rather than this function)
  pred_card_types <- predictions_cards %>%
    transmute(type_len = nchar(.data$geo_value),
              has_chars = str_detect(.data$geo_value, "[aA-zZ]")) %>%
    distinct()
  geo_type <- match.arg(geo_type, c("county", "hrr", "msa", "dma", "state"))
  validate_geo_type(geo_type, pred_card_types)
}

validate_geo_type <- function(geo_type, pred_card_types) {
  if (nrow(pred_card_types) > 1L) {
    assert_that(geo_type %in% c("hrr", "msa", "dma"),
                msg = paste("predictions_cards",
                            "contain multiple geo_types."))
  }
  if (any(pred_card_types$has_chars)) {
    assert_that(geo_type %in% c("state","county"),
                msg = paste("predictions_cards contain non-numeric characters",
                            "but geo_type was not `state`"))
  }
  if (geo_type == "state") {
    if (pred_card_types$type_len == 5L && !pred_card_types$has_chars) {
      geo_type = "county" # switch this if it's ok
    } else {
      assert_that(pred_card_types$has_chars, # actually is state
                  pred_card_types$type_len == 2L,
                  msg = paste("input was geo_type = ",
                              "state, but predictions_cards do not entirely",
                              "conform to this selection."))
    }
  }
  if (geo_type == "county") {
    if (pred_card_types$type_len == 2L && pred_card_types$has_chars) {
      geo_type = "state" # switch this automatically
    } else {
      assert_that(!pred_card_types$has_chars, # actually is state
                  pred_card_types$type_len == 5L,
                  msg = paste("input was geo_type = ",
                              "county, but predictions_cards do not entirely",
                              "conform to this selection."))
    }
  }
  return(geo_type)
}