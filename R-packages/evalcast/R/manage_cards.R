#' Remove all quantile forecasts
#'
#' @param cards either predictions_cards or scorecards
#'
#' @return cards of the same class but with only one row for each 
#'   geo_value/forecast_date/ahead/forecaster (the point estimate)
#'   
#' @export
collapse_cards <- function(cards){
  cls <- class(cards)[1]
  assert_that(cls %in% c("predictions_cards", "score_cards"),
              msg=paste("This function is only appropriate for",
                        "predictions_cards or score_cards classes."))
  cards <- cards %>%
    filter(abs(.data$quantile - 0.5) < 1e-8 | is.na(.data$quantile)) %>%
    mutate(quantile = ifelse(is.na(.data$quantile), "p","m"))
  if (n_distinct(cards$quantile) == 1) {
    cards <- cards %>%
      mutate(quantile = ifelse(.data$quantile == "p", NA, 0.5))
  } else {
    cards <- cards %>%
      pivot_wider(names_from = .data$quantile, values_from = .data$value) %>%
      mutate(quantile = ifelse(is.na(.data$p), 0.5, NA),
             value = ifelse(is.na(.data$p), .data$m, .data$p)) %>%
      select(-.data$p, -.data$m)
  }
  if ("geo_value" %in% colnames(cards)) {
    cards <- cards %>%
      relocate(.data$quantile:.data$value, .after = .data$geo_value)
  }
  class(cards) = c(cls, class(cards))
  cards
}


unique_for_ahead <- function(cards, attribute) {
  # ensure that some attribute column contains a unique value for any aheads
  att <- select(cards, !!attribute) %>% distinct()
  ahead <- cards$ahead[1]
  assert_that(nrow(att) == 1,
              msg = sprintf(
                "These cards do not all have the same %s for ahead %i.",
                attribute, ahead))
  invisible(pull(att))
}


#' Find common averaging "locations"
#' 
#' Many scoring or plotting functions compute averages over "locations"
#' for a number of different grouping facets. We say "locations" because this
#' most often the geo_value that gets averaged over, while the groupings are
#' the forecaster, forecast horizon, and forecast date. But under other 
#' combinations may be desired.
#' 
#' In the case that we want to make comparisons, we want the avg_vars to be
#' common. This function finds common avg_vars. An example would be if one
#' forecaster makes predictions for a location that others don't, we would
#' want to throw it when ever we compute error measures. 
#'
#' @param cards long data frame 
#' @param grp_vars character vector of indicating variables to group on
#' @param avg_vars character vector of variables to average over
#'
#' @return
#' @export
intersect_averagers <- function(cards, grp_vars, avg_vars) {
  
  assert_that(length(intersect(grp_vars, avg_vars)) == 0L,
              msg = paste("In intersect_averagers: grp_vars and avg_vars",
                          "must not overlap"))
  distinct_averagers <- cards %>%
    select(all_of(avg_vars)) %>%
    distinct()
  
  averagers_intersected <- cards %>% 
    select(all_of(c(grp_vars, avg_vars))) %>%
    group_by(across(all_of(grp_vars))) %>%
    group_split(.keep = FALSE) %>% 
    lapply(distinct) %>%
    reduce(intersect)
  
  assert_that(nrow(averagers_intersected) > 1L,
              msg = paste("In intersect_averagers: there was at most one",
                          "common row to average over."))

  if (nrow(distinct_averagers) > nrow(averagers_intersected)) {
      message(paste("In intersect_averagers: Some avg_vars were not common",
                    "to all grp_vars. Only the intersection is used for",
                    "averaging."))
  }
  cards %>% right_join(averagers_intersected)
}

#' Scale error measures based on those of a particular forecaster.
#'
#' @param score_card score_card like that returned by `evaluate_predictions()`
#' @param vars vector of column names in `score_card` to normalize
#' @param denom_forecaster name of forecaster in `score_card$forecaster` column by whose error
#'   values the remaining forecasters' errors will be scaled
#' @param err_cols vector of column names in `score_card` that contain error measures.
#'   Elements of `err_cols` that are not in `vars` will be dropped from the final output to
#'   avoid a mix of scaled and unscaled measures.
#'
#' @return
#' @export
scale_by_forecaster <- function(score_card, vars, denom_forecaster, 
                                err_cols = c("ae", "wis")) {
  df_list <- map(vars, function(var) {
    score_card %>% 
      select(setdiff(names(score_card), setdiff(err_cols, var))) %>% 
      pivot_wider(names_from = "forecaster", 
                  names_prefix = var, 
                  values_from = var) %>% 
      mutate(across(starts_with(var), ~ .x /
                      !!sym(paste0(var, denom_forecaster)))) %>%
      pivot_longer(cols = starts_with(var), 
                   names_to = "forecaster",
                   values_to = var) %>%
      mutate(forecaster = substring(forecaster, nchar(var) + 1)) %>%
      filter(forecaster != denom_forecaster)
  })
  return(reduce(df_list, left_join))
}
