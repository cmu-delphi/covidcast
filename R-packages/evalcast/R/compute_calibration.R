#' Compute calibration of a quantile forecaster
#'
#' @param predictions_cards tibble containing at least columns actual, quantile, 
#'   value and any grouping or averaging variables named in the next arguments
#' @template geo_type-template
#' @param grp_vars character vector of named columns in the score_card at which
#'   average performance will be returned
#' @param avg_vars character vector of named columns in the score_card over which
#'   averaging performance will be computed
#'   
#' @details Note that no checks are performed to ensure that averaging variables
#'   all contain the same predicted quantiles. avg_vars and grp_vars must
#'   have an empty intersection.
#'
#' @return A tibble containing grp_vars, nominal_prob (the claimed interval
#'   coverage), prop_below (the proportion of actual values falling below
#'   the forecast quantile) and prop_above (the proportion of
#'   actual values falling above the forecast quantile). All 
#'   proportions are calculated for each available probability at each
#'   combination of grouping variables by averaging over any variables listed
#'   in avg_vars.
#' @export
#'
compute_calibration <- function(
  predictions_cards,
  geo_type,
  grp_vars = c("forecaster", "forecast_date", "ahead"),
  avg_vars = c("geo_value")) {
  
  score_card <- left_join(predictions_cards,
                          get_covidcast_data(predictions_cards,
                                             backfill_buffer,
                                             geo_type),
                          by = c("geo_value",
                                 "forecast_date",
                                 "ahead"))
  
  assert_that(all(c(grp_vars, avg_vars, "quantile", "value", "actual") %in%
                    names(score_card)),
              msg = paste("In compute_actual_vs_nominal_prob:",
                          "grp_vars must be present in the score_card.",
                          "See details."))
  assert_that(is_empty(intersect(grp_vars, avg_vars)),
              msg = paste("In compute_actual_vs_nominal_prob:",
                          "grp_vars and avg_vars must have empty intersection.",
                          "See details."))
  score_card <- intersect_averagers(score_card, grp_vars, avg_vars)
  score_card <- filter(score_card, !is.na(.data$quantile)) %>%
    select(all_of(grp_vars), all_of(avg_vars),
           .data$actual, .data$quantile, .data$value)
  
  score_card <- averaging_checks(score_card, grp_vars, avg_vars)
  
  score_card <- score_card %>% 
    mutate(below = .data$actual < .data$value,
           above = .data$actual > .data$value) %>%
    group_by(across(all_of(grp_vars)), .data$quantile) %>%
    summarise(prop_below = mean(.data$below, na.rm = TRUE),
              prop_above = mean(.data$above, na.rm = TRUE)) %>%
    rename(nominal_prob = .data$quantile)
  return(score_card)
}

#' @inherit compute_calibration
#' @export
compute_actual_vs_nominal_prob <- compute_calibration
