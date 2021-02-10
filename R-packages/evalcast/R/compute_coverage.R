#' Compute observed coverage from a quantile forecaster
#'
#' @param score_card tibble containing at least columns actual, quantile, 
#'   value and any grouping or averaging variables named in the next arguments
#' @param grp_vars character vector of named columns in the score_card at which
#'   average performance will be returned
#' @param avg_vars character vector of named columns in the score_card over which
#'   averaging performance will be computed
#'   
#' @details Checks __are__ performed to ensure that averaging variables
#'   all contain the same confidence intervals though these may vary over
#'   grouping variables. 
#'   avg_vars and grp_vars must
#'   have an empty intersection.
#'
#' @return A tibble containing grp_vars, nominal_prob (the claimed interval
#'   coverage), prop_below (the proportion of actual values falling below
#'   the lower end of the confidence interval), prop_above (the proportion of
#'   actual values falling above the upper end of the confidence interval),
#'   and prop_covered (the proportion falling inside the interval). All 
#'   proportions are calculated for each available symmetric interval at each
#'   combination of grouping variables by averaging over any variables listed
#'   in avg_vars.
#' @export
#'
compute_coverage <- function(
  score_card, 
  grp_vars = c("forecaster", "forecast_date", "ahead"),
  avg_vars = c("geo_value")) {
  
  assert_that(all(c(grp_vars, avg_vars, "quantile", "value", "actual") %in%
                    names(score_card)),
              msg = paste("In compute_coverage:",
                          "grp_vars and avg_vars must be present in the score_card.",
                          "See details."))
  assert_that(is_empty(intersect(grp_vars, avg_vars)),
              msg = paste("In compute_coverage:",
                          "grp_vars and avg_vars must have empty intersection.",
                          "See details."))
  score_card <- intersect_averagers(score_card, grp_vars, avg_vars)
  score_card <- score_card %>% 
    filter(!is.na(.data$quantile)) %>%
    select(all_of(grp_vars), all_of(avg_vars),
           .data$quantile, .data$value, .data$actual) 
  
  score_card <- averaging_checks(score_card, grp_vars, avg_vars)
  
  score_card <- score_card %>% 
    group_by(across(all_of(c(grp_vars,avg_vars)))) %>%
    mutate(rev_value = rev(.data$value)) %>%
    filter(.data$quantile < 0.5) %>%
    mutate(is_below = .data$actual < .data$value,
           is_above = .data$actual > .data$rev_value,
           is_covered = !.data$is_below & !.data$is_above,
           nominal_prob = 1 - 2 * .data$quantile) %>%
    ungroup() %>%
    select(all_of(grp_vars), .data$is_above, .data$is_below, .data$is_covered,
           .data$nominal_prob) %>%
    group_by(across(all_of(grp_vars)), .data$nominal_prob) %>%
    summarise(prop_below = mean(.data$is_below, na.rm = TRUE),
              prop_above = mean(.data$is_above, na.rm = TRUE),
              prop_covered = mean(.data$is_covered, na.rm = TRUE))
  return(score_card)
}
