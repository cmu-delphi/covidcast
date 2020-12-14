

check_valid_coverage_probs <- function(qq) {
  u_q <- unique(qq)
  if (length(qq) %% length(u_q) != 0 ||
      sum(abs(u_q - qq)) > 1e-8 ||
      length(qq) %% 2 != 1 ||
      any(abs(u_q + rev(u_q) - 1) > 1e-3)) return(FALSE)
  TRUE
}

averaging_checks <- function(score_card, grp_vars, avg_vars){
  averaging_checks <- score_card %>% 
    group_by(across(all_of(grp_vars)), across(all_of(avg_vars))) %>%
    summarise(valid = check_valid_coverage_probs(.data$quantile))
  assert_that(any(averaging_checks$valid),
              msg = paste(
                "no groupings have valid quantile forecasts.",
                "quantiles may be different, be unsorted",
                "be asymmetric, not include 0.5, etc.",
                "Check your forecaster output."))
  if (!all(averaging_checks$valid)) {
    warning(paste(
      "some grouping/averaging pairs have invalid quantile forecasts.",
      "quantiles may be different, be unsorted",
      "be asymmetric, not include 0.5, etc.",
      "These combinations were dropped.",
      "Check your forecaster output."))
    averaging_checks <- filter(averaging_checks, .data$valid) %>% 
      select(-.data$valid)
    score_card <- left_join(averaging_checks, score_card, 
                            by = c(grp_vars, avg_vars))
  }
  score_card
}



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
  
  score_card %>% 
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
}

#' Compute calibration of a quantile forecaster
#'
#' @param score_card tibble containing at least columns actual, quantile, 
#'   value and any grouping or averaging variables named in the next arguments
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
  score_card, 
  grp_vars = c("forecaster", "forecast_date", "ahead"),
  avg_vars = c("geo_value")) {
  
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
  score_card
}

#' @inherit compute_calibration
#' @export
compute_actual_vs_nominal_prob <- compute_calibration
