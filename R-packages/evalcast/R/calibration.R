

check_valid_coverage_probs <- function(df, avg_vars) {
  # The goal is to ensure that for a particular group, the quantiles at which
  # forecasts are made are the same and reasonable
  # Input is a grouped data frame, with a column of "locations" that all need to
  # comply. Rather than trying to locate some minimal set of "locations" that
  # work, if there's a problem, we toss the whole group. The reason being,
  # this likely reflects an error in the forecaster or the setup rather than
  # something we should work around.
  df <- df %>% filter(!is.na(.data$quantile)) # dump out any point forecasts
  uq <- sort(unique(df$quantile))
  n_uq <- length(uq)
  # first, we check the whole group
  valid <- n_uq %% 2 == 1 && is_symmetric(uq) # has median, and is symmetric
  if (!valid) return(FALSE)
  # now we check each "location"
  df <- df %>% group_by(across(all_of(avg_vars))) %>%
    summarise(valid = length(.data$quantile) == n_uq && # number of q's
                # sorted + commensurate
                all(find_quantile_match(.data$quantile, uq))) %>%
    ungroup() %>%
    summarise(valid = all(valid))
  return(df$valid)
}

#' Check if quantile forecasts are commensurate across groups
#'
#' @param cards a data frame containing at least the columns named
#'   in `grp_vars`, `avg_vars` and a column called `quantile`
#' @param grp_vars character vector of named columns in the score_card at which
#'   average performance will be returned
#' @param avg_vars character vector of named columns in the score_card over which
#'   averages will be computed
#' @param return_checks useful for debugging. returns a data frame indicating
#'   which groups have valid or invalid avg_vars
#'
#' @return filtered cards based on which groups are completely valid 
#'   (meaning "correct" quantiles are present for all `avg_vars` in the group)
#' @export
averaging_checks <- function(
  cards, 
  grp_vars = c("forecaster", "forecast_date", "ahead"),
  avg_vars = c("geo_value"),
  return_checks = FALSE) {
    
  gr <- cards %>% 
    group_by(across(all_of(grp_vars)))
  checks <- gr %>% group_keys()
  checks$valid <- sapply(gr %>% group_split(), 
                         function(x) check_valid_coverage_probs(x, avg_vars))
  assert_that(any(checks$valid),
              msg = paste(
                "no groupings have valid quantile forecasts.",
                "quantiles may be different, be unsorted",
                "be asymmetric, not include 0.5, etc.",
                "Check your forecaster output."))
  if (!all(checks$valid)) {
    warning(paste(
      "some grouping/averaging pairs have invalid quantile forecasts.",
      "quantiles may be different, be unsorted",
      "be asymmetric, not include 0.5, etc.",
      "These combinations were dropped.",
      "Check your forecaster output."))
    if (return_checks) return(checks)
    checks <- filter(checks, .data$valid) %>% 
      select(-.data$valid)
    cards <- left_join(checks, cards, by = grp_vars)
  }
  return(cards)
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
  return(score_card)
}

#' @inherit compute_calibration
#' @export
compute_actual_vs_nominal_prob <- compute_calibration
