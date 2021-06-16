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
