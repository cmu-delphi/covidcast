#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom purrr map_dfr map_dbl
#' @importFrom dplyr group_by summarize mutate select
compute_actual_vs_nominal_prob <- function(score_card) {
  nominal_probs <- score_card$forecast_distribution[[1]]$probs
  as.list(1:length(nominal_probs)) %>%
    map_dfr(function(i) {
      score_card %>%
        group_by(.data$forecast_date) %>%
        summarize(prop_below = mean(.data$actual < map_dbl(.data$forecast_distribution,
                                                           ~ .x$quantiles[i]),
                                    na.rm = TRUE))
    }, .id = "nominal_id") %>%
    mutate(nominal_prob = nominal_probs[as.integer(.data$nominal_id)]) %>%
    select(-.data$nominal_id)
}

#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom purrr map_dfr map_dbl
#' @importFrom dplyr group_by summarize mutate select
#' @importFrom assertthat assert_that
compute_coverage <- function(score_card) {
  assert_that(nrow(score_card) != 0,
              msg="Can't compute coverage for an empty score_card.")
  probs <- score_card$forecast_distribution[[1]]$probs
  assert_that(length(probs) %% 2 == 1 && all(abs(probs + rev(probs) - 1) < 1e-3),
              msg="Quantile levels need to be symmetric around 0.5 (and include 0.5).")
  num_intervals <- (length(probs) - 1) / 2
  nominal_coverage_probs <- 1 - 2 * probs[1:num_intervals]
  as.list(1:num_intervals) %>%
    map_dfr(function(i) {
      itop <- length(probs) - i + 1
      score_card %>%
        mutate(is_below = .data$actual < map_dbl(.data$forecast_distribution, ~ .x$quantiles[i]),
               is_above = .data$actual > map_dbl(.data$forecast_distribution, ~ .x$quantiles[itop]),
               is_covered = !.data$is_below & !.data$is_above) %>%
        group_by(.data$forecast_date) %>%
        summarize(prop_below = mean(.data$is_below, na.rm = TRUE),
                  prop_above = mean(.data$is_above, na.rm = TRUE),
                  prop_covered = mean(.data$is_covered, na.rm = TRUE))
    }, .id = "nominal_id") %>%
    mutate(nominal_coverage_prob = nominal_coverage_probs[as.integer(.data$nominal_id)]) %>%
    select(-.data$nominal_id)

}

#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom purrr map_dfr map_dbl
#' @importFrom dplyr group_by summarize mutate select
#' @importFrom assertthat assert_that
compute_calibration <- function(score_card) {
  probs <- score_card$forecast_distribution[[1]]$probs
  assert_that(length(probs) %% 2 == 1 && all(abs(probs + rev(probs) - 1) < 1e-3),
              msg="Quantile levels need to be symmetric around 0.5 (and include 0.5).")
  as.list(1:length(probs)) %>%
    map_dfr(function(i) {
      # itop <- length(probs) - i + 1
      score_card %>%
        mutate(is_below = .data$actual < map_dbl(.data$forecast_distribution, ~ .x$quantiles[i]),
               is_above = .data$actual > map_dbl(.data$forecast_distribution, ~ .x$quantiles[i])) %>%
        group_by(.data$forecast_date) %>%
        summarize(prop_below = mean(.data$is_below, na.rm = TRUE),
                  prop_above = mean(.data$is_above, na.rm = TRUE))
    }, .id = "nominal_id") %>%
    mutate(nominal_quantile = probs[as.integer(.data$nominal_id)]) %>%
    select(-.data$nominal_id)
}
