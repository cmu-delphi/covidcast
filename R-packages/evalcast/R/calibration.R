
compute_actual_vs_nominal_prob <- function(score_card) {
  nominal_probs <- score_card$forecast_distribution[[1]]$probs
  as.list(1:length(nominal_probs)) %>%
    map_dfr(function(i) {
      score_card %>%
        group_by(forecast_date) %>%
        summarize(prop_below = mean(actual < map_dbl(forecast_distribution,
                                                     ~ .x$quantiles[i]),
                                    na.rm = TRUE))
    }, .id = "nominal_id") %>%
    mutate(nominal_prob = nominal_probs[as.integer(nominal_id)]) %>%
    select(-nominal_id)
}

compute_coverage <- function(score_card) {
  if (nrow(score_card) == 0)
    stop("Can't compute coverage for an empty score_card.")
  probs <- score_card$forecast_distribution[[1]]$probs
  stopifnot(length(probs) %% 2 == 1,
            abs(probs + rev(probs) - 1) < 1e-3)
  num_intervals <- (length(probs) - 1) / 2
  nominal_coverage_probs <- 1 - 2 * probs[1:num_intervals]
  as.list(1:num_intervals) %>%
    map_dfr(function(i) {
      itop <- length(probs) - i + 1
      score_card %>%
        mutate(is_below = actual < map_dbl(forecast_distribution, ~ .x$quantiles[i]),
               is_above = actual > map_dbl(forecast_distribution, ~ .x$quantiles[itop]),
               is_covered = !is_below & !is_above) %>%
        group_by(forecast_date) %>%
        summarize(prop_below = mean(is_below, na.rm = TRUE),
                  prop_above = mean(is_above, na.rm = TRUE),
                  prop_covered = mean(is_covered, na.rm = TRUE))
    }, .id = "nominal_id") %>%
    mutate(nominal_coverage_prob = nominal_coverage_probs[as.integer(nominal_id)]) %>%
    select(-nominal_id)
}

compute_calibration <- function(score_card) {
  probs <- score_card$forecast_distribution[[1]]$probs
  stopifnot(length(probs) %% 2 == 1,
            abs(probs + rev(probs) - 1) < 1e-3)
  as.list(1:length(probs)) %>%
    map_dfr(function(i) {
      # itop <- length(probs) - i + 1
      score_card %>%
        mutate(is_below = actual < map_dbl(forecast_distribution, ~ .x$quantiles[i]),
               is_above = actual > map_dbl(forecast_distribution, ~ .x$quantiles[i])) %>%
        group_by(forecast_date) %>%
        summarize(prop_below = mean(is_below, na.rm = TRUE),
                  prop_above = mean(is_above, na.rm = TRUE))
    }, .id = "nominal_id") %>%
    mutate(nominal_quantile = probs[as.integer(nominal_id)]) %>%
    select(-nominal_id)
}
