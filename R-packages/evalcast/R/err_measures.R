#' Compute weighted interval score
#'
#' Computes weighted interval score (WIS), a well-known quantile-based
#' approximation of the commonly-used continuous ranked probability score
#' (CRPS). WIS is a proper score, and can be thought of as a distributional
#' generalization of absolute error. For example, see [Bracher et
#' al. (2020)](https://arxiv.org/abs/2005.12881) for discussion in the context
#' of COVID-19 forecasting.
#' 
#' @param quantile_forecasts Tibble of quantile forecasts.
#' @param actual_value Actual value.
#' 
#' @importFrom rlang .data
#' @importFrom tibble tibble
#' @importFrom assertthat assert_that
#' @export
weighted_interval_score <- function(quantile_forecasts, actual_value) {
  # computes the weighted interval score
  #
  # interval score: width + (2/alpha) * dist_from_interval
  #
  # weighted interval score:
  # (|y - median| + sum_k (alpha_k/2) * interval_score_k) / (num_intervals + 1)
  #
  # (|y - median| + sum_k [alpha_k*width_k/2 + dfi_k]) / (num_intervals + 1)
  # where dfi_k = dist_from_interval_k
  if (is.na(actual_value)) return(NA)
  num_prob <- nrow(quantile_forecasts) # 23
  assert_that(num_prob %% 2 == 1 && num_prob >= 3,
              msg=paste("Number of predicted quantiles must be an odd number",
                        "greater than or equal to 3."))
  num_intervals <- (num_prob - 1) / 2 # 11
  q <- quantile_forecasts$quantiles
  assert_that(all(!is.na(q)), msg="Quantiles cannot be NA")
  assert_that(all(diff(q) >= 0), msg="Quantiles must be in increasing order.")
  probs <- quantile_forecasts$probs
  assert_that(all(abs(probs + rev(probs) - 1) < 1e-10),
              msg="Quantile levels must be symmetric around 0.5 (and include 0.5).")
  # note: I will treat the median as a 0% predictive interval
  # (alpha = 1) of width 0.  This is equivalent to the expression above
  int <- tibble::tibble(lower = q[1:(num_intervals + 1)],
                        upper = q[num_prob:(num_prob - num_intervals)],
                        alpha = 2 * probs[1:(num_intervals + 1)],
                        width = .data$upper - .data$lower,
                        dist_from_interval = pmax(actual_value - .data$upper,
                                                  .data$lower - actual_value,
                                                  0),
                        scaled_int_scores = .data$alpha * .data$width / 2 + .data$dist_from_interval)
  return(mean(int$scaled_int_scores))
}

#' Compute absolute error
#'
#' Computes absolute error between the actual value and the median of the
#' forecast distribution.
#'
#' @param quantile_forecasts Tibble of quantile forecasts.
#' @param actual_value Actual value.
#' 
#' @importFrom magrittr %>%
#' @importFrom dplyr filter transmute pull
#' @importFrom rlang .data
#' @export
absolute_error <- function(quantile_forecasts, actual_value) {
  quantile_forecasts %>%
      dplyr::filter(.data$probs == 0.5) %>%
      dplyr::transmute(err = abs(.data$quantiles - actual_value)) %>%
      dplyr::pull(.data$err)
}

#' Generate interval coverage error measure function
#'
#' Returns an error measure function indicating whether a central interval
#' covers the actual value. The interval is defined as the (alpha/2)-quantile
#' to the (1 - alpha/2)-quantile.
#'
#' @param alpha Parameter defining nominal interval coverage.
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom dplyr filter pull
#' @importFrom assertthat assert_that
#' @export
interval_coverage <- function(alpha) {
  function(quantile_forecasts, actual_value) {
    assert_that(any(abs(quantile_forecasts$probs - alpha / 2) < 1e-10) &&
                any(abs(quantile_forecasts$probs - (1 - alpha / 2)) < 1e-10),
                msg=paste("Forecaster must return values to cover a (1-alpha)",
                          "interval centered at 0.5."))
    lower <- quantile_forecasts %>%
      filter(abs(.data$probs - alpha / 2) < 1e-10) %>%
      pull(.data$quantiles)
    upper <- quantile_forecasts %>%
      filter(abs(.data$probs - (1 - alpha / 2)) < 1e-10) %>%
      pull(.data$quantiles)
    return(actual_value >= lower & actual_value <= upper)
  }
}
