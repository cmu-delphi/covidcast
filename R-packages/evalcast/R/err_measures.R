#' Compute weighted interval score
#'
#' For more details, see https://arxiv.org/abs/2005.12881
#'
#' Bracher, J., Ray, E. L., Gneiting, T., & Reich, N. G. (2020). Evaluating
#' epidemic forecasts in an interval format. arXiv preprint arXiv:2005.12881.
#'
#' @param quantile_forecasts the tibble of quantile forecasts (23 rows typically)
#' @param actual_value the actual value for median
#' @export
#' @importFrom rlang .data
#' @importFrom tibble tibble
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
  assert_that(num_prob %% 2 == 1 & num_prob >= 3,
              msg="Number of quantiles computed must be odd number 
                  greater than or equal to 3.")
  num_intervals <- (num_prob - 1) / 2 # 11
  q <- quantile_forecasts$quantiles
  probs <- quantile_forecasts$probs
  assert_that(all(abs(probs + rev(probs) - 1) < 1e-8), 
              msg="Quantile levels need to be symmetric around 0.5 (and include 0.5).")
  assert_that(diff(q) >= 0 | is.na(diff(q)),
              msg="Quantiles must be in increasing order.")
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
#' @param quantile_forecasts the quantile forecasts tibble
#' @param actual_value the actual value
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
#' This function returns an error measure function indicating
#' whether a central interval covers the actual value.  The interval
#' is defined as the (alpha / 2)-quantile to the (1 - alpha / 2)-quantile.
#'
#' @param alpha used to specify the nominal coverage of the interval
#' @export
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom dplyr filter pull
interval_coverage <- function(alpha) {
  function(quantile_forecasts, actual_value) {
    assert_that(any(abs(quantile_forecasts$probs - alpha / 2) < 1e-10) &
                any(abs(quantile_forecasts$probs - (1 - alpha / 2)) < 1e-10),
                msg="Forecaster must return values to cover a (1-alpha) interval
                     centered at 0.5.")
    lower <- quantile_forecasts %>%
      filter(abs(.data$probs - alpha / 2) < 1e-10) %>%
      pull(.data$quantiles)
    upper <- quantile_forecasts %>%
      filter(abs(.data$probs - (1 - alpha / 2)) < 1e-10) %>%
      pull(.data$quantiles)
    return(actual_value >= lower & actual_value <= upper)
  }
}
