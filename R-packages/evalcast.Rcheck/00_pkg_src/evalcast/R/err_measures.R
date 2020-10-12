#' Compute weighted interval score
#'
#' For more details, see https://arxiv.org/abs/2005.12881
#'
#' Bracher, J., Ray, E. L., Gneiting, T., & Reich, N. G. (2020). Evaluating
#' epidemic forecasts in an interval format. arXiv preprint arXiv:2005.12881.
#'
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
  stopifnot(num_prob %% 2 == 1, num_prob >= 3)
  num_intervals <- (num_prob - 1) / 2 # 11
  q <- quantile_forecasts$quantiles
  probs <- quantile_forecasts$probs
  stopifnot(abs(probs + rev(probs) - 1) < 1e-8)
  stopifnot(diff(q) >= 0 | is.na(diff(q)))
  # note: I will treat the median as a 0% predictive interval
  # (alpha = 1) of width 0.  This is equivalent to the expression above
  int <- tibble(lower = q[1:(num_intervals + 1)],
                upper = q[num_prob:(num_prob - num_intervals)],
                alpha = 2 * probs[1:(num_intervals + 1)],
                width = upper - lower,
                dist_from_interval = pmax(actual_value - upper,
                                          lower - actual_value,
                                          0),
                scaled_int_scores = alpha * width / 2 + dist_from_interval)
  return(mean(int$scaled_int_scores))
}

#' Compute absolute error
#'
#' Computes absolute error between the actual value and the median of the
#' forecast distribution.
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
interval_coverage <- function(alpha) {
  function(quantile_forecasts, actual_value) {
    stopifnot(any(abs(quantile_forecasts$probs - alpha / 2) < 1e-10))
    stopifnot(any(abs(quantile_forecasts$probs - (1 - alpha / 2)) < 1e-10))
    lower <- quantile_forecasts %>%
      filter(abs(probs - alpha / 2) < 1e-10) %>%
      pull(quantiles)
    upper <- quantile_forecasts %>%
      filter(abs(probs - (1 - alpha / 2)) < 1e-10) %>%
      pull(quantiles)
    return(actual_value >= lower & actual_value <= upper)
  }
}
