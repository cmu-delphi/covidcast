#' Compute weighted interval score
#'
#' Computes weighted interval score (WIS), a well-known quantile-based
#' approximation of the commonly-used continuous ranked probability score
#' (CRPS). WIS is a proper score, and can be thought of as a distributional
#' generalization of absolute error. For example, see [Bracher et
#' al. (2020)](https://arxiv.org/abs/2005.12881) for discussion in the context
#' of COVID-19 forecasting.
#' 
#' @param quantile vector of forecasted quantiles
#' @param value vector of forecasted values
#' @param actual_value Actual value.
#' 
#' @export
weighted_interval_score <- function(quantile, value, actual_value) {
  # computes the weighted interval score
  #
  # interval score: width + (2/alpha) * dist_from_interval
  #
  # weighted interval score:
  # (|y - median| + sum_k (alpha_k/2) * interval_score_k) / (num_intervals + 1)
  #
  # (|y - median| + sum_k [alpha_k*width_k/2 + dfi_k]) / (num_intervals + 1)
  # where dfi_k = dist_from_interval_k
  point_fcast <- is.na(quantile)
  quantile <- quantile[!point_fcast]
  value <- value[!point_fcast]
  if (all(is.na(actual_value))) return(NA)
  actual_value <- actual_value[1]
  num_prob <- length(quantile) # 23
  if (num_prob %% 2 != 1 || num_prob < 3){
    warning(paste("WIS calculation:",
                  "Forecaster must return at least 3 symmetric probabilities",
                  "for this calculation. Returning NA."))
    return(NA)
  }
  if (is.unsorted(quantile)) {
    warning(paste("WIS computation requires sorted quantiles to be in",
                  "increasing order. Returning NA."))
    return(NA)
  }
  num_intervals <- (num_prob - 1) / 2 # 11
  if (any(is.na(value)) || any(diff(value)<0)) {
    warning(paste("some predicted values may have quantile crossings or are",
                  "missing. Returning NA for WIS computation."))
    return(NA)
  }
  if (!all(abs(quantile + rev(quantile) - 1) < 1e-10)) {
    warning(paste("Predicted quantiles are not symmetric about 0.5.",
                  "Returning NA for WIS computation."))
    return(NA)
  }
  # note: I will treat the median as a 0% predictive interval
  # (alpha = 1) of width 0.  This is equivalent to the expression above
  lower <- value[1:(num_intervals + 1)]
  upper <- value[num_prob:(num_prob - num_intervals)]
  alpha <- 2 * quantile[1:(num_intervals + 1)]
  width <- upper - lower
  dist_from_interval <- pmax(actual_value - upper, lower - actual_value, 0)
  scaled_int_scores <- alpha * width / 2 + dist_from_interval
  mean(scaled_int_scores)
}

#' Compute absolute error
#'
#' Computes absolute error between the actual value and the median of the
#' forecast distribution.
#'
#' @param quantile vector of forecasted quantiles
#' @param value vector of forecasted values
#' @param actual_value Actual value.
#' 
#' @export
absolute_error <- function(quantile, value, actual_value) {
  point_fcast <- which(is.na(quantile))
  if (length(point_fcast) == 1L) {
    return(abs(value[point_fcast] - actual_value[point_fcast]))
  }
  point_fcast <- which(abs(quantile - 0.5) < 1e-10)
  if (length(point_fcast) == 1L) {
    return(abs(value[point_fcast] - actual_value[point_fcast]))
  }
  warning(paste("Absolute error: Forecaster must return either a point forecast",
                "with quantile == NA or a median with quantile == 0.5",
                "Returning NA."))
  return(NA)
}

#' Generate interval coverage error measure function
#'
#' Returns an error measure function indicating whether a central interval
#' covers the actual value. The interval is defined as the (alpha/2)-quantile
#' to the (1 - alpha/2)-quantile, where alpha = 1 - coverage.
#'
#' @param coverage Nominal interval coverage (from 0 to 1).
#'
#' @export
interval_coverage <- function(coverage) {
  function(quantiles, value, actual_value) {
    if(any(duplicated(quantiles))){
      warning(paste("Interval Coverage:",
                    "Quantiles must be unique.",
                    "Returning NA"))
      return(NA)
    }
    alpha = 1 - coverage
    lower_interval = alpha / 2
    upper_interval = 1 - (alpha / 2)
    if (all(abs(quantiles - lower_interval) > 1e-10) &
        all(abs(quantiles - upper_interval) > 1e-10)) {
      warning(paste("Interval Coverage:",
                    "Quantiles must cover an interval of specified width",
                    "centered at 0.5. Returning NA."))
      return(NA)
    }
    
    lower <- value[which.min(abs(quantiles - lower_interval))]
    upper <- value[which.min(abs(quantiles - upper_interval))]
    return(actual_value[1] >= lower & actual_value[1] <= upper)
  }
}
