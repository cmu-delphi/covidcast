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
  score_func_param_checker(quantile, value, actual_value, "weighted_interval_score")
  if (all(is.na(actual_value))) return(NA)
  # `score_func_param_checker` above has already checked for uniqueness, so we
  # can save a bit of effort and just take the first actual.
  actual_value <- actual_value[1]

  value <- value[!is.na(quantile)]
  quantile <- quantile[!is.na(quantile)]
  
  # per Ryan: WIS is equivalent to quantile loss modulo an extra 0.5 AE term
  # for the median forecast (counted twice). 
  # 
  # update: WIS is now being redefined to match exactly, still some question
  # about the correct denominator but the formula seems to be  1 / (K + 0.5)
  # 
  # Finally, the multiplication by 2 is because alpha_k = 2*quantile_k
  # 
  med <- value[find_quantile_match(quantile, 0.5)]
  
  if (length(med) > 1L) return(NA)
  
  wis <- 2 * mean(pmax(
    quantile * (actual_value - value),
    (1 - quantile) * (value - actual_value), 
    na.rm = TRUE))
  
  return(wis)
}

#' Compute absolute error
#'
#' Absolute error of a forecaster 
#' 
#' 
#' Intended to be used with `evaluate_predictions()`, it expects three arguments
#' of the same length, finds the location of the point forecast, and returns
#' the absolute error.
#'
#' @param quantile vector of forecasted quantiles
#' @param value vector of forecasted values
#' @param actual_value vector of actual values of the same length as 
#'   `quantile`/`value` or a scalar
#' 
#' @export
absolute_error <- function(quantile, value, actual_value) {
  score_func_param_checker(quantile, value, actual_value, "absolute_error")
  point_fcast <- which(is.na(quantile))
  ae <- abs(actual_value - value)
  if (length(point_fcast) == 1L) return(ae[point_fcast])
  point_fcast <- which(find_quantile_match(quantile, 0.5))
  if (length(point_fcast) == 1L) return(ae[point_fcast])
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
    score_func_param_checker(quantiles, value, actual_value, "interval_coverage")
    value = value[!is.na(quantiles)]
    quantiles = quantiles[!is.na(quantiles)]
    alpha = 1 - coverage
    lower_interval = alpha / 2
    upper_interval = 1 - (alpha / 2)
    if (!any(find_quantile_match(quantiles, lower_interval)) |
        !any(find_quantile_match(quantiles, upper_interval))) {
      warning(paste("Interval Coverage:",
                    "Quantiles must cover an interval of specified width",
                    "centered at 0.5. Returning NA."))
      return(NA)
    }
    
    lower <- value[which(find_quantile_match(quantiles, lower_interval))]
    upper <- value[which(find_quantile_match(quantiles, upper_interval))]
    return(actual_value[1] >= lower & actual_value[1] <= upper)
  }
}

#' Overprediction component of the weighted interval score
#' 
#' Requires symmetric quantile forecasts. Roughly, a penalty for predicted 
#' quantiles smaller than .5 falling above the observed value.
#'
#' @param quantile vector of forecasted quantiles
#' @param value vector of forecasted values
#' @param actual_value Actual value.
#' 
#' @export
overprediction <- function(quantile, value, actual_value) {
  score_func_param_checker(quantile, value, actual_value, "overprediction")
  if (!is_symmetric(quantile)) {
    warning(paste0("overprediction/underprediction/sharpness require",
                   "symmetric quantile forecasts. Using NA."))
    return(NA)
  }
  if (all(is.na(actual_value))) return(NA)
  # `score_func_param_checker` above has already checked for uniqueness, so we
  # can save a bit of effort and just take the first actual.
  actual_value <- actual_value[1]
  
  lower <- value[!is.na(quantile) & quantile < .5]
  med <- value[find_quantile_match(quantile, 0.5)]
  
  if (length(med) > 1L) return(NA)
  m <- ifelse(length(med == 1L),
              (med - actual_value) * (med > actual_value),
              NULL)
  
    
  
  ans <- mean(c(
    rep((lower - actual_value) * (lower > actual_value), 2), m))
    
  
  return(ans)
}


#' Underprediction component of the weighted interval score
#' 
#' Requires symmetric quantile forecasts. Roughly, a penalty for predicted 
#' quantiles larger than .5 falling under the observed value.
#'
#' @param quantile vector of forecasted quantiles
#' @param value vector of forecasted values
#' @param actual_value Actual value.
#' 
#' @export
underprediction <- function(quantile, value, actual_value) {
  score_func_param_checker(quantile, value, actual_value, "underprediction")
  if (!is_symmetric(quantile)) {
    warning(paste0("overprediction/underprediction/sharpness require",
                   "symmetric quantile forecasts. Using NA."))
    return(NA)
  }
  if (all(is.na(actual_value))) return(NA)
  # `score_func_param_checker` above has already checked for uniqueness, so we
  # can save a bit of effort and just take the first actual.
  actual_value <- actual_value[1]
  
  upper <- value[!is.na(quantile) & quantile > .5]
  med <- value[find_quantile_match(quantile, 0.5)]
  
  if (length(med) > 1L) return(NA)
  m <- ifelse(length(med == 1L),
              (actual_value - med) * (med < actual_value),
              NULL)
  ans <- mean(c(
    rep((actual_value - upper) * (upper < actual_value),2), m))
  
  return(ans)
}
#' Sharpness component of the weighted interval score
#' 
#' Requires symmetric quantile forecasts. Roughly, a penalty for the
#' width of predicted quantiles.
#'
#' @param quantile vector of forecasted quantiles
#' @param value vector of forecasted values
#' @param actual_value Actual value.
#' 
#' @export

sharpness <- function(quantile, value, actual_value) {
  weighted_interval_score(quantile, value, actual_value) - 
    overprediction(quantile, value, actual_value) - 
    underprediction(quantile, value, actual_value)
}


#' Common parameter checks for score functions
#'
#' A set of common checks for score functions, meant to identify common causes
#' of issues. Avoids `assert_that` for speed.
#'
#' @param quantiles vector of forecasted quantiles
#' @param values vector of forecasted values
#' @param actual_value actual_value, either as a scalar or a vector of the same 
#'   length as `quantiles` and `values`
#' @param id string to identify the caller of the function and displayed in
#'   error messages (recommended to be the parent function's name)
score_func_param_checker <- function(quantiles, values, actual_value, id = ""){
  id_str = paste0(id, ": ")
  if (length(actual_value) > 1) {
    if (length(actual_value) != length(values)) {
      stop(paste0(id_str,
                  "actual_value must be a scalar or the same length",
                  " as values"))
    }
    actual_value = unique(actual_value)
  }

  if (length(actual_value) != 1) {
    stop(paste0(id_str,
                "actual_value must have exactly 1 unique value"))
  }
  if (length(quantiles) != length(values)) {
    stop(paste0(id_str,
                "quantiles and values must be of the same length"))
  }

  if (anyDuplicated(quantiles)) {
    stop(paste0(id_str,
                "quantiles must be unique.")
    )
  }
}

is_symmetric <- function(x, tol=1e-8) {
  x <- sort(x)
  all(abs(x + rev(x) - 1) < tol)
}

find_quantile_match <- function(quantiles, val_to_match, tol=1e-8){
  return(abs(quantiles - val_to_match) < tol  & !is.na(quantiles))
}
