#' Estimate derivatives of values in `covidcast_signal` data frame
#' 
#' Estimates derivatives of the values in a `covidcast_signal` data frame, using
#' a local (in time) linear regression or smoothing spline. (When multiple issue
#' dates are present, only the latest issue is considered.)  See the
#' [derivatives vignette]() for examples.
#'
#' @param df The `covidcast_signal` data frame under consideration. 
#' @param method One of "linear-reg" or "smooth-spline", indicating the method
#'   to use for the derivative calculation. To estimate the derivative at any
#'   time point, we run the given method on the last `n` days of data, and use
#'   the corresponding predicted derivative (that is, the derivative of the
#'   underlying estimated function, either linear or cubic spline) at the
#'   current time point.
#' @param n Size of the local window (in days) to use. For example, if `n = 5`,
#'   then to estimate the derivative on November 5, we train the given method on
#'   data in between November 1 and November 5. Default is 14.
#' @param new_col String indicating the name of the new column that will contain
#'   the derivative values. Default is "deriv"; note that we can set `new_col = 
#'   "value"` to overwrite the existing "value" column. 
#' @param deriv Order of derivative to estimate. For `method = "linear-reg"`,
#'   the order can only be 1. For `method = "smooth-spline"`, the order can be
#'   1 or 2.
#' @param params List of additional arguments to pass to `lsfit()` when `method
#'   = "linear-reg"`, or `smooth.spline()` when `method = "smooth-spline"`.  
#' 
#' @return A data frame given by appending a new column to `df` named according
#'   to the `new_col` argument, containing the estimated derivative values.
#'
#' @importFrom dplyr %>% group_by
#' @importFrom lubridate days
#' @importFrom slider slide_index_dbl
#' @export
estimate_deriv = function(df, method = c("linear-reg", "smooth-spline"), n = 14, 
                          new_col = "deriv", deriv = 1, params = list()) {
  # Check we have the minimal columns we need
  if (!all(c("geo_value", "time_value", "value") %in% colnames(df))) {
    stop("`df` must have columns 'geo_value', 'time_value', and 'value'.")
  }
  # df = covidcast::latest_issue(df) # TODO we need to export this function

  # Define the slider function
  method = match.arg(method)
  slider_fun = switch(method,
                      "linear-reg" = linear_reg_deriv,
                      "smooth-spline" = smooth_spline_deriv)
  
  # Slide the slider function and return
  params = c(params, deriv = deriv)
  return(df %>%
         group_by(geo_value) %>%
         arrange(time_value) %>%
         mutate(!!new_col := slide_index_dbl(
                    .x = dplyr::tibble(x = as.numeric(time_value),
                                       y = value), 
                    .i = time_value,
                    .f = slider_fun, params,
                    .before = days(n-1))) %>%
         ungroup())
}

linear_reg_deriv = function(data, params) {
  params$deriv = NULL
  data = na.omit(data) 
  params$x = data$x
  params$y = data$y
  
  return(tryCatch(suppressWarnings(suppressMessages(
    End(coef(do.call(stats::lsfit, params))))), 
    error = function(e) return(NA)))
}

smooth_spline_deriv = function(data, params) {
  deriv = params$deriv
  params$deriv = NULL
  data = na.omit(data) 
  params$x = data$x
  params$y = data$y
  
  return(tryCatch(suppressWarnings(suppressMessages(
    stats::predict(
             object = do.call(stats::smooth.spline, params),
             x = End(params$x), deriv = deriv)$y)),
    error = function(e) return(NA)))
}

Start = function(x) head(x, 1)
End = function(x) tail(x, 1)

quiet = function(x) { 
  sink(tempfile()) 
  on.exit(sink()) 
  invisible(force(x)) 
}

