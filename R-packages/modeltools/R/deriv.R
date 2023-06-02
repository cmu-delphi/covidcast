#' Estimate derivatives of values in `covidcast_signal` data frame
#' 
#' Estimates derivatives of the values in a `covidcast_signal` data frame, using
#' a local (in time) linear regression or smoothing spline. (When multiple issue
#' dates are present, only the latest issue is considered.)  See the
#' [estimating derivatives
#' vignette](https://cmu-delphi.github.io/covidcast/modeltoolsR/articles/estimate-deriv.html)
#' for examples.  
#'
#' @param x The `covidcast_signal` data frame under consideration. 
#' @param method One of "lin", "ss", or "tf" indicating the method to use for
#'   the derivative calculation. To estimate the derivative at any time point,
#'   we run the given method on the last `n` days of data, and use the
#'   corresponding predicted derivative (that is, the derivative of the
#'   underlying estimated function, linear or spline) at the current time
#'   point. Default is "lin". See details below.
#' @param n Size of the local window (in days) to use. For example, if `n = 5`,
#'   then to estimate the derivative on November 5, we train the given method on
#'   data in between November 1 and November 5. Default is 14.
#' @param col_name String indicating the name of the new column that will
#'   contain the derivative values. Default is "deriv"; note that setting
#'   `col_name = "value"` will overwrite the existing "value" column.
#' @param keep_obj Should the fitted object (from linear regression, smoothing 
#'   spline, or trend filtering) be kept as a separate column? If `TRUE`, then
#'   this column name is given by  appending "_obj" to `col_name`. Default is
#'   `FALSE`.  
#' @param deriv Order of derivative to estimate. Only orders 1 or 2 are allowed,
#'   with the default being 1. (In some cases, a second-order derivative will
#'   return a trivial result: for example: when `method = "lin"`, this will
#'   always be zero.) 
#' @param ... Additional arguments to pass to the function that estimates
#'   derivatives. See details below.    
#'
#' @details Derivatives are estimated using:
#'
#' \itemize{
#' \item Linear regression, when `method = "lin"`, via `stats::lsfit()`. 
#' \item Cubic smoothing spline, when `method = "ss"`, via
#'   `stats::smooth.spline()`.
#' \item Polynomial trend filtering, when `method = "tf"`, via
#'   `genlasso::trendfilter()`.
#' }
#'
#' The second and third cases base the derivative calculation on a nonparametric
#'   fit and should typically be used with a larger window `n`. The third case
#'   (trend filtering) is more locally adaptive than the second (smoothing
#'   spline) and can work better when there are sharp changes in the smoothness
#'   of the underlying values.
#' 
#' In the first and second cases (linear regression and smoothing spline), the
#'   additional arguments in `...` are directly passed to the underlying 
#'   estimation function (`stats::lsfit()` and `stats::smooth.spline()`).   
#'
#' The third case (trend filtering) works a little differently. Here, a custom 
#'   set of arguments is allowed (and are internally distributed as appropriate
#'   to `genlasso::trendfilter()`, `genlasso::cv.trendfilter()`, and
#'   `genlasso::coef.genlasso()`): 
#'
#' \describe{
#' \item{`ord`}{Order of piecewise polynomial for the trend filtering fit,
#'   default is 2.}
#' \item{`maxsteps`}{Maximum number of steps to take in the solution path before
#'   terminating, default is 100.} 
#' \item{`cv`}{Boolean indicating whether cross-validation should be used to
#'   choose an effective degrees of freedom for the fit, default is `FALSE`.}
#' \item{`k`}{Number of folds if cross-validation is to be used. Default is 5.} 
#' \item{`df`}{Desired effective degrees of freedom for the trend filtering
#'   fit. If `cv = FALSE`, then `df` must be an integer; if `cv = TRUE`, then
#'   `df` should be one of "min" or "1se" indicating the selection rule to use
#'   based on the cross-validation error curve (minimum or 1-standard-error
#'   rule, respectively). Default is 8 when `cv = FALSE`, and "1se" when `cv =
#'   TRUE`.} 
#' }
#' 
#' @return A data frame given by appending a new column to `x` named according
#'   to the `col_name` argument, containing the estimated derivative values. 
#'   
#' @examples \dontrun{
#' df <- covidcast::covidcast_signal("fb-survey", "smoothed_cli", 
#'     start_day = "2021-01-01", 
#'     end_day = "2021-02-28",
#'     geo_type = "state",
#'     geo_values = c("ca", "fl"))
#' 
#' # estimate derivative using linear regression and n = 10 days
#' estimate_deriv(df, method = "lin", n = 10)
#' 
#' # keep the linear regression fits
#' estimate_deriv(df, method = "lin", n = 10, keep_obj = TRUE)
#' 
#' # estimate derivative using smoothing spline with 8 degrees of freedom
#' estimate_deriv(df, method = "ss", n = 28, df = 8)
#' 
#' # estimate derivative using trend filtering with 8 degrees of freedom
#' estimate_deriv(df, method = "tf", n = 28, df = 8)
#' }
#'
#' @export
estimate_deriv = function(x, method = c("lin", "ss", "tf"), n = 14,
                          col_name = "deriv", keep_obj = FALSE, deriv = 1,
                          ...) {  
  # Define the slider function
  method = match.arg(method)
  slide_fun = switch(method,
                     "lin" = linear_reg_deriv,
                     "ss" = smooth_spline_deriv,
                     "tf" = trend_filter_deriv)
  
  # Check the derivative order
  if (!(deriv == 1 || deriv == 2)) {
    stop("`deriv` must be either 1 or 2.")
  }

  # Slide the derivative function
  x = slide_by_geo(x, slide_fun, n, col_name = "temp", col_type = "list",
                   keep_obj = keep_obj, deriv = deriv, ...)

  # Grab the derivative result
  x = x %>% rowwise() %>% mutate(!!col_name := temp$result)

  # Grab the derivative object, if we're asked to
  if (keep_obj) {
    x = x %>% rowwise() %>%
      mutate(!!paste0(col_name, "_obj") := list(temp$object))
  }
  
  # Delete the temp column and return
  return(select(x, -temp))
}

#' Compute derivatives function via linear regression
#' @importFrom stats lsfit coef
#' @importFrom tidyr drop_na
#' @noRd
linear_reg_deriv = function(x, ...) {
  params = list(...)
  params[[1]] = NULL # dplyr::group_modify() includes the group here

  # If derivative order is 2, then return trivial result
  if (params$deriv == 2) return(list(object = NULL, result = 0))
  
  keep_obj = params$keep_obj
  params$keep_obj = NULL
  params$deriv = NULL
  x = x %>% select(time_value, value) %>% drop_na()
  params$x = as.numeric(x$time_value)
  params$y = x$value
  
  return(tryCatch(suppressWarnings(suppressMessages({
    object = do.call(lsfit, params)
    result = End(coef(object))
    if (!keep_obj) object = NULL # For memory sake
    list(object = object, result = result)
  })),
  error = function(e) list(object = NA, result = NA)))
}

#' Compute derivatives function via smoothing spline
#' @importFrom stats smooth.spline predict
#' @noRd
smooth_spline_deriv = function(x, ...) {
  params = list(...)
  params[[1]] = NULL # dplyr::group_modify() includes the group here

  keep_obj = params$keep_obj
  deriv = params$deriv
  params$keep_obj = NULL
  params$deriv = NULL
  x = x %>% select(time_value, value) %>% drop_na()
  params$x = as.numeric(x$time_value)
  params$y = x$value
  
  return(tryCatch(suppressWarnings(suppressMessages({
    object = do.call(smooth.spline, params)
    result = predict(object, x = End(params$x), deriv = deriv)$y
    if (!keep_obj) object = NULL # For memory sake
    list(object = object, result = result)
  })),
  error = function(e) list(object = NA, result = NA)))
}

#' Compute derivatives function via trend filtering
#' @importFrom genlasso trendfilter cv.trendfilter coef.genlasso
#' @noRd
trend_filter_deriv = function(data, ...) {
  params = list(...)
  params[[1]] = NULL # dplyr::group_modify() includes the group here
  
  keep_obj = params$keep_obj
  deriv = params$deriv
  params$keep_obj = NULL
  params$deriv = NULL
  
  cv = params$cv
  ord = params$ord
  maxsteps = params$maxsteps
  k = params$k
  df = params$df

  if (is.null(cv)) cv = FALSE
  if (is.null(ord)) ord = 2
  if (is.null(maxsteps)) maxsteps = 100
  if (is.null(k)) k = 5
  if (is.null(df)) df = ifelse(cv, "1se", 8)
  
  data = data %>% select(time_value, value) %>% drop_na()
  x = as.numeric(data$time_value)
  y = data$value

  return(tryCatch(suppressWarnings(suppressMessages({
    object = trendfilter(y = y, pos = x, ord = ord, max = maxsteps)
    if (cv) {
      cv_object = quiet(cv.trendfilter(object, k = k, mode = "df"))
      df = ifelse(df == "1se", cv_object$df.1se, cv_object$df.min)
    }
    else cv_object = NULL
    
    beta = coef.genlasso(object, df = df)$beta
    beta = approx(x, beta, Min(x):Max(x))$y # TODO use poly interpolation 
    result = End(diff(beta, diff = deriv))
    
    if (!keep_obj) { object = NULL; cv_object = NULL } # For memory sake
    list(object = list(trendfilter = object, cv.trendfilter = cv_object),
         result = result)
  })),
  error = function(e) list(object = NA, result = NA)))
}
