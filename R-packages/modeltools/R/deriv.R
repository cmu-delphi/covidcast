#' Estimate derivatives of values in `covidcast_signal` data frame
#' 
#' Estimates derivatives of the values in a `covidcast_signal` data frame, using
#' a local (in time) linear regression or smoothing spline. (When multiple issue
#' dates are present, only the latest issue is considered.)  See the
#' [derivatives vignette]() for examples.
#'
#' @param df The `covidcast_signal` data frame under consideration. 
#' @param method One of "linear-reg", "smooth-spline", or "trend-filter"
#'   indicating the method to use for the derivative calculation. To estimate
#'   the derivative at any time point, we run the given method on the last `n`
#'   days of data, and use the corresponding predicted derivative (that is, the
#'   derivative of the underlying estimated function, linear or spline) at the
#'   current time point. See details below. 
#' @param n Size of the local window (in days) to use. For example, if `n = 5`,
#'   then to estimate the derivative on November 5, we train the given method on
#'   data in between November 1 and November 5. Default is 14.
#' @param new_col String indicating the name of the new column that will contain
#'   the derivative values. Default is "deriv"; note that setting `new_col =
#'   "value"` will overwrite the existing "value" column.  
#' @param deriv Order of derivative to estimate. Only orders 1 or 2 are allowed, 
#'   with the default being 1. (In some cases, a second-order derivative will
#'   return a trivial result: for example: when `method = "linear-reg"`, this
#'   will always be zero.)
#' @param params List of additional arguments to pass to the function that
#'   estimates derivatives. See details below.   
#'
#' @details Derivatives are estimated using:
#'
#' \itemize{
#' \item Linear regression, when `method = "linear-reg"`, via `stats::lsfit()`. 
#' \item Cubic smoothing spline, when `method = "smooth-spline"`, via
#'   `stats::smooth.spline()`.
#' \item Polynomial trend filtering, when `method = "trend-filter"`, via 
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
#'   list of arguments `params` is passed directly to the underlying estimation
#'   function (`stats::lsfit()` and `stats::smooth.spline()`). 
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
#' @return A data frame given by appending a new column to `df` named according
#'   to the `new_col` argument, containing the estimated derivative values.
#'
#' @export
estimate_deriv = function(df, method = c("linear-reg", "smooth-spline",
                                         "trend-filter"),
                          n = 14, new_col = "deriv", deriv = 1,
                          params = list()) {
  # Define the slider function
  method = match.arg(method)
  slide_fun = switch(method,
                     "linear-reg" = linear_reg_deriv,
                     "smooth-spline" = smooth_spline_deriv,
                     "trend-filter" = trend_filter_deriv)

  # Check the derivative order
  if (!(deriv == 1 || deriv == 2)) {
    stop("`deriv` must be either 1 or 2.")
  }
  
  # Slide the slider function and return
  params = c(params, deriv = deriv)
  return(slide_by_geo(df, slide_fun, n, new_col, params))
}

#' Compute derivatives via linear regression
#' @importFrom stats lsfit
#' @noRd
linear_reg_deriv = function(data, params) {
  if (params$deriv == 2) return(0)
  params$deriv = NULL
  data = na.omit(data) 
  params$x = data$x
  params$y = data$y
  
  return(tryCatch(suppressWarnings(suppressMessages(
    End(coef(do.call(lsfit, params))))), 
    error = function(e) return(NA)))
}

#' Compute derivatives via smoothing spline
#' @importFrom stats smooth.spline predict
#' @noRd
smooth_spline_deriv = function(data, params) {
  deriv = params$deriv
  params$deriv = NULL
  data = na.omit(data) 
  params$x = data$x
  params$y = data$y
  
  return(tryCatch(suppressWarnings(suppressMessages(
    predict(
      object = do.call(smooth.spline, params),
      x = End(params$x), deriv = deriv)$y)),
    error = function(e) return(NA)))
}

#' Compute derivatives via trend filtering
#' @importFrom genlasso trendfilter cv.trendfilter coef.genlasso
#' @noRd
trend_filter_deriv = function(data, params) {
  deriv = params$deriv
  ord = params$ord
  maxsteps = params$maxsteps
  cv = params$cv
  k = params$k
  df = params$df

  if (is.null(cv)) cv = FALSE
  if (is.null(ord)) ord = 2
  if (is.null(maxsteps)) maxsteps = 100
  if (is.null(k)) k = 5
  if (is.null(df)) df = ifelse(cv, "1se", 8)
  
  data = na.omit(data) 
  x = data$x
  y = data$y

  return(tryCatch(suppressWarnings(suppressMessages({
    obj = trendfilter(y = y, pos = x, ord = ord, max = maxsteps)
    if (!cv) beta = coef(obj, df = df)$beta
    else {
      cv_obj = quiet(cv.trendfilter(obj, k = k, mode = "df"))
      cv_df = ifelse(df == "1se", cv_obj$df.1se, cv_obj$df.min)
      beta = coef.genlasso(obj, df = cv_df)$beta
    }
    beta = approx(x, beta, Min(x):Max(x))$y # Should be poly
    End(diff(beta, diff = deriv))
  })),
  error = function(e) return(NA)))
}
