#' Compute percentage change of values in `covidcast_signal` data frame
#' 
#' Computes the percentage change of the values in a `covidcast_signal` data
#' frame. (When multiple issue dates are present, only the latest issue is
#' considered.)  See the [percentage change
#' vignette](https://cmu-delphi.github.io/covidcast/modeltoolsR/articles/pct-change.html)
#' for examples.  
#'
#' @param x The `covidcast_signal` data frame under consideration.
#' @param n Size of the local window (in days) to use (bumped up to the nearest
#'   even integer, if needed). For example, if `n = 10`, then to compute the
#'   percentage change on November 10, we use 100 * (B - A) / A, where A is the
#'   sum of the values between November 6 and November 10, and A is the sum of
#'   the values between November 1 and November 5. Default is 14.
#' @param col_name String indicating the name of the new column that will
#'   contain the percentage change values. Default is "pct_change"; note that we
#'   can set `col_name = "value"` to overwrite the existing "value" column.  
#' 
#' @return A data frame given by appending a new column to `x` named according
#'   to the `col_name` argument, containing the percentage change values. 
#'
#' @examples \dontrun{
#' df <- covidcast::covidcast_signal("fb-survey", "smoothed_cli", 
#'     start_day = "2021-01-01", 
#'     end_day = "2021-01-31",
#'     geo_type = "state")
#' 
#' # percentage change between back-to-back weeks (default, as n = 14)
#' pct_change(df)
#' 
#' # percentage change between back-to-back days
#' pct_change(df, n = 2)
#' }
#'
#' @export
pct_change = function(x, n = 14, col_name = "pct_change") {
  # Check if n is odd and if so bump it up one
  if (n %% 2 == 1) n = n + 1

  # Slide the percentage change function and return
  return(slide_by_geo(x, pct_change_fun, n, col_name, N = n))
}

#' Compute percentage change function
#' @noRd
pct_change_fun = function(x, ...) {
  params = list(...)
  N = params$N
  if (nrow(x) != N) return(NA)

  a = Sum(x$value[1:(N/2)])
  b = Sum(x$value[(N/2 + 1):N])
  return(100 * (b - a) / a)
}
