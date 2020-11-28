#' Slide a function over values in `covidcast_signal` data frame, grouped by
#' `geo_value`  
#'
#' Slides a given function over the values in a  `covidcast_signal` data frame,
#' grouped by `geo_value`. (When multiple issue dates are present, only the
#' latest issue is considered.)  See the [slide vignette]() for examples. 
#'
#' @param df The `covidcast_signal` data frame under consideration. 
#' @param slide_fun Function to slide over the values in `df`, grouped by
#'   `geo_value`. To "slide" means to apply the function over a trailing window
#'   of `n` days (at any time point, we run the function over the last `n` days
#'   of data).  
#' @param n Size of the local window (in days) to use. For example, if `n = 5`,
#'   then to estimate the derivative on November 5, we train the given method on
#'   data in between November 1 and November 5. Default is 14.
#' @param new_col String indicating the name of the new column that will contain
#'   the derivative values. Default is "slide_value"; note that setting `new_col
#'   = "value"` will overwrite the existing "value" column.  
#' @param params List of additional arguments to pass to the function specified
#'   via `slide_fun`. 
#' 
#' @return A data frame given by appending a new column to `df` named according
#'   to the `new_col` argument, containing the function values.
#'
#' @importFrom dplyr %>% group_by
#' @importFrom lubridate days
#' @importFrom slider slide_index_dbl
#' @export
slide_by_geo = function(df, slide_fun, n = 14, new_col = "slide_value",
                        params = list()) { 
  # Check we have the minimal columns we need
  if (!all(c("geo_value", "time_value", "value") %in% colnames(df))) {
    stop("`df` must have columns 'geo_value', 'time_value', and 'value'.")
  }
  df = covidcast:::latest_issue(df) # TODO shouldn't require issue col

  # Slide the given function and return
  return(df %>%
         group_by(geo_value) %>%
         arrange(time_value) %>%
         mutate(!!new_col := slide_index_dbl(
                    .x = dplyr::tibble(x = as.numeric(time_value),
                                       y = value), 
                    .i = time_value,
                    .f = slide_fun, params,
                    .before = days(n-1))) %>%
         ungroup())
}
