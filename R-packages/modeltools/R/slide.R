#' Slide a function over values in `covidcast_signal` data frame, grouped by
#' `geo_value`  
#'
#' Slides a given function over the values in a `covidcast_signal` data frame,
#' grouped by `geo_value`. (When multiple issue dates are present, only the
#' latest issue is considered.) See the [getting started
#' guide](https://cmu-delphi.github.io/covidcast/modeltoolsR/articles/modeltools.html)
#' for examples.    
#'
#' @param x The `covidcast_signal` data frame under consideration. 
#' @param slide_fun Function or formula to slide over the values in `x`, grouped
#'   by `geo_value`. To "slide" means to apply the function or formula over a
#'   right-aligned window of `n` days of data. If a function, `slide_fun` must take
#'   `x`, a data frame the same column names as the original data frame;
#'   followed by any number of named additional arguments; and ending with
#'   `...`, to capture general additional arguments. If a formula, `slide_fun`
#'   can operate directly on `.x$value`, `.x$time_value`, etc., as in `~
#'   mean(.x$value)` to compute a trailing mean over the last `n` days of data.
#' @param n Size of the local window (in days) to use.  Default is 14.
#' @param shift Number of days forward from the `time_value` to shift the window.  Because the
#'   window is right-aligned, if `n = 5` and `shift = 0`, then for a `time_value` of November 5,
#'   the function will be applied to values from November 1 to November 5.  A positive value shifts
#'   the window forwards in time, while negative values shift the window backwards in time.  Ex:
#'   for a `time_value` of November 5 `shift = 3` captures November 4 through November 8;
#'   `shift = -3` captures October 29 through November 2.  Default is not to shift.
#' @param col_name String indicating the name of the new column that will
#'   contain the derivative values. Default is "slide_value"; note that setting
#'   `col_name = "value"` will overwrite the existing "value" column. 
#' @param col_type One of "dbl", "int", "lgl", "chr", or "list", indicating the
#'   data type (as tibble abbreviation) for the new column. Default is "dbl".  
#' @param ... Additional arguments to pass to the function or formula specified
#'   via `slide_fun`.  
#' 
#' @return A data frame given by appending a new column to `x` named according
#'   to the `col_name` argument, containing the function values.
#'   
#' @examples \dontrun{
#' df <- covidcast::covidcast_signal("fb-survey", "smoothed_cli", 
#'     start_day = "2021-01-01", 
#'     end_day = "2021-01-31",
#'     geo_type = "state")
#'     
#' # two equivalent ways to compute 7-day trailing averages
#' slide_by_geo(df, slide_fun = ~ Mean(.x$value), n = 7)
#' slide_by_geo(df, slide_fun = function(x, ...) Mean(x$value) , n = 7)
#' }
#'
#' @importFrom dplyr %>% arrange group_by group_modify mutate ungroup
#' @importFrom lubridate days
#' @export
slide_by_geo <- function(x,
                         slide_fun,
                         n = 14,
                         shift = 0,
                         col_name = "slide_value",
                         col_type = c("dbl", "int", "lgl", "chr", "list"),
                         ...) {
  # Check we have the minimal columns we need
  if (!all(c("geo_value", "time_value") %in% colnames(x))) {
    stop("`x` must have columns 'geo_value' and 'time_value'.")
  }
  # x = covidcast:::latest_issue(x) # TODO is this needed?

  # Which slide_index function?
  col_type <- match.arg(col_type)
  slide_index_zzz <- switch(col_type,
                            "dbl" = slider::slide_index_dbl,
                            "int" = slider::slide_index_int,
                            "lgl" = slider::slide_index_lgl,
                            "chr" = slider::slide_index_chr,
                            "list" = slider::slide_index)

  # Slide over a single geo value
  slide_one_geo <- function(.data_group, slide_fun, n, col_name, ...) {
    slide_values <- slide_index_zzz(.x = .data_group,
                                    .i = .data_group$time_value,
                                    .f = slide_fun, ..., 
                                    .before = days(n - 1 - shift),
                                    .after = days(shift)
                                    )
    return(mutate(.data_group, !!col_name := slide_values))
  }
  
  # Apply slide function per group and return
  return(x %>%
         group_by(geo_value) %>%
         arrange(time_value) %>%
         group_modify(slide_one_geo, slide_fun = slide_fun,
                      n = n, col_name = col_name, ...) %>%
         ungroup())
}
