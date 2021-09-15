#' Compute sliding window operations on the values in a `covidcast_signal` data
#' frame, grouped by geographic location
#'
#' Slides a given function or formula over the values in a  `covidcast_signal` 
#' data frame, grouped by `geo_value`. See the 
#' [`slide_by_geo` vignette](https://cmu-delphi.github.io/covidcast/modeltoolsR/articles/slide_by_geo.html)
#' for examples.    
#'
#' @param x The `covidcast_signal` data frame under consideration. 
#' @param slide_fun Function or formula to slide over the values in `x`, grouped
#'   by `geo_value`. To "slide" means to apply the function or formula over a
#'   trailing window of `n` days of data. If a function, `slide_fun` must take
#'   `x`, a data frame the same column names as the original data frame;
#'   followed by any number of named additional arguments; and ending with
#'   `...`, to capture general additional arguments. If a formula, `slide_fun`
#'   can operate directly on `.x$value`, `.x$time_value`, etc., as in `~
#'   mean(.x$value)` to compute a trailing mean over the last `n` days of data.
#' @param n Size of the trailing window (in days) to use. Defaults to `n = 14`.
#'   The sliding window includes the current day and the previous `n - 1` days.
#'   So, e.g., an `n = 14` sliding mean calculation on November 14 is an 
#'   average of the 14 days of data from November 1 to November 14. 
#' @param col_name String indicating the name of the new column that will
#'   contain the output of the sliding operation. Default is "slide_value"; 
#'   note that setting `col_name = "value"` will overwrite the existing "value"
#'   column. 
#' @param col_type One of "dbl", "int", "lgl", "chr", or "list", indicating the
#'   data type (as tibble abbreviation) for the new column. Default is "dbl".  
#' @param ... Additional arguments to pass to the function or formula specified
#'   via `slide_fun`.  
#' 
#' @return A data frame given by appending a new column to `x` named according
#'   to the `col_name` argument, containing the function values.
#'
#' @export slide_by_geo


#' @importFrom dplyr %>% arrange group_by group_modify mutate ungroup
#' @importFrom lubridate days
slide_by_geo <- function(x, slide_fun, n = 14, col_name = "slide_value",
                         col_type = c("dbl", "int", "lgl", "chr", "list"), ...)
  {
  
  # Check we have the minimal set of columns
  if ( !all(c("geo_value", "time_value") %in% colnames(x)) ){
    stop("`x` must have columns 'geo_value' and 'time_value'.")
  }
  
  # Which slide_index function?
  col_type <- match.arg(col_type)
  slide_index_zzz <- switch(col_type,
                            "dbl" = slider::slide_index_dbl,
                            "int" = slider::slide_index_int,
                            "lgl" = slider::slide_index_lgl,
                            "chr" = slider::slide_index_chr,
                            "list" = slider::slide_index
                            )

  # Slide over a single geo value
  slide_one_geo <- function(.data_group, slide_fun, n, col_name, ...){
    slide_values <- slide_index_zzz(.x = .data_group,
                                    .i = .data_group$time_value,
                                    .f = slide_fun, ..., 
                                    .before = days(n-1)
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
