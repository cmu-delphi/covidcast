#'
#'
#' Compute correlations between two covidcast_signal data frames
#'
#' Computes correlations between two covidcast_signal data frames.
#' 
#' @param x,y The `covidcast_signal` data frames to correlate.
#' @param dt_x,dt_y Time shifts to consider for `x` and `y`, respectively,
#'   before computing correlations. These must both be nonnegative, and only one
#'   can be positive. If `dt_x = 1`, for example, then data for `x` is shifted
#'   forward 1 day in time (so, data on June 1 becomes data on June 2, and so
#'   on). Default is 0 for both. 
#' @param by If "geo_value", then correlations are computed for each geo
#'   location, over all time. If "time_value", then correlations are computed
#'   for each time, over all geo locations. Default is "geo_value".
#' @param use,method Arguments to pass to `cor()`, with "na.or.complete" the
#'   default for `use`, and "spearman" the default for `method` (different than
#'   the defaults used by `cor()`).
#' 
#' @return 
#' @export
covidcast_cor = function(x, y, dt_x = 0, dt_y = 0,
                         by = c("geo_value", "time_value"),
                         use = "na.or.complete", 
                         method = c("spearman", "pearson", "kendall")) {
  by = match.arg(by)
  method = match.arg(method)
  if (dt_x < 0 || dt_y < 0) stop("Both dt_x and dt_y must be nonnegative")
  if (dt_x > 0 && dt_y > 0) stop("Only one of dt_x and dt_y can be positive")
  z = dplyr::full_join(x, y, by = c("geo_value", "time_value"))
  return(z %>% dplyr::group_by(geo_value) %>% # group by geo value
         dplyr::arrange(time_value) %>%  # sort rows by increasing time
         dplyr::mutate(value.x = dplyr::lag(value.x, n = dt_x), # shift values
                       value.y = dplyr::lag(value.y, n = dt_y)) %>% 
         dplyr::ungroup() %>% # get rid of grouping by geo value
         dplyr::group_by(.dots = as.symbol(by)) %>% # group by what's specified
         dplyr::summarize(value = cor(x = value.x, y = value.y, # compute cor
                                      use = use, method = method)))
}
