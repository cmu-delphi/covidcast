
#' Find common averaging "locations"
#' 
#' Many scoring or plotting functions compute averages over "locations"
#' for a number of different grouping facets. We say "locations" because this
#' most often the geo_value that gets averaged over, while the groupings are
#' the forecaster, forecast horizon, and forecast date. But under other 
#' combinations may be desired.
#' 
#' In the case that we want to make comparisons, we want the avg_vars to be
#' common. This function finds common avg_vars. An example would be if one
#' forecaster makes predictions for a location that others don't, we would
#' want to throw it when ever we compute error measures. 
#'
#' @param cards long data frame 
#' @param grp_vars character vector of indicating variables to group on
#' @param avg_vars character vector of variables to average over
#'
#' @return a data frame of the same type as input 
#' @export
intersect_averagers <- function(cards, grp_vars, avg_vars) {
  
  assert_that(length(intersect(grp_vars, avg_vars)) == 0L,
              msg = paste("In intersect_averagers: grp_vars and avg_vars",
                          "must not overlap"))
  distinct_averagers <- cards %>%
    select(all_of(avg_vars)) %>%
    distinct()
  
  averagers_intersected <- cards %>% 
    select(all_of(c(grp_vars, avg_vars))) %>%
    group_by(across(all_of(grp_vars))) %>%
    group_split(.keep = FALSE) %>% 
    lapply(distinct) %>%
    reduce(intersect)
  
  assert_that(nrow(averagers_intersected) > 1L,
              msg = paste("In intersect_averagers: there was at most one",
                          "common row to average over."))
  
  if (nrow(distinct_averagers) > nrow(averagers_intersected)) {
    message(paste("In intersect_averagers: Some avg_vars were not common",
                  "to all grp_vars. Only the intersection is used for",
                  "averaging."))
  }
  cards %>% right_join(averagers_intersected)
}
