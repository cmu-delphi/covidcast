
grp_processing_for_facets <- function(cards, grp_vars, bad_n, fname){
  
  assert_that(all(grp_vars %in% names(cards)),
              msg = paste("When processing by groups, all grp_vars",
                          "must be in the data frame."))
  grp_tbl <- cards %>% 
    summarise(across(all_of(grp_vars), ~n_distinct(.x))) %>%
    pivot_longer(everything()) %>%
    filter(.data$value > 1) 
  if (nrow(grp_tbl) > 0) {
    grps <- grp_tbl %>% 
      arrange(desc(.data$value)) %>%
      select(.data$name) %>%
      pull()
  }
  if (missing(bad_n) || missing(fname)) return(grps)
  assert_that(
    nrow(grp_tbl) < bad_n,
    msg = str_glue("For {fname}, it's",
                   "challenging to see results",
                   "with more than {bad_n - 1} groupings. Either filter",
                   "your scorecard or try writing your own version."))
  grps
}