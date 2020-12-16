grp_processing_for_facets <- function(cards, 
                                      grp_vars, 
                                      grp_limit = NULL, 
                                      fname = NULL){
  
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
  if (is.null(grp_limit) || is.null(fname)) return(grps)
  assert_that(
    nrow(grp_tbl) < grp_limit,
    msg = str_glue("For {fname}, it's",
                   "challenging to see results",
                   "with more than {grp_limit - 1} groupings. Either filter",
                   "your scorecard or try writing your own version."))
  grps
}
