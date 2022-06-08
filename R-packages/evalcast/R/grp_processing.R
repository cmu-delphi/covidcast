#' @importFrom tidyr pivot_longer
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
  return(grps)
}


test_legal_faceting <- function(facet_rows, facet_cols, grp_vars) {
  if (!is.null(facet_rows)) {
    non_grouped_facet <- setdiff(facet_rows, grp_vars)
    assert_that(length(non_grouped_facet) == 0,
                msg = paste("Variables must be grouped in order to be faceted in rows:",
                            non_grouped_facet))
  }
  if (!is.null(facet_cols)) {
    non_grouped_facet <- setdiff(facet_cols, grp_vars)
    assert_that(length(non_grouped_facet) == 0,
                msg = paste("Variables must be grouped in order to be faceted in cols:",
                            non_grouped_facet))
  }
  invisible(TRUE)
}

# Helpful wrapper on interaction() for our plotting functions
Interaction <- function(...) {
  params <- list(...)
  if (length(params) == 0) return(NULL)
  else if (length(params) == 1) return(as.factor(params[[1]]))
  else return(interaction(...))
}

