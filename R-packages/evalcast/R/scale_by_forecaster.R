#' Scale error measures based on those of a particular forecaster.
#'
#' @param score_card score_card like that returned by `evaluate_predictions()`
#' @param score_cols vector of column names in `score_card` to normalize
#' @param base_forecaster_name name of forecaster in `score_card$forecaster` column by whose error
#'   values the remaining forecasters' errors will be scaled
#' @param numeric_err_cols vector of column names in `score_card` that contain numeric error
#'   measures. Elements of `numeric_err_cols` that are not in `score_cols` will be dropped from the
#'   final output to avoid a mix of scaled and unscaled measures.
#'
#' @return
#' @export
scale_by_forecaster <- function(score_card,
                                score_cols,
                                base_forecaster_name, 
                                numeric_err_cols = c("ae", "wis")) {
    # Validate columns arguments
    columns <- colnames(score_card)
    assert_that("forecaster" %in% columns,
                msg = 'score_card must have a column named "forecaster"')
    
    extraneous_score_cols <- setdiff(score_cols, columns)
    assert_that(length(extraneous_score_cols) == 0,
                msg = paste("score_cols contains columns",
                            paste(extraneous_score_cols, collapse=", "),
                            "not present in the columns of score_card"))

    nonnumeric_score_cols <- setdiff(score_cols, numeric_err_cols)
    assert_that(length(nonnumeric_score_cols) == 0,
                msg = paste("score_cols contains columns",
                            paste(nonnumeric_score_cols, collapse=", "),
                            "that are not numeric errors"))

    # Validate contents of score_card's forecaster column
    unique_forecasters <- unique(score_card$forecaster)
    assert_that(base_forecaster_name %in% unique_forecasters,
                msg = paste("score_card has no forecaster named ",
                            base_forecaster_name,
                            ". Available forecasters: ",
                            paste(unique_forecasters, collapse=", "),
                            sep = ""))
    assert_that(length(unique_forecasters) > 1, 
                msg = paste("scale_by_forecaster requires the score card to have forecasters",
                            "other than", base_forecaster_name))

    df_list <- map(score_cols, function(var) {
        score_card %>% 
        select(setdiff(columns, setdiff(numeric_err_cols, var))) %>% 
        pivot_wider(names_from = "forecaster", 
                    names_prefix = var, 
                    values_from = var) %>% 
        mutate(across(starts_with(var), ~ .x /
                        !!sym(paste0(var, base_forecaster_name)))) %>%
        pivot_longer(cols = starts_with(var), 
                    names_to = "forecaster",
                    values_to = var) %>%
        mutate(forecaster = substring(forecaster, nchar(var) + 1)) %>%
        filter(forecaster != base_forecaster_name)
    })
    return(reduce(df_list, left_join))
}
