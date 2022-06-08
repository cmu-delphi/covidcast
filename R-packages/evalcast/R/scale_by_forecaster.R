#' Scale error measures based on those of a particular forecaster.
#'
#' @param score_card score_card like that returned by `evaluate_predictions()`
#' @param score_cols vector of column names in `score_card` to normalize
#' @param base_forecaster_name name of forecaster in `score_card$forecaster` column by whose error
#'   values the remaining forecasters' errors will be scaled
#' @param id_cols vector of column names in `score_card` that identify distinct forecasts (i.e. the
#'   independent variables of `score_card`).
#' @param drop_base_entries whether to drop the entries in `score_card` from 
#'   `base_forecaster_name` (when FALSE their corresponding `score_cols` values will all be 1)
#'
#' @return A tibble whose columns are `c(id_cols, score_cols)` whose `id_cols` values are copied
#'   directly from `score_card` and whose `score_cols` values are normalized with respect to 
#'   `base_forecaster_name`.
#' 
#' @importFrom tidyr pivot_wider pivot_longer
#'
#' @export
scale_by_forecaster <- function(score_card,
                                score_cols,
                                base_forecaster_name, 
                                id_cols = c("forecaster", "ahead", "geo_value", "forecast_date",
                                            "data_source", "signal", "target_end_date",
                                            "incidence_period"),
                                drop_base_entries = TRUE) {
    # Validate columns arguments
    columns <- colnames(score_card)
    assert_that("forecaster" %in% columns,
                msg = 'score_card must have a column named "forecaster"')
    assert_that(all(score_cols %in% columns),
                msg = paste("score_cols contains columns",
                            paste(setdiff(score_cols, columns), collapse=", "),
                            "not present in the columns of score_card"))
    assert_that(all(id_cols %in% columns),
                msg = paste("id_cols contains columns",
                            paste(setdiff(id_cols, columns), collapse=", "),
                            "not present in the columns of score_card"))

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
    for (var in score_cols){
        base_values <- filter(score_card,
                              .data$forecaster == base_forecaster_name)[[var]]
        if (any(base_values[!is.na(base_values)] == 0)) {
            warning("scale_by_forecaster will divide by zero in column ", var)
        }
    }

    df_list <- map(score_cols, function(var) {
        normalized_card <- score_card %>% 
            select(all_of(c(id_cols, var))) %>% 
            pivot_wider(names_from = "forecaster", 
                        names_prefix = var, 
                        values_from = var) %>%
            # for some reason, we fail to scale cols after the baseline
            # so we move to the end
            relocate(!!sym(paste0(var, base_forecaster_name)), 
                     .after = last_col()) %>% 
            mutate(across(starts_with(var), ~ .x /
                            !!sym(paste0(var, base_forecaster_name)))) %>%
            pivot_longer(cols = starts_with(var), 
                        names_to = "forecaster",
                        values_to = var) %>%
            mutate(forecaster = substring(.data$forecaster, nchar(var) + 1))
        if (drop_base_entries)
            normalized_card <- filter(normalized_card, 
                                      .data$forecaster != base_forecaster_name)
        return(normalized_card)
    })
    return(reduce(df_list, left_join))
}
