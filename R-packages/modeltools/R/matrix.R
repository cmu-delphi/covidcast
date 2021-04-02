#' Create training and test data matrices and a training response for given aheads.
#' 
#' Create training and test data matrices and training response for a set of
#' given aheads. Works for both single ahead values and a vector of ahead values.
#' For multiple ahead values, the function has the ability to return separate
#' data matrices and responses for each ahead, or a single data matrix and
#' response matrix for all aheads at once.
#'
#' @param lagged_df Data frame of lagged data. It should have the following columns:
#'     \itemize{
#'     \item `geo_value`: Strings of geographic locations.
#'     \item `time_value`: Dates of training data.
#'     \item Covariate columns: Columns with names of the form `value-{days}:{signal}` or
#'         `value+0:{signal}` whose values correspond to `{signal}` `{days}` before `time_value`.
#'     \item Response columns: Columns with names of the form `response+{n}:{response}` whose values
#'         correspond to `{response}` `{n}` incidence period units after `time_value`.
#'     }
#'     A data frame in this format can be made using `covidcast::aggregate_signals()` and
#'     `modeltools::get_response_columns()`.
#' @param ahead Number of incidence period units (i.e., epiweeks, days, etc.) ahead to forecast.
#' Can be a single positive integer or a vector of positive integers. Note that 
#' for each `{a}` in `ahead`, the column `response+{a}:{response}` should be
#' present in `lagged_df`.
#' @param training_window_size Size of the local training window in days to use. For example, if
#'     `training_window_size = 14`, then to make a 1-day-ahead forecast on December 15, we train on
#'     data from December 1 to December 14.
#' @param aheads_separate If `length(ahead) > 1`, should there be separate
#' data matrices and responses for each ahead? Default is `TRUE`.
#'
#' @return For a single ahead value, named list with entries:
#'     \itemize{
#'     \item `train_x`: Matrix of training data whose columns correspond to the
#'         `value-{days}:{signal}` columns in `lagged_df`.  The training data consists of the
#'         latest date with an non-null response, plus all data from the `training_window_size`
#'         days prior to it.
#'     \item `train_y`: Vector of response data from the `response+{ahead}:{response}` column of
#'         `lagged_df` corresponding to the rows of `train_x`.
#'     \item `predict_x`: Matrix of prediction data in the same format as `train_x`.  The
#'         prediction data contains the most recent `training_window_size` days.
#'     \item `predict_geo_values`: Vector of `geo_values` corresponding to the rows of `predict_x`.
#'     \item `train_end_date`: Latest `time_value` used in the training period.
#'     }
#'     For multiple ahead values and `aheads_separate = TRUE`, a list having
#'     the same length as `ahead`, with each element being a named list as above.
#'     For multiple ahead values and `ahead_separate = FALSE`, a named list as
#'     above, except `train_y` is a matrix of responses rather than a vector.
#'
#' @examples \dontrun{
#' create_train_and_predict_matrices(
#'   tibble(
#'     geo_value = rep(c("az", "wv"), 5),
#'     time_value = rep(
#'       as.Date(c("2021-01-25", "2021-01-26", "2021-01-27", "2021-01-28", "2021-01-29")),
#'       each = 2),
#'     `value-2:signal_1` = seq(-3, 6),
#'     `value-1:signal_1` = seq(-1, 8),
#'     `value+0:signal_1` = seq(1, 10),
#'     `response+2:signal_1` = c(seq(5, 10), rep(NA, 4))
#'   ),
#'   ahead = 2,
#'   training_window_size = 1)
#' }
#'
#' @importFrom tibble tibble
#' @importFrom assertthat assert_that
#'
#' @export
create_train_and_predict_matrices <- function(lagged_df, ahead, training_window_size,
                                              aheads_separate = TRUE) {
    all_out <- list()
    
    # prediction matrices are the same for all aheads
    predict_x <- lagged_df %>%
        filter(time_value == max(time_value)) %>%
        select(tidyselect::starts_with("value")) %>%
        as.matrix()
    predict_geo_values <- lagged_df %>%
        filter(time_value == max(time_value)) %>%
        select(geo_value) %>% pull()
    
    # make sure the response columns are unique
    for (a in ahead) {
        responses_at_ahead <- lagged_df %>%
            select(tidyselect::starts_with(sprintf("response+%i:", a))) %>%
            ncol()
        assert_that(responses_at_ahead == 1,
                    msg = paste("multiple responses at ahead =", a))
    }
    
    if (aheads_separate) {
        for (a in ahead) {
            out <- list()  # matrices corresponding to ahead+a
            
            # Find the last possible date of training data
            response_end_date <- lagged_df %>%
                select(time_value, tidyselect::starts_with(sprintf("response+%i:", a))) %>%
                tidyr::drop_na() %>%
                summarize(max(time_value)) %>%
                pull()
            train_end_date <- min(max(lagged_df$time_value), response_end_date)
            
            # Training matrices
            out$train_x <- lagged_df %>%
                select(geo_value, time_value, tidyselect::starts_with("value")) %>%
                filter(between(time_value,
                               train_end_date - training_window_size + 1,
                               train_end_date)) %>%
                select(-c(geo_value, time_value)) %>%
                as.matrix()
            out$train_y <- lagged_df %>%
                filter(between(time_value,
                               train_end_date - training_window_size + 1,
                               train_end_date)) %>%
                select(tidyselect::starts_with(sprintf("response+%i:", a))) %>%
                pull()
            
            # Add prediction matrices and training end date
            out$predict_x <- predict_x
            out$predict_geo_values <- predict_geo_values
            out$train_end_date <- train_end_date
            
            all_out[[paste0("ahead+", a)]] <- out
        }
        
        if (length(ahead) == 1) {
            return(all_out[[1]])
        } else {
            return(all_out)
        }
    } else {
        # We want a single training data matrix and a matrix of 
        # training responses containing all the aheads
        # Need to recompute training_window_size 
        training_window_size <- training_window_size + max(ahead) - min(ahead)
        
        out <- list()
        
        # Find the last possible date of training data
        # (corresponds to the smallest ahead)
        response_end_date <- lagged_df %>%
            select(time_value, 
                   tidyselect::starts_with(sprintf("response+%i:", min(ahead)))) %>%
            tidyr::drop_na() %>%
            summarize(max(time_value)) %>%
            pull()
        train_end_date <- min(max(lagged_df$time_value), response_end_date)
        
        # Training matrices
        out$train_x <- lagged_df %>%
            select(geo_value, time_value, tidyselect::starts_with("value")) %>%
            filter(between(time_value,
                           train_end_date - training_window_size + 1,
                           train_end_date)) %>%
            select(-c(geo_value, time_value)) %>%
            as.matrix()
        out$train_y <- lagged_df %>%
            filter(between(time_value,
                           train_end_date - training_window_size + 1,
                           train_end_date)) %>%
            select(tidyselect::starts_with(paste0("response+", ahead, ":"))) %>%
            as.matrix()
        
        # Add prediction matrices and training end date
        out$predict_x <- predict_x
        out$predict_geo_values <- predict_geo_values
        out$train_end_date <- train_end_date
        
        return(out)
    }
}
