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
#' @param ahead Number of incidence period units (i.e., epiweeks, days, etc.) 
#'     ahead to forecast. Can be a single positive integer or a vector of 
#'     positive integers. Note that for each `{a}` in `ahead`, the column 
#'     `response+{a}:{response}` should be present in `lagged_df`.
#' @param training_window_size Size of the local training window in days to use. For example, if
#'     `training_window_size = 14`, then to make a 1-day-ahead forecast on December 15, we train on
#'     data from December 1 to December 14.
#' @param aheads_separate If `length(ahead) > 1`, should there be separate
#'     data matrices and responses for each ahead? Default is `TRUE`.
#'
#' @return For a single ahead value, named list with entries:
#'     \itemize{
#'     \item `train_x`: Matrix of training data whose columns correspond to the
#'         `value-{days}:{signal}` columns in `lagged_df`.  The training data consists of the
#'         latest date with an non-null response, plus all data from the `training_window_size`
#'         days prior to it.
#'     \item `train_y`: Vector of response data from the `response+{ahead}:{response}` column of
#'         `lagged_df` corresponding to the rows of `train_x`.
#'     \item `train_geo_values`: Vector of geo values corresponding to the rows
#'         of `train_x`.
#'     \item `train_time_values`: Vector of time values corresponding to the rows
#'         of `train_x`.
#'     \item `train_end_date`: Latest `time_value` used in the training period.
#'     \item `predict_x`: Matrix of prediction data in the same format as `train_x`.  The
#'         prediction data contains the most recent `training_window_size` days.
#'     \item `predict_geo_values`: Vector of `geo_values` corresponding to the rows of `predict_x`.
#'     \item `predict_time_value`: Time value corresponding to `predict_x`.
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
#' @importFrom lubridate as_date
#' @importFrom purrr map_dbl map2
#'
#' @export
create_train_and_predict_matrices <- function(lagged_df, ahead, training_window_size,
                                              aheads_separate = TRUE) {
    # make sure the response columns are unique
    for (a in ahead) {
        responses_at_ahead <- lagged_df %>%
            select(tidyselect::starts_with(sprintf("response+%i:", a))) %>%
            ncol()
        assert_that(responses_at_ahead == 1,
                    msg = paste("multiple responses at ahead =", a))
    }
    
    # prediction matrices are the same for all aheads
    predict_time_value <- max(lagged_df$time_value)
    predict_x <- lagged_df %>%
        filter(time_value == predict_time_value) %>%
        select(tidyselect::starts_with("value")) %>%
        as.matrix()
    predict_geo_values <- lagged_df %>%
        filter(time_value == predict_time_value) %>%
        select(geo_value) %>% pull()
    
    all_out <- list()
    
    if (aheads_separate) {
        train_end_dates <- ahead %>%
            purrr::map_dbl(~ get_train_end_date(lagged_df, .x)) %>%
            lubridate::as_date()
        
        all_out <- purrr::map2(ahead, train_end_dates,
                              ~ create_train_matrices(
                                  lagged_df, .x, training_window_size, .y))
    } else {  # ahead_separate = FALSE: We want a single training data matrix and 
              # a matrix of training responses containing all the aheads
        # Need to recompute training_window_size 
        training_window_size <- training_window_size + max(ahead) - min(ahead)
        
        # Find the last possible date of training data
        # (corresponds to the smallest ahead)
        train_end_date <- get_train_end_date(lagged_df, ahead)
        
        # Training matrices
        all_out[[1]] <- create_train_matrices(lagged_df, ahead, 
                                              training_window_size,
                                              train_end_date)
    }
    
    # Add prediction matrices / info
    for (i in seq_along(all_out)) {
        all_out[[i]]$predict_x <- predict_x
        all_out[[i]]$predict_geo_values <- predict_geo_values
        all_out[[i]]$predict_time_value <- predict_time_value
    }
    
    if (length(ahead) == 1 || aheads_separate == FALSE) {
        return(all_out[[1]])
    } else {
        names(all_out) <- paste0("ahead+", ahead)
        return(all_out)
    }
}

#' Get last possible date of training data for given aheads.
#' 
#' Returns the last possible date of the training data for a given set of
#' aheads. If more than one ahead is given, the date return corresponds to the
#' last possible date corresponding to the smallest ahead value. This is because
#' the smallest ahead value will have the latest possible date.
#'
#' @param lagged_df Data frame of lagged data as in `create_train_and_predict_matrices()`.
#' @param ahead Number of days ahead to forecast. Can be a single positive 
#'     integer or a vector of positive integers.
#' 
#' @return Single date corresponding to the last possible date of the training
#' data.
get_train_end_date <- function(lagged_df, ahead) {
    response_end_date <- lagged_df %>%
        select(time_value, 
               tidyselect::starts_with(sprintf("response+%i:", min(ahead)))) %>%
        tidyr::drop_na() %>%
        summarize(max(time_value)) %>%
        pull()
    train_end_date <- min(max(lagged_df$time_value), response_end_date)
    return(train_end_date)
}
    
#' Create training data matrix and a training response for given aheads.
#' 
#' Create training and data matrix and training response for a set of
#' given aheads. Works for both single ahead values and a vector of ahead 
#' values. However, note that this function works different from
#' `create_train_and_predict_matrices()` for multiple ahead values. If
#' multiple ahead values are supplied, we return a matrix of responses
#' containing all aheads at once.
#'
#' @param lagged_df Data frame of lagged data as in `create_train_and_predict_matrices()`.
#' @param ahead Number of incidence period units (i.e., epiweeks, days, etc.) 
#'     ahead to forecast. Can be a single positive integer or a vector of 
#'     positive integers. Note that for each `{a}` in `ahead`, the column 
#'     `response+{a}:{response}` should be present in `lagged_df`.
#' @param n_days Number of days worth of data to pull.
#' @param train_end_date The last date to be included in the training data.
#'
#' @return A named list with entries:
#'     \itemize{
#'     \item `train_x`: Matrix of training data whose columns correspond to the
#'         `value-{days}:{signal}` columns in `lagged_df`.  The training data consists of the
#'         latest date with an non-null response, plus all data from the `training_window_size`
#'         days prior to it.
#'     \item `train_y`: Vector of response data from the `response+{ahead}:{response}` column of
#'         `lagged_df` corresponding to the rows of `train_x`. If multiple ahead
#'         values are provided, then this is a matrix instead.
#'     \item `train_geo_values`: Vector of geo values corresponding to the rows
#'         of `train_x`.
#'     \item `train_time_values`: Vector of time values corresponding to the rows
#'         of `train_x`.
#'     \item `train_end_date`: Latest `time_value` used in the training period.
#'     }
create_train_matrices <- function(lagged_df, ahead, n_days,
                                   train_end_date) {
    train_df <- lagged_df %>%
        filter(between(time_value,
                       train_end_date - n_days + 1,
                       train_end_date))
    out <- list()
    out$train_x <- train_df %>%
        select(tidyselect::starts_with("value")) %>%
        as.matrix()
    
    train_y <- train_df %>%
        select(tidyselect::starts_with(paste0("response+", ahead, ":")))
    if (length(ahead) == 1) {
        out$train_y <- pull(train_y)
    } else {
        out$train_y <- as.matrix(train_y)
    }
    
    out$train_geo_values  <- train_df$geo_value
    out$train_time_values <- train_df$time_value
    out$train_end_date    <- as.Date(train_end_date)
    
    return(out)
}
