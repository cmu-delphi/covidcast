#' Create a training and prediction matrices for a given ahead.
#'
#' @param lagged_df Data frame of lagged data.  It should have the following columns:
#'     \itemize{
#'     \item{`geo_value`}{Strings of geographic locations.}
#'     \item{`time_value`}{Dates of training data.}
#'     \item{Covariate columns}{Columns with names of the form `value-{days}:{signal}` whose values
#'         correspond to `{signal}` `{days}` before `time_value`}
#'     \item{Response columns}{Columns with names of the form `value+{n}:{response}` whose values
#'         correspond to `{response}` `{n}` incidence period units after `time_value`.  Since
#'         columns beginning with "value+" are interpretted as the response rather than a
#'         covariate, observed data from the same day as `time_value` should be in columns
#'         `value-0:{signal}` rather than `value+0:{signal}`}
#'     }
#'     A data frame in this format can be made using `covidcast::aggregate_signals()`.
#' @param ahead Number of incidence period units (i.e., epiweeks, days, etc.) ahead to forecast
#' @param training_window_size Size of the local training window in days to use. For example, if
#'     `training_window_size = 14`, then to make a 1-day-ahead forecast on December 15, we train on
#'     data from December 1 to December 14.
#'
#' @return Named list with entries:
#'     \itemize{
#'     \item{`train_x`}{Matrix of training data whose columns correspond to the
#'         `value-{days}:{signal}` columns in `lagged_df`.  The training data consists of the
#'         latest date `d` such that there is an observed response at time `d + ahead` and all data
#'         from the `training_window_size` days prior to it.}
#'     \item{`train_y`}{Vector of response data from the `value+{ahead}:{response}` column of
#'         `lagged_df` corresponding to the rows of `train_x`.}
#'     \item{`predict_x`}{Matrix of prediction data in the same format as `train_x`.  The
#'         prediction data contains the most recent `training_window_size` days.}
#'     \item{`predict_geo_values`}{Vector of `geo_values` corresponding to the rows of `predict_x`.}
#'     }
#'
#' @importFrom tibble tibble
#'
#' @export 
create_train_and_predict_matrices <- function(lagged_df, ahead, training_window_size) {
    out <- list()

    train_df <- lagged_df %>%
        select(geo_value, time_value, tidyselect::matches("^value(\\+0|-)"))

    # Find the last possible date of training data   
    response_end_date <- lagged_df %>%
        select(time_value, tidyselect::starts_with(sprintf("value+%i:", ahead))) %>%
        tidyr::drop_na() %>%
        summarize(max(time_value)) %>%
        pull()
    train_end_date <- min(max(lagged_df$time_value), response_end_date)

    # Training matrices
    out$train_x <- train_df %>%
        filter(between(time_value,
                       train_end_date - training_window_size + 1,
                       train_end_date)) %>%
        select(-c(geo_value, time_value)) %>%
        as.matrix()
    out$train_y <- lagged_df %>%
        filter(between(time_value,
                       train_end_date - training_window_size + 1,
                       train_end_date)) %>%
        select(tidyselect::starts_with(sprintf("value+%i:", ahead))) %>%
        pull()

    # Prediction matrices
    out$predict_x <- lagged_df %>%
        filter(time_value == max(time_value)) %>%
        select(tidyselect::matches("^value(\\+0|-)")) %>%
        as.matrix()
    out$predict_geo_values <- lagged_df %>%
        filter(time_value == max(time_value)) %>%
        select(geo_value) %>% pull()

    return(out)
}
