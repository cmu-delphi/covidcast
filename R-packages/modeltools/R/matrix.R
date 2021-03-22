#' Create training and testing data matrices and a training response vector for a given ahead.
#'
#' @param lagged_df Data frame of lagged data.  It should have the following columns:
#'     \itemize{
#'     \item{`geo_value`}{Strings of geographic locations.}
#'     \item{`time_value`}{Dates of training data.}
#'     \item{Covariate columns}{Columns with names of the form `value-{days}:{signal}` or
#'         `value+0:{signal} whose values correspond to `{signal}` `{days}` before `time_value`}
#'     \item{Response columns}{Columns with names of the form `response+{n}:{response}` whose values
#'         correspond to `{response}` `{n}` incidence period units after `time_value`.}
#'     }
#'     A data frame in this format can be made using `covidcast::aggregate_signals()` and
#'     `modeltools::get_response_columns()`.
#' @param ahead Number of incidence period units (i.e., epiweeks, days, etc.) ahead to forecast
#' @param training_window_size Size of the local training window in days to use. For example, if
#'     `training_window_size = 14`, then to make a 1-day-ahead forecast on December 15, we train on
#'     data from December 1 to December 14.
#'
#' @return Named list with entries:
#'     \itemize{
#'     \item{`train_x`}{Matrix of training data whose columns correspond to the
#'         `value-{days}:{signal}` columns in `lagged_df`.  The training data consists of the
#'         latest date with an non-null response, plus all data from the `training_window_size`
#'         days prior to it.}
#'     \item{`train_y`}{Vector of response data from the `response+{ahead}:{response}` column of
#'         `lagged_df` corresponding to the rows of `train_x`.}
#'     \item{`predict_x`}{Matrix of prediction data in the same format as `train_x`.  The
#'         prediction data contains the most recent `training_window_size` days.}
#'     \item{`predict_geo_values`}{Vector of `geo_values` corresponding to the rows of `predict_x`.}
#'     }
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
#' )
#' }
#'
#' @importFrom tibble tibble
#'
#' @export 
create_train_and_predict_matrices <- function(lagged_df, ahead, training_window_size) {
    out <- list()

    train_df <- lagged_df %>%
        select(geo_value, time_value, tidyselect::starts_with("value"))

    # Find the last possible date of training data   
    response_end_date <- lagged_df %>%
        select(time_value, tidyselect::starts_with(sprintf("response+%i:", ahead))) %>%
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
        select(tidyselect::starts_with(sprintf("response+%i:", ahead))) %>%
        pull()

    # Prediction matrices
    out$predict_x <- lagged_df %>%
        filter(time_value == max(time_value)) %>%
        select(tidyselect::starts_with("value")) %>%
        as.matrix()
    out$predict_geo_values <- lagged_df %>%
        filter(time_value == max(time_value)) %>%
        select(geo_value) %>% pull()

    return(out)
}
