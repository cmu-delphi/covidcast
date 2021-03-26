#' Simple example forecaster that forecasts confirmed cases.
#'
#' @param df_list List of downloaded covidcast signals.
#' @param forecast_date Date from which the forecast should be made.
#' @param ahead Vector of epiweeks ahead to predict.
#' @param training_window_size Number of days prior to `forecast_date` to use for training.
#' @param lags Vector of lags or list of vectors of lags to apply to `df_list`.  See documentation
#'     of `dt` argument to `covidcast::aggregate_signals()` for details.
#' @param quantiles Vector of quantile values at which to predict
#'
#' preds <- evalcast::get_predictions(modeltools::example_forecaster,
#'                                    "example",
#'                                    tibble(
#'                                      data_source = "jhu-csse",
#'                                      signal = "confirmed_incidence_num",
#'                                      geo_type = "state",
#'                                      start_date = as.Date("2021-01-01"),
#'                                    ),
#'                                    as.Date("2021-03-01"),
#'                                    "epiweek")
#'
#' @importFrom dplyr mutate bind_cols bind_rows
#' @importFrom tidyr pivot_longer
#'
#' @export
example_forecaster <- function(df_list,
                               forecast_date,
                               ahead = 1:4,
                               training_window_size = 14,
                               lags = seq(0, -1*training_window_size, -7),
                               quantiles = modeltools::covidhub_probs) {
    covariates <- covidcast::aggregate_signals(df_list, dt = lags, format = "wide")
    covariates_with_response <- add_response_columns(covariates,
                                                     "jhu-csse_confirmed_incidence_num",
                                                     forecast_date,
                                                     "epiweek",
                                                     ahead)

    results <- list()
    for (a in ahead) {
        mats <- create_train_and_predict_matrices(covariates_with_response, a, training_window_size)
        model <- quantgen::quantile_lasso(mats$train_x, mats$train_y, quantiles, lambda = 0)
        predictions <- quantgen:::predict.quantile_genlasso(model, mats$predict_x)

        colnames(predictions) <- quantiles
        predict_df <- bind_cols(geo_value = mats$predict_geo_values,
                                predictions) %>%
            pivot_longer(cols = -geo_value,
                         names_to = "quantile",
                         values_to = "value") %>%
            mutate(ahead = a)

        results[[a]] <- predict_df
    }
    return(bind_rows(results))
}
