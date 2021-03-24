#' Simple example forecaster that forecasts confirmed cases.
#'
#' @param df_list List of downloaded covidcast signals
#' @param forecast_date Date from which the forecast should be made.
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
#' @export
example_forecaster <- function(df_list, forecast_date) {
    ahead <- 1:4
    lags <- c(0, -1, -2, -3, -7, -14)
    quantiles <- modeltools::covidhub_probs()

    covariates <- covidcast::aggregate_signals(df_list, dt = lags, format = "wide")
    covariates_with_response <- add_response_columns(covariates,
                                                     "jhu-csse_confirmed_incidence_num",
                                                     forecast_date,
                                                     "epiweek",
                                                     ahead)

    results <- list()
    for (a in ahead) {
        mats <- create_train_and_predict_matrices(covariates_with_response, a, 14)
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
