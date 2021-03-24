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
        print(predictions)
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