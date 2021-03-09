#' Simple quantile autoregressive forecaster template based on `quantgen` 
#' (unpenalized)
#'
#' A simple quantile autoregressive forecaster template based on `quantgen`, to 
#' be used with `evalcast`, via [evalcast::get_predictions()]. This is a 
#' simplified, unpenalized version of [modeltools::quantgen_forecaster()] 
#' that is meant for illustration purposes only. Only works with 
#' `incidence_period = "day"`.
#' 
#' @param df Data frame of signal values to use for forecasting, of the format
#'   that is returned by [covidcast::covidcast_signals()].
#' @param forecast_date Date object or string of the form "YYYY-MM-DD",
#'   indicating the date on which forecasts will be made. For example, if
#'   `forecast_date = "2020-05-11"` and `ahead = 3`, then, forecasts 
#'   would be made for "2020-05-14". 
#' @param ahead Vector of ahead values, indicating how many days ahead
#'   to forecast. E.g. `ahead = 1` means the day after forecast date.
#' @param tau Vector of quantile levels for the probabilistic forecast. If not
#'   specified, defaults to the levels required by the COVID Forecast Hub.
#' @param n Size of the local training window (in days) to use. For example, 
#'   if `n = 14`, then to make a 1-day-ahead forecast on December 15, we 
#'   train on data from December 1 to December 14.
#' @param lags Vector of lag values to use as features in the autoregressive
#'   model. For example, setting `lags = c(0, 7, 14)` means we use the current 
#'   value of each signal, as well as the values 7 and 14 days ago, as the 
#'   features. Note that `lags` can also be a list of vectors of lag
#'   values, this list having the same length as `df`, 
#'   in order to apply a different set of shifts to each signal. 
#'   Default is 0, which means no additional lags (only current values) for each
#'   signal.
#' @param verbose Should progress be printed out to the console? Default is
#'   `FALSE`. 
#' @param ... Additional arguments.
#' 
#' @return Data frame with columns `ahead`, `geo_value`, `quantile`, and
#'   `value`. The `quantile` column gives the probabilities associated with
#'   quantile forecasts for that location and ahead. 
#' 
#' @importFrom dplyr filter select pull summarize between bind_cols
#' @importFrom tidyr pivot_longer
#' @export
quantreg_forecaster <- function(df, forecast_date, ahead,
                                tau = modeltools::covidhub_probs,
                                n = 28, lags = 0,
                                verbose = FALSE, ...) {
  #####
  # STEP 1: CONVERT RAW DATA INTO TRANSFORMED DATA
  # This should probably all go into a helper function
  #####
  if (!is.list(df)) df <- list(df)  # needed if df is of a single signal
  
  # Check lags vector or list
  if (any(unlist(lags) < 0)) stop("All lags must be nonnegative.")
  if (!is.list(lags)) lags <- rep(list(lags), length(df))
  else if (length(lags) != length(df)) {
    stop(paste("If `lags` is a list, it must have length equal to the number",
               "of signals."))
  }
  
  # Define dt by flipping the sign of lags, include dt = +ahead as a response 
  # shift, for each ahead value, for convenience later 
  dt <- lapply(lags, "-")
  dt[[1]] <- c(dt[[1]], ahead)
    
  # Append shifts, and aggregate into wide format
  df_wide <- covidcast::aggregate_signals(df, dt = dt, format = "wide")
  
  # Separate out into feature data frame
  df_features <- df_wide %>%
    select(geo_value, time_value, tidyselect::matches("^value(\\+0|-)"))
  feature_end_date <- df_features %>%
    summarize(max(time_value)) %>% pull()
  #####
  # END STEP 1
  #####
  
  # Identify params for quantgen training and prediction functions
  params <- list(...)
  params$tau <- tau
  params$lambda <- 0  # no penalization
  train_names <- names(as.list(args(quantgen::quantile_lasso)))
  predict_names <- names(as.list(args(quantgen:::predict.quantile_genlasso)))
  train_params <- params[names(params) %in% train_names]
  predict_params <- params[names(params) %in% predict_names]
  
  # Test objects that remain invariant over ahead values
  test_geo_value <- df_features %>%
    filter(time_value == max(time_value)) %>%
    select(geo_value) %>% pull()
  newx <- df_features %>%
    filter(time_value == max(time_value)) %>%
    select(-c(geo_value, time_value)) %>% as.matrix()
    
  # Loop over ahead values, fit model, make predictions 
  result <- vector(mode = "list", length = length(ahead))
  for (i in 1:length(ahead)) {
    a <- ahead[i]
    if (verbose) cat(sprintf("%s%i", ifelse(i == 1, "\nahead = ", ", "), a))
    
    #####
    # STEP 2A: Pull out the relevant training x and y
    #####
    # Training end date    
    response_end_date <- df_wide %>%
      select(time_value, tidyselect::starts_with(sprintf("value+%i:", a))) %>%
      tidyr::drop_na() %>%
      summarize(max(time_value)) %>% pull()
    train_end_date = min(feature_end_date, response_end_date)

    # Training x and y
    x <- df_features %>%
      filter(between(time_value,
                     train_end_date - n + 1,
                     train_end_date)) %>%
      select(-c(geo_value, time_value)) %>% as.matrix()
    y <- df_wide %>%
      filter(between(time_value,
                     train_end_date - n + 1,
                     train_end_date)) %>%
      select(tidyselect::starts_with(sprintf("value+%i:", a))) %>% pull()
    
    #####
    # STEP 2B: Fit the model (unpenalized quantile regression)
    #####
    # Add training x and y to training params list, fit model
    train_params$x <- x
    train_params$y <- y
    train_obj <- do.call(quantgen::quantile_lasso, train_params)

    #####
    # STEP 3: Make final predictions
    #####
    # Add training object and newx to test params list, make predictions
    predict_params$object <- train_obj
    predict_params$newx <- newx
    predict_mat <- do.call(quantgen:::predict.quantile_genlasso, predict_params)
    
    #####
    # POST-STEP 3: this code should probably go into a helper function
    #####
    # Do some wrangling to get it into evalcast "long" format
    colnames(predict_mat) <- tau
    predict_df <- bind_cols(geo_value = test_geo_value,
                           predict_mat) %>%
      pivot_longer(cols = -geo_value,
                   names_to = "quantile",
                   values_to = "value") %>%
      mutate(ahead = a)

    result[[i]] <- predict_df
  }
  if (verbose) cat("\n")

  # Collapse predictions into one big data frame, and return
  return(do.call(rbind, result))
}
