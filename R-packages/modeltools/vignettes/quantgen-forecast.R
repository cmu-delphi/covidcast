library(covidcast)
library(evalcast)
library(modeltools)
library(dplyr)

## Setup 

# What are we forecasting?
response_source <- "jhu-csse"
response_signal <- "confirmed_7dav_incidence_prop"
incidence_period <- "day"
ahead <- 1:21
geo_type <- "state" 
forecast_dates <- seq(as.Date("2020-07-01"), as.Date("2020-08-15"), by = "day")

# Some quantgen parameters 
n <- 21               # Training set size (in days) 
lags <- c(0, 7, 14)   # Lags (in days) for features
lp_solver <- "gurobi"
sort <- TRUE
nonneg <- TRUE

# Important: functions to considerably speed up data fetching steps. Only pull 
# recent data for each forecast date, depending on the training set size (and 
# other parameters for quantgen)

# Due to inevitable latency in the signals, if we look at training sets defined
# by taking all n days before the forecast date, then we don't actually have n
# time points worth of data available to us (we're always fetching data "as of"
# the forecast date). As here n = max(ahead) = 28, the baseline forecaster can't 
# actually produce 28-days-ahead forecasts with the most recent n time points
# worth of data, since it would need to calculate differences of signal values
# spaced 28 days apart. This is why we've included an extra 4 days in the data
# we make available to the baseline forecaster
start_day_baseline <- function(forecast_date) {
  return(as.Date(forecast_date) - n - 4 + 1)
}

# For the autoregressive models fit by quantgen, we need to ensure that we pull
# enough training data so that 1. we actually have the response (defined by some
# number of days ahead into the future) and 2. we have the lagged features 
start_day_quantgen <- function(forecast_date) {
  return(as.Date(forecast_date) - max(ahead) - n - max(lags) + 1)
}

## Produce forecasts

# Produce forecasts using a baseline forecaster
pred_baseline <- get_predictions(
  forecaster = baseline_forecaster, 
  name_of_forecaster = "Baseline",
  signals = tibble::tibble(
                      data_source = response_source, 
                      signal = response_signal,
                      start_day = list(start_day_baseline)),
  forecast_dates = forecast_dates, 
  incidence_period = incidence_period, 
  ahead = ahead, geo_type = geo_type, 
  signal_aggregation = "long") 

# Quantile autoregression with 3 lags, or QAR3
pred_quantgen1 <- get_predictions(
  forecaster = quantgen_forecaster, 
  name_of_forecaster = "QAR3",
  signals = tibble::tibble(
                      data_source = response_source, 
                      signal = response_signal,
                      start_day = list(start_day_quantgen)),
  forecast_dates = forecast_dates, 
  incidence_period = incidence_period, 
  ahead = ahead, geo_type = geo_type, 
  signal_aggregation = "list", 
  n = n, lags = lags, lambda = 0, # Just do quantile regression 
  lp_solver = lp_solver, sort = sort, nonneg = nonneg)

# Quantile autoregression with 3 lags, plus 3 lags of the CLI-in-community
# signal from Delphi's symptom survey, or QAR3 + CLI3  
pred_quantgen2 <- get_predictions(
  forecaster = quantgen_forecaster, 
  name_of_forecaster = "QAR3 + CLI3",
  signals = tibble::tibble(
                      data_source = c(response_source, "fb-survey"),
                      signal = c(response_signal, "smoothed_hh_cmnty_cli"),
                      start_day = list(start_day_quantgen)),
  forecast_dates = forecast_dates, 
  incidence_period = incidence_period, 
  ahead = ahead, geo_type = geo_type, 
  signal_aggregation = "list", 
  n = n, lags = lags, lambda = 0, # Just do quantile regression
  lp_solver = lp_solver, sort = sort, nonneg = nonneg)

## Evaluate forecasts

# Now "evaluate" all of these predictions. In quotes because we pass a fake
# evaluation function so we can do it ourselves later. This is because I'd
# rather see results compressed down to have one row per forecast task (not one
# row per forecasted quantile value) and it's easier to use dplyr::summarize()
results <- evaluate_predictions(
  predictions_cards = rbind(pred_baseline, pred_quantgen1, pred_quantgen2),
  err_measures = list(temp = function(quantile, value, actual) NA)) %>%
  select(geo_value, ahead, quantile, forecaster, forecast_date, target_end_date,
         value, actual)

# Overwrite evalcast::absolute_error() and evalcast::weighted_interval_score(), 
# because I was having some problems with them, see issues #392 and #393 on the
# cmu-delphi/covidcast repo. (This might have been fixed now but I'm still just 
# leaving these function definitions in place to be safe)
absolute_error <- function(tau, value, actual) {
  return(abs(actual - value)[tau == 0.5])
}

weighted_interval_score <- function(tau, value, actual) {
  return((absolute_error(tau, value, actual) / 2 +
         sum(pmax(tau * (actual - value),
         (tau - 1) * (actual - value), na.rm = TRUE))) /
         (length(tau) + 1) * 2)
}

# Do the evaluations ourselves
evals <- results %>%
  filter(!is.na(quantile)) %>% # NAs are problematic for simple my functions
  group_by(geo_value, ahead, forecaster, forecast_date, target_end_date) %>% 
  summarize(ae = absolute_error(quantile, value, actual),
            wis = weighted_interval_score(quantile, value, actual)) %>%
    ungroup()
            
# Save everything to file
save(list = ls(), file = "quantgen-forecast.rda", compress = "xz")
