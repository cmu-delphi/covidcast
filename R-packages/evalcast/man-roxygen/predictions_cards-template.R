#' @return Long data frame of forecasts with a class of `predictions_cards`.
#'   The first 4 columns are the same as those returned by the forecaster. The
#'   remainder specify the prediction task, 10 columns in total: 
#'   `ahead`, `geo_value`, `quantile`, `value`, `forecaster`, `forecast_date`,
#'   `data_source`, `signal`, `target_end_date`, and `incidence_period`. Here
#'   `data_source` and `signal` correspond to the response variable only.
