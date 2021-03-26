# In the signals tibble, required by `get_predictions()`,
# start_day` and `as_of` can possibly be functions
# of the forecast_date. Here we evaluate those functions as well as creating
# default values for `geo_values` and `geo_type` if necessary
signal_listcols <- function(signals, forecast_date) {
  
  signal_names <- colnames(signals)
  num_signals <- nrow(signals)
  # start_day should be NULL or date. We handle date/chr/fun(forecast_date) input
  if ("start_day" %in% signal_names && is.list(signals$start_day)) {
    start_days <- Date(num_signals)
    for (i in seq_len(num_signals)) {
      if (is.function(signals$start_day[[i]])) {
        start_days[i] <- as.Date(signals$start_day[[i]](forecast_date))
      }
    }
    signals$start_day <- start_days
  }
  
  if ("as_of" %in% signal_names) {
    if (is.list(signals$as_of)) {
      as_of <- Date(num_signals)
      for (i in seq_len(num_signals)) {
        if (is.function(signals$as_of[[i]])) {
          as_of[i] <- as.Date(signals$as_of[[i]](forecast_date))
        }
      }
      signals$as_of <- as_of
    }
  } else {
    signals$as_of <- forecast_date
  }
  
  if (! "geo_values" %in% signal_names) signals$geo_values <- "*"
  if (! "geo_type" %in% signal_names) signals$geo_type <- "county"
  
  return(signals)
  
}
