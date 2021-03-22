# In the signals tibble, required by `get_predictions()`,
# start_day` and `as_of` can possibly be functions
# of the forecast_date. Here we evaluate those functions as well as creating
# default values for `geo_values` and `geo_type` if necessary
signal_listcols <- function(signals, forecast_date) {
  
  cn <- colnames(signals)
  ns <- nrow(signals)
  # start_day should be NULL or date. We handle date/chr/fun(forecast_date) input
  if ("start_day" %in% cn && is.list(signals$start_day)) {
    std <- Date(ns)
    for (i in 1:ns) {
      if (is.function(signals$start_day[[i]])) {
        std[i] <- as.Date(signals$start_day[[i]](forecast_date))
      }
    }
    signals$start_day <- std
  }
  
  if (! "as_of" %in% cn) {
    signals$as_of <- forecast_date
  } else {
    if (is.list(signals$as_of)) {
      as_of <- Date(ns)
      for (i in 1:length(signals$as_of)) {
        if (is.function(signals$as_of[[i]])) {
          as_of[i] <- as.Date(signals$as_of[[i]](forecast_date))
        }
      }
      signals$as_of <- as_of
    }
  }
  
  if (! "geo_values" %in% cn) signals$geo_values <- "*"
  if (! "geo_type" %in% cn) signals$geo_type <- "county"
  
  return(signals)
  
}
