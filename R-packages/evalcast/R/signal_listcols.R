signal_listcols <- function(signals, forecast_date) {
  
  cn <- colnames(signals)
  # start_day should be NULL or date. We handle date/chr/fun(forecast_date) input
  if ("start_day" %in% cn && is.list(signals$start_day)) {
    for (i in 1:length(signals$start_day)) {
      if (is.function(signals$start_day[[i]])) {
        signals$start_day[[i]] <- signals$start_day[[i]](forecast_date)
      }
    }
    signals$start_day <- sapply(signals$start_day, as.character)
  }
  
  if ("as_of" %in% cn && is.list(signals$as_of)) {
    for (i in 1:length(signals$as_of)) {
      if (is.function(signals$as_of[[i]])) {
        signals$as_of[[i]] <- signals$as_of[[i]](forecast_date)
      }
    }
    signals$as_of <- sapply(signals$as_of, as.character)
  }
  
  if (! "geo_values" %in% cn) signals$geo_values = "*"
  if (! "geo_type" %in% cn) signals$geo_type = "county"
  
  return(signals)
  
}