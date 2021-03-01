library(lubridate)
library(evalcast)
library(dplyr)
library(stringr)

create_prediction_cards = function(prediction_cards_filename, weeks_back = NULL,
                                   forecasters = get_covidhub_forecaster_names(),
                                   signals = NULL){
  
  if(is.null(weeks_back)){
    start_date = as.Date("2018-01-01") # arbitrary pre-pandemic date
  } else {
    start_date = today() - 7 * weeks_back
  }
  if (is.null(forecasters)){
    forecasters = get_covidhub_forecaster_names()
  }
  
  num_forecasters = length(forecasters)
  print(str_interp("Getting forecasts for ${num_forecasters} forecasters."))
  # Get all forecast dates for these forecasters from COVID Hub
  forecast_dates = vector("list", length = length(forecasters))
  for (i in 1:length(forecasters)) {
    forecast_dates[[i]] = tryCatch({
      as_date(get_covidhub_forecast_dates(forecasters[i]))
    },
    error = function(e) cat(sprintf("%i. %s\n", i, e$message))
    )
  }
  
  forecast_dates = lapply(forecast_dates, function(date) date[date >= start_date])
  
  # Load data from previous run so we don't have to re-ingest / process it. This
  # data could end up out of date if a forecast is retrospectively updated, but in
  # that case it's no longer a true prediction. We can always restart from scratch
  # by deleting predictions_cards.rds.
  
  if (file.exists(prediction_cards_filename)) {
    print("Reading from existing prediction cards")
    predictions_cards = readRDS(file = prediction_cards_filename)
    seen_dates = predictions_cards %>% 
      distinct(forecast_date, forecaster)
    print("Existing prediction cards loaded")
  }else{
    print("No prediction cards found, will need to regenerate.")
  }
  
  # new_dates, as opposed to dates for which we already have data for a forecaster
  new_dates = list()
  
  # Now figure out "comparable" forecast dates: making a forecast on a Sunday or a 
  # Monday of the same epiweek should be comparable...
  for (i in 1:length(forecasters)) {
    given_dates = forecast_dates[[i]]
    comparable_forecast_dates = given_dates[wday(given_dates) %in% c(1, 2)]
    # ...but if forecasts were made on both Sunday and Monday, only take Monday
    comparable_forecast_dates = comparable_forecast_dates[!((comparable_forecast_dates + 1) %in% comparable_forecast_dates)]
    if(exists("seen_dates")){
      if(forecasters[[i]] %in% seen_dates$forecaster){
        seen_forecaster_dates = (seen_dates %>% 
                                   filter(forecaster == forecasters[[i]]))$forecast_date
        comparable_forecast_dates = as_date(setdiff(comparable_forecast_dates, seen_forecaster_dates))
      }
    }
    new_dates[[i]] = comparable_forecast_dates
  }
  names(new_dates) = forecasters
  
  # Now get new predictions for each forecaster
  
  predictions_cards_list = vector("list", length = length(forecasters))
  for (i in 1:length(forecasters)) {
    cat(str_interp("${i}/${num_forecasters}:${forecasters[i]} ...\n"))
    if (length(new_dates[[i]] > 0)){
      predictions_cards_list[[i]] = tryCatch({
        get_covidhub_predictions(forecasters[i], 
                                 rev(new_dates[[i]])) %>% 
          filter(ahead < 5) %>%
          filter(nchar(geo_value) %in% c(2, 5))
      },
      error = function(e) cat(e$message))
      if (!is.null(signals)){
        predictions_cards_list[[i]] = predictions_cards_list[[i]] %>%
                                        filter(signal %in% signals)
      }
    }
  }
  predictions_cards_new = bind_rows(predictions_cards_list)
  
  # Combine old and new predictions cards
  if(exists("predictions_cards")){
    predictions_cards = rbind(predictions_cards, predictions_cards_new)
  } else {
    predictions_cards = predictions_cards_new
  }
  predictions_cards = predictions_cards %>%
                        filter(forecast_date >= start_date, 
                               !is.na(target_end_date)) 

  saveRDS(predictions_cards,
          file = prediction_cards_filename, 
          compress = "xz")
}