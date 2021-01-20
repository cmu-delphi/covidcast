library(lubridate)
library(evalcast)
library(dplyr)

# TODO: Use `get_covidhub_forecaster_names()` instead of listing forecasters
create_prediction_cards = function(){
  start_date = today() - 12 * 7 # last 12 weeks
  
  forecasters = c("CMU-TimeSeries",
                  "CovidAnalytics-DELPHI",
                  "CU-select",
                  #   "Google_Harvard-CPF", Excluded for now. Doesn't have quantiles for all forecasts
                  "GT-DeepCOVID", 
                  "IEM_MED-CovidProject",
                  "IowaStateLW-STEM",
                  "IHME-CurveFit", 
                  "JHUAPL-Bucky",
                  "JHU_IDD-CovidSP",
                  "JHU_UNC_GAS-StatMechPool",
                  "Karlen-pypm",
                  "LANL-GrowthRate", 
                  "LNQ-ens1",
                  "MOBS-GLEAM_COVID",
                  "OliverWyman-Navigator", 
                  "OneQuietNight-ML",
                  "PandemicCentral-USCounty",
                  "UCLA-SuEIR",
                  "UMass-MechBayes",
                  "UT-Mobility",
                  "UVA-Ensemble",
                  "Yu_Group-CLEP",
                  "YYG-ParamSearch", 
                  "COVIDhub-ensemble", 
                  "COVIDhub-baseline")
  
  
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
  
  if (file.exists("predictions_cards.rds")) {
    predictions_cards = readRDS(file = "predictions_cards.rds")
  }
  if(exists("predictions_cards")){
    seen_dates = predictions_cards %>% 
      distinct(forecast_date, forecaster)
  }
  
  # Now figure out "comparable" forecast dates: making a forecast on a Sunday or a 
  # Monday of the same epiweek should be comparable.
  
  forecast_dates_cmu = forecast_dates[[which(forecasters == "CMU-TimeSeries")]]
  
  # new_dates, as opposed to dates for which we already have data for a forecaster
  new_dates = list()
  for (i in 1:length(forecasters)) {
    given_dates = forecast_dates[[i]]
    # If the dates match exactly, or the given date falls on a Sunday and the
    # CMU date falls on a Monday of the same epiweek, then call it comparable...
    comparable_forecast_dates = given_dates[(given_dates %in% forecast_dates_cmu | 
                                               ((given_dates + 1) %in% forecast_dates_cmu) &
                                               wday(given_dates) == 1)]
    
    # ...but if there is an exact match on dates, ignore predictions made on the
    # previous day
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
  deaths_sig = "deaths_incidence_num"
  cases_sig = "confirmed_incidence_num"
  for (i in 1:length(forecasters)) {
    cat(forecasters[i], "...\n")
    if (length(new_dates[[i]] > 0)){
      predictions_cards_list[[i]] = tryCatch({
        get_covidhub_predictions(forecasters[i], 
                                 rev(new_dates[[i]])) %>% 
          filter(ahead < 5) %>% 
          filter((nchar(geo_value) == 2 & signal == deaths_sig) |
                   (nchar(geo_value) == 5 & signal == cases_sig))
      },
      error = function(e) cat(e$message))
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
                        filter(forecast_date >= start_date)
  
  # Hack: must change the response data source to be USAFacts, as JHU-CSSE data is
  # currently unstable. **TODO**: we shouldn't require `evaluate_predictions()` to 
  # have the response match what's in the forecaster. If I train my forecaster on
  # (say) JHU-CSSE data, then I should be able to evaluate it on USAFacts data. 
  
  predictions_cards$data_source = "usa-facts"
  saveRDS(predictions_cards,
          file = "predictions_cards.rds", 
          compress = "xz")
}