forecasters = c("CMU-TimeSeries", 
                "YYG-ParamSearch", 
                "UMass-MechBayes", 
                "GT-DeepCOVID", 
                "IHME-CurveFit", 
                "LANL-GrowthRate", 
                "UCLA-SuEIR", 
                "MOBS-GLEAM_COVID", 
                "UT-Mobility", 
                "OliverWyman-Navigator", 
                "JHU_IDD-CovidSP", 
                "CovidAnalytics-DELPHI", 
                #   "Google_Harvard-CPF", Excluded for now. Doesn't have quantiles for all forecasts
                "Yu_Group-CLEP", 
                "COVIDhub-ensemble", 
                "COVIDhub-baseline")

# First fetch forecast dates for a bunch of forecasters. **TODO**:
# we should do this more comprehensively. Pull all forecasters that have enough 
# submissions to make useful comparisons. To do this, we can expose the function 
# `get_covidhub_forecaster_names()`.

# Get all forecast dates for these forecasters from COVID Hub
forecast_dates = vector("list", length = length(forecasters))
for (i in 1:length(forecasters)) {
  forecast_dates[[i]] = tryCatch({
    as_date(get_forecast_dates(forecasters[i]))
  },
  error = function(e) cat(sprintf("%i. %s\n", i, e$message))
  )
}

# Load data from previous run so we don't have to reingest / process it. This
# data could end up out of date if a forecast is retrospectively updated, but in
# that case it's no longer a true prediction. We can always restart from scratch
# by deleting predictions_cards.rda.

if (file.exists("predictions_cards.rda")) {
  load(file = "predictions_cards.rda")
}
if(exists("predictions_cards")){
  seen_dates = predictions_cards %>% 
    distinct(forecast_date, forecaster)
}

# Now figure out "comparable" forecast dates: making a forecast on a Sunday or a 
# Monday of the same epiweek should be comparable. **TODO**: we should switch over 
# to using the Zoltar API, and soon it should have an "as of" parameter, so then 
# we shouldn't need to do this.

forecast_dates_cmu = forecast_dates[[1]]
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

# Now get predictions for each forecaster, looping over forecast dates from the
# CMU-TimeSeries model. **TODO**: this part is very very slow. Maybe (hopefully) 
# by changing to use the Zoltar API, this will be much faster.

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

# Discard any forecasters without data
forecasters = unique(as.data.frame(predictions_cards)$forecaster)

# On reflection: part of the problem here is that `get_covidhub_predictions()` 
# downloads *all* predictions from COVID Hub and *then* filters them as needed. 
# This makes the above extra slow because we end up downloading state and county 
# forecasts, when we just want state forecasts. **TODO**: even before switching 
# over to use the Zoltar API, we should redesign `get_covidhub_predictions()` so 
# that it fetches from GitHub only the forecasts we specify, if possible.

# Hack: must change the response data source to be USAFacts, as JHU-CSSE data is
# currently unstable. **TODO**: we shouldn't require `evaluate_predictions()` to 
# have the response match what's in the forecaster. If I train my forecaster on
# (say) JHU-CSSE data, then I should be able to evaluate it on USAFacts data. 

predictions_cards$data_source = "usa-facts"
save(list = c("predictions_cards"), 
     file = "predictions_cards.rda", 
     compress = "xz")