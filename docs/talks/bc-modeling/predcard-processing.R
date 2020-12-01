rm(list=ls())
library(evalcast)
library(tidyverse)
library(lubridate)


forecast_dates <- seq(lubridate::ymd("2020-08-10"),
                      lubridate::ymd("2020-10-26"), by = 7)
cmu <- get_covidhub_predictions("CMU-TimeSeries", 
                                forecast_dates,
                                ahead = 1:4,
                                geo_type="state",
                                response_signal="deaths_incidence_num")
CH_baseline <- get_covidhub_predictions("COVIDhub-baseline",
                                         forecast_dates, 
                                         ahead = 1:4,
                                         geo_type = "state", 
                                         response_signal = "deaths_incidence_num")
CH_ensemble <- get_covidhub_predictions("COVIDhub-ensemble", 
                                         forecast_dates, 
                                         ahead = 1:4,
                                         geo_type = "state", 
                                         response_signal = "deaths_incidence_num")
#scorecard_CH_baseline <- evaluate_predictions(CH_baseline)[[1]]
#scorecard_CH_ensemble <- evaluate_predictions(CH_ensemble)[[1]]
#scorecards <- list(scorecard_CH_baseline, scorecard_CH_ensemble)

predcards_to_df <- function(predictions_cards){
  metadata <- grab_all_pc_attributes(predictions_cards)
  pcard_list <- list()
  for(i in 1:length(predictions_cards)){
    pcard <-  predcard_to_df(predictions_cards[[i]])
    pcard_list[[i]] <- bind_cols(pcard, metadata[i,])
  }
  bind_rows(pcard_list)
}

grab_all_pc_attributes <- function(predictions_cards) {
  # variation of evalcast function in manage_cards.R
  # using do.call in the next line to keep these of class Date:
  # https://stackoverflow.com/questions/15659783/why-does-unlist-kill-dates-in-r
  forecast_dates <- do.call("c",
                            predictions_cards %>%
                              map(~ attr(.x, "forecast_date")))
  signals = evalcast:::all_attr(predictions_cards, "signals")
  tibble(
    forecast_dates = forecast_dates,
    name_of_forecaster = unlist(evalcast:::all_attr(predictions_cards, "name_of_forecaster")),
    data_source = unlist(signals %>% map(function(x) x$data_source)),
    signal = unlist(signals %>% map(function(x) x$signal)),
    ahead = as.numeric(evalcast:::all_attr(predictions_cards, "ahead")),
    incidence_period = unlist(evalcast:::all_attr(predictions_cards, "incidence_period")),
    geo_type = unlist(evalcast:::all_attr(predictions_cards, "geo_type"))
  )
}


predcard_to_df <- function(predcard){
  listy = predcard$forecast_distribution
  names(listy) = predcard$location
  long_df = bind_rows(listy, .id = "location")
  long_df
}

which_loc = "12"

long_cmu = predcards_to_df(cmu)
long_ensemble = predcards_to_df(CH_ensemble)
long_baseline = predcards_to_df(CH_baseline)
cmu_loc = long_cmu %>%
  mutate(pred_date = forecast_dates + duration(ahead, "weeks")-duration(2,"days")) %>%
  filter(probs %in% c(.1,.5,.9), location==which_loc) %>%
  pivot_wider(names_from = probs, values_from = quantiles) %>%
  group_by(forecast_dates)
ens_loc = long_ensemble %>%
  mutate(pred_date = forecast_dates + duration(ahead, "weeks")-duration(2,"days")) %>%
  filter(probs %in% c(.1,.5,.9), location==which_loc) %>%
  pivot_wider(names_from = probs, values_from = quantiles) %>%
  group_by(forecast_dates)
base_loc = long_baseline %>%
  mutate(pred_date = forecast_dates + duration(ahead, "weeks")-duration(2,"days")) %>%
  filter(probs %in% c(.1,.5,.9), location==which_loc) %>%
  pivot_wider(names_from = probs, values_from = quantiles) %>%
  group_by(forecast_dates)

# hack while API JHU is down
truth_csv = read_csv("https://raw.githubusercontent.com/reichlab/covid19-forecast-hub/master/data-truth/truth-Incident%20Deaths.csv")
truth_csv = truth_csv %>%
  group_by(location) %>%
  mutate(lag_sum = RcppRoll::roll_sumr(value, 7L, na.rm = TRUE),
         dofw = wday(ymd(date)))
loc_truth = truth_csv %>% 
  filter(dofw==7, location == which_loc, date < ymd("2020-10-24"))

# library(covidcast)
# 
# observed <- evalcast:::get_target_response(
#   tibble(data_source="usa-facts", signal="deaths_incidence_num"),
#   forecast_dates,
#   "epiweek",
#   1L,
#   "state")
# 
# loc_observed = filter(observed, location==which_loc)
# 

save.image(file="predictions.Rdata")
