library(rmarkdown)

preds_filename = "predictions_cards.rds"
start_date = today() - 12 * 7
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

signals = c("deaths_incidence_num", "confirmed_incidence_num")

date_filter <- function(date_list){
  date_filter_helper <- function(dates){
    end_week_dates = dates[wday(dates) %in% c(1, 2)]
    ret_dates = end_week_dates[!(end_week_dates + 1) %in% end_week_dates]
    return(ret_dates) 
  }
  return(lapply(date_list, date_filter_helper))
}

preds = readRDS(preds_filename)
preds = preds %>% filter(forecast_date >= start_date)

preds = get_covidhub_predictions(forecasters,
                               signal = signals,
                               predictions_cards = preds,
                               start_date = start_date,
                               date_filtering_function = date_filter)

saveRDS(predictions_cards,
        file = prediction_cards_filename, 
        compress = "xz")

source("score.R")
create_score_cards(prediction_cards_filepath = preds_filename,
                   signal_name = "deaths_incidence_num",
                   geo_type = "state",
                   weeks_back = 12)
create_score_cards(prediction_cards_filepath = preds_filename,
                   signal_name = "confirmed_incidence_num",
                   geo_type = "county",
                   weeks_back = 12)
render("covidhub_evaluation.Rmd", 
           params = list(score_file = "score_cards_state_deaths.rds",
                         highlight_forecasters = c("CMU-TimeSeries", "COVIDhub-baseline", "COVIDhub-ensemble"),
                         signal = "deaths_incidence_num"),
           output_file = "state_evaluations.html")
render("covidhub_evaluation.Rmd", 
           params = list(score_file = "score_cards_county_cases.rds",
                         highlight_forecasters = c("CMU-TimeSeries", "COVIDhub-baseline", "COVIDhub-ensemble"),
                         signal = "confirmed_incidence_num"),
           output_file = "county_evaluations.html")
