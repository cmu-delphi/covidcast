library(rmarkdown)

source("predictions.R")
preds_filename = "predictions_cards.rds"
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

create_prediction_cards(prediction_cards_filename = preds_filename,
                        weeks_back = 12,
                        forecasters = forecasters,
                        signals = signals)

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
           params = list(score_file = "score_cards_state_deaths.rds"),
           output_file = "state_evaluations.html")
render("covidhub_evaluation.Rmd", 
           params = list(score_file = "score_cards_county_cases.rds"),
           output_file = "county_evaluations.html")
