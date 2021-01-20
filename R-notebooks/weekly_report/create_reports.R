library(rmarkdown)

source("predictions.R")
create_prediction_cards()

source("score.R")
create_score_cards("county")
create_score_cards("state")
render("covidhub_evaluation.Rmd", 
           params = list(score_file = "score_cards_state.rds"),
           output_file = "state_evaluations.html")
render("covidhub_evaluation.Rmd", 
           params = list(score_file = "score_cards_county.rds"),
           output_file = "county_evaluations.html")
