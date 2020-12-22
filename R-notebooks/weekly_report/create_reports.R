library(rmarkdown)

source("predictions.R")
create_prediction_cards()

source("score.R")
create_score_cards("county")
create_score_cards("state")
render("covidhub_evaluation.Rmd", 
           params = list(
             score_file = "score_cards_state.rda",
             card_name = "score_cards_state"
           ),
           output_file = "state_evaluations.html")
render("covidhub_evaluation.Rmd", 
           params = list(
             score_file = "score_cards_county.rda",
             card_name = "score_cards_county"
           ),
           output_file = "county_evaluations.html")
