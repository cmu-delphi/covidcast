library("evalcast")
library("dplyr")
library("lubridate")

create_score_cards = function(geo_type, output_file_name = NULL, score_cards_name = NULL){
  if (!exists("predictions_cards")){
    load("predictions_cards.rda")
  }
  if (is.null(output_file_name)){
    output_file_name = paste0("score_cards_", geo_type, ".rda")
  }
  if (is.null(score_cards_name)){
    score_cards_name = paste0("score_cards_", geo_type)
  }
  err_measures = list(wis = weighted_interval_score, ae = absolute_error,
                      cov_80 = interval_coverage(coverage = 0.8)) 
  preds_to_eval = predictions_cards %>% 
    filter(target_end_date < today())
  
  if (geo_type == "state"){
    preds_to_eval = preds_to_eval %>% 
      filter(nchar(geo_value) == 2)
  } else if (geo_type == "county"){
    preds_to_eval = preds_to_eval %>% 
      filter(nchar(geo_value) == 5)
  }
  if (file.exists(output_file_name)) {
    load(output_file_name)
  }
  if (exists(score_cards_name)){
    score_cards = get(score_cards_name)
  }
  if(exists("score_cards")){
    preds_to_eval = anti_join(preds_to_eval, 
                              score_cards, 
                              by = c("ahead", "forecaster", "forecast_date"))
  }
  
  #Only score forecasters with atleast 3 forecasts (i.e. more than mean and median)
  quantile_forecasters = preds_to_eval %>% 
    group_by(forecaster) %>% 
    summarize(num_quantiles = n_distinct(quantile)) %>%
    filter(num_quantiles > 2) %>%
    pull(forecaster)
  preds_to_eval = preds_to_eval %>%
    filter(forecaster %in% quantile_forecasters)
  if(nrow(preds_to_eval) > 0){
    score_cards_new = evaluate_predictions(preds_to_eval, 
                                           err_measures,
                                           backfill_buffer = 0)
  } else {
    score_cards_new = data.frame()
  }
  
  if(exists("score_cards")){
    score_cards = rbind(score_cards, score_cards_new)
  } else {
    score_cards = score_cards_new
  }
  assign(score_cards_name, score_cards)
  save(list = score_cards_name, 
       file = output_file_name, 
       compress = "xz")
}