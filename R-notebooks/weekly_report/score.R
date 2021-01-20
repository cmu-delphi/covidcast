library("evalcast")
library("dplyr")
library("lubridate")

create_score_cards = function(geo_type, output_file_name = NULL){
  start_date = today() - 12 * 7 # last 12 weeks
  if (!exists("predictions_cards")){
    predictions_cards = readRDS("predictions_cards.rds")
  }
  if (is.null(output_file_name)){
    output_file_name = paste0("score_cards_", geo_type, ".rds")
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
    score_cards = readRDS(output_file_name)
  }
  if(exists("score_cards")){
    preds_to_eval = anti_join(preds_to_eval, 
                              score_cards, 
                              by = c("ahead", "forecaster", "forecast_date"))
  }
  
  #Only score forecasters with atleast 3 forecasts (i.e. more than mean and median)
  quantile_forecasts = preds_to_eval %>% 
    group_by(forecaster, forecast_date, geo_value, ahead) %>% 
    summarize(num_quantiles = n_distinct(quantile)) %>%
    filter(num_quantiles > 2) %>%
    select(-c(num_quantiles))
  preds_to_eval = semi_join(preds_to_eval, quantile_forecasts)
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
  score_cards = score_cards %>% filter(forecast_date >= start_date)
  
  saveRDS(score_cards, 
       file = output_file_name, 
       compress = "xz")
}