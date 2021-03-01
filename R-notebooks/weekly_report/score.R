library("evalcast")
library("dplyr")
library("lubridate")
library("assertthat")

create_score_cards = function(prediction_cards_filepath,
                              geo_type,
                              signal_name = NULL,
                              output_file_name = NULL,
                              weeks_back = NULL,
                              output_dir = ".",
                              err_measures = list(wis = weighted_interval_score,
                                                  ae = absolute_error,
                                                  cov_80 = interval_coverage(coverage = 0.8)),
                              use_chu_data = FALSE){
  if (is.null(weeks_back)){
    start_date = as.Date("2018-01-01")
  } else {
    start_date = today() - weeks_back * 7
  }
  assert_that(file.exists(prediction_cards_filepath),
              msg = paste("prediction_cards_filepath", predictions_cards_filepath, "not found"))
  predictions_cards = readRDS(prediction_cards_filepath)
    
  signals = (predictions_cards %>% distinct(signal))$signal
  if (is.null(signal_name)){
    assert_that(length(signals) == 1,
                msg = "If no signal is specified, prediction_cards may only have 1 signal")
    signal_name = signals
  } else {
    assert_that(signal_name %in% signals,
                msg = "signal is not in prediction_cards")
    predictions_cards = predictions_cards %>% filter(signal == signal_name)
  }
  if (is.null(output_file_name)){
    cases_sig = "confirmed_incidence_num"
    deaths_sig = "deaths_incidence_num"
    assert_that(signal_name %in% c(cases_sig, deaths_sig),
                msg = paste("If no output file is provided, signal must be in:",
                            cases_sig,
                            deaths_sig))
    if (signal_name == cases_sig){
      sig_suffix = "cases"
    } else {
      sig_suffix = "deaths"
    }
    output_file_name = file.path(output_dir,paste0("score_cards_", geo_type, "_", sig_suffix, ".rds"))
  }
  preds_to_eval = predictions_cards %>% 
    filter(target_end_date < today())
  
  if (geo_type == "state"){
    preds_to_eval = preds_to_eval %>% 
      filter(nchar(geo_value) == 2, geo_value != "us")
  } else if (geo_type == "county"){
    preds_to_eval = preds_to_eval %>% 
      filter(nchar(geo_value) == 5)
  } else if (geo_type == "nation"){
    preds_to_eval = preds_to_eval %>%
      filter(geo_value == "us")
  }
  
  #Only score forecasters with atleast 3 forecasts (i.e. more than mean and median)
  quantile_forecasts = preds_to_eval %>% 
    group_by(forecaster, forecast_date, geo_value, ahead) %>% 
    summarize(num_quantiles = n_distinct(quantile)) %>%
    filter(num_quantiles > 2) %>%
    select(-c(num_quantiles))
  preds_to_eval = semi_join(preds_to_eval, quantile_forecasts)
  if(nrow(preds_to_eval) > 0){
    score_cards = evaluate_predictions(preds_to_eval, 
                                       err_measures,
                                       backfill_buffer = 0,
                                       geo_type = geo_type)
    # filter out scores that couldn't be evaluated to try to evaluate with
    # covidHubUtils.
    na_scores = score_cards %>% filter(is.na(actual))
    score_cards = score_cards %>% filter(!is.na(actual))
  } else {
    score_cards = data.frame()
  }

  if (use_chu_data){
    if (nrow(na_scores) > 0){
      warning(msg = 
                paste("covidcast could not provide truth data for some",
                      "predictions. Attempting to use covidHubUtils instead."))
      has_jhu_signal = TRUE
      if (signal_name == "confirmed_incidence_num"){
        jhu_signal = "inc case"
      } else if (signal_name == "deaths_incidence_num"){
        jhu_signal = "inc death"
      } else if (signal_name == "deaths_cumulative_num"){
        jhu_signal = "cum death"
      } else {
        has_jhu_signal == FALSE
        warning(msg = paste("covidHubUtils cannot process provided signal:",
                            signal_name))
      }
      if(has_jhu_signal){
        na_preds = semi_join(preds_to_eval, na_scores, by = c("forecaster", "ahead", "geo_value", "forecast_date"))
        chu_truth = covidHubUtils::load_truth("JHU", jhu_signal)
        chu_truth = chu_truth %>%
          rename(actual = value) %>%
          select(-c(model,
                    target_variable,
                    location,
                    location_name,
                    population,
                    geo_type,
                    abbreviation))
        chu_scores = evaluate_predictions(na_preds, 
                                          err_measures,
                                          backfill_buffer = 0,
                                          geo_type = geo_type,
                                          side_truth = chu_truth)
        if (any(is.na(chu_scores$actual))){
          warning(msg = 
                    paste("covidHubUtils could not provide actual data for some",
                          "predictions. These predictions have not been scored."))
          chu_scores = chu_scores %>% filter(!is.na(actual))
        }
        score_cards = rbind(score_cards, chu_scores)
      }
    }
  }
  
  saveRDS(score_cards, 
       file = output_file_name, 
       compress = "xz")
}