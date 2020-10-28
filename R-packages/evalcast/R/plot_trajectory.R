#' @import ggplot2
plot_trajectory <- function(list_of_predictions_cards,
                            first_day = "2020-07-01",
                            last_day = NULL,
                            alpha = .2)
{
  # make sure predictions cards are for the same forecasting task (except ahead)
  forecast_date <- unique_attr(list_of_predictions_cards, "forecast_date")
  response <- unique_attr(list_of_predictions_cards,"signals")
  incidence_period <- unique_attr(list_of_predictions_cards,"incidence_period")
  geo_type <- unique_attr(list_of_predictions_cards, "geo_type")
  assertthat::assert_that(incidence_period %in% c("day","epiweek"))
  assertthat::assert_that(geo_type %in% c("county","state"))
  
  
  # predicted quantiles to plot
  plot_probs <- c(.5 - (1 - alpha)/2,.5,.5 + (1 - alpha)/2 )
  preds_df <- list_of_predictions_cards %>% 
    map_dfr(function(predictions_card){
      predictions_card %>%
      unnest(forecast_distribution) %>%
      mutate(forecaster_name = attributes(predictions_card)$name_of_forecaster,
             ahead = attributes(predictions_card)$ahead,
             prob_type = case_when(
               abs(probs - plot_probs[1]) <= 1e-8 ~ "lower",
               abs(probs - plot_probs[2]) <= 1e-8 ~ "point",
               abs(probs - plot_probs[3]) <= 1e-8 ~ "upper"
             )) %>%
      filter(!is.na(prob_type)) %>%
      mutate(target_period = get_target_period_num(forecast_date,ahead,incidence_period)) %>%
      select(location,quantiles,forecaster_name,prob_type,target_period) %>%
      pivot_wider(values_from = quantiles,names_from = prob_type)
    })
  
  # ground truth to plot
  if(incidence_period == "day")
  {
    response_df <- download_signal(data_source = response$data_source,
                                   signal = response$signal,
                                   start_day = first_day,
                                   end_day = last_day,
                                   geo_type = geo_type) %>%
      select(location, time_value,value) %>%
      rename(reference_period = time_value) %>%
      filter(location %in% preds_df$location) 
  } else{
    # avoid summing over partial epiweeks
    date_range <- covidcast::covidcast_meta() %>% 
      filter(data_source == response$data_source & signal == response$signal & geo_type == !!geo_type) %>%
      select(min_time,max_time)
    sunday_following_first_day <- shift_day_to_following_xxxday(max(ymd(date_range$min_time,first_day)),xxx = 1)
    saturday_preceding_last_day <- shift_day_to_preceding_xxxday(min(ymd(date_range$max_time,last_day)),xxx = 7)
    assertthat::assert_that(sunday_following_first_day < saturday_preceding_last_day,
                            msg = "Pick first and last day to span at least one full epiweek.")
    
    response_df <- download_signal(data_source = response$data_source,
                                   signal = response$signal,
                                   start_day = sunday_following_first_day,
                                   end_day = saturday_preceding_last_day,
                                   geo_type = geo_type) %>%
      select(location, time_value,value) %>%
      sum_to_epiweek() %>%
      rename(reference_period = epiweek) %>%
      filter(location %in% preds_df$location)
  }
  
  # plot
  bind_rows(response_df,
                  preds_df %>% rename(reference_period = target_period)) %>%
    mutate(location_abbr = if(geo_type == "state") fips_to_abbr(paste0(location,"000")) else fips_to_name(location)) %>%
    filter(!is.na(location_abbr)) %>%
    ggplot(aes(x = .data$reference_period)) +
    geom_line(aes(y = .data$value),
              size = 1) +
    geom_line(aes(y = .data$point,
                  col = .data$forecaster_name),
              size = 1) + 
    geom_ribbon(aes(ymin = .data$lower,
                    ymax = .data$upper,
                    fill = .data$forecaster_name),
                alpha = .1,
                colour = NA,
                show.legend = F) + 
    facet_wrap(~ .data$location_abbr, scales = "free") +
    scale_color_discrete(na.translate = F) +
    labs(x = incidence_period,
         y = paste0(response$data_source,"-",response$signal),
         colour = "forecaster_name") +
    theme_bw()
}

sum_to_epiweek <- function(daily_df)
{
  daily_df %>%
    mutate(epiweek = MMWRweek::MMWRweek(time_value)$MMWRweek) %>%
    select(-time_value) %>%
    group_by(location,epiweek) %>%
    summarise(value = sum(value)) %>%
    ungroup()
}

shift_day_to_preceding_xxxday <- function(day,xxx)
{
  ew_day <- MMWRweek::MMWRweek(day)
  if(ew_day$MMWRday < xxx)
  {
    MMWRweek::MMWRweek2Date(MMWRyear = ew_day$MMWRyear, MMWRweek = ew_day$MMWRweek, MMWRday = xxx) - 7
  } else{
    MMWRweek::MMWRweek2Date(MMWRyear = ew_day$MMWRyear, MMWRweek = ew_day$MMWRweek, MMWRday = xxx)
  }
}

shift_day_to_following_xxxday <- function(day,xxx)
{
  ew_day <- MMWRweek::MMWRweek(day)
  if(ew_day$MMWRday > xxx)
  {
    MMWRweek::MMWRweek2Date(MMWRyear = ew_day$MMWRyear, MMWRweek = ew_day$MMWRweek, MMWRday = xxx) + 7
  } else{
    MMWRweek::MMWRweek2Date(MMWRyear = ew_day$MMWRyear, MMWRweek = ew_day$MMWRweek, MMWRday = xxx)
  }
}

get_target_period_num <- function(forecast_date,ahead,incidence_period)
{
  assertthat::assert_that(
    incidence_period %in% c("day","epiweek"), 
    msg = "Incidence period must be one of day or epiweek."
  )
  if(incidence_period == "day")
  {
    forecast_date + ahead
  } else {
    ifelse(wday(forecast_date) <= 2, 
           MMWRweek::MMWRweek(forecast_date)$MMWRweek + ahead - 1,
           MMWRweek::MMWRweek(forecast_date)$MMWRweek + ahead)
  }
}




