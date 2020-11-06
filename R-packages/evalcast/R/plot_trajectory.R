#' Produce trajectory plots of forecasts and observed data
#'
#' @param list_of_predictions_cards collection of prediction cards as returned 
#'   by `get_predictions()`. Supports collections of multiple forecast dates and
#'   forecasters
#' @param first_day the earliest date to display
#' @param last_day the latest date to display. Defaults to the most recent data
#'   available (plus future, if forecasts are past today)
#' @param alpha Displayed ribbons are the 1-alpha coverage results
#'
#' @return Produces a ggplot object
#' @export
#' @importFrom rlang .data
#'
#' @examples
#' sister_preds = get_predictions(
#'   baseline_forecaster, "sister",
#'   tibble::tibble(data_source="usa-facts", signal = "deaths_incidence_num",
#'     start_day=lubridate::ymd("2020-07-01")),
#'     lubridate::ymd("2020-09-01"),"epiweek", 1:4, "state", c("mi"))
#' baby_preds = get_predictions(
#'   baseline_forecaster, "baby",
#'   tibble::tibble(data_source="usa-facts", signal = "deaths_incidence_num",
#'     start_day=lubridate::ymd("2020-07-01")),
#'     lubridate::ymd("2020-09-08"),"epiweek", 1:4, "state", c("mi"))
#'  plot_trajectory(c(sister_preds, baby_preds), last_day="2020-10-01")
plot_trajectory <- function(list_of_predictions_cards,
                            first_day = "2020-07-01",
                            last_day = Sys.Date(),
                            alpha = .2,
                            nrow = 6,
                            ncol = 4,  
                            page_facet = 1，
                            cutoff = 52){
  # make sure predictions cards are for the same forecasting task (except ahead, and forecast_date)
  response <- unique_attr(list_of_predictions_cards,"signals")
  incidence_period <- unique_attr(list_of_predictions_cards,"incidence_period")
  geo_type <- unique_attr(list_of_predictions_cards, "geo_type")
  assertthat::assert_that(incidence_period %in% c("day","epiweek"))
  assertthat::assert_that(geo_type %in% c("county","state"))
  
  
  # predicted quantiles to plot
  plot_probs <- c(.5 - (1-alpha)/2, .5, .5 + (1-alpha)/2)
  
  preds_df <- list_of_predictions_cards %>% 
                purrr::map_dfr(function(predictions_card) {
                  fcast_date = attributes(predictions_card)$forecast_date
      
                  predictions_card %>%
                    tidyr::unnest(.data$forecast_distribution) %>%
                    dplyr::mutate(
                      forecaster_name = attributes(predictions_card)$name_of_forecaster,
                      ahead = attributes(predictions_card)$ahead,
                      prob_type = dplyr::case_when(
                        abs(probs - plot_probs[1]) <= 1e-8 ~ "lower",
                        abs(probs - plot_probs[2]) <= 1e-8 ~ "point",
                        abs(probs - plot_probs[3]) <= 1e-8 ~ "upper")) %>%
                    filter(!is.na(.data$prob_type)) %>%
                    dplyr::mutate(target_period = get_target_period_num(fcast_date,.data$ahead,incidence_period)) %>%
                    dplyr::select(.data$location,.data$quantiles,.data$forecaster_name,.data$prob_type,.data$target_period) %>%
                    tidyr::pivot_wider(values_from = .data$quantiles,names_from = .data$prob_type)
                })
  
  # ground truth to plot
  if(incidence_period == "day") {
    response_df <- download_signal(data_source = response$data_source,
                                   signal = response$signal,
                                   start_day = first_day,
                                   end_day = last_day,
                                   geo_type = geo_type) %>%
      dplyr::select(.data$location, .data$time_value, .data$value) %>%
      dplyr::rename(reference_period = .data$time_value) %>%
      dplyr::filter(location %in% preds_df$location) 
  } else {
    # avoid summing over partial epiweeks
    date_range <- covidcast::covidcast_meta() %>% 
      dplyr::filter(data_source == "usa-facts" & signal == response$signal & geo_type == !!geo_type) %>%
      #dplyr::filter(data_source == response$data_source & signal == response$signal & geo_type == !!geo_type) %>%
      dplyr::select(min_time,max_time)
    
    sunday_following_first_day <- shift_day_to_following_xxxday(max(ymd(date_range$min_time,first_day)),xxx = 1)
    saturday_preceding_last_day <- shift_day_to_preceding_xxxday(min(ymd(date_range$max_time,last_day)),xxx = 7)
    assertthat::assert_that(sunday_following_first_day < saturday_preceding_last_day,
                            msg = "Pick first and last day to span at least one full epiweek.")
    
    response_df <- download_signal(data_source = "usa-facts",#response$data_source,
                                   signal = response$signal,
                                   start_day = sunday_following_first_day,
                                   end_day = saturday_preceding_last_day,
                                   geo_type = geo_type) %>%
                   dplyr::select(location, .data$time_value,.data$value) %>%
                   sum_to_epiweek() %>%
                   dplyr::rename(reference_period = epiweek) %>%
                   dplyr::filter(location %in% preds_df$location)
    }
  
  
  
  # Set up the prediction df and response df
  preds_df <- dplyr::rename(preds_df, reference_period = target_period) %>% 
              dplyr::mutate(location_abbr = if(geo_type == "state") {
                                                 covidcast::fips_to_abbr(paste0(location,"000")) 
                                              } else {
                                                 covidcast::fips_to_name(location)
                                              } 
                             ) %>% 
              dplyr::filter(!is.na(location_abbr))
  
              
  response_df <- response_df %>% 
                 dplyr::mutate(location_abbr = if(geo_type == "state") {
                                                  covidcast::fips_to_abbr(paste0(location,"000")) 
                                               } else {
                                                  covidcast::fips_to_name(location) 
                                               }
                               ) %>% 
                 dplyr::filter(!is.na(location_abbr))
  
    
  # Set up the page number by the number of facets
  nfacets = length(unique(preds_df$location_abbr)) 
  if(ncol*nrow*page_facet < nfacets) page_facet = ceiling(nfacets / (ncol*nrow))
  

  # Framework of the trajectory plot
  p <- ggplot(preds_df, aes(x = .data$reference_period)) +
       geom_line(aes(y = .data$point,
                     col = .data$forecaster_name, 
                     group = .data$forecast_date),
                     size = 1) + 
       geom_point(aes(y = .data$point,
                      col = .data$forecaster_name, 
                      group = .data$forecast_date)) +
       geom_ribbon(aes(ymin = .data$lower,
                       ymax = .data$upper,
                       fill = .data$forecaster_name,
                       group = .data$forecast_date),
                       alpha = .1,
                       colour = NA,
                       show.legend = FALSE) + 
       geom_line(data=response_df, 
                 aes(y = .data$value),
                 size = 1) + 
       scale_color_discrete(na.translate = FALSE) +
       labs(x = incidence_period,
            y = paste0(response$data_source,": ",response$signal),
            colour = "forecaster_name") +
       theme_bw() + 
       theme(axis.text = element_text(size = 8), 
             strip.text = element_text(size = 10,face = "bold"))
  
 
  # Facetting. The default cutoff is 52, and plots that exceed 52 facets will be paginated.
  if (nfacets <= cutoff) {
    print(p + ggplot2::facet_wrap(~.data$location_abbr, scales = "free", ncol = 6, nrow=NULL))
  } else { 
    for (i in 1: page_facet){
      print(
        p + ggforce::facet_wrap_paginate(
          ~.data$location_abbr, scales = "free_y", ncol = ncol, nrow=nrow,page=i
        )
      ) 
    }
  }
}



sum_to_epiweek <- function(daily_df){
  daily_df %>%
    mutate(epiweek = MMWRweek::MMWRweek(time_value)$MMWRweek) %>%
    select(-.data$time_value) %>%
    group_by(location,.data$epiweek) %>%
    summarise(value = sum(.data$value)) %>%
    ungroup()
}

shift_day_to_preceding_xxxday <- function(day,xxx){
  ew_day <- MMWRweek::MMWRweek(day)
  if(ew_day$MMWRday < xxx)
  {
    MMWRweek::MMWRweek2Date(MMWRyear = ew_day$MMWRyear, MMWRweek = ew_day$MMWRweek, MMWRday = xxx) - 7
  } else {
    MMWRweek::MMWRweek2Date(MMWRyear = ew_day$MMWRyear, MMWRweek = ew_day$MMWRweek, MMWRday = xxx)
  }
}

shift_day_to_following_xxxday <- function(day,xxx){
  ew_day <- MMWRweek::MMWRweek(day)
  if(ew_day$MMWRday > xxx)
  {
    MMWRweek::MMWRweek2Date(MMWRyear = ew_day$MMWRyear, MMWRweek = ew_day$MMWRweek, MMWRday = xxx) + 7
  } else {
    MMWRweek::MMWRweek2Date(MMWRyear = ew_day$MMWRyear, MMWRweek = ew_day$MMWRweek, MMWRday = xxx)
  }
}

get_target_period_num <- function(forecast_date,ahead,incidence_period){
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




