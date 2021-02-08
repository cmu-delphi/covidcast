#' Plot predictions along with truth
#'
#' @param predictions_cards long data frame of forecasts with a class of 
#'   `predictions_cards` as created by [get_predictions()] or 
#'   [get_zoltar_predictions()] or [get_covidhub_predictions()].
#'   The first 4 columns are the same as those returned by the forecaster. The
#'   remainder specify the prediction task, 10 columns in total: 
#'   `ahead`, `geo_value`, `quantile`, `value`, `forecaster`, `forecast_date`,
#'   `data_source`, `signal`, `target_end_date`, and `incidence_period`. Here
#'   `data_source` and `signal` correspond to the response variable only.
#' @param intervals vector of confidence intervals to show. More than 3 is ugly 
#'   and will be reduced to the default set.
#' @param plot_it should we actually produce the figure. If you have many 
#'   geo_values or forecasters, best not to. Then you can assign the output
#'   to an object and facet or apply aesthetic specifications to meet your
#'   own standards.
#' @param show_geo_value a subset of `geo_values` in your `predictions_cards`. 
#'   Default uses all available. If you only want to see some, this may speed 
#'   up the call.
#' @param show_forecaster a subset of `forecasters` in your `predictions_cards`. 
#'   Default uses all available. If you only want to see some, this may speed 
#'   up the call. 
#' @param side_truth optional. Use your own truth data rather than downloading
#'   with [covidcast::covidcast_signal()]. Must contain columns `geo_value`,
#'   `value` and `target_end_date`
#' @param show_points do you want points as well as lines
#' @param show_quantiles do you want to plot the quantiles or just lines
#' @template geo_type-template
#' @param ... additional arguments passed to [covidcast::covidcast_signal()]
#'
#' @return invisibly returns a ggplot object
#' @export
plot_trajectory <- function(predictions_cards,
                            intervals = c(.5, .8, .95),
                            plot_it = TRUE,
                            show_geo_value = NULL,
                            show_forecaster = NULL,
                            side_truth = NULL,
                            show_points = TRUE,
                            show_quantiles = TRUE,
                            geo_type,
                            ...) {
  if (!is.null(show_geo_value)) {
    predictions_cards <- predictions_cards %>%
      filter(.data$geo_value %in% show_geo_value)
  }
  if (!is.null(show_forecaster)) {
    predictions_cards <- predictions_cards %>%
      filter(.data$forecaster %in% show_forecaster)
  }
  if (length(intervals) > 3) {
    warning("More than 3 intervals are difficult to see. Resetting to default.")
    intervals = c(.5, .8, .95)
  }
  pd <- setup_plot_trajectory(predictions_cards, intervals, side_truth, 
                              geo_type, ...)
  
  g <- ggplot(pd$truth_df, mapping = aes(x = .data$target_end_date))
  
  if (show_quantiles && !is.null(pd$quantiles_df)){
    n_quantiles = nlevels(pd$quantiles_df$interval)
    l_quantiles = levels(pd$quantiles_df$interval)
    alp = c(.4, .2, .1)
    for (qq in n_quantiles:1) {
      g <- g + geom_ribbon(data = pd$quantiles_df %>%
                             filter(.data$interval == l_quantiles[qq]),
                           mapping = aes(ymin = .data$lower, 
                                         ymax = .data$upper,
                                         group = .data$forecast_date,
                                         fill = .data$forecaster),
                           alpha = alp[qq]) 
    }
  }
    
  g <- g +
    geom_line(aes(y = .data$value)) +
    geom_line(data = pd$points_df, 
              mapping = aes(y = .data$value, 
                            group = .data$forecast_date, 
                            color = .data$forecaster))
  if (show_points) {
    g <- g + 
      geom_point(aes(y = .data$value)) +
      geom_point(data = pd$points_df, 
                 mapping = aes(y = .data$value, 
                               group = .data$forecast_date, 
                               color = .data$forecaster))
  }
  
  if (plot_it) {
    gp <- g + theme_bw() + scale_fill_viridis_d() + scale_color_viridis_d()
    if (show_quantiles && !is.null(pd$quantiles_df) && 
        nlevels(pd$quantiles_df$forecaster > 1)) {
      gp + facet_grid(.data$forecaster ~ .data$geo_value, scales = "free_y")
    } else {
      gp <- gp + facet_wrap(~.data$geo_value, scales = "free_y")
    }
    print(gp)
  } else {
    return(invisible(g))
  }
}

setup_plot_trajectory <- function(predictions_cards,
                                  intervals = c(.5, .8, .95),
                                  side_truth = NULL,
                                  geo_type = "county",
                                  ...){
  args <- list(...)
  max_api_geos <- 30 # threshold for the API. Not really important. Easier
                     # to grab everything if this is big (typical use case)
  ## add check that incidence period and geo_type are all the same, default to
  ## the first otherwise
  if (is.null(side_truth)) {
    if (is.null(args$geo_values)) { # what do we download
      geo_values <- unique(predictions_cards$geo_value) 
    } else {
      geo_values <- args$geo_values
    }
    if (length(geo_values) > max_api_geos) { # if lots, grab everything
      args$geo_values <-  "*"
    } else {
      args$geo_values <- geo_values
    }
    if (is.null(args$data_source)) {
      args$data_source <- predictions_cards$data_source[1]
      args$signal <- predictions_cards$signal[1]
    }

    ip <- predictions_cards$incidence_period[1]
    assert_that(ip %in% c("epiweek", "day"),
                msg = paste("When grabbing data from covidcast, the incidence",
                            "period must be either `epiweek' or `day'."))
    truth_data <- download_signal(
      data_source = args$data_source,
      signal = args$signal,
      geo_type = geo_type,
      geo_values = args$geo_values,
      ...)
    if (geo_values != "*" && length(geo_values) > max_api_geos ) {
      truth_data <- filter(truth_data, .data$geo_value %in% geo_values)
    }
    if (ip == "epiweek") {
      assert_that(
        min(truth_data$time_value) < max(truth_data$time_value) - 14,
        msg = paste("For epiweek plots,",
                    "available truth data should span at least",
                    "2 weeks."))
      truth_data <- truth_data %>%
        filter(.data$time_value >= 
                 shift_day_to_following_xxxday(min(.data$time_value), 1),
               .data$time_value <= 
                 shift_day_to_preceding_xxxday(max(.data$time_value), 7)) %>%
        dplyr::select(.data$geo_value, .data$time_value, .data$value) %>%
        sum_to_epiweek() %>%
        rename(target_end_date = .data$time_value)
    }
  }
  
  ## Now we process the predictions_cards
  predictions_cards <- predictions_cards %>%
    dplyr::select(.data$geo_value, .data$quantile, 
                  .data$value, .data$forecaster, .data$forecast_date,
                  .data$target_end_date)
  
  lower_bounds <- predictions_cards %>% 
    select(.data$quantile) %>%
    filter(.data$quantile < 0.5) %>%
    unique() %>%
    pull()
  
  # if quantile forecasts are not available, work with point forecasts only
  if (length(lower_bounds) < 1){
    intervals <- NULL
  } else {
    itvals <- 1 - 2*lower_bounds
    if (!sum(abs(outer(intervals, itvals, "-")) < 1e-8) == length(intervals)){
      intervals <- NULL
    }
  }
  
  if (is.null(intervals)) {
    quantiles_df <- NULL
  } else {
    quantiles_to_plot <- as.integer(sort(
      round(500L * (1 + intervals %o% c(-1L,1L)))))
    quantiles_df <- predictions_cards %>%
      filter(as.integer(round(.data$quantile*1000)) %in% c(quantiles_to_plot)) %>%
      mutate(endpoint_type = if_else(.data$quantile < 0.5, 'lower', 'upper'),
             alp = if_else(.data$endpoint_type == 'lower',
                           format(2*.data$quantile, digits=3, nsmall=3),
                           format(2*(1-.data$quantile), digits=3, nsmall=3)),
             interval = forcats::fct_rev(
               paste0((1-as.numeric(.data$alp))*100, "%"))) %>%
      select(-.data$quantile, -.data$alp) %>%
      pivot_wider(names_from = "endpoint_type", values_from = "value")
  }
  
  points_df <- predictions_cards %>%
    filter(as.integer(round(.data$quantile*1000)) == 500L | 
             is.na(.data$quantile))
  if (any(is.na(points_df$quantile))) {
    points_df <- points_df %>%
      pivot_wider(names_from = "quantile", values_from = "value") %>%
      mutate(value = if_else(!is.na(.data$`NA`), .data$`NA`, .data$`0.5`)) %>%
      select(-.data$`0.5`, -.data$`NA`)
  } else {
    points_df <- points_df %>%
      select(-.data$quantile)
  }
  
  if(!is.null(side_truth)) truth_data <- side_truth
  list(truth_df = truth_data, 
       points_df = points_df, 
       quantiles_df = quantiles_df)
}
