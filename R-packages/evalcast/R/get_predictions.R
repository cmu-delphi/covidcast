#' Get predictions
#'
#' For each of the provided forecast dates, runs a forecaster using the data
#' that would have been available as of that given forecast date. Returns a list
#' of "predictions cards", where each list element corresponds to a different
#' forecast date. A predictions card is a data frame giving the forecast
#' distributions of a given forecaster for a given forecast task. A forecast
#' task is specified by the forecast date, ahead, response, incidence period,
#' and geo type (e.g., 1-epiweek-ahead death forecasting at the state level with
#' predictions made using the information as of September 14).
#'
#' @param forecaster Function that outputs a tibble with columns `ahead`,
#'   `location`, `quantile`, and `value`. The `quantile` column gives the
#'   probabilities associated with quantile forecasts for that location and
#'   ahead. If your forecaster produces point forecasts, then set `quantile=NA`.
#' @param name_of_forecaster String indicating name of the forecaster.
#' @template signals-template
#' @template forecast_dates-template
#' @template incidence_period-template
#' @template ahead-template
#' @template geo_type-template
#' @template geo_values-template
#' @template apply_corrections-template
#' @param signal_aggregation this and the next argument control the type of
#'   data your forecaster expects to receive from covidcast. By default,
#'   different signals are passed in "long" format. But you may alternatively
#'   request "wide" or "list". See [covidcast::covidcast_signals()] and 
#'   [covidcast::aggregate_signals()] for more details.
#' @param signal_aggregation_dt for any data format, 
#'   [covidcast::aggregate_signals()] can perform leading and lagging for you.
#'   See that documentation for more details.
#' @param ... Additional arguments to be passed to `forecaster()`.
#' @return Long data frame of forecasts with a class of `predictions_cards`.
#'   The first 4 columns are the same as those returned by the forecaster. The
#'   remainder specify the prediction task, 10 columns in total: 
#'   `ahead`, `location`, `quantile`, `value`, `forecaster`, `forecast_date`,
#'   `data_source`, `signal`, `target_end_date`, and `incidence_period`. Here
#'   `data_source` and `signal` correspond to the response varible only.
#' 
#' 
#'
#' @examples
#' baby_predictions = get_predictions(
#'   baseline_forecaster, "baby",
#'   tibble::tibble(
#'     data_source=c("jhu-csse", "usa-facts"),
#'     signal = c("deaths_incidence_num","confirmed_incidence_num"),
#'     start_day="2020-08-15"), "2020-10-01","epiweek", 1:4, 
#'     "state", "mi")
#'
#' @export
get_predictions <- function(forecaster,
                            name_of_forecaster,
                            signals,
                            forecast_dates,
                            incidence_period,
                            ahead,
                            geo_type,
                            geo_values = "*",
                            apply_corrections = NULL,
                            signal_aggregation = c("long", "wide", "list"),
                            signal_aggregation_dt = NULL,
                            ...) {
  assert_that(is_tibble(signals), msg="`signals` should be a tibble.")
  signal_aggregation = match.arg(signal_aggregation, c("long", "wide", "list"))
  params <- list(...)
  out <- forecast_dates %>%
    map_dfr(~ do.call(
          get_predictions_single_date,
          c(list(forecaster = forecaster,
                 name_of_forecaster = name_of_forecaster,
                 signals = signals,
                 forecast_date = .x,
                 incidence_period = incidence_period,
                 ahead = ahead,
                 geo_type = geo_type,
                 geo_values = geo_values,
                 apply_corrections = apply_corrections,
                 signal_aggregation = signal_aggregation,
                 signal_aggregation_dt = signal_aggregation_dt),
            params)))
  class(out) <- c("predictions_card", class(out))
  out
}

#' Get predictions cards for a single date
#'
#' @param forecaster Function that outputs a tibble with columns `ahead`,
#'   `location`, `probs`, `quantiles`. The `quantiles` column gives the
#'   predictive quantiles of the forecast distribution for that location and
#'   ahead.
#' @param name_of_forecaster String indicating name of the forecaster.
#' @template signals-template
#' @template forecast_date-template
#' @template incidence_period-template
#' @template ahead-template
#' @template geo_type-template
#' @template geo_values-template
#' @template apply_corrections-template
#' @param ... Additional arguments to be passed to `forecaster()`.
#' 
get_predictions_single_date <- function(forecaster,
                                        name_of_forecaster,
                                        signals,
                                        forecast_date,
                                        incidence_period,
                                        ahead,
                                        geo_type,
                                        geo_values,
                                        apply_corrections,
                                        signal_aggregation,
                                        signal_aggregation_dt,
                                        ...) {
  #if (length(geo_values) > 1) geo_values <- list(geo_values)
  forecast_date <- lubridate::ymd(forecast_date)
  # compute the start_day from the forecast_date, if we need to
  if (!is.null(signals$start_day) && is.list(signals$start_day)) {
    for (i in 1:length(signals$start_day)) {
      if (is.function(signals$start_day[[i]])) {
        signals$start_day <- signals$start_day[[i]](forecast_date)
      }
    }
  }
  # get data that would have been available as of forecast_date
  args <- list(...)
  df <- download_signals(data_source=signals$data_source,
                         signal = signals$signal,
                         start_day = signals$start_day,
                         end_day = forecast_date,
                         as_of = forecast_date,
                         geo_type = geo_type,
                         geo_values = geo_values,
                         signal_aggregation = signal_aggregation,
                         signal_aggregation_dt = signal_aggregation_dt)

  if(!is.null(apply_corrections)){
    df <- data_corrector(df, apply_corrections)
  } else {
    apply_corrections <- NA
  }

  out <- forecaster(df,
                    forecast_date,
                    signals,
                    incidence_period,
                    ahead,
                    geo_type,
                    ...)
  # make a predictions card for each ahead
  out %>% 
    mutate(
      forecaster = name_of_forecaster,
      forecast_date = forecast_date,
      data_source = signals$data_source[1],
      signal = signals$signal[1],
      target_end_date = get_target_period(forecast_date, 
                                          incidence_period, ahead)$end,
      incidence_period = incidence_period,
      ) 
      ## dropped attributes: other signals, geo_type,
      ##   geo_values, corrections_applied, from_covidhub,
      ##   forecaster_params
}

