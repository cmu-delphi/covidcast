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
#'   `geo_value`, `quantile`, and `value`. The `quantile` column gives the
#'   probabilities associated with quantile forecasts for that location and
#'   ahead. If your forecaster produces point forecasts, then set `quantile=NA`.
#' @param name_of_forecaster String indicating name of the forecaster.
#' @template signals-template
#' @template forecast_dates-template
#' @template incidence_period-template
#' @template apply_corrections-template
#' @param signal_aggregation this and the next argument control the type of
#'   data your forecaster expects to receive from covidcast. By default,
#'   different signals are passed in "list" format. But you may alternatively
#'   request "wide" or "long". See [covidcast::covidcast_signals()] and 
#'   [covidcast::aggregate_signals()] for more details.
#' @param signal_aggregation_dt for any data format, 
#'   [covidcast::aggregate_signals()] can perform leading and lagging for you.
#'   See that documentation for more details.
#' @param as_of_override by default, the `as_of` date of data downloaded from
#'   covidcast is loaded with `as_of = forecast_date`. This means that data
#'   is "rewound" to days in the past. Any data revisions made since, would
#'   not have been present at that time, and would not be available to the
#'   forecaster. It's likely, for example, that no data would actually exist
#'   for the forecast date on the forecast date (there is some latency between
#'   the time signals are reported and the dates for which they are reported).
#'   You can override this functionality, though we strongly advise you do so
#'   with care, by passing a function of a single forecast_date here. The 
#'   function should return a date.
#' @param forecaster_args List of forecaster-specific arguments to pass to `forecaster()` 
#' @template predictions_cards-template
#' 
#'
#' @examples \dontrun{
#' baby_predictions = get_predictions(
#'   baseline_forecaster, "baby",
#'   tibble::tibble(
#'     data_source=c("jhu-csse", "usa-facts"),
#'     signal = c("deaths_incidence_num","confirmed_incidence_num"),
#'     start_day="2020-08-15",
#'     geo_type = "state"), 
#'   "2020-10-01","epiweek", signal_aggregation="long")
#' }
#' 
#' @export
get_predictions <- function(forecaster,
                            name_of_forecaster,
                            signals,
                            forecast_dates,
                            incidence_period,
                            apply_corrections = NULL,
                            signal_aggregation = c("list", "wide", "long"),
                            signal_aggregation_dt = NULL,
                            as_of_override = function(forecast_date) forecast_date,
                            forecaster_args = list()) {
  assert_that(is_tibble(signals), msg="`signals` should be a tibble.")
  signal_aggregation = match.arg(signal_aggregation, c("list", "wide", "long"))
  out <- forecast_dates %>%
    map(~ do.call(
          get_predictions_single_date,
          c(list(forecaster = forecaster,
                 name_of_forecaster = name_of_forecaster,
                 signals = signals,
                 forecast_date = .x,
                 incidence_period = incidence_period,
                 apply_corrections = apply_corrections,
                 signal_aggregation = signal_aggregation,
                 signal_aggregation_dt = signal_aggregation_dt,
                 as_of_override = as_of_override,
                 forecaster_args = forecaster_args)))) %>%
    bind_rows()

  # for some reason, `value` gets named and ends up in attr
  names(out$value) = NULL 
  class(out) <- c("predictions_cards", class(out))
  out
}


get_predictions_single_date <- function(forecaster,
                                        name_of_forecaster,
                                        signals,
                                        forecast_date,
                                        incidence_period,
                                        apply_corrections,
                                        signal_aggregation,
                                        signal_aggregation_dt,
                                        as_of_override,
                                        forecaster_args) {
  # see get_predictions() for descriptions of the arguments

  forecast_date <- lubridate::ymd(forecast_date)
  # make sure we have something in the start_day column
  if (!("start_day" %in% names(signals))) {
    signals$start_day <- NULL
  }
  # compute the start_day from the forecast_date, if we need to
  else if (is.list(signals$start_day)) {
    for (i in 1:length(signals$start_day)) {
      if (is.function(signals$start_day[[i]])) {
        signals$start_day <- signals$start_day[[i]](forecast_date)
      }
    }
  }

  df_list <- signals %>%
    pmap(function(...) {
      sig <- list(...)
      download_signal(
        data_source = sig$data_source,
        signal = sig$signal,
        start_day = sig$start_day,
        end_day = forecast_date,
        as_of = as_of_override(forecast_date),
        geo_type = sig$geo_type,
        geo_values = "*")
    })

  # Downloaded data postprocessing
  if (signal_aggregation != "list") {
    covidcast::aggregate_signals(df_list, signal_aggregation_dt, signal_aggregation)
  }
  if(!is.null(apply_corrections)) df_list <- apply_corrections(df_list)

  out <- forecaster(df_list,
                    forecast_date,
                    forecaster_args)
  assert_that(all(c("ahead", "geo_value", "quantile", "value") %in% names(out)),
              msg = paste("Your forecaster must return a data frame with",
                          "(at least) the columnns `ahead`, `geo_value`,",
                          "`quantile`, and `value`."))
  # make a predictions card for each ahead
  out %>% 
    mutate(
      forecaster = name_of_forecaster,
      forecast_date = forecast_date,
      data_source = signals$data_source[1],
      signal = signals$signal[1],
      target_end_date = get_target_period(forecast_date, 
                                          incidence_period, ahead)$end,
      incidence_period = incidence_period)
}
