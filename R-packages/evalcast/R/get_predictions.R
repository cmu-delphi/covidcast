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
#'   
#'   The first argument must be the list of signals returned by a call to the COVIDcast. 
#'   The forecaster will also receive a single `forecast_date` as a named argument.
#'   Any additional named arguments can be passed via the `forecaster_params`
#'   argument below.
#' @param name_of_forecaster String indicating name of the forecaster.
#' @template signals-template
#' @template forecast_dates-template
#' @template ahead-template
#' @template incidence_period-template
#' @template apply_corrections-template
#' @param response_data_source String indicating the `data_source` of the response.
#'   This is used mainly for downstream evaluation. By default, this will be the 
#'   same as the first row in the `signals_to_use` tibble.
#' @param response_data_signal String indicating the `signal` of the response.
#'   This is used mainly for downstream evaluation. By default, this will be the 
#'   same as the first row in the `signals_to_use` tibble.
#' @param forecaster_params a list of additional named arguments to be passed 
#'   to `forecaster()`. A common use case would be to pass the period ahead
#'   (e.g. predict 1 day, 2 days, ..., k days ahead). Note that `ahead` is a 
#'   required component of the forecaster output (see above).
#' @template predictions_cards-template
#' 
#'
#' @examples \dontrun{
#' baby_predictions = get_predictions(
#'   baseline_forecaster, "baby",
#'   tibble::tibble(
#'     data_source=c("jhu-csse", "usa-facts"),
#'     signal = c("deaths_incidence_num","confirmed_incidence_num"),
#'     start_day="2020-08-15"
#'     geo_values = "mi"), 
#'   forecast_dates = "2020-10-01",
#'   response_geo_type = "state")
#' }
#' 
#' @export
get_predictions <- function(forecaster,
                            name_of_forecaster,
                            signals_to_use,
                            forecast_dates,
                            incidence_period = c("epiweek","day"),
                            apply_corrections = function(signals) signals,
                            response_geo_type = c("county", "hrr", "msa", "dma",
                                                  "state", "hhs", "nation"),
                            response_data_source = signals_to_use$data_source[1],
                            response_data_signal = signals_to_use$signal[1],
                            forecaster_params = list()) {
  assert_that(is_tibble(signals), msg="`signals` should be a tibble.")
  out <- forecast_dates %>%
    map(~ do.call(
          get_predictions_single_date,
          list(forecaster = forecaster,
               signals = signals_to_use,
               forecast_date = .x,
               apply_corrections = apply_corrections,
               forecaster_params = forecaster_params)
          )
        ) %>%
    bind_rows()

  # for some reason, `value` gets named and ends up in attr
  names(out$value) = NULL 
  out <- out %>% 
    mutate(
      forecaster = name_of_forecaster,
      data_source = response_data_source,
      signal = response_data_signal,
      target_end_date = get_target_period(
        .data$forecast_date, 
        incidence_period, 
        .data$ahead)$end,
      incidence_period = incidence_period
      )
  out %>% relocate(.data$forecaster, .before = .data$forecast_date)
  
  class(out) <- c("predictions_cards", class(out))
  out
}


get_predictions_single_date <- function(forecaster,
                                        signals,
                                        forecast_date,
                                        apply_corrections,
                                        forecaster_params) {
  # see get_predictions() for descriptions of the arguments

  forecast_date <- lubridate::ymd(forecast_date)
  # compute the start_day from the forecast_date, if we need to
  signals <- signal_listcols(signals, forecast_date)

  df_list <- signals %>%
    pmap(function(...) {
      sig <- list(...)
      download_signal(
        data_source = sig$data_source,
        signal = sig$signal,
        start_day = sig$start_day,
        end_day = forecast_date,
        as_of = sig$as_of,
        geo_type = sig$geo_type,
        geo_values = sig$geo_values)
    })

  # Downloaded data postprocessing
  if(!is.null(apply_corrections)) df_list <- apply_corrections(df_list)

  out <- forecaster(df_list,
                    forecast_date,
                    signals,
                    incidence_period,
                    ahead,
                    geo_type,
                    ...)
  assert_that(all(c("ahead", "geo_value", "quantile", "value") %in% names(out)),
              msg = paste("Your forecaster must return a data frame with",
                          "(at least) the columnns `ahead`, `geo_value`,",
                          "`quantile`, and `value`."))
  # make a predictions card for each ahead
  out$forecast_date = forecast_date
  return(out)
}
