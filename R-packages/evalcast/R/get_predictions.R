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
#'   `location`, `probs`, `quantiles`. The `quantiles` column gives the
#'   predictive quantiles of the forecast distribution for that location and
#'   ahead.
#' @param name_of_forecaster String indicating name of the forecaster.
#' @template forecast_dates-template
#' @template signals-template
#' @template incidence_period-template
#' @template ahead-template
#' @template geo_type-template
#' @template geo_values-template
#' @param ... Additional arguments to be passed to `forecaster()`.
#' 
#' @return List of "predictions cards", with one element per forecast date. Each
#'   predictions card is a data frame with two columns:
#'
#' \item{location}{FIPS code of the location. For counties, this is the same as
#'   the `geo_value` used in the COVIDcast API.  However, for states, `location`
#'   and `geo_value` are slightly different, in that the former is truncated to
#'   two digits, as in "42" for Pennsylvania (whereas the latter is always five
#'   digits, as in "42000").}
#' \item{forecast_distribution}{Tibble containing the predicted quantiles for
#'   that location.}
#' 
#' Each predictions card has attributes that specify the exact forecasting task
#'   that was being carried out, along with the name of the forecaster.
#' 
#' @importFrom assertthat assert_that
#' @export
get_predictions <- function(forecaster,
                            name_of_forecaster,
                            signals,
                            forecast_dates,
                            incidence_period,
                            ahead,
                            geo_type,
                            geo_values = "*",
                            ...) {
  assert_that(is_tibble(signals), msg="`signals` should be a tibble.")
  params <- list(...)
  forecast_dates %>%
    map(~ do.call(
          get_predictions_single_date,
          c(list(forecaster = forecaster,
                 name_of_forecaster = name_of_forecaster,
                 signals = signals,
                 forecast_date = .x,
                 incidence_period = incidence_period,
                 ahead = ahead,
                 geo_type = geo_type,
                 geo_values = geo_values),
            params))) %>%
    flatten()
}

#' Get predictions cards for a single date
#'
#' @importFrom stringr str_glue
get_predictions_single_date <- function(forecaster,
                                        name_of_forecaster,
                                        signals,
                                        forecast_date,
                                        incidence_period,
                                        ahead,
                                        geo_type,
                                        geo_values,
                                        ...) {
  if (length(geo_values) > 1) geo_values <- list(geo_values)
  forecast_date <- lubridate::ymd(forecast_date)
  # get data that would have been available as of forecast_date
  df <- signals %>%
    pmap_dfr(function(...) {
      args <- list(...)
      download_signal(data_source=args$data_source,
                      signal = args$signal,
                      start_day = args$start_day,
                      end_day = forecast_date,
                      as_of = forecast_date,
                      geo_type = geo_type,
                      geo_values = geo_values)
    })
  out <- forecaster(df,
                    forecast_date,
                    signals,
                    incidence_period,
                    ahead,
                    geo_type,
                    ...)
  # make a predictions card for each ahead
  pcards <- out %>%
    group_by(location, ahead) %>%
    group_modify(~ tibble(forecast_distribution = list(.))) %>%
    ungroup() %>%
    group_by(ahead) %>%
    group_map(~ .x) # break into a separate data frame for each ahead
  for (i in seq_along(ahead)) {
    attributes(pcards[[i]]) <- c(
      attributes(pcards[[i]]),
      list(forecaster = forecaster,
           name_of_forecaster = name_of_forecaster,
           signals = signals,
           forecast_date = forecast_date,
           incidence_period = incidence_period,
           ahead = ahead[i],
           geo_type = geo_type,
           geo_values = geo_values,
           from_covidhub = FALSE,
           ...)
    )
  }
  return(pcards)
}
