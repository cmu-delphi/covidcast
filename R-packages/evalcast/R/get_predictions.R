#' Get predictions
#'
#' For each of the provided forecast dates, runs a forecaster using the data
#' that would have been available as of that given forecast date. Returns a list
#' of "predictions cards."  A prediction card is a data frame giving the
#' forecast distributions of a given forecaster for a given forecast task.  A
#' forecast task is specified by the forecast date, ahead, response, incidence
#' period, and geo_type (e.g., 1-epiweek ahead death forecasting at the state
#' level with predictions made using the information as of Sept. 14).
#'
#' A predictions card has two columns:
#' \enumerate{
#' \item location - the FIPS code of the location.  For counties, this is the
#' same as the geo_value used in the API.  However, for states, the location
#' and geo_value are different because the API uses state abbreviations instead
#' of FIPS for geo_value.
#' \item forecast_distribution - this is a list column... each element itself
#' contains a tibble with the covidhub quantiles.}
#' A predictions card has attributes that specify the exact forecasting
#' task that was being carried out and the name of the forecaster.
#'
#' @param forecaster a function that outputs a tibble with columns...
#' @param name_of_forecaster the name of forecaster
#' @param forecast_dates a vector of class Date on which forecasts will be made.
#'   e.g. \code{c(lubridate::ymd("2020-09-07"), lubridate::ymd("2020-09-14"))}
#' @template signals-template
#' @template incidence_period-template
#' @template ahead-template
#' @template geo_type-template
#' @param geo_values see \link[covidcast]{covidcast_signal} for a description
#'   of this parameter.
#' @param apply_corrections an optional function that applies data corrections
#'   to the signals. Input is a data frame as returned by `download_signals()`.
#'   The returned object must be the same but with additional variables.
#'   Corrected values must exist in the column `corrected`.
#' @return a list of predictions cards
#'
#' @examples
#' baby_predictions = get_predictions(
#'   baseline_forecaster, "baby",
#'   tibble::tibble(
#'     data_source=c("jhu-csse", "usa-facts"),
#'     signal = c("deaths_incidence_num","confirmed_incidence_num"),
#'     start_day=lubridate::ymd("2020-09-15")),
#'   lubridate::ymd("2020-10-01"),"epiweek", 1L, "state", "mi")
#'
#' baby_correct <- function(x) dplyr::mutate(x, corrected = 2*value)
#'
#' baby_corrected = get_predictions(
#'   baseline_forecaster, "baby",
#'   tibble::tibble(
#'     data_source=c("jhu-csse", "usa-facts"),
#'     signal = c("deaths_incidence_num","confirmed_incidence_num"),
#'     start_day=lubridate::ymd("2020-09-15")),
#'   lubridate::ymd("2020-10-01"),"epiweek", 1L, "state", "mi",
#'   apply_corrections = baby_correct)
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
                            apply_corrections = NULL) {
  if (!is_tibble(signals)) stop("signals should be a tibble.")
  forecast_dates %>%
    map(~ get_predictions_single_date(
      forecaster = forecaster,
      name_of_forecaster = name_of_forecaster,
      signals = signals,
      forecast_date = .x,
      incidence_period = incidence_period,
      ahead = ahead,
      geo_type = geo_type,
      geo_values = geo_values,
      apply_corrections
    )) %>%
    flatten()
}

#' Get predictions cards for a single date
#' @importFrom stringr str_glue
get_predictions_single_date <- function(forecaster,
                                        name_of_forecaster,
                                        signals,
                                        forecast_date,
                                        incidence_period,
                                        ahead,
                                        geo_type,
                                        geo_values,
                                        apply_corrections) {
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
  if(!is.null(apply_corrections)){
    corrected <- apply_corrections(df)
    df$value = corrected$corrected
  }
  out <- forecaster(df,
                    forecast_date,
                    signals,
                    incidence_period,
                    ahead,
                    geo_type)
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
           from_covidhub = FALSE)
    )
  }
  return(pcards)
}
