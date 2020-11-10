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
#' @template signals-template
#' @template forecast_dates-template
#' @template incidence_period-template
#' @template ahead-template
#' @template geo_type-template
#' @template geo_values-template
#' @template apply_corrections-template
#' @param ... Additional arguments to be passed to `forecaster()`.
#' @return List of "predictions cards", with one element per forecast date. Each
#'   predictions card is a data frame with two columns:
#'
#' \item{location}{FIPS codes of the locations. For counties, this matches the
#'   `geo_value` used in the COVIDcast API. However, for states, `location` and
#'   `geo_value` are slightly different, in that the former is a two-digit FIPS
#'   code, as in "42", whereas the latter is a state abbreviation, as in "pa".}
#' \item{forecast_distribution}{List column of tibbles containing the predicted
#'   quantiles for each location.}
#' 
#' Each predictions card has attributes that specify the exact forecasting task
#'   that was being carried out, along with the name of the forecaster.
#' 
#'
#' @examples
#' baby_predictions = get_predictions(
#'   baseline_forecaster, "baby",
#'   tibble::tibble(
#'     data_source=c("jhu-csse", "usa-facts"),
#'     signal = c("deaths_incidence_num","confirmed_incidence_num"),
#'     start_day=lubridate::ymd("2020-09-15")),
#'   lubridate::ymd("2020-10-01"),"epiweek", 1:4, "state", "mi")
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
                            apply_corrections = NULL,
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
                 geo_values = geo_values,
                 apply_corrections = apply_corrections),
            params))) %>%
    flatten()
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
#' @importFrom stringr str_glue
get_predictions_single_date <- function(forecaster,
                                        name_of_forecaster,
                                        signals,
                                        forecast_date,
                                        incidence_period,
                                        ahead,
                                        geo_type,
                                        geo_values,
                                        apply_corrections,
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
           corrections_applied = apply_corrections,
           from_covidhub = FALSE,
           forecaster_params = list(...))
    )
  }
  return(pcards)
}

