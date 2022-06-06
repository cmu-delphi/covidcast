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
#'   One argument to `forecaster` must be named `df_list`. It will be
#'   populated with the list of historical data returned by a call
#'   to COVIDcast. The list will be the same length as the number of rows in
#'   the `signals` tibble (see below).
#'   The forecaster will also receive a single `forecast_date` as a named argument.
#'   Any additional named arguments can be passed via the `forecaster_args`
#'   argument below.
#'
#'   Thus, the forecaster should have a signature like
#'   `forecaster(df_list = data, forecast_data = forecast_date, ...)`
#' @param name_of_forecaster String indicating name of the forecaster.
#' @template signals-template
#' @template forecast_dates-template
#' @template incidence_period-template
#' @template apply_corrections-template
#' @param response_data_source String indicating the `data_source` of the response.
#'   This is used mainly for downstream evaluation. By default, this will be the
#'   same as the `data_source` in the first row of the `signals` tibble.
#' @param response_data_signal String indicating the `signal` of the response.
#'   This is used mainly for downstream evaluation. By default, this will be the
#'   same as the `signal` in the first row in the `signals` tibble.
#' @param forecaster_args a list of additional named arguments to be passed
#'   to `forecaster()`. A common use case would be to pass the period ahead
#'   (e.g. predict 1 day, 2 days, ..., k days ahead). Note that `ahead` is a
#'   required component of the forecaster output (see above).
#' @param parallel_execution TRUE, FALSE, or a single positive integer. If TRUE,
#'   uses bettermc::mclapply to execute each forecast date prediction in
#'   parallel on max number of cores - 1. If FALSE, the code is run on a single
#'   core. If integer, runs in parallel on that many cores, clipping to the max
#'   number of detected cores if a greater number is requested.
#' @param additional_mclapply_args a named list of additional arguments to pass
#'   to \link[bettermc:mclapply]{`bettermc::mclapply`} (besides `X`, `FUN`, and
#'   `mc.cores`.)
#' @param honest_as_of a boolean that, if true, ensures that the forecast_day, end_day,
#' and as_of are all equal when downloading data. Otherwise, as_of is allowed to be freely
#' set (and defaults to current date if not presented).
#' @param offline_signal_dir the directory that stores the cached data for each
#' (signal, forecast day) pair. If this is null, no caching is done and the data is
#' downloaded from covidcast.
#'
#' @template predictions_cards-template
#'
#' @examples \dontrun{
#' baby_predictions = get_predictions(
#'   baseline_forecaster, "baby",
#'   tibble::tibble(
#'     data_source = "jhu-csse",
#'     signal = "deaths_incidence_num",
#'     start_day = "2020-08-15",
#'     geo_values = "mi",
#'     geo_type = "state"),
#'   forecast_dates = "2020-10-01",
#'   incidence_period = "epiweek",
#'   forecaster_args = list(
#'     incidence_period = "epiweek",
#'     ahead = 1:4
#'   ))
#' }
#'
#' @export
get_predictions <- function(forecaster,
                            name_of_forecaster,
                            signals,
                            forecast_dates,
                            incidence_period = c("epiweek", "day"),
                            apply_corrections = function(signals) signals,
                            response_data_source = signals$data_source[1],
                            response_data_signal = signals$signal[1],
                            forecaster_args = list(),
                            parallel_execution = TRUE,
                            additional_mclapply_args = list(),
                            honest_as_of = TRUE,
                            offline_signal_dir = NULL) {

  assert_that(is_tibble(signals), msg = "`signals` should be a tibble.")
  assert_that(xor(honest_as_of, "as_of" %in% names(signals)), msg = "`honest_as_of` should be set if and only if `as_of` is not set. Either remove as_of specification or set honest_as_of to FALSE.")
  incidence_period <- match.arg(incidence_period)

  if (incidence_period == "epiweek") rlang::warn("incidence_period of 'epiweek' only selects weekly (Saturday) target end dates, actual 'epiweek' signals not supported.", "evalcast::get_predictions")
  if (!is.null(signals$time_type) & (signals$time_type == "week" || signals$time_type == "epiweek")) rlang::abort("time_type in the signals arg can only be 'day'.", "evalcast::get_predictions")
  if (!is.null(signals$lag)) rlang::abort("lag in the signals arg will be ignored.", "evalcast::get_predictions")
  if (!is.null(signals$issues)) rlang::abort("issues in the signals arg will be ignored.", "evalcast::get_predictions")

  if (is.logical(parallel_execution) && parallel_execution == TRUE) {
    num_cores <- max(1, parallel::detectCores() - 1)
  } else if (is.logical(parallel_execution) && parallel_execution == FALSE) {
    num_cores <- 1
  } else if (is.integer(parallel_execution) && length(parallel_execution)==1L && parallel_execution >= 1L) {
    num_cores <- max(1, min(parallel::detectCores(), parallel_execution))
  } else {
    stop("parallel_execution argument is neither boolean nor integer.")
  }

  assert_that(rlang::is_named2(additional_mclapply_args) &&
              all(rlang::names2(additional_mclapply_args) %in% rlang::fn_fmls_names(bettermc::mclapply)))

  get_predictions_single_date_ <- function(forecast_date) {
    preds <- do.call(get_predictions_single_date,
      list(
        forecaster = forecaster,
        signals = signals,
        forecast_date = forecast_date,
        apply_corrections = apply_corrections,
        forecaster_args = forecaster_args,
        honest_as_of = honest_as_of,
        offline_signal_dir = offline_signal_dir
      )
    )
    return(preds)
  }

  out <- rlang::inject(bettermc::mclapply(forecast_dates, get_predictions_single_date_, mc.cores = num_cores, !!!additional_mclapply_args)) %>% bind_rows()

  # for some reason, `value` gets named and ends up in attr
  names(out$value) <- NULL
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
      ) %>%
    relocate(.data$forecaster, .before = .data$forecast_date)

  class(out) <- c("predictions_cards", class(out))
  out
}

get_predictions_single_date <- function(forecaster,
                                        signals,
                                        forecast_date,
                                        apply_corrections,
                                        forecaster_args,
                                        honest_as_of = TRUE,
                                        offline_signal_dir = NULL) {

  forecast_date <- lubridate::ymd(forecast_date)
  signals <- signal_listcols(signals, forecast_date)

  df_list <- signals %>%
    pmap(function(...) {
      sig <- list(...)
      if (honest_as_of) sig$as_of <- forecast_date

      download_signal(
        data_source = sig$data_source,
        signal = sig$signal,
        start_day = sig$start_day,
        end_day = forecast_date,
        geo_type = sig$geo_type,
        geo_values = sig$geo_values,
        as_of = sig$as_of,
        time_type = "day",
        offline_signal_dir = offline_signal_dir
      )
    })

  # Downloaded data postprocessing
  if (!is.null(apply_corrections)) df_list <- apply_corrections(df_list)

  forecaster_args$forecast_date <- forecast_date
  forecaster_args$df_list <- df_list # pass in the data named to the right arg

  out <- do.call(forecaster, forecaster_args)
  assert_that(all(c("ahead", "geo_value", "quantile", "value") %in% names(out)),
              msg = paste("Your forecaster must return a data frame with",
                          "(at least) the columnns `ahead`, `geo_value`,",
                          "`quantile`, and `value`."))

  # make a predictions card
  out$forecast_date <- forecast_date
  return(out)
}
