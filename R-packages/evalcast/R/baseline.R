#' Baseline forecaster
#'
#' The "flat-line" forecaster, which essentially mirrors the baseline in the
#' [COVID Forecast Hub](https://github.com/reichlab/covid19-forecast-hub). It
#' augments a flat-line point prediction with a forecast distribution around
#' this point based on quantiles of symmetrized week-to-week residuals.
#'
#' @param df Data frame of the format that is returned by
#'   [covidcast::covidcast_signal()].  
#' @template forecast_date-template
#' @template signals-template
#' @template incidence_period-template
#' @template ahead-template
#' @template geo_type-template
#'
#' @return Data frame with columns `ahead`, `location`, `probs`, `quantiles`.
#'   The `quantiles` column gives the predictive quantiles of the forecast
#'   distribution for that location and ahead.
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom lubridate ymd
#' @importFrom dplyr filter group_by arrange mutate select group_by group_modify ungroup bind_rows lag
#' @importFrom tibble tibble
#' @importFrom zoo rollsum
#' @importFrom stats quantile
#' @export
baseline_forecaster <- function(df,
                                forecast_date,
                                signals,
                                incidence_period = c("epiweek", "day"),
                                ahead,
                                geo_type) {
  incidence_period <- match.arg(incidence_period)
  forecast_date <- lubridate::ymd(forecast_date)
  target_period <- get_target_period(forecast_date, incidence_period, ahead)
  incidence_length <- ifelse(incidence_period == "epiweek", 7, 1)

  covidhub_probs <- c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)
  dat <- list()
  for (a in ahead) {
    # recall the first row of signals is the response
    dat[[a]] <- df %>%
        dplyr::filter(.data$data_source == signals$data_source[1],
                      .data$signal == signals$signal[1]) %>%
        dplyr::group_by(.data$location) %>%
        dplyr::arrange(.data$time_value) %>%
        dplyr::mutate(summed = zoo::rollsum(.data$value,
                                            k = incidence_length,
                                            fill = NA,
                                            align = "right"),
                      resid = .data$summed - dplyr::lag(.data$summed, n = incidence_length * a)) %>%
        dplyr::select(.data$location, .data$time_value, .data$value, .data$summed, .data$resid) %>%
        dplyr::group_modify(~ {
            point <- .x$summed[.x$time_value == max(.x$time_value)]
            tibble::tibble(probs = covidhub_probs,
                           quantiles = point + stats::quantile(.x$resid,
                                                               probs = covidhub_probs,
                                                               na.rm = TRUE))
        }, .keep = TRUE) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(quantiles = pmax(.data$quantiles, 0))
  }
  dat <- dat[ahead]
  names(dat) <- as.character(ahead)
  dplyr::bind_rows(dat, .id = "ahead") %>%
      dplyr::mutate(ahead = as.integer(ahead))
}
