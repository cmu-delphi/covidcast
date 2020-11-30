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
#' @param symmetrize Should symmetrized residuals be used, or unsymmetrized
#'   (raw) residuals? Default is `TRUE`, which results in the flat-line point
#'   prediction. If `FALSE`, then point predictions can be increasing or
#'   decreasing, depending on the historical trend.
#'
#' @return Data frame with columns `ahead`, `location`, `probs`, `quantiles`.
#'   The `quantiles` column gives the predictive quantiles of the forecast
#'   distribution for that location and ahead.
#'
#' @export
baseline_forecaster <- function(df,
                                forecast_date,
                                signals,
                                incidence_period = c("epiweek", "day"),
                                ahead,
                                geo_type,
                                symmetrize = TRUE) {
  incidence_period <- match.arg(incidence_period)
  forecast_date <- lubridate::ymd(forecast_date)
  target_period <- get_target_period(forecast_date, incidence_period, ahead)
  incidence_length <- ifelse(incidence_period == "epiweek", 7, 1)
  covidhub_probs <- c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)
  dat <- list()
  s <- ifelse(symmetrize, -1, NA)
  for (a in ahead) {
    # recall the first row of signals is the response
    dat[[a]] <- df %>%
        dplyr::filter(.data$data_source == signals$data_source[1],
                      .data$signal == signals$signal[1]) %>%
        dplyr::group_by(.data$location) %>%
        dplyr::arrange(.data$time_value) %>%
        dplyr::mutate(
          summed = zoo::rollsum(.data$value, k = incidence_length, fill = NA, 
                                align = "right"),
          resid = .data$summed - 
            dplyr::lag(.data$summed, n = incidence_length * a)) %>%
        dplyr::select(.data$location, .data$time_value, 
                      .data$summed, .data$resid) %>%
        dplyr::group_modify(~ {
            point <- .x$summed[.x$time_value == max(.x$time_value)]
            tibble::tibble(quantile = covidhub_probs,
                           value = point + 
                             stats::quantile(c(.x$resid, s * .x$resid),
                                             probs = covidhub_probs,
                                             na.rm = TRUE))
        }, .keep = TRUE) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(value = pmax(.data$value, 0))
  }
  dat <- dat[ahead]
  names(dat) <- as.character(ahead)
  dplyr::bind_rows(dat, .id = "ahead") %>%
      dplyr::mutate(ahead = as.integer(ahead))
}
