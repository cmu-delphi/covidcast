#' Baseline forecaster
#'
#' This serves as a template for a forecaster.  It's not intended to be a great
#' forecaster.
#'
#' @param df a data frame of the format that is outputted by
#'   \code{\link[covidcast]{covidcast_signal}}.
#' @param forecast_date date on which forecasts will be made about some period
#'   (e.g., epiweek).  For example, if forecast_date is ymd("2020-05-11"),
#'   incidence_period is "day",  and ahead = 3, then, we'd be making forecasts
#'   for "2020-05-14".
#' @template signals-template
#' @template incidence_period-template
#' @template ahead-template
#' @template geo_type-template
#'
#' @return A data frame with columns "ahead", "location", "probs", "quantiles".
#'   The quantiles column gives the probs-quantile of the forecast distribution
#'   for that location and ahead.
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
