#' @export
#' @importFrom MMWRweek MMWRweek  MMWRweek2Date
#' @importFrom lubridate wday
get_target_period <- function(forecast_date, incidence_period, ahead) {
  # This function gives the start and end dates of the target period,
  # based on the system described in the COVIDHub rules here:
  # https://github.com/reichlab/covid19-forecast-hub/blob/master/data-processed/README.md
  #
  # Inputs:
  #  forecast_date: can be a vector of dates
  #  incidence_period: one of "epiweek" or "day"
  #  ahead: how many epiweeks/days ahead are you forecasting?
  forecast_date <- lubridate::ymd(forecast_date)
  if (incidence_period == "day")
    return(tibble(start = forecast_date + ahead, end = forecast_date + ahead))
  assert_that(incidence_period == "epiweek",
              msg="Unsupported incidence_period")
  # incidence_period: epiweek
  ew_frcst_date <- MMWRweek(forecast_date) # get epiweek of forecast_dates
  sunday_of_ew_frcst_date <- MMWRweek2Date(MMWRyear = ew_frcst_date$MMWRyear,
                                           MMWRweek = ew_frcst_date$MMWRweek,
                                           MMWRday = 1) # 1 is Sunday
  # From https://github.com/reichlab/covid19-forecast-hub/blob/master/data-processed/README.md:
  # "For week-ahead forecasts with forecast_date of Sunday or Monday of EW12, a
  # 1 week ahead forecast corresponds to EW12 and should have target_end_date of
  # the Saturday of EW12. For week-ahead forecasts with forecast_date of Tuesday
  # through Saturday of EW12, a 1 week ahead forecast corresponds to EW13 and
  # should have target_end_date of the Saturday of EW13."
  week_ahead <- ifelse(wday(forecast_date) <= 2, # forecasting on a Sun/Monday
                       ahead - 1,
                       ahead)
  tibble(start = sunday_of_ew_frcst_date + week_ahead * 7,
         end = sunday_of_ew_frcst_date + (week_ahead + 1) * 7 - 1)
}

#' returns data frame with column names
#' "forecast_date", "location", "target_start", "target_end", "actual"
get_target_response <- function(signals,
                                forecast_dates,
                                incidence_period,
                                ahead,
                                geo_type) {
  response <- signals[1, ]
  target_periods <- forecast_dates %>%
    enframe(name = NULL, value = "forecast_date") %>%
    mutate(incidence_period = incidence_period,
           ahead = ahead) %>%
    pmap_dfr(get_target_period)

  # Compute the actual values that the forecaster is trying to
  # predict. In particular,
  # - get most recent data available from covidcast for these target periods
  # - sum up the response over the target incidence period
  problems <- target_periods %>%
    mutate(not_available = end > Sys.Date())
  assert_that(!any(problems$not_available),
              msg=paste0("For ahead = ", ahead, " it is too soon to evaluate forecasts on ",
                  "these forecast dates: ",
                  paste(forecast_dates[problems$not_available], collapse = ", ")))
  out <- target_periods %>%
    rename(start_day = start, end_day = end) %>%
    mutate(data_source = response$data_source,
           signal = response$signal,
           geo_type = geo_type) %>%
    pmap(download_signal)

  problem_date <- out %>% map_lgl(~ nrow(.x) == 0) %>% which()
  assert_that(length(problem_date) == 0,
              msg=paste0("No data available for the target periods of these forecast 
                  dates: ",
                  paste(forecast_dates[problem_date], collapse = ", ")))
  names(out) <- forecast_dates
  out <- out %>%
    bind_rows(.id = "forecast_date") %>%
    mutate(forecast_date = lubridate::ymd(forecast_date)) %>%
    group_by(location, forecast_date) %>%
    summarize(actual = sum(value)) %>%
#    mutate(forecast_date = forecast_dates[as.numeric(forecast_date)]) %>%
    left_join(target_periods %>% mutate(forecast_date = forecast_dates),
              by = "forecast_date")
  # record date that this function was run for reproducibility
  attr(out, "as_of") <- Sys.Date()
  return(out)
}
