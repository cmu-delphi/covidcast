#' Get the target period for a forecast date, incidence period and ahead
#'
#' @template forecast_date-template
#' @template incidence_period-template
#' @template ahead-template
#' 
#' @export
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
              msg="Unsupported `incidence_period`.")
  # incidence_period: epiweek
  ew_frcst_date <- MMWRweek::MMWRweek(forecast_date) # get epiweek of forecast_dates
  sunday_of_ew_frcst_date <- MMWRweek::MMWRweek2Date(
    MMWRyear = ew_frcst_date$MMWRyear,
    MMWRweek = ew_frcst_date$MMWRweek,
    MMWRday = 1) # 1 is Sunday
  # From https://github.com/reichlab/covid19-forecast-hub/blob/master/data-processed/README.md:
  # "For week-ahead forecasts with forecast_date of Sunday or Monday of EW12, a
  # 1 week ahead forecast corresponds to EW12 and should have target_end_date of
  # the Saturday of EW12. For week-ahead forecasts with forecast_date of Tuesday
  # through Saturday of EW12, a 1 week ahead forecast corresponds to EW13 and
  # should have target_end_date of the Saturday of EW13."
  week_ahead <- ifelse(lubridate::wday(forecast_date) <= 2, # forecasting on a Sun/Monday
                       ahead - 1,
                       ahead)
  tibble(start = sunday_of_ew_frcst_date + week_ahead * 7,
         end = sunday_of_ew_frcst_date + (week_ahead + 1) * 7 - 1)
}


#' Compute forecast target date in days
#'
#' This function is most useful for converting a forecast date and an ahead
#' specification from epiweek to days. It allows days as well. 
#'
#' @template forecast_date-template
#' @template incidence_period-template
#' @template ahead-template
#' 
#' @seealso [get_target_period()]
#'
#' @export
#' 
#' @examples 
#' fd <- "2021-03-22" # a monday
#' get_target_ahead(fd, "epiweek", 1) # forecast the following Saturday
#' get_target_ahead("2021-03-23", "epiweek", 1) # forecast the Saturday after next
#' get_target_ahead(fd, "day", 1) # forecast tomorrow
get_target_ahead <- function(forecast_date, incidence_period, ahead) {
  forecast_date <- lubridate::ymd(forecast_date)
  ed <- get_target_period(forecast_date, incidence_period, ahead)$end
  return(as.numeric(ed - forecast_date))
}

