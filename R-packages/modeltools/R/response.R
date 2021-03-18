make_response <- function(signals,
                          response_name,
                          forecast_date,
                          incidence_period,
                          ahead) {
    out_df <- signals
    for (a in ahead) {
        target_period <- as.list(get_target_period(forecast_date, incidence_period, a)[1, ])
        window_size <- as.numeric(target_period$end - target_period$start) + 1
        out_df <- slide_by_geo(out_df,
                               ~ Mean(window_size * .x[[paste0("value-0:", response_name)]]),
                               n = window_size,
                               shift = as.numeric(target_period$end - forecast_date),
                               col_name = paste0("value+", a, ":", response_name))
    }
    return(out_df)
}

#' Get the target period for a forecast date, incidence period and ahead
#'
#' 
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