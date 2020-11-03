#' @param ahead Vector of (one or more) integers. How many epiweeks/days ahead
#'   are you forecasting? If `incidence_period = "epiweek"` and forecast_date is
#'   Sunday or Monday, then `ahead = 1` means the epiweek that includes the
#'   forecast date; if `forecast_date` falls on a Tuesday through Saturday, then
#'   it is the following epiweek. If `incidence_period = "day"`, then `ahead =
#'   1` means the day after forecast date.
