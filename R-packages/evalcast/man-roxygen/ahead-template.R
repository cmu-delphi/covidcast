#' @param ahead vector of (one or more) integers.  How many epiweeks/days ahead
#'   are you forecasting?  If incidence_period is "epiweek" and forecast_date is
#'   Sunday or Monday, then ahead = 1 means the epiweek that includes the
#'   forecast date;  if forecast_date is Tuesday-Saturday, then it is the
#'   following epiweek. If incidence_period is "day" then ahead = 1 means the
#'   day after forecast date.
