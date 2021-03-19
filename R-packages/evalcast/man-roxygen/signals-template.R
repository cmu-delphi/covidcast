#' @param signals Tibble specifying which variables from the COVIDcast API will be downloaded and passed along to `forecaster`.
#'   Each row of `signals` represents a separate variable to be downloaded.  Available data
#'   sources, signals, and geo resolutions are documented in the [COVIDcast signal
#'   documentation](https://cmu-delphi.github.io/delphi-epidata/api/covidcast_signals.html).
#'   We expect the tibble to have the following columns:
#'   \enumerate{
#'       \item `data_source`:  name of the source of the signal
#'       \item `signal`:  name of the signal
#'       \item `geo_type`:  geo resolution at which to pull the data
#'       \item `start_day` (optional):  date on which to start downloading the signal.  Can be
#'           represented as a Date, string in "YYYY-MM-DD" format, or as a function of a Date or 
#'           string.  The latter is useful when the start date should be computed dynamically from
#'           the forecast date (e.g., when `forecaster` only trains on the most recent 4 weeks of 
#'           data).
#'   }
