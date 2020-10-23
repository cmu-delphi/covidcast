#' @param signals a tibble with columns "data_source" and "signal" that
#'   specifies which variables from the covidcast API will be used by
#'   my_forecaster. The first row of signals is taken to be the response.   If
#'   using incidence_period "epiweek", the response should be something for
#'   which summing daily values over an epiweek makes sense (e.g., counts or
#'   proportions but not log(counts) or log(proportions)).  Available data
#'   sources and signals are documented in the [COVIDcast signal
#'   documentation](https://cmu-delphi.github.io/delphi-epidata/api/covidcast_signals.html)
#'    documentation.  Can optionally include a column "first_day" giving the
#'   earliest day of data needed from that data source.
