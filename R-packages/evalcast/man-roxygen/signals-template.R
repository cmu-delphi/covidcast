#' @param signals Tibble with columns `data_source` and `signal` that specifies
#'   which variables from the COVIDcast API will be used by `forecaster`. Each
#'   row of `signals` represents a separate signal, and first row is taken to be
#'   the response. If using `incidence_period = "epiweek"`, the response should
#'   be something for which summing daily values over an epiweek makes sense
#'   (e.g., counts or proportions but not log(counts) or log(proportions)).
#'   Available data sources and signals are documented in the [COVIDcast signal
#'   documentation](https://cmu-delphi.github.io/delphi-epidata/api/covidcast_signals.html).
#'   A few additional optional columns are allowed.
#'   
#'   A column `start_day` can also be included. This can be a Date
#'   object or string in the form "YYYY-MM-DD", indicating the earliest date of
#'   data needed from that data source. Importantly, `start_day` can also be a
#'   function (represented as a list column) that takes a forecast date and
#'   returns a start date for model training (again, Date object or string in
#'   the form "YYYY-MM-DD"). The latter is useful when the start date should be
#'   computed dynamically from the forecast date (e.g., when `forecaster` only
#'   trains on the most recent 4 weeks of data). 
#'   
#'   You may also include a `geo_type` column, a `geo_values` column and/or a
#'   `as_of` column. The first two should contain a string as described below.
#'   The arguments below specify the `geo_type` / `geo_values` to be forecast.
#'   Those will be used by default. Here we allow you to use different data than
#'   what you're actually trying to predict, say using state-level data to 
#'   predict national outcomes. `as_of` will behaves as described for the 
#'   `as_of_override` parameter below.
