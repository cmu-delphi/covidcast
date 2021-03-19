#' @param signals_to_use Tibble with mandatory columns `data_source` and `signal` that 
#'   specifies which variables from the COVIDcast API will be used by `forecaster`. Each
#'   row of `signals` represents a separate signal, and first row is taken to be
#'   the response. If using `incidence_period = "epiweek"`, the response should
#'   be something for which summing daily values over an epiweek makes sense
#'   (e.g., counts or proportions but not log(counts) or log(proportions)).
#'   Available data sources and signals are documented in the [COVIDcast signal
#'   documentation](https://cmu-delphi.github.io/delphi-epidata/api/covidcast_signals.html).
#'   A few optional columns are also allowed. If not specified, these will default
#'   to the values of the similarly named argument.
#'   
#'   A column `start_day` can be included. This can be a [Date]
#'   object or string in the form "YYYY-MM-DD", indicating the earliest date of
#'   data needed from that data source. Importantly, `start_day` can also be a
#'   function (represented as a list column) that takes a forecast date and
#'   returns a start date for model training (again, Date object or string in
#'   the form "YYYY-MM-DD"). The latter is useful when the start date should be
#'   computed dynamically from the forecast date (e.g., when `forecaster` only
#'   trains on the most recent 4 weeks of data). 
#'   
#'   You may also include a `geo_type` column, a `geo_values` column and/or an
#'   `as_of` column. 
#'   
#'   The first two should contain a string. If unspecified, these will have
#'   the same defaults as `covidcast::covidcast_signal()`, namely 
#'   `geo_type = "county"` and `geo_values = "*"`.
#'   These arguments allow you to override 
#'   to download different data than
#'   what you're actually trying to predict, say using state-level data to 
#'   predict national outcomes. 
#'   
#'   
#'   By default, the `as_of` date of data downloaded from
#'   COVIDcast is loaded with `as_of = forecast_date`. This means that data
#'   is "rewound" to days in the past. Any data revisions made since, would
#'   not have been present at that time, and would not be available to the
#'   forecaster. It's likely, for example, that no data would actually exist
#'   for the forecast date on the forecast date (there is some latency between
#'   the time signals are reported and the dates for which they are reported).
#'   You can override this functionality, though we strongly advise you do so
#'   with care, by passing a function of the forecast_date or a single date 
#'   here. The function should return a [Date].
#'   
#'   
