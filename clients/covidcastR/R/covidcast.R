## API base url
COVIDCAST_BASE_URL <- 'https://delphi.cmu.edu/epidata/api.php'

#' Produce a data frame for one signal.
#'
#' Obtains data for selected date ranges for all geographic regions of the
#' United States. Available data sources and signals are documented in the
#' [COVIDcast signal
#' documentation](https://cmu-delphi.github.io/delphi-epidata/api/covidcast_signals.html).
#' Most (but not all) data sources are available at the county level, but the
#' API can also return data aggregated to metropolitan statistical areas,
#' hospital referral regions, or states, as desired, by using the `geo_type`
#' argument. View `vignette("covidcastR")` for detailed example usage.
#'
#' Downloading large amounts of data may be slow, so this function prints
#' messages for each day of data it downloads. To suppress these, use
#' [base::suppressMessages()], as in `suppressMessages(covidcast_signal("fb-survey",
#' ...))`.
#'
#' @param data_source String identifying the data source to query. See the
#'   [signal
#'   documentation](https://cmu-delphi.github.io/delphi-epidata/api/covidcast_signals.html)
#'   for a list of available sources and signals.
#' @param signal String identifying the signal from that source to query.
#' @param start_day Query data beginning on this date. Date object, or string in
#'   the form YYYYMMDD. If `start_day` is `NULL`, defaults to first day data is
#'   available for this signal.
#' @param end_day Query data up to this date, inclusive. Date object or string
#'   in the form YYYYMMDD. If `end_day` is `NULL`, defaults to the most recent
#'   day data is available for this signal.
#' @param geo_type The geography type for which to request this data, such as
#'   `"county"` or `"state"`. Available types are described in the COVIDcast
#'   signal documentation. Defaults to `"county"`.
#' @param geo_value Which geography to return. The default, `"*"`, fetches all
#'   geographies. To fetch a specific geography, specify its ID as a string.
#' @return Data frame with matching data. Contains `geo_value`, `time_value`,
#'   `direction`, `value`, `stderr`, and `sample_size` columns. `geo_value`
#'   identifies the location, such as a state name or county FIPS code;
#'   `time_value` contains Date objects. `value` is the signal quantity
#'   requested and `stderr` its standard error if available. `sample_size`
#'   indicates the sample size available in that geography on that day; sample
#'   size may not be available. `direction` uses a local linear fit to estimate
#'   whether the signal in this region is currently increasing or decreasing.
#'   Consult the signal documentation for more details.
#' @references COVIDcast API documentation:
#'   \url{https://cmu-delphi.github.io/delphi-epidata/api/covidcast.html}
#'
#' COVIDcast public map: \url{https://covidcast.cmu.edu/}
#' @examples
#' \dontrun{
#' ## fetches all counties from 2020-05-10 to the most recent available data:
#' covidcast_signal("fb-survey", "raw_cli", start_day = "20200510")
#' ## fetches all counties on just 2020-05-10 and no other days
#' covidcast_signal("fb-survey", "raw_cli", start_day = "20200510",
#'                  end_day = "20200510")
#' ## fetch all states on 2020-05-10, 2020-05-11, 2020-05-12
#' covidcast_signal("fb-survey", "raw_cli", start_day = "20200510",
#'                  end_day = "20200512", geo_type = "state")
#' ## fetch all available data for just Pennsylvania
#' covidcast_signal("fb-survey", "raw_cli", geo_type = "state",
#'                  geo_value = "pa")
#' ## fetch all available data in Pittsburgh metropolitan area (identified by
#' ## CBSA ID)
#' covidcast_signal("fb-survey", "raw_cli", geo_type = "msa",
#'                  geo_value = "38300")
#' }
#' @export
#' @importFrom dplyr %>%
covidcast_signal <- function(data_source, signal,
                             start_day = NULL, end_day = NULL,
                             geo_type = c("county", "hrr", "msa", "dma", "state"),
                             geo_value = "*") {
  geo_type <- match.arg(geo_type)

  if (is.null(start_day) | is.null(end_day)) {
    meta <- dplyr::bind_rows(covidcast_meta()$epidata)

    given_data_source <- data_source
    given_signal <- signal
    given_geo_type <- geo_type

    relevant_meta <- meta %>%
      dplyr::filter(data_source == given_data_source,
                    signal == given_signal,
                    time_type == "day",
                    geo_type == given_geo_type)

    if (nrow(relevant_meta) == 0) {
      stop("No match in metadata for source '", data_source,
           "', signal '", signal, "', and geo_type '", geo_type,
           "' at the daily level. ",
           "Check that the source and signal are correctly spelled and that ",
           "the signal is available at this geographic level.")
    }
  }

  if (is.null(start_day)) {
    start_day <- relevant_meta %>%
      dplyr::pull(min_time) %>%
      as.character() %>%
      as.Date(format = "%Y%m%d")
  } else {
      start_day <- as.Date(start_day, format = "%Y%m%d")
  }

  if (is.null(end_day)) {
    end_day <- relevant_meta %>%
      dplyr::pull(max_time) %>%
      as.character() %>%
      as.Date(format = "%Y%m%d")
  } else {
      end_day <- as.Date(end_day, format = "%Y%m%d")
  }

  ndays <- as.numeric(end_day - start_day)
  dat <- list()

  ## The API limits the number of rows that can be returned at once, so we query
  ## each day separately.
  for (i in seq(ndays + 1)) {
    day <- date_to_string(start_day + i - 1)
    dat[[i]] <- covidcast(data_source = data_source,
                          signal = signal,
                          time_type = "day",
                          geo_type = geo_type,
                          time_values = day,
                          geo_value = geo_value)
    message(sprintf("Fetched day %s: %s, %s, num_entries = %s",
                    day,
                    dat[[i]]$result,
                    dat[[i]]$message,
                    length(dat[[i]]$epidata)))

    if (dat[[i]]$message != "success") {
      warning("Failed to obtain data for ", day, ": ", dat[[i]]$message)
    }
  }

  df <- dat %>%
    purrr::map("epidata") %>% # just want $epidata part
    purrr::map(purrr::compact) %>% # this removes the list elements that are NULL
    dplyr::bind_rows() # make this into a data frame

  if (nrow(df) > 0) {
    # If no data is found, there is no time_value column to report
    df$time_value <- as.Date(as.character(df$time_value), format = "%Y%m%d")
  }

  return(df)
}



## Fetch Delphi's COVID-19 Surveillance Streams
covidcast <- function(data_source, signal, time_type, geo_type, time_values, geo_value) {
  # Check parameters
  if(missing(data_source) || missing(signal) || missing(time_type) || missing(geo_type) || missing(time_values) || missing(geo_value)) {
    stop('`data_source`, `signal`, `time_type`, `geo_type`, `time_values`, and `geo_value` are all required')
  }
  # Set up request
  params <- list(
    source = 'covidcast',
    data_source = data_source,
    signal = signal,
    time_type = time_type,
    geo_type = geo_type,
    time_values = .list(time_values),
    geo_value = geo_value
  )
  # Make the API call
  return(.request(params))
}

# Fetch Delphi's COVID-19 Surveillance Streams metadata
covidcast_meta <- function() {
  return(.request(list(source='covidcast_meta', cached="true")))
}

# Helper function to cast values and/or ranges to strings
.listitem <- function(value) {
  if(is.list(value) && 'from' %in% names(value) && 'to' %in% names(value)) {
    return(paste0(toString(value$from), '-', toString(value$to)))
  } else {
    return(toString(value))
  }
}

# Helper function to build a list of values and/or ranges
.list <- function(values) {
  if(!is.list(values) || ('from' %in% names(values) && 'to' %in% names(values))) {
    values <- list(values)
  }
  return(paste0(sapply(values, .listitem), collapse=','))
}

# Helper function to request and parse epidata
.request <- function(params) {
  # API call
  response <- httr::GET(COVIDCAST_BASE_URL, query=params)

  httr::stop_for_status(response, task = "fetch data from API")

  return(jsonlite::fromJSON(httr::content(response, as = "text",
                                          encoding = "utf-8")))
}

# e.g. date_to_string(ymd("20200506")) gives "20200506"; this is the format
# expected by the API
date_to_string <- function(mydate) {
  format(mydate, "%Y%m%d")
}
