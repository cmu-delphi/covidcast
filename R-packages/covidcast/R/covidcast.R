#' covidcast: Client for Delphi's COVIDcast API
#'
#' The covidcast package provides access to numerous COVID-19 data streams,
#' updated daily, covering the United States of America. These include publicly
#' reported cases and deaths data, along with data the Delphi research group
#' collects or obtains from partners.
#'
#' @section Finding data sources and documentation:
#'
#' The COVIDcast API includes:
#'
#' * publicly reported COVID case and death data
#' * insurance claims data reporting on COVID-related doctor's visits and
#'   hospitalizations, obtained from health partners
#' * aggregate results from massive COVID symptom surveys conducted by Delphi
#' * mobility data aggregated from SafeGraph
#' * symptom search trends from Google
#'
#' and numerous other important signals, most available daily at the county
#' level.
#'
#' Each data stream is identified by its data source and signal names. These are
#' documented on the COVIDcast API website:
#' <https://cmu-delphi.github.io/delphi-epidata/api/covidcast_signals.html>
#'
#' Each data stream has a page giving detailed technical documentation on how
#' the data is collected, how it is aggregated, and any limitations or known
#' problems with the data.
#'
#' @section Getting started:
#'
#' We recommend the Getting Started vignette, which includes numerous examples
#' and is provided online here:
#' <https://cmu-delphi.github.io/covidcast/covidcastR/articles/covidcast.html>
#'
#' See also `covidcast_signal()` for details on how to obtain COVIDcast data as
#' a data frame.
#'
#' @docType package
#' @name covidcast
NULL

# API base url
COVIDCAST_BASE_URL <- 'https://api.covidcast.cmu.edu/epidata/api.php'

.onAttach <- function(libname, pkgname) {
  msg <- c("We encourage COVIDcast API users to register on our mailing list:",
           "https://lists.andrew.cmu.edu/mailman/listinfo/delphi-covidcast-api",
           "We'll send announcements about new data sources, package updates,",
           "server maintenance, and new features.")
  packageStartupMessage(paste(msg, collapse = "\n"))
}

#' Obtain a data frame for one COVIDcast signal
#'
#' Obtains data for selected date ranges for all geographic regions of the
#' United States. Available data sources and signals are documented in the
#' [COVIDcast signal
#' documentation](https://cmu-delphi.github.io/delphi-epidata/api/covidcast_signals.html).
#' Most (but not all) data sources are available at the county level, but the
#' API can also return data aggregated to metropolitan statistical areas,
#' hospital referral regions, or states, as desired, by using the `geo_type`
#' argument. View the [covidcast
#' vignette](https://cmu-delphi.github.io/covidcast/covidcastR/articles/covidcast.html)
#' for detailed example usage.
#'
#' For data on counties, metropolitan statistical areas, and states, this
#' package provides the [`county_census`], [`msa_census`], and [`state_census`]
#' datasets. These include each area's unique identifier, used in the
#' `geo_values` argument to select specific areas, and basic information on
#' population and other Census data.
#'
#' The COVIDcast API tracks updates and changes to its underlying data, and
#' records the first date each observation became available. For example, a data
#' source may report its estimate for a specific state on June 3rd on June 5th,
#' once records become available. This data is considered "issued" on June 5th.
#' Later, the data source may update its estimate for June 3rd based on revised
#' data, creating a new issue on June 8th. By default, `covidcast_signal()`
#' returns the most recent issue available for every observation. The `as_of`,
#' `issues`, and `lag` parameters allow the user to select specific issues
#' instead, or to see all updates to observations. These options are mutually
#' exclusive, and you should only specify one; if you specify more than one, you
#' may get an error or confusing results.
#'
#' Note that the API only tracks the initial value of an estimate and *changes*
#' to that value. If a value was first issued on June 5th and never updated,
#' asking for data issued on June 6th (using `issues` or `lag`) would *not*
#' return that value, though asking for data `as_of` June 6th would. See the 
#' [covidcast
#' vignette](https://cmu-delphi.github.io/covidcast/covidcastR/articles/covidcast.html)
#' for examples.
#'
#' Note also that the API enforces a maximum result row limit; results beyond
#' the maximum limit are truncated. This limit is sufficient to fetch
#' observations in all counties in the United States on one day. This client
#' automatically splits queries for multiple days across multiple API calls.
#' However, if data for one day has been issued many times, using the `issues`
#' argument may return more results than the query limit. A warning will be
#' issued in this case. To see all results, split your query across multiple
#' calls with different `issues` arguments.
#'
#' Downloading large amounts of data may be slow, so this function prints
#' messages for each day of data it downloads. To suppress these, use
#' [base::suppressMessages()], as in
#' `suppressMessages(covidcast_signal("fb-survey", ...))`.
#'
#' @param data_source String identifying the data source to query. See the
#'   [signal
#'   documentation](https://cmu-delphi.github.io/delphi-epidata/api/covidcast_signals.html)
#'   for a list of available data sources.
#' @param signal String identifying the signal from that source to query. Again,
#'   see the [signal
#'   documentation](https://cmu-delphi.github.io/delphi-epidata/api/covidcast_signals.html)
#'   for a list of available signals.
#' @param start_day Query data beginning on this date. Date object, or string in
#'   the form "YYYY-MM-DD". If `start_day` is `NULL`, defaults to first day
#'   data is available for this signal.
#' @param end_day Query data up to this date, inclusive. Date object or string
#'   in the form "YYYY-MM-DD". If `end_day` is `NULL`, defaults to the most
#'   recent day data is available for this signal.
#' @param geo_type The geography type for which to request this data, such as
#'   "county" or "state". Defaults to "county". See the [geographic coding
#'   documentation](https://cmu-delphi.github.io/delphi-epidata/api/covidcast_geography.html)
#'   for details on which types are available.
#' @param geo_values Which geographies to return. The default, "*", fetches
#'   all geographies. To fetch specific geographies, specify their IDs as a
#'   vector or list of strings. See the [geographic coding
#'   documentation](https://cmu-delphi.github.io/delphi-epidata/api/covidcast_geography.html)
#'   for details on how to specify these IDs.
#' @param as_of Fetch only data that was available on or before this date,
#'   provided as a `Date` object or string in the form "YYYY-MM-DD". If
#'   `NULL`, the default, return the most recent available data. Note that only
#'   one of `as_of`, `issues`, and `lag` should be provided; it does not make
#'   sense to specify more than one.
#' @param issues Fetch only data that was published or updated ("issued") on
#'   these dates. Provided as either a single `Date` object (or string in the
#'   form "YYYY-MM-DD"), indicating a single date to fetch data issued on, or
#'   a vector specifying two dates, start and end. In this case, return all data
#'   issued in this range. There may be multiple rows for each observation,
#'   indicating several updates to its value. If `NULL`, the default, return the
#'   most recently issued data.
#' @param lag Integer. If, for example, `lag = 3`, then we fetch only data that
#'   was published or updated exactly 3 days after the date. For example, a row
#'   with `time_value` of June 3 will only be included in the results if its
#'   data was issued or updated on June 6. If `NULL`, the default, return the
#'   most recently issued data regardless of its lag.
#'
#' @return Data frame with matching data. Each row is one observation of one
#'   signal on one day in one geographic location. Contains the following
#'   columns:
#'
#'   \item{data_source}{Data source from which this observation was obtained.}
#'   \item{signal}{Signal from which this observation was obtained.}
#'   \item{geo_value}{String identifying the location, such as a state name or
#'   county FIPS code.}
#'   \item{time_value}{Date object identifying the date of this observation.}
#'   \item{issue}{Date object identifying the date this estimate was issued.
#'   For example, an estimate with a `time_value` of June 3 might have been
#'   issued on June 5, after the data for June 3rd was collected and ingested
#'   into the API.}
#'   \item{lag}{Integer giving the difference between `issue` and `time_value`,
#'   in days.}
#'   \item{value}{Signal value being requested. For example, in a query for the
#'   "confirmed_cumulative_num" signal from the "usa-facts" source, this would
#'   be the cumulative number of confirmed cases in the area, as of the given
#'   `time_value`.}
#'   \item{stderr}{Associated standard error of the signal value, if available.}
#'   \item{sample_size}{Integer indicating the sample size available in that
#'   geography on that day; sample size may not be available for all signals,
#'   due to privacy or other constraints, in which case it will be `NA`.}
#'
#'   Consult the signal documentation for more details on how values and
#'   standard errors are calculated for specific signals.
#' 
#' @references COVIDcast API documentation:
#'   \url{https://cmu-delphi.github.io/delphi-epidata/api/covidcast.html}
#'
#' Documentation of all COVIDcast sources and signals:
#' \url{https://cmu-delphi.github.io/delphi-epidata/api/covidcast_signals.html}
#'
#' COVIDcast public map: \url{https://covidcast.cmu.edu/}
#'
#' @examples \dontrun{
#' ## Fetch all counties from 2020-05-10 to the most recent available data
#' covidcast_signal("fb-survey", "smoothed_cli", start_day = "2020-05-10")
#' ## Fetch all counties on just 2020-05-10 and no other days
#' covidcast_signal("fb-survey", "smoothed_cli", start_day = "2020-05-10",
#'                  end_day = "2020-05-10")
#' ## Fetch all states on 2020-05-10, 2020-05-11, 2020-05-12
#' covidcast_signal("fb-survey", "smoothed_cli", start_day = "2020-05-10",
#'                  end_day = "2020-05-12", geo_type = "state")
#' ## Fetch all available data for just Pennsylvania and New Jersey
#' covidcast_signal("fb-survey", "smoothed_cli", geo_type = "state",
#'                  geo_values = c("pa", "nj"))
#' ## Fetch all available data in the Pittsburgh metropolitan area
#' covidcast_signal("fb-survey", "smoothed_cli", geo_type = "msa",
#'                  geo_values = name_to_cbsa("Pittsburgh"))
#' }
#'
#' @seealso [plot.covidcast_signal()], [`county_census`], [`msa_census`],
#'     [`state_census`]
#' @export
#' @importFrom rlang abort
#' @importFrom dplyr %>%
covidcast_signal <- function(data_source, signal,
                             start_day = NULL, end_day = NULL,
                             geo_type = c("county", "hrr", "msa", "dma", "state"),
                             geo_values = "*",
                             as_of = NULL, issues = NULL, lag = NULL) {
  geo_type <- match.arg(geo_type)
  meta <- covidcast_meta()
  given_data_source <- data_source
  given_signal <- signal
  given_geo_type <- geo_type
  relevant_meta <- meta %>%
    dplyr::filter(.data$data_source == given_data_source,
                  .data$signal == given_signal,
                  .data$time_type == "day",
                  .data$geo_type == given_geo_type)

  if (nrow(relevant_meta) == 0) {
    # even if no other metadata is available, we should set the geo_type so
    # plotting functions can deal with this signal.
    relevant_meta <- list(geo_type = geo_type)
  }

  if (is.null(start_day) || is.null(end_day)) {
    if (is.null(relevant_meta$max_time) || is.null(relevant_meta$min_time)) {
      abort(
        paste0("No match in metadata for source '", data_source,
               "', signal '", signal, "', and geo_type '", geo_type,
               "' at the daily level. ",
               "Check that the source and signal are correctly spelled and ",
               "that the signal is available at this geographic level."),
        data_source = data_source,
        signal = signal,
        geo_type = geo_type,
        class = "covidcast_meta_not_found"
      )
    }
  }

  if (is.null(start_day)) {
    start_day <- relevant_meta$min_time
  } else {
    start_day <- as.Date(start_day)
  }

  if (is.null(end_day)) {
    end_day <- relevant_meta$max_time
  } else {
    end_day <- as.Date(end_day)
  }

  if (start_day > end_day) {
    stop("`end_day` must be on or after `start_day`.")
  }

  if (!is.null(as_of)) {
    as_of <- as.Date(as_of)
  }

  if (!is.null(issues)) {
    issues <- as.Date(issues)
  }

  df <- covidcast_days(data_source, signal, start_day, end_day, geo_type,
                       geo_values, as_of, issues, lag)

  # Drop direction column (if it still exists)
  df$direction <- NULL

  # Assign covidcast_signal class and add some helpful attributes
  class(df) <- c("covidcast_signal", "data.frame")
  attributes(df)$metadata <- relevant_meta
  return(df)
}

#' Print `covidcast_signal` object
#'
#' Prints a brief summary of the data source, signal, and geographic level, and
#' then prints the underlying data frame, for an object returned by
#' `covidcast_signal()`. 
#'
#' @param x The `covidcast_signal` object.
#' @param ... Additional arguments passed to `print.data.frame()` to print the 
#'   data.
#'
#' @method print covidcast_signal
#' @export
print.covidcast_signal = function(x, ...) {
  cat(sprintf("A `covidcast_signal` data frame with %i rows and %i columns.\n\n",
              nrow(x), ncol(x)))
  cat(sprintf("%-12s: %s\n", "data_source", x$data_source[1]))
  cat(sprintf("%-12s: %s\n", "signal", x$signal[1]))
  cat(sprintf("%-12s: %s\n\n", "geo_type", attributes(x)$metadata$geo_type))

  # forward to print the data as well
  NextMethod("print")
}

#' @method head covidcast_signal
#' @importFrom utils head
#' @export
head.covidcast_signal = function(x, ...) {
  head(as.data.frame(x), ...)
}

#' Summarize `covidcast_signal` object
#'
#' Prints a variety of summary statistics about the underlying data, such as
#' median values, the date range included, sample sizes, and so on, for an
#' object returned by `covidcast_signal()`.
#'
#' @param object The `covidcast_signal` object.
#' @param ... Additional arguments, for compatibility with `summary()`.
#'   Currently unused.
#'
#' @method summary covidcast_signal
#' @importFrom stats median
#' @export
summary.covidcast_signal = function(object, ...) {
  x <- object
  cat(sprintf("A `covidcast_signal` data frame with %i rows and %i columns.\n\n",
              nrow(x), ncol(x)))
  cat(sprintf("%-12s: %s\n", "data_source", x$data_source[1]))
  cat(sprintf("%-12s: %s\n", "signal", x$signal[1]))
  cat(sprintf("%-12s: %s\n\n", "geo_type", attributes(x)$metadata$geo_type))
  cat(sprintf("%-36s: %s\n", "first date", min(x$time_value)))
  cat(sprintf("%-36s: %s\n", "last date", max(x$time_value)))
  cat(sprintf("%-36s: %i\n", "median number of geo_values per day",
              as.integer(x %>% dplyr::group_by(.data$time_value) %>%
                         dplyr::summarize(num = dplyr::n()) %>%
                         dplyr::summarize(median(.data$num)))))
}

##########

#' Obtain multiple COVIDcast signals at once
#'
#' This convenience function uses `covidcast_signal()` to obtain multiple
#' signals, potentially from multiple data sources.
#'
#' @details The argument structure is just as in `covidcast_signal()`, except
#'   the first four arguments `data_source`, `signal`, `start_day`, `end_day`
#'   are permitted to be vectors. The first two arguments `data_source`, `signal` are
#'   recycled appropriately, in the calls to `covidcast_signal()`; see example
#'   below. The next two arguments `start_day`, `end_day`, unless `NULL`, must
#'   be either length 1 or N.
#'
#' @return A list of `covidcast_signal` data frames, of length `N =
#'     max(length(data_source), length(signal))`. This list can be aggregated
#'     into a single data frame of either "wide" or "long" format using
#'     `aggregate_signals()`.
#'
#' @inheritParams covidcast_signal
#'
#' @seealso [covidcast_signal()], [aggregate_signals()]
#' @examples
#' \dontrun{
#' ## Fetch USAFacts confirmed cases and deaths over the same time period
#' covidcast_signals("usa-facts", signal=c("confirmed_incidence_num",
#'                                         "deaths_incidence_num"),
#'                    start_day = "2020-08-15", end_day = "2020-10-01")
#' }
#' @export
covidcast_signals <- function(data_source, signal,
                              start_day = NULL, end_day = NULL,
                              geo_type = c("county", "hrr", "msa", "dma", "state"),
                              geo_values = "*",
                              as_of = NULL, issues = NULL, lag = NULL) {
  N <- max(length(data_source), length(signal))
  df_list <- vector("list", N)

  if (!(length(start_day) %in% c(0, 1, N))) {
    stop("When `start_day` is not `NULL`, it should have length 1 or N, where ",
         "`N = max(length(data_source), length(signal)`.")
  }
  
  if (!(length(end_day) %in% c(0, 1, N))) { 
    stop("When `end_day` is not `NULL`, it should have length 1 or N, where ",
         "`N = max(length(data_source), length(signal)`.")
  }

  data_source <- rep(data_source, length.out = N)
  signal <- rep(signal, length.out = N)
  start_day <- rep(start_day, length.out = N)
  end_day <- rep(end_day, length.out = N)
  
  for (i in 1:N) {
    df_list[[i]] <- covidcast_signal(data_source = data_source[i],
                                     signal = signal[i],
                                     start_day = start_day[i],
                                     end_day = end_day[i],
                                     geo_type = geo_type,
                                     geo_values = geo_values,
                                     as_of = as_of, issues = issues,
                                     lag = lag)
  }

  return(df_list)
}

##########

#' Obtain COVIDcast metadata
#'
#' Obtains a data frame of metadata describing all publicly available data
#' streams from the COVIDcast API.
#'
#' @return Data frame containing one row per signal, with the following columns:
#'   \item{data_source}{Data source name.}
#'   \item{signal}{Signal name.}
#'   \item{min_time}{First day for which this signal is available.}
#'   \item{max_time}{Most recent day for which this signal is available.}
#'   \item{geo_type}{Geographic level for which this signal is available, such
#'   as county, state, msa, or hrr. Most signals are available at multiple
#'   geographic levels and will hence be listed in multiple rows with their
#'   own metadata.}
#'   \item{time_type}{Temporal resolution at which this signal is reported.
#'   "day", for example, means the signal is reported daily.}
#'   \item{num_locations}{Number of distinct geographic locations available for
#'   this signal. For example, if `geo_type` is county, the number of counties
#'   for which this signal has ever been reported.}
#'   \item{min_value}{Smallest value that has ever been reported.}
#'   \item{max_value}{Largest value that has ever been reported.}
#'   \item{mean_value}{Arithmetic mean of all reported values.}
#'   \item{stdev_value}{Sample standard deviation of all reported values.}
#'   \item{max_issue}{Most recent issue date for this signal.}
#'   \item{min_lag}{Smallest lag from observation to issue, in `time_type` units}
#'   \item{max_lag}{Largest lag from observation to issue, in `time_type` units}
#'
#' @references COVIDcast API sources and signals documentation:
#'   \url{https://cmu-delphi.github.io/delphi-epidata/api/covidcast_signals.html}
#'
#' @seealso [summary.covidcast_meta()]
#'
#' @export
covidcast_meta <- function() {
  meta <- .request(list(source='covidcast_meta', cached="true"))

  if (meta$message != "success") {
    abort(paste0("Failed to obtain metadata: ", meta$message, "."),
          err_msg = meta$message,
          class = "covidcast_meta_fetch_failed")
  }

  meta <- meta$epidata %>%
    dplyr::mutate(min_time = as.Date(as.character(.data$min_time), format = "%Y%m%d"),
                  max_time = as.Date(as.character(.data$max_time), format = "%Y%m%d"),
                  max_issue = as.Date(as.character(.data$max_issue), format = "%Y%m%d"))

  class(meta) <- c("covidcast_meta", "data.frame")
  return(meta)
}

#' Print `covidcast_meta` object
#'
#' Prints a brief summary of the metadata, and then prints the underlying data
#' frame, for an object returned by `covidcast_meta()`.
#'
#' @param x The `covidcast_meta` object.
#' @param ... Additional arguments passed to `print.data.frame()` to print the
#'   data.
#'
#' @method print covidcast_meta
#' @export
print.covidcast_meta = function(x, ...) {
  cat(sprintf("A `covidcast_meta` data frame with %i rows and %i columns.\n\n",
              nrow(x), ncol(x)))
  cat(sprintf("%-23s: %s\n", "Number of data sources",
              length(unique(x$data_source))))
  cat(sprintf("%-23s: %s\n\n", "Number of signals",
              length(unique(paste(x$data_source, x$signal)))))

  # forward to print the data as well
  NextMethod("print")
}

#' @method head covidcast_meta
#' @importFrom utils head
#' @export
head.covidcast_meta = function(x, ...) {
  head(as.data.frame(x), ...)
}

#' Summarize `covidcast_meta` object
#'
#' Prints a tabular summary of the object returned by `covidcast_meta()`.
#'
#' @param object The `covidcast_meta` object.
#' @param ... Additional arguments, for compatibility with `summary()`.
#'   Currently unused.
#'
#' @method summary covidcast_meta
#' @export
summary.covidcast_meta = function(object, ...) {
  x <- object
  cat(sprintf("A `covidcast_meta` data frame with %i rows and %i columns.\n\n",
              nrow(x), ncol(x)))
  cat(sprintf("%-23s: %s\n", "Number of data sources",
              length(unique(x$data_source))))
  cat(sprintf("%-23s: %s\n\n", "Number of signals",
              length(unique(paste(x$data_source, x$signal)))))
  cat("Summary:\n\n")
  df <- suppressMessages(
    x %>% dplyr::group_by(data_source, signal) %>%
    dplyr::summarize(county = ifelse("county" %in% geo_type, "*", ""),
                     msa = ifelse("msa" %in% geo_type, "*", ""),
                     hrr = ifelse("hrr" %in% geo_type, "*", ""),
                     state = ifelse("state" %in% geo_type, "*", "")) %>%
    dplyr::ungroup()
  )
  print(as.data.frame(df), right = FALSE, row.names = FALSE)
  invisible(df)
}

##########

# Helper function, not user-facing, to loop through a sequence of days, call
# covidcast for each one and combine the results
covidcast_days <- function(data_source, signal, start_day, end_day, geo_type,
                       geo_value, as_of, issues, lag) {
  ndays <- as.numeric(end_day - start_day)
  dat <- list()

  # The API limits the number of rows that can be returned at once, so we query
  # each day separately.
  for (i in seq(ndays + 1)) {
    query_day <- start_day + i - 1
    day_str <- date_to_string(query_day)
    dat[[i]] <- covidcast(data_source = data_source,
                          signal = signal,
                          time_type = "day",
                          geo_type = geo_type,
                          time_values = day_str,
                          geo_value = geo_value,
                          as_of = as_of,
                          issues = issues,
                          lag = lag)
    summary <- sprintf(
      "Fetched day %s: %s, %s, num_entries = %s",
      query_day,
      dat[[i]]$result,
      dat[[i]]$message,
      nrow(dat[[i]]$epidata)
    )
    if (length(summary) != 0) {
      message(summary)
    }
    if (dat[[i]]$message == "success") {
      returned_geo_values <- dat[[i]]$epidata$geo_value
      if (!identical("*", geo_value)) {
        missed_geos <- setdiff(tolower(geo_value),
                               tolower(returned_geo_values))
        if (length(missed_geos) > 0) {
          missed_geos_str <- paste0(missed_geos, collapse = ", ")
          warn(sprintf("Data not fetched for some geographies on %s: %s",
                         query_day, missed_geos_str),
               data_source = data_source,
               signal = signal,
               day = query_day,
               geo_value = geo_value,
               api_msg = dat[[i]]$message,
               class = "covidcast_missing_geo_values"
               )
        }
      }
    } else {
      warn(paste0("Fetching ", signal, " from ", data_source, " for ",
                  query_day, " in geography '", geo_value, "': ",
                  dat[[i]]$message),
           data_source = data_source,
           signal = signal,
           day = query_day,
           geo_value = geo_value,
           api_msg = dat[[i]]$message,
           class = "covidcast_fetch_failed")
    }
  }

  df <- dat %>%
    purrr::map("epidata") %>% # just want $epidata part
    purrr::map(purrr::compact) %>% # remove the list elements that are NULL
    dplyr::bind_rows() # make this into a data frame

  if (nrow(df) > 0) {
    # If no data is found, there is no time_value column to report
    df$time_value <- as.Date(as.character(df$time_value), format = "%Y%m%d")
    df$issue <- as.Date(as.character(df$issue), format = "%Y%m%d")
    df$data_source <- data_source
    df$signal <- signal

    # Reorder data_source, signal, geo_value, time_value, so that they appear in
    # this order
    df <- dplyr::relocate(df, .data$data_source, .data$signal, .data$geo_value,
                          .data$time_value)
  }

  return(df)
}

# Fetch Delphi's COVID-19 indicators
covidcast <- function(data_source, signal, time_type, geo_type, time_values,
                      geo_value, as_of, issues, lag) {
  # Check parameters
  if(missing(data_source) || missing(signal) || missing(time_type) ||
       missing(geo_type) || missing(time_values) || missing(geo_value)) {
    stop("`data_source`, `signal`, `time_type`, `geo_type`, `time_values`, ",
         "and `geo_value` are all required.")
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
  if (length(params$geo_value) > 1) {
    params$geo_values <- paste0(params$geo_value, collapse = ",") #convert to string
    params$geo_value <- NULL
  }
  if (!is.null(as_of)) {
    params$as_of <- date_to_string(as_of)
  }

  if (!is.null(issues)) {
    if (length(issues) == 2) {
      params$issues <- paste0(date_to_string(issues[1]),
                              "-",
                              date_to_string(issues[2]))
    } else if (length(issues) == 1) {
      params$issues <- date_to_string(issues)
    } else {
      stop("`issues` must be either a single date or a date interval.")
    }
  }

  if (!is.null(lag)) {
    params$lag <- lag
  }

  # Make the API call
  return(.request(params))
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
  response <- httr::GET(COVIDCAST_BASE_URL, httr::user_agent("covidcastR"),
                        query=params)

  httr::stop_for_status(response, task = "fetch data from API")

  return(jsonlite::fromJSON(httr::content(response, as = "text",
                                          encoding = "utf-8")))
}

# This is the date format expected by the API
date_to_string <- function(mydate) {
  format(mydate, "%Y%m%d")
}
