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
#' We recommend browsing the vignettes, which include numerous examples:
#' `browseVignettes(package = "covidcast")`.
#'
#' See also `covidcast_signal()` for details on how to obtain COVIDcast data as
#' a data frame.
#'
#' @references A. Reinhart et al., An open repository of real-time COVID-19
#'   indicators. Proc. Natl. Acad. Sci. U.S.A. 118, e2111452118 (2021).
#'   \doi{10.1073/pnas.2111452118}
#'
#' @keywords internal
"_PACKAGE"

# API base url
COVIDCAST_BASE_URL <- 'https://api.covidcast.cmu.edu/epidata/'

# Max rows returned by API
MAX_RESULTS <- 1000000

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
#' COVIDcast signal documentation.
#' Most (but not all) data sources are available at the county level, but the
#' API can also return data aggregated to metropolitan statistical areas,
#' hospital referral regions, or states, as desired, by using the `geo_type`
#' argument.
#'
#' For data on counties, metropolitan statistical areas, and states, this
#' package provides the [`county_census`], [`msa_census`], and [`state_census`]
#' datasets. These include each area's unique identifier, used in the
#' `geo_values` argument to select specific areas, and basic information on
#' population and other Census data.
#'
#' Downloading large amounts of data may be slow, so this function prints
#' messages for each chunk of data it downloads. To suppress these, use
#' [base::suppressMessages()], as in
#' `suppressMessages(covidcast_signal("fb-survey", ...))`.
#'
#' @section Metadata:
#'
#' The returned object has a `metadata` attribute attached containing basic
#' information about the signal. Use `attributes(x)$metadata` to access this
#' metadata. The metadata is stored as a data frame of one row, and contains the
#' same information that `covidcast_meta()` would return for a given signal.
#'
#' Note that not all `covidcast_signal` objects may have all fields of metadata
#' attached; for example, an object created with `as.covidcast_signal()` using
#' data from another source may only contain the `geo_type` variable, along with
#' `data_source` and `signal`. Before using the metadata of a `covidcast_signal`
#' object, always check for the presence of the attributes you need.
#'
#' @section Issue dates and revisions:
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
#' return that value, though asking for data `as_of` June 6th would. See
#' `vignette("covidcast")` for examples.
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
#' @param data_source String identifying the data source to query. See
#'   <https://cmu-delphi.github.io/delphi-epidata/api/covidcast_signals.html>
#'   for a list of available data sources.
#' @param signal String identifying the signal from that source to query. Again,
#'   see <https://cmu-delphi.github.io/delphi-epidata/api/covidcast_signals.html>
#'   for a list of available signals.
#' @param start_day Query data beginning on this date. Date object, or string in
#'   the form "YYYY-MM-DD". If `start_day` is `NULL`, defaults to first day data
#'   is available for this signal.
#' @param end_day Query data up to this date, inclusive. Date object or string
#'   in the form "YYYY-MM-DD". If `end_day` is `NULL`, defaults to the most
#'   recent day data is available for this signal.
#' @param geo_type The geography type for which to request this data, such as
#'   "county" or "state". Defaults to "county". See
#'   <https://cmu-delphi.github.io/delphi-epidata/api/covidcast_geography.html>
#'   for details on which types are available.
#' @param geo_values Which geographies to return. The default, "*", fetches all
#'   geographies. To fetch specific geographies, specify their IDs as a vector
#'   or list of strings. See
#'   <https://cmu-delphi.github.io/delphi-epidata/api/covidcast_geography.html>
#'   for details on how to specify these IDs.
#' @param as_of Fetch only data that was available on or before this date,
#'   provided as a `Date` object or string in the form "YYYY-MM-DD". If `NULL`,
#'   the default, return the most recent available data. Note that only one of
#'   `as_of`, `issues`, and `lag` should be provided; it does not make sense to
#'   specify more than one. For more on data revisions, see
#'   "Issue dates and revisions" below.
#' @param issues Fetch only data that was published or updated ("issued") on
#'   these dates. Provided as either a single `Date` object (or string in the
#'   form "YYYY-MM-DD"), indicating a single date to fetch data issued on, or a
#'   vector specifying two dates, start and end. In this case, return all data
#'   issued in this range. There may be multiple rows for each observation,
#'   indicating several updates to its value. If `NULL`, the default, return the
#'   most recently issued data.
#' @param lag Integer. If, for example, `lag = 3`, then we fetch only data that
#'   was published or updated exactly 3 days after the date. For example, a row
#'   with `time_value` of June 3 will only be included in the results if its
#'   data was issued or updated on June 6. If `NULL`, the default, return the
#'   most recently issued data regardless of its lag.
#' @param time_type The temporal resolution to request this data. Most signals
#'   are available at the "day" resolution (the default); some are only
#'   available at the "week" resolution, representing an MMWR week ("epiweek").
#'
#' @return `covidcast_signal` object with matching data. The object is a data
#'   frame with additional metadata attached. Each row is one observation of one
#'   signal on one day in one geographic location. Contains the following
#'   columns:
#'
#'   \item{data_source}{Data source from which this observation was obtained.}
#'   \item{signal}{Signal from which this observation was obtained.}
#'   \item{geo_value}{String identifying the location, such as a state name or
#'   county FIPS code.}
#'   \item{time_value}{Date object identifying the date of this observation. For
#'   data with `time_type = "week"`, this is the first day of the corresponding
#'   epiweek.}
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
#'   The returned data frame has a `metadata` attribute containing metadata
#'   about the signal contained within; see "Metadata" below for details.
#'
#' @references COVIDcast API documentation:
#'   \url{https://cmu-delphi.github.io/delphi-epidata/api/covidcast.html}
#'
#' Documentation of all COVIDcast sources and signals:
#' \url{https://cmu-delphi.github.io/delphi-epidata/api/covidcast_signals.html}
#'
#' COVIDcast public dashboard: \url{https://delphi.cmu.edu/covidcast/}
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
#' @seealso [plot.covidcast_signal()], [covidcast_signals()],
#'   [`as.covidcast_signal()`], [`county_census`], [`msa_census`],
#'   [`state_census`]
#' @export
#' @importFrom rlang abort `:=`
#' @importFrom dplyr `%>%`
covidcast_signal <- function(data_source, signal,
                             start_day = NULL, end_day = NULL,
                             geo_type = c("county", "hrr", "msa", "dma",
                                          "state", "hhs", "nation"),
                             geo_values = "*",
                             as_of = NULL, issues = NULL, lag = NULL,
                             time_type = c("day", "week")) {
  geo_type <- match.arg(geo_type)
  time_type <- match.arg(time_type)

  relevant_meta <- specific_meta(data_source, signal, geo_type, time_type)

  if (is.null(start_day) || is.null(end_day)) {
    if (is.null(relevant_meta$max_time) || is.null(relevant_meta$min_time)) {
      abort(
        paste0("No match in metadata for source '", data_source,
               "', signal '", signal, "', and geo_type '", geo_type,
               "' at the ", time_type, " level. ",
               "Check that the source and signal are correctly spelled and ",
               "that the signal is available at this geographic level."),
        data_source = data_source,
        signal = signal,
        geo_type = geo_type,
        time_type = time_type,
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

    # By definition, there can never be data from the future. So clamp `end_day`
    # to be no larger than `as_of`.
    end_day <- min(as_of, end_day)
  }

  if (!is.null(issues)) {
    issues <- as.Date(issues)
  }
  if (identical(geo_values, "*")) {
    max_geos <- relevant_meta$num_locations
  } else {
    max_geos <- length(geo_values)
  }
  df <- covidcast_days(data_source, signal, start_day, end_day, geo_type,
                       geo_values, time_type, as_of, issues, lag, max_geos)

  # Drop direction column (if it still exists)
  df$direction <- NULL

  return(as.covidcast_signal(df, signal, geo_type, time_type, data_source,
                             metadata = relevant_meta))
}

#' Convert data from an external source into a form compatible with
#' `covidcast_signal`.
#'
#' Several methods are provided to convert common objects (such as data frames)
#' into `covidcast_signal` objects, which can be used with the various
#' `covidcast_signal` methods (such as `plot.covidcast_signal()` or
#' `covidcast_cor()`). See `vignette("external-data")` for examples.
#'
#' @param x Object to be converted. See Methods section below for details on
#'   formatting of each input type.
#' @param geo_type The geography type stored in this object.
#' @param time_type The time resolution stored in this object. If "day", the
#'   default, each observation covers one day. If "week", each time value is
#'   assumed to be the start date of the epiweek (MMWR week) that the data
#'   represents.
#' @param data_source The name of the data source to use as a label for this
#'   data.
#' @param signal The signal name to use for this data.
#' @param issue Issue date to use for this data, if not present in `x`, as a
#'   `Date` object. If no issue date is present in `x` and `issue` is `NULL`,
#'   today's date will be used.
#' @param metadata List of metadata to attach to the `covidcast_signal` object.
#'   See the "Metadata" section of `covidcast_signal()`. All objects will have
#'   `geo_type`, `data_source`, and `signal` columns included in their metadata;
#'   named entries in this list are added as additional columns.
#' @param ... Additional arguments passed to methods.
#' @return `covidcast_signal` object; see `covidcast_signal()` for documentation
#'   of fields and structure.
#' @seealso [`covidcast_signal()`]
#' @export
as.covidcast_signal <- function(x, ...) {
  UseMethod("as.covidcast_signal")
}

#' @method as.covidcast_signal covidcast_signal
#' @describeIn as.covidcast_signal Simply returns the `covidcast_signal` object
#'   unchanged.
#' @export
as.covidcast_signal.covidcast_signal <- function(x, ...) {
  return(x)
}

#' @method as.covidcast_signal data.frame
#' @describeIn as.covidcast_signal The input data frame `x` must contain the
#'   columns `time_value`, `value`, and `geo_value`. If an `issue` column is
#'   present in `x`, it will be used as the issue date for each observation; if
#'   not, the `issue` argument will be used. Other columns will be preserved
#'   as-is.
#' @export
as.covidcast_signal.data.frame <- function(x,
                                           signal = NULL,
                                           geo_type = c(
                                             "county",
                                             "msa",
                                             "hrr",
                                             "dma",
                                             "state",
                                             "hhs",
                                             "nation"
                                           ),
                                           time_type = c("day", "week"),
                                           data_source = "user",
                                           issue = NULL,
                                           metadata = list(),
                                           ...) {
  if (is.null(signal)) {
    abort(paste0(c(
      "when `x` is a data frame, signal name must",
      " be provided to as.covidcast_signal"
    ), collapse = ""), class = "covidcast_coerce_signal")
  }

  geo_type <- match.arg(geo_type)
  time_type <- match.arg(time_type)

  metadata$data_source <- data_source
  metadata$signal <- signal
  metadata$geo_type <- geo_type
  metadata$time_type <- time_type
  metadata <- as.data.frame(metadata)

  if (!("data_source" %in% names(x))) {
    x$data_source <- data_source
  }

  if (!("signal" %in% names(x))) {
    x$signal <- signal
  }

  class(x) <- c("covidcast_signal", "data.frame")

  if (nrow(x) == 0) {
    # No data; columns don't matter.
    return(x)
  }

  if (!("time_value" %in% names(x))) {
    abort(paste0(c(
      "`x` must contain a `time_value` column",
      " containing the time of each observation"
    ), collapse = ""), class = "covidcast_coerce_time_value")
  }

  if (!("value" %in% names(x))) {
    abort(paste0(c(
      "`x` must contain a `value` column",
      " containing the value of each observation"
    ), collapse = ""), class = "covidcast_coerce_value")
  }

  if (!("geo_value" %in% names(x))) {
    abort(paste0(c(
      "`x` must contain a `geo_value` column",
      " containing the location of each observation"
    ), collapse = ""), class = "covidcast_coerce_geo_value")
  }

  # issue is optional; if omitted, use default
  if (!("issue" %in% names(x))) {
    if (is.null(issue)) {
      x$issue <- Sys.Date()
    } else {
      x$issue <- issue
    }
  }

  # Reorder data_source, signal, geo_value, time_value, so that they appear in
  # this order.
  x <- dplyr::relocate(x, "data_source", "signal", "geo_value", "time_value")

  attributes(x)$metadata <- metadata

  return(x)
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
#' @return The `covidcast_signal` object, unchanged.
#'
#' @method print covidcast_signal
#' @export
print.covidcast_signal <- function(x, ...) {
  cat(sprintf("A `covidcast_signal` dataframe with %i rows and %i columns.\n\n",
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
head.covidcast_signal <- function(x, ...) {
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
#' @return No return value; called only to print summary statistics.
#'
#' @method summary covidcast_signal
#' @importFrom stats median
#' @export
summary.covidcast_signal <- function(object, ...) {
  x <- object
  cat(sprintf("A `covidcast_signal` dataframe with %i rows and %i columns.\n\n",
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
#'   are permitted to be vectors. The first two arguments `data_source` and
#'   `signal` are recycled appropriately in the calls to `covidcast_signal()`;
#'   see example below. The next two arguments `start_day`, `end_day`, unless
#'   `NULL`, must be either length 1 or N.
#'
#' See `vignette("multi-signals")` for additional examples.
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
#' covidcast_signals("usa-facts", signal = c("confirmed_incidence_num",
#'                                           "deaths_incidence_num"),
#'                    start_day = "2020-08-15", end_day = "2020-10-01")
#' }
#' @export
covidcast_signals <- function(data_source, signal,
                              start_day = NULL, end_day = NULL,
                              geo_type = c(
                                "county",
                                "hrr",
                                "msa",
                                "dma",
                                "state",
                                "hhs",
                                "nation"
                              ),
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

  # rep() throws a warning on NULLs (and you can't make a vector of NULLs), so
  # only replicate these when provided. When NULL, leave as NULL so
  # `covidcast_signal()` can determine correct dates from metadata.
  if (!is.null(start_day)) {
    start_day <- rep(start_day, length.out = N)
  }

  if (!is.null(end_day)) {
    end_day <- rep(end_day, length.out = N)
  }

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
#'  this signal. For example, if `geo_type` is county, the number of counties
#'  for which this signal has ever been reported.}
#'  \item{min_value}{Smallest value that has ever been reported.}
#'  \item{max_value}{Largest value that has ever been reported.}
#'  \item{mean_value}{Arithmetic mean of all reported values.}
#'  \item{stdev_value}{Sample standard deviation of all reported values.}
#'  \item{max_issue}{Most recent issue date for this signal.}
#'  \item{min_lag}{Smallest lag from observation to issue, in `time_type` units}
#'  \item{max_lag}{Largest lag from observation to issue, in `time_type` units}
#'
#' @references COVIDcast API sources and signals documentation:
#'  \url{https://cmu-delphi.github.io/delphi-epidata/api/covidcast_signals.html}
#'
#' @seealso [summary.covidcast_meta()]
#'
#' @importFrom utils read.csv
#' @export
covidcast_meta <- function() {
  meta <- .request("covidcast_meta", list(format = "csv"))

  if (nchar(meta) == 0) {
    abort("Failed to obtain metadata", class = "covidcast_meta_fetch_failed")
  }

  # helper to do the right api_to_date for each time_type, since it does not
  # take a vectorized time_type
  adjust_dates <- function(col, time_type) {
    # map2 returns a list of entries and doesn't provide a way to get a vector
    # of Dates automatically. however, if we c() everything together, we get the
    # right type.
    do.call("c", purrr::map2(col, time_type, api_to_date))
  }

  meta <- read.csv(textConnection(meta), stringsAsFactors = FALSE) %>%
    dplyr::mutate(min_time = adjust_dates(.data$min_time, .data$time_type),
                  max_time = adjust_dates(.data$max_time, .data$time_type),
                  max_issue = adjust_dates(.data$max_issue, .data$time_type))

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
#' @return The `covidcast_meta` object, unchanged.
#'
#' @method print covidcast_meta
#' @export
print.covidcast_meta <- function(x, ...) {
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
head.covidcast_meta <- function(x, ...) {
  head(as.data.frame(x), ...)
}

#' Summarize `covidcast_meta` object
#'
#' Prints a tabular summary of the object returned by `covidcast_meta()`,
#' containing each source and signal and a summary of the geographic levels it
#' is available at.
#'
#' @param object The `covidcast_meta` object.
#' @param ... Additional arguments, for compatibility with `summary()`.
#'   Currently unused.
#' @return A data frame with one row per unique signal in the metadata, with
#'   the following columns:
#' \item{data_source}{Data source name}
#' \item{signal}{Signal name}
#' \item{county}{"*" if this signal is available at the county level, `""`
#' otherwise}
#' \item{msa}{`"*"` if this signal is available at the Metropolitan Statistical
#' Area level, `""` otherwise}
#' \item{dma}{`"*"` if this signal is available at the Designated Marketing Area
#' level, `""` otherwise}
#' \item{hrr}{`"*"` if this signal is available at the Hospital Referral Region
#' level, `""` otherwise}
#' \item{state}{`"*"` if this signal is available at the state level, `""`
#' otherwise}
#' \item{hhs}{`"*"` if this signal is available at the Health and Human Services
#' region level, `""` otherwise}
#' \item{nation}{`"*"` if this signal is available at the national level, `""`
#' otherwise}
#'
#' @method summary covidcast_meta
#' @export
summary.covidcast_meta <- function(object, ...) {
  x <- object
  cat(sprintf("A `covidcast_meta` data frame with %i rows and %i columns.\n\n",
              nrow(x), ncol(x)))
  cat(sprintf("%-23s: %s\n", "Number of data sources",
              length(unique(x$data_source))))
  cat(sprintf("%-23s: %s\n\n", "Number of signals",
              length(unique(paste(x$data_source, x$signal)))))
  cat("Summary:\n\n")
  df <- suppressMessages(
    x %>% dplyr::group_by(.data$data_source, .data$signal) %>%
    dplyr::summarize(county = ifelse("county" %in% .data$geo_type, "*", ""),
                     msa = ifelse("msa" %in% .data$geo_type, "*", ""),
                     dma = ifelse("dma" %in% .data$geo_type, "*", ""),
                     hrr = ifelse("hrr" %in% .data$geo_type, "*", ""),
                     state = ifelse("state" %in% .data$geo_type, "*", ""),
                     hhs = ifelse("hhs" %in% .data$geo_type, "*", ""),
                     nation = ifelse("nation" %in% .data$geo_type, "*", "")
                     ) %>%
    dplyr::ungroup()
  )
  print(as.data.frame(df), right = FALSE, row.names = FALSE)
  invisible(df)
}

# Retrieve only the metadata that is specific to the values of interest (signal,
# geo_type, etc.)
specific_meta <- function(data_source, signal, geo_type, time_type = "day") {
  meta_info <- covidcast_meta()
  relevant_meta <- meta_info[meta_info$data_source == data_source &
                             meta_info$signal == signal &
                             meta_info$geo_type == geo_type &
                             meta_info$time_type == time_type, ]

  # If no metadata for source/signal/geo_type combo, still return minimal data.
  # Use maximum observed values of desired geo_type as an upper bound for
  # num_locations.
  if (nrow(relevant_meta) == 0) {
    geo_nums <- max(meta_info[meta_info$geo_type == geo_type, ]$num_locations)
    relevant_meta <- list(geo_type = geo_type, num_locations = geo_nums)
  }
  return(relevant_meta)
}


# Helper function, not user-facing, to loop through a sequence of days. Chooses
# batches of days based on expected number of results, queries covidcast for
# each batch and combines the resutls.
covidcast_days <- function(data_source, signal, start_day, end_day, geo_type,
                           geo_value, time_type, as_of, issues, lag,
                           max_geos) {
  days <- date_sequence(start_day, end_day, time_type)
  ndays <- length(days)

  # issues is either a single date, or a vector with a start and end date.
  if (length(issues) == 2) {
    nissues <- length(date_sequence(issues[1], issues[2], time_type))
  } else {
    nissues <- 1
  }

  max_results <- getOption("covidcast.max_results", default = MAX_RESULTS)

  # Theoretically, each geo_value could have data issued each day. Likely
  # overestimates when handling multiple issue dates, resulting in more batches.
  max_days_at_time <- floor(max_results / (max_geos * nissues))

  # In theory, we could exceed max rows with 1 day, but try anyway
  if (max_days_at_time == 0) {
    max_days_at_time <- 1
  }
  num_batches <- ceiling(ndays / max_days_at_time)
  dat <- list()

  # The API limits the number of rows that can be returned at once, so we query
  # in batches.
  for (i in seq_len(num_batches)) {
    start_offset <- (i - 1) * max_days_at_time
    end_offset <- min(i * max_days_at_time, ndays) - 1
    query_start_day <- start_day + start_offset
    query_end_day <- start_day + end_offset

    # Expected number of unique dates fetched, to check against later
    num_time_values <- length(days[(start_offset + 1):(end_offset + 1)])

    # Use range calls where possible for speed.
    time_values <- paste0(days[(start_offset + 1)], "-", days[(end_offset + 1)])
    response <- covidcast(data_source = data_source,
                          signal = signal,
                          time_type = time_type,
                          geo_type = geo_type,
                          time_values = time_values,
                          geo_value = geo_value,
                          as_of = as_of,
                          issues = issues,
                          lag = lag)

    if (is.null(response)) {
      warn(paste0("Fetching ", signal, " from ", data_source, " for ",
                  query_start_day, " to ", query_end_day,
                  " in geography '", geo_value, "': no results"),
           data_source = data_source,
           signal = signal,
           start_day = query_start_day,
           end_day = query_end_day,
           geo_value = geo_value,
           time_type = time_type,
           class = "covidcast_fetch_failed")

      next
    }

    dat[[i]] <- response

    summary <- sprintf(
      "Fetched day %s to %s: num_entries = %s",
      query_start_day,
      query_end_day,
      nrow(response))

    if (length(summary) != 0) {
      message(summary)
    }

    if (nrow(response) > 0) {
      desired_geos <- tolower(unique(geo_value))

      returned_geo_array <- response %>%
        dplyr::select("geo_value", "time_value") %>%
        dplyr::group_by(.data$time_value) %>%
        dplyr::summarize(geo_value = list(geo_value))
      returned_time_values <- returned_geo_array$time_value

      if (length(returned_time_values) != num_time_values) {
        missing_time_values <- setdiff(time_values, returned_time_values)
        missing_dates <- api_to_date(missing_time_values, time_type)

        warn(sprintf("Data not fetched for the following days: %s",
                     paste(missing_dates, collapse = ", ")),
             data_source = data_source,
             signal = signal,
             day = missing_dates,
             geo_value = geo_value,
             time_type = time_type,
             class = "covidcast_missing_time_values"
        )
      }

      if (!identical("*", geo_value)) {
        missing_geo_array <- returned_geo_array[
          lapply(returned_geo_array$geo_value, length) < length(desired_geos), ]

        if (nrow(missing_geo_array) > 0) {
          missing_geo_array$warning <-
            unlist(apply(returned_geo_array,
                         1,
                         FUN = function(row) {
                           geo_warning_message(row, desired_geos, time_type)
                         }))
          warn(missing_geo_array$warning,
               data_source = data_source,
               signal = signal,
               day = api_to_date(missing_geo_array$time_value, time_type),
               geo_value = geo_value,
               time_type = time_type,
               class = "covidcast_missing_geo_values")
        }
      }
    }
  }

  df <- dat %>%
    purrr::map(purrr::compact) %>% # remove the list elements that are NULL
    dplyr::bind_rows() # make this into a data frame

  if (nrow(df) > 0) {
    # If no data is found, there is no time_value column to report
    df$time_value <- api_to_date(df$time_value, time_type)
    df$issue <- api_to_date(df$issue, time_type)
  }

  return(df)
}

# Get a sequence of dates, in the format required by the API, representing the
# range from start_day to end_day.
#' @importFrom MMWRweek MMWRweek
date_sequence <- function(start_day, end_day, time_type = c("day", "week")) {
  time_type <- match.arg(time_type)

  if (time_type == "day") {
    return(date_to_string(seq(start_day, end_day, by = 1)))
  } else if (time_type == "week") {
    dates <- seq(start_day, end_day, by = "1 week")
    return(date_to_string(dates, time_type))
  }
}

# Helper function (not user facing) to create warning messages when geo_values
# are missing.
geo_warning_message <- function(row, desired_geos, time_type) {
  missing_geos <- setdiff(desired_geos, unlist(row$geo_value))
  if (length(missing_geos) > 0) {
    missing_geos_str <- paste0(missing_geos, collapse = ", ")
    err_msg <- sprintf("Data not fetched for some geographies on %s: %s",
                       api_to_date(row$time_value, time_type), missing_geos_str)
    return(err_msg)
  }
}

# Fetch Delphi's COVID-19 indicators
covidcast <- function(data_source, signal, time_type, geo_type, time_values,
                      geo_value, as_of, issues, lag) {
  # Check parameters
  if (missing(data_source) || missing(signal) || missing(time_type) ||
       missing(geo_type) || missing(time_values) || missing(geo_value)) {
    stop("`data_source`, `signal`, `time_type`, `geo_type`, `time_values`, ",
         "and `geo_value` are all required.")
  }

  # Set up request
  params <- list(
    data_source = data_source,
    signal = signal,
    time_type = time_type,
    geo_type = geo_type,
    time_values = .list(time_values),
    geo_value = geo_value,
    format = "csv"
  )

  if (length(params$geo_value) > 1) {
    # convert to string
    params$geo_values <- paste0(params$geo_value, collapse = ",")
    params$geo_value <- NULL
  }
  if (!is.null(as_of)) {
    params$as_of <- date_to_string(as_of, time_type)
  }

  if (!is.null(issues)) {
    if (length(issues) == 2) {
      params$issues <- paste0(date_to_string(issues[1], time_type),
                              "-",
                              date_to_string(issues[2], time_type))
    } else if (length(issues) == 1) {
      params$issues <- date_to_string(issues, time_type)
    } else {
      stop("`issues` must be either a single date or a date interval.")
    }
  }

  if (!is.null(lag)) {
    params$lag <- lag
  }

  # Make the API call. If the API returns a non-200 status code, indicating e.g.
  # a database error, .request() raises an error. It returns an empty string if
  # there are no results for our query.
  response <- .request("covidcast", params)
  if (nchar(response) == 0) {
    # empty if no results
    return(NULL)
  }

  # geo_value must be read as character so FIPS codes are returned as character,
  # not numbers (with leading 0s potentially removed)
  return(read.csv(textConnection(response), stringsAsFactors = FALSE,
                  colClasses = c("geo_value" = "character")))
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
  if(!is.list(values) || (
    'from' %in% names(values) && 'to' %in% names(values))) {
    values <- list(values)
  }
  return(paste0(unlist(lapply(values, .listitem)), collapse=','))
}

# Helper function to request and parse epidata
.request <- function(endpoint, params) {
  # API call. Allow base API URL to be replaced, e.g. to use a staging/testing
  # server when needed.
  url <- paste0(getOption("covidcast.base_url", default = COVIDCAST_BASE_URL),
                endpoint, "/")
  auth <- getOption("covidcast.auth", default = NA)
  user_agent <- httr::user_agent("covidcastR")
  headers <- httr::add_headers(Authorization = ifelse(is.na(auth), "", paste("Bearer", auth)))

  response <- httr::GET(url,
                        user_agent,
                        headers,
                        query = params)

  # Query URL can be too long if lots of time values or geo values are involved;
  # switch to POST
  if (httr::status_code(response) == 414) {
    response <- httr::POST(url, user_agent,
                           headers,
                           body = params)
  }

  msg <- "fetch data from API"
  if (httr::status_code(response) %in% c(429, 401)) {
    msg <- paste(msg, "anonymously - to register for an API key, visit TODO")
  }
  httr::stop_for_status(response, task = msg)

  return(httr::content(response, as = "text",
                       encoding = "utf-8"))
}

# This is the date format expected by the API
date_to_string <- function(mydate, time_type = c("day", "week")) {
  time_type <- match.arg(time_type)

  if (time_type == "day") {
    format(mydate, "%Y%m%d")
  } else {
    weeks <- MMWRweek(mydate)

    # API takes MMWRweeks in YYYYMM format, so force zero-padding as needed.
    return(sprintf("%d%02d", weeks$MMWRyear, weeks$MMWRweek))
  }
}

# Convert dates from API to Date objects. For days, get a Date; for epiweeks,
# get the Date of the first day in the epiweek.
#' @importFrom MMWRweek MMWRweek2Date
api_to_date <- function(str, time_type = c("day", "week")) {
  time_type <- match.arg(time_type)

  if (time_type == "day") {
    return(as.Date(as.character(str), format = "%Y%m%d"))
  } else if (time_type == "week") {
    # Extract year and week number from string.
    years <- as.numeric(substr(str, 1, 4))
    weeks <- as.numeric(substr(str, 5, 6))
    return(MMWRweek2Date(years, weeks))
  }
}
