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
#'   for a list of available data sources.
#' @param signal String identifying the signal from that source to query. Again,
#'   see the [signal
#'   documentation](https://cmu-delphi.github.io/delphi-epidata/api/covidcast_signals.html)
#'   for a list of available signals.
#' @param start_day Query data beginning on this date. Date object, or string in
#'   the form YYYYMMDD. If `start_day` is `NULL`, defaults to first day data is
#'   available for this signal.
#' @param end_day Query data up to this date, inclusive. Date object or string
#'   in the form YYYYMMDD. If `end_day` is `NULL`, defaults to the most recent
#'   day data is available for this signal.
#' @param geo_type The geography type for which to request this data, such as
#'   `"county"` or `"state"`. Defaults to `"county"`. See the [geographic coding
#'   documentation](https://cmu-delphi.github.io/delphi-epidata/api/covidcast_geography.html)
#'   for details on which types are available.
#' @param geo_values Which geographies to return. The default, `"*"`, fetches
#'   all geographies. To fetch specific geographies, specify their IDs as a
#'   vector or list of strings. See the [geographic coding
#'   documentation](https://cmu-delphi.github.io/delphi-epidata/api/covidcast_geography.html)
#'   for details on how to specify these IDs.
#' @return Data frame with matching data. Each row is one observation on one day
#'   in one geographic location. Contains the following columns:
#'
#'   \item{geo_value}{identifies the location, such as a state name or county
#'   FIPS code}
#'   \item{time_value}{a `Date` object identifying the date of this observation}
#'   \item{value}{the signal quantity requested. For example, in a query for the
#'   `confirmed_cumulative_num` signal from the `usa-facts` source, this would
#'   be the cumulative number of confirmed cases in the area, as of the
#'   `time_value`.}
#'   \item{stderr}{the value's standard error, if available}
#'   \item{sample_size}{indicates the sample size available in that geography on
#'   that day; sample size may not be available for all signals, due to privacy
#'   or other constraints, in which case they will be `NA`.}
#'   \item{direction}{uses a local linear fit to estimate whether the signal in
#'   this region is currently increasing or decreasing (reported as -1 for
#'   decreasing, 1 for increasing, and 0 for neither).}
#'
#'   Consult the signal documentation for more details on how values and
#'   standard errors are calculated for specific signals.
#' @references COVIDcast API documentation:
#'   \url{https://cmu-delphi.github.io/delphi-epidata/api/covidcast.html}
#'
#' Documentation of all COVIDcast sources and signals:
#' \url{https://cmu-delphi.github.io/delphi-epidata/api/covidcast_signals.html}
#'
#' COVIDcast public map: \url{https://covidcast.cmu.edu/}
#' @examples
#' \dontrun{
#' ## fetch all counties from 2020-05-10 to the most recent available data:
#' covidcast_signal("fb-survey", "raw_cli", start_day = "20200510")
#' ## fetch all counties on just 2020-05-10 and no other days
#' covidcast_signal("fb-survey", "raw_cli", start_day = "20200510",
#'                  end_day = "20200510")
#' ## fetch all states on 2020-05-10, 2020-05-11, 2020-05-12
#' covidcast_signal("fb-survey", "raw_cli", start_day = "20200510",
#'                  end_day = "20200512", geo_type = "state")
#' ## fetch all available data for just Pennsylvania and New Jersey
#' covidcast_signal("fb-survey", "raw_cli", geo_type = "state",
#'                  geo_values = c("pa", "nj"))
#' ## fetch all available data in Pittsburgh metropolitan area (identified by
#' ## CBSA ID)
#' covidcast_signal("fb-survey", "raw_cli", geo_type = "msa",
#'                  geo_values = "38300")
#' }
#' @export
#' @importFrom dplyr %>%
covidcast_signal <- function(data_source, signal,
                             start_day = NULL, end_day = NULL,
                             geo_type = c("county", "hrr", "msa", "dma", "state"),
                             geo_values = "*") {
  geo_type <- match.arg(geo_type)
  meta <- covidcast_meta()
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
  
  if (is.null(start_day)) {
    start_day <- relevant_meta %>%
      dplyr::pull(min_time)
  } else {
    start_day <- as.Date(start_day, format = "%Y%m%d")
  }

  if (is.null(end_day)) {
    end_day <- relevant_meta %>%
      dplyr::pull(max_time)
  } else {
    end_day <- as.Date(end_day, format = "%Y%m%d")
  }

  if (start_day > end_day) {
    stop("end_day must be on or after start_day, but start_day = '",
         start_day, "' and end_day = '", end_day, "'")
  }

  df <- purrr::map_dfr(geo_values, function(geo_val) {
      single_geo(data_source, signal, start_day, end_day, geo_type, geo_val)
  })

  # Assign covidcast_signal class and add some helpful attributes
  class(df) = c("covidcast_signal", "data.frame")
  attributes(df)$data_source = data_source
  attributes(df)$signal = signal
  attributes(df)$geo_type = geo_type
  attributes(df)$mean_value = relevant_meta$mean_value
  attributes(df)$stdev_value = relevant_meta$stdev_value
  return(df)
}

#' Print function for covidcast_signal object
#'
#' @param x The \code{covidcast_signal} object.
#' 
#' @method print covidcast_signal
#' @export
print.covidcast_signal = function(x) {
  cat(sprintf("A `covidcast_signal` data frame with %i rows and %i columns.\n\n",
              nrow(x), ncol(x)))
  cat(sprintf("%-12s: %s\n", "data_source", attributes(x)$data_source))
  cat(sprintf("%-12s: %s\n", "signal", attributes(x)$signal))
  cat(sprintf("%-12s: %s\n", "geo_type", attributes(x)$geo_type))
}

#' Summary function for covidcast_signal object
#'
#' @param x The \code{covidcast_signal} object.
#' 
#' @method summary covidcast_signal
#' @export
summary.covidcast_signal = function(x) {
  cat(sprintf("A `covidcast_signal` data frame with %i rows and %i columns.\n\n",
              nrow(x), ncol(x)))
  cat(sprintf("%-12s: %s\n", "data_source", attributes(x)$data_source))
  cat(sprintf("%-12s: %s\n", "signal", attributes(x)$signal))
  cat(sprintf("%-12s: %s\n\n", "geo_type", attributes(x)$geo_type))
  cat(sprintf("%-37s: %s\n", "first date", min(x$time_value)))
  cat(sprintf("%-37s: %s\n", "last date", max(x$time_value)))
  cat(sprintf("%-37s: %i\n", "median number of geo_value's per day",
              as.integer(x %>% dplyr::group_by(time_value) %>%
                         dplyr::summarize(num = n()) %>%
                         dplyr::summarize(median(num)))))
  cat(sprintf("%-37s: %g\n", "median value", median(x$value, na.rm=TRUE)))
  cat(sprintf("%-37s: %g\n", "median stderr",median(x$stderr, na.rm=TRUE)))
  cat(sprintf("%-37s: %i\n", "median direction",
              median(x$direction, na.rm=TRUE)))
  cat(sprintf("%-37s: %g\n", "median sample_size",
              median(x$sample_size, na.rm=TRUE)))
}

#' Plot function for covidcast_signal object
#'
#' @param x The \code{covidcast_signal} object.
#' @param plot_type One of "choro", "bubble", "line" indicating whether to plot
#'   a choropleth map, bubble map, or line (time series) graph, respectively.
#'   The default is "choro".
#' @param time_values Vector of Date objects (or strings in the form YYYY-MM-DD)
#'   specifying the days for plotting. For choropleth and bubble maps, only a
#'   single day can be plotted at one time, and this is taken to be maximum date 
#'   in `time_values`. If `NULL`, the default, then the last date in `x` is used
#'   for the maps, and (up to) the last 2 weeks in `x` is used for the time
#'   series plot.  
#' @param include Vector of state abbreviations (case insensitive, so "pa" and
#'   "PA" both denote Pennsylvania) indicating which states to include in the
#'   choropleth and bubble maps. Default is `c()`, which is interpreted to mean
#'   all states.
#' @param geo_values Vector of `geo_values` to include in the time series
#'   plot. If `NULL`, the default, then the first 6 `geo_values` as found in `x`
#'   are used.
#' @param range Vector of two values: min and max, in this order, to use when
#'   defining the color scale for choropleth maps and the size scale for bubble
#'   maps. If `NULL`, the default, then the min and max are set to be the mean
#'   +/- 3 standard deviations, where this mean and standard deviation are as
#'   provided in the meta data for the given data source and signal.
#' @param choro_col Vector of colors, as specified in hex code, to use for the
#'   choropleth color scale. Can be arbitrary in length. Default is similar to
#'   that from covidcast.cmu.edu.   
#' @param alpha Number between 0 and 1, indicating the transparency level to be
#'   used in the maps. For choropleth maps, this determines the transparency
#'   level for the mega counties. For bubble maps, this determines the
#'   transparency level for the bubbles. Default is 0.5.
#' @param direction Should direction be visualized (instead of intensity) for
#'   the maps? Default is `FALSE`.
#' @param dir_col Vector of colors, as specified in hex code, to use for the
#'   direction color scale. Must be of length 3. Default is similar to that from
#'   covidcast.cmu.edu.
#' @param bubble_col Bubble color for the bubble map. Default is "purple".
#' @param line_col Vector of colors for the time series plot. This will be
#'   recycled as necessary. Default is `1:6`. 
#' @param lty Vector of line types for the time series plot. This will be
#'   recycled as necessary. Default is `1:5`.
#' @param title Title for the plot. If `NULL`, the default, then a simple title
#'   is used based on the given data source, signal, and time values.
#' @param choro_params,bubble_params,line_params Additional parameter lists for
#'   the different plot types, for further customization. See details below. 
#' 
#' @method plot covidcast_signal
#' @export
plot.covidcast_signal = function(x, plot_type = c("choro", "bubble", "line"),
                                 time_values = NULL, include = c(),
                                 geo_values = NULL, range = NULL,
                                 choro_col = c("#FFFFCC", "#FD893C", "#800026"),
                                 alpha = 0.5, direction = FALSE,
                                 dir_col = c("#6F9CC6", "#F7F793", "#C56B59"),
                                 bubble_col = "purple", line_col = 1:6,
                                 lty = 1:5, title = NULL, choro_params = list(),
                                 bubble_params = list(), line_params = list()) { 
  plot_type = match.arg(plot_type)
  
  # Choropleth map
  if (plot_type == "choro") {
    plot_choro(x, time_value = time_values, include = include, range = range,
               col = choro_col, alpha = alpha, direction = direction,
               dir_col = dir_col, title = title, params = choro_params)
  }

  # Bubble map
  else if (plot_type == "bubble") {
    plot_bubble(x, time_value = time_values, include = include, range = range,
                col = bubble_col, alpha = alpha, direction = direction,
                dir_col = dir_col, title = title, params = bubble_params)
  }

  # Line (time series) plot
  else {
    plot_line(x, time_values = time_values, geo_values = geo_values,
              range = range, col = line_col, lty = lty, direction = direction,
              dir_col = dir_col, title = title, params = line_params) 
  }
}

#' Fetch Delphi's COVID-19 Surveillance Streams metadata.
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
#'   \item{min_value}{The smallest value that has ever been reported.}
#'   \item{max_value}{The largest value that has ever been reported.}
#'   \item{mean_value}{The arithmetic mean of all reported values.}
#'   \item{stdev_value}{The sample standard deviation of all reported values.}
#'
#' @references COVIDcast API sources and signals documentation:
#'   \url{https://cmu-delphi.github.io/delphi-epidata/api/covidcast_signals.html}
#' @export
#' @importFrom dplyr %>%
covidcast_meta <- function() {
  meta <- .request(list(source='covidcast_meta', cached="true"))

  if (meta$message != "success") {
    stop("Failed to obtain metadata: ", meta$message)
  }

  meta <- meta$epidata %>%
    dplyr::mutate(min_time = as.Date(as.character(min_time), format = "%Y%m%d"),
                  max_time = as.Date(as.character(max_time), format = "%Y%m%d"))

  return(meta)
}

## Helper function, not user-facing, to fetch a single geo-value.
## covidcast_signal can then loop over multiple geos to produce its result.
single_geo <- function(data_source, signal, start_day, end_day, geo_type, geo_value) {
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
                    nrow(dat[[i]]$epidata)))

    if (dat[[i]]$message != "success") {
      warning("Failed to obtain data for ", day,
              " in geography '", geo_value, "': ", dat[[i]]$message)
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
covidcast <- function(data_source, signal, time_type, geo_type, time_values,
                      geo_value) {
  # Check parameters
  if(missing(data_source) || missing(signal) || missing(time_type) ||
       missing(geo_type) || missing(time_values) || missing(geo_value)) {
    stop('`data_source`, `signal`, `time_type`, `geo_type`, `time_values`, and ',
         '`geo_value` are all required')
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

# e.g. date_to_string(ymd("20200506")) gives "20200506"; this is the format
# expected by the API
date_to_string <- function(mydate) {
  format(mydate, "%Y%m%d")
}
