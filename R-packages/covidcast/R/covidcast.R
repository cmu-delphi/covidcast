# API base url
COVIDCAST_BASE_URL <- 'https://api.covidcast.cmu.edu/epidata/api.php'

.onAttach <- function(libname, pkgname) {
  msg <- c("We encourage COVIDcast API users to register on our mailing list:",
           "https://lists.andrew.cmu.edu/mailman/listinfo/delphi-covidcast-api",
           "We'll send announcements about new data sources, package updates,",
           "server maintenance, and new features.")
  packageStartupMessage(paste(msg, collapse = "\n"))
}

#' Produce a data frame for one signal.
#'
#' Obtains data for selected date ranges for all geographic regions of the
#' United States. Available data sources and signals are documented in the
#' [COVIDcast signal
#' documentation](https://cmu-delphi.github.io/delphi-epidata/api/covidcast_signals.html).
#' Most (but not all) data sources are available at the county level, but the
#' API can also return data aggregated to metropolitan statistical areas,
#' hospital referral regions, or states, as desired, by using the `geo_type`
#' argument. View `vignette("covidcast")` for detailed example usage.
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
#' exclusive; if you specify more than one, `as_of` will take priority over
#' `issues`, which will take priority over `lag`.
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
#'   the form `"YYYY-MM-DD"`. If `start_day` is `NULL`, defaults to first day
#'   data is available for this signal.
#' @param end_day Query data up to this date, inclusive. Date object or string
#'   in the form `"YYYY-MM-DD"`. If `end_day` is `NULL`, defaults to the most
#'   recent day data is available for this signal.
#' @param geo_type The geography type for which to request this data, such as
#'   `"county"` or `"state"`. Defaults to `"county"`. See the [geographic coding
#'   documentation](https://cmu-delphi.github.io/delphi-epidata/api/covidcast_geography.html)
#'   for details on which types are available.
#' @param geo_values Which geographies to return. The default, `"*"`, fetches
#'   all geographies. To fetch specific geographies, specify their IDs as a
#'   vector or list of strings. See the [geographic coding
#'   documentation](https://cmu-delphi.github.io/delphi-epidata/api/covidcast_geography.html)
#'   for details on how to specify these IDs.
#' @param as_of Fetch only data that was available on or before this date,
#'   provided as a `Date` object or string in the form `"YYYY-MM-DD"`. If
#'   `NULL`, the default, return the most recent available data.
#' @param issues Fetch only data that was published or updated ("issued") on
#'   these dates. Provided as either a single `Date` object (or string in the
#'   form `"YYYY-MM-DD"`), indicating a single date to fetch data issued on, or
#'   a vector specifying two dates, start and end. In this case, return all data
#'   issued in this range. There may be multiple rows for each observation,
#'   indicating several updates to its value. If `NULL`, the default, return the
#'   most recently issued data.
#' @param lag Integer. If, for example, `lag=3`, fetch only data that was
#'   published or updated exactly 3 days after the date. For example, a row with
#'   `time_value` of June 3 will only be included in the results if its data was
#'   issued or updated on June 6. If `NULL`, the default, return the most
#'   recently issued data regardless of its lag.
#' 
#' @return Data frame with matching data. Each row is one observation of one
#'   signal on one day in one geographic location. Contains the following
#'   columns:
#'
#'   \item{data_source}{Data source from which this observation was obtained.}
#'   \item{signal}{The signal from which this observation was obtained.}
#'   \item{geo_value}{identifies the location, such as a state name or county
#'   FIPS code}
#'   \item{time_value}{a `Date` object identifying the date of this observation}
#'   \item{issue}{a `Date` object identifying the date this estimate was issued.
#'   For example, an estimate with a `time_value` of June 3 might have been
#'   issued on June 5, after the data for June 3rd was collected and ingested
#'   into the API.}
#'   \item{lag}{an integer giving the difference between ``issue`` and
#'   ``time_value``, in days.}
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
#' covidcast_signal("fb-survey", "raw_cli", start_day = "2020-05-10")
#' ## Fetch all counties on just 2020-05-10 and no other days
#' covidcast_signal("fb-survey", "raw_cli", start_day = "2020-05-10",
#'                  end_day = "2020-05-10")
#' ## Fetch all states on 2020-05-10, 2020-05-11, 2020-05-12
#' covidcast_signal("fb-survey", "raw_cli", start_day = "2020-05-10",
#'                  end_day = "2020-05-12", geo_type = "state")
#' ## Fetch all available data for just Pennsylvania and New Jersey
#' covidcast_signal("fb-survey", "raw_cli", geo_type = "state",
#'                  geo_values = c("pa", "nj"))
#' ## Fetch all available data in the Pittsburgh metropolitan area
#' covidcast_signal("fb-survey", "raw_cli", geo_type = "msa",
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
    relevant_meta <- list()
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
    stop("end_day must be on or after start_day, but start_day = '",
         start_day, "' and end_day = '", end_day, "'")
  }

  if (!is.null(as_of)) {
    as_of <- as.Date(as_of)
  }

  if (!is.null(issues)) {
    issues <- as.Date(issues)
  }

  df <- purrr::map_dfr(geo_values, function(geo_val) {
    single_geo(data_source, signal, start_day, end_day, geo_type, geo_val,
               as_of, issues, lag)
  })

  # Assign covidcast_signal class and add some helpful attributes
  class(df) <- c("covidcast_signal", "data.frame")
  attributes(df)$metadata <- relevant_meta
  attributes(df)$geo_type <- geo_type
  return(df)
}

#' Print `covidcast_signal` objects
#'
#' Prints a brief summary of the data source, signal, and geographic level, and
#' then prints the underlying data frame, for objects returned by
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
  cat(sprintf("%-12s: %s\n\n", "geo_type", attributes(x)$geo_type))

  # forward to print the data as well
  NextMethod("print")
}

#' @method head covidcast_signal
#' @importFrom utils head
#' @export
head.covidcast_signal = function(x, ...) {
  head(as.data.frame(x), ...)
}

#' Summarize `covidcast_signal` objects
#'
#' Prints a variety of summary statistics about the underlying data, such as
#' median values, the date range included, sample sizes, and so on, for objects
#' returned by `covidcast_signal()`.
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
  cat(sprintf("%-12s: %s\n\n", "geo_type", attributes(x)$geo_type))
  cat(sprintf("%-36s: %s\n", "first date", min(x$time_value)))
  cat(sprintf("%-36s: %s\n", "last date", max(x$time_value)))
  cat(sprintf("%-36s: %i\n", "median number of geo_values per day",
              as.integer(x %>% dplyr::group_by(.data$time_value) %>%
                         dplyr::summarize(num = dplyr::n()) %>%
                         dplyr::summarize(median(.data$num)))))
}

#' Plot `covidcast_signal` objects
#'
#' Several plot types are provided, including choropleth plots (maps), bubble
#' plots, and time series plots showing the change of signals over time, for
#' objects returned by `covidcast_signal()`.
#'
#' @param x The `covidcast_signal` object to map or plot. If the object contains
#'   multiple issues of the same observation, only the most recent issue is
#'   mapped or plotted. 
#' @param plot_type One of "choro", "bubble", "line" indicating whether to plot
#'   a choropleth map, bubble map, or line (time series) graph, respectively.
#'   The default is "choro".
#' @param time_value Date object (or string in the form `"YYYY-MM-DD"`)
#'   specifying the day to map, for choropleth and bubble maps. If `NULL`, the
#'   default, then the last date in `x` is used for the maps. Time series plots
#'   always include all available time values in `x`.
#' @param include Vector of state abbreviations (case insensitive, so "pa" and
#'   "PA" both denote Pennsylvania) indicating which states to include in the
#'   choropleth and bubble maps. Default is `c()`, which is interpreted to mean
#'   all states.
#' @param range Vector of two values: min and max, in this order, to use when
#'   defining the color scale for choropleth maps and the size scale for bubble
#'   maps, or the range of the y-axis for the time series plot. If `NULL`, the
#'   default, then for the maps, the min and max are set to be the mean +/- 3
#'   standard deviations, where this mean and standard deviation are as provided
#'   in the metadata for the given data source and signal; and for the time
#'   series plot, they are set to be the observed min and max of the values over  
#'   the given time period.
#' @param choro_col Vector of colors, as specified in hex code, to use for the
#'   choropleth color scale. Can be arbitrary in length. Default is similar to
#'   that from covidcast.cmu.edu.
#' @param alpha Number between 0 and 1, indicating the transparency level to be
#'   used in the maps. For choropleth maps, this determines the transparency
#'   level for the mega counties. For bubble maps, this determines the
#'   transparency level for the bubbles. Default is 0.5.
#' @param direction Should direction be visualized (instead of intensity) for
#'   the choropleth map? Default is `FALSE`.
#' @param dir_col Vector of colors, as specified in hex code, to use for the
#'   direction color scale. Must be of length 3. Default is similar to that from
#'   covidcast.cmu.edu.
#' @param bubble_col Bubble color for the bubble map. Default is "purple".
#' @param num_bins Number of bins for determining the bubble sizes for the
#'   bubble map (here and throughout, to be precise, by bubble size we mean
#'   bubble area). Default is 8. These bins are evenly-spaced in between the min
#'   and max as specified through the `range` parameter. Each bin is assigned
#'   the same bubble size. Also, values of zero special: it has its own separate
#'   (small) bin, and values mapped to the zero bin are not drawn.
#' @param title Title for the plot. If `NULL`, the default, then a simple title
#'   is used based on the given data source, signal, and time values.
#' @param choro_params,bubble_params,line_params Additional parameter lists for
#'   the different plot types, for further customization. See details below.
#' @param ... Additional arguments, for compatibility with `plot()`. Currently
#'   unused.
#'
#' @details The following named arguments are supported through the lists 
#'   `choro_params`, `bubble_params`, and `line_params`.
#'
#' For both choropleth and bubble maps:
#' \describe{
#' \item{`subtitle`}{Subtitle for the map.}
#' \item{`missing_col`}{Color assigned to missing or NA geo locations.}
#' \item{`border_col`}{Border color for geo locations.}
#' \item{`border_size`}{Border size for geo locations.}
#' \item{`legend_position`}{Position for legend; use "none" to hide legend.} 
#' \item{`legend_height`, `legend_width`}{Height and width of the legend.} 
#' \item{`breaks`}{Breaks for a custom (discrete) color or size scale.  Note
#'   that we must set `breaks` to be a vector of the same length as `choro_col`
#'   for choropleth maps. This works as follows: we assign the `i`th color for
#'   choropleth maps, or the `i`th size for bubble maps, if and only if the
#'   given value satisfies `breaks[i] <= value < breaks[i+1]`, where we take by 
#'   convention `breaks[0] = -Inf` and `breaks[N+1] = Inf` for `N =
#'   length(breaks)`.}   
#' \item{`legend_digits`}{Number of decimal places to show for the legend
#'   labels.}  
#' }
#'
#' For choropleth maps only:
#' \describe{
#' \item{`legend_n`}{Number of values to label on the legend color bar. Ignored
#'   for discrete color scales (when `breaks` is set manually) and for direction 
#'   maps.} 
#' }
#'
#' For bubble maps only:
#' \describe{
#' \item{`remove_zero`}{Should zeros be excluded from the size scale (hence
#'   effectively drawn as bubbles of zero size)?}
#' \item{`min_size`, `max_size`}{Min size for the size scale.}
#' }
#'
#' For line graphs:
#' \describe{
#' \item{`xlab`, `ylab`}{Labels for the x-axis and y-axis.}
#' \item{`stderr_bands`}{Should standard error bands be drawn?}
#' \item{`stderr_alpha`}{Transparency level for the standard error bands.}
#' }
#'
#' @method plot covidcast_signal
#' @importFrom stats sd
#' @importFrom rlang warn
#' @export
plot.covidcast_signal <- function(x, plot_type = c("choro", "bubble", "line"),
                                  time_value = NULL, include = c(),
                                  range = NULL,
                                  choro_col = c("#FFFFCC", "#FD893C", "#800026"),
                                  alpha = 0.5, direction = FALSE,
                                  dir_col = c("#6F9CC6", "#E4E4E4", "#C56B59"),
                                  bubble_col = "purple", num_bins = 8,
                                  title = NULL, choro_params = list(),
                                  bubble_params = list(), line_params = list(),
                                  ...) {
  plot_type <- match.arg(plot_type)
  x <- latest_issue(x)

  # For the maps, set range, if we need to (to mean +/- 3 standard deviations,
  # from metadata) 
  if (is.null(range) && plot_type == "choro" || plot_type == "bubble") {
    if (is.null(attributes(x)$metadata)) { 
      warn(
        paste0("Metadata for signal mean and standard deviation not ",
               "available; defaulting to observed mean and standard ",
               "deviation to set plot range."),
        class = "covidcast_plot_meta_not_found")
      mean_value <- mean(x$value)
      stdev_value <- sd(x$value)
    } else {
      # TODO: Presently we assume there is only one signal type in the data
      # frame. Will need special handling when there can be more.
      mean_value <- attributes(x)$metadata$mean_value
      stdev_value <- attributes(x)$metadata$stdev_value
    }
    range <- c(mean_value - 3 * stdev_value, mean_value + 3 * stdev_value)
    range <- pmax(0, range)
    # TODO: figure out for which signals we need to clip the top of the range.
    # For example, for percentages, we need to clip it at 100
  }

  # For the maps, take the most recent time value if more than one is passed,
  # and check that the include arguments indeed contains state names
  if (plot_type == "choro" || plot_type == "bubble") {
    if (!is.null(include)) {
      include <- toupper(include)
      no_match <- which(!(include %in% c(state.abb, "DC")))

      if (length(no_match) > 0) {
        warning("'include' must only contain US state abbreviations or 'DC'.")
        include <- include[-no_match]
      }
    }
  }

  # Choropleth map
  if (plot_type == "choro") {
    plot_choro(x, time_value = time_value, include = include, range = range,
               col = choro_col, alpha = alpha, direction = direction,
               dir_col = dir_col, title = title, params = choro_params)
  }


  # Bubble map
  else if (plot_type == "bubble") {
    plot_bubble(x, time_value = time_value, include = include, range = range,
                col = bubble_col, alpha = alpha, num_bins = num_bins,
                title = title, params = bubble_params)
  }

  # Line (time series) plot
  else {
    plot_line(x, range = range, title = title, params = line_params)
  }
}

##########

#' Obtain multiple signals in one data frame.
#'
#' This convenience function uses `covidcast_signal()` to obtain multiple
#' signals, potentially from multiple data sources, in one data frame. See the
#' `covidcast_signal()` documentation for further details.
#'
#' Deliberately not exported, because it's unclear how to plot data frames with
#' multiple signals. Once `plot.covidcast_signal()` supports plotting the
#' objects returned by `covidcast_signals()`, this function can be exported.
#'
#' @param signals Data frame with two columns: `data_source` and `signal`. Each
#'   row specifies one signal to be fetched from the COVIDcast API.
#' @inheritParams covidcast_signal
#' @inherit covidcast_signal return references
#' @seealso [covidcast_signal()]
#' @examples
#' \dontrun{
#' signals <= data.frame(data_source=c("jhu-csse", "fb-survey"),
#'                       signal=c("confirmed_incidence_num", "smoothed_cli"))
#' covidcast_signals(signals, "2020-07-01", "2020-07-14", geo_type="state")
#' }
#' @noRd
covidcast_signals <- function(signals, start_day = NULL, end_day = NULL,
                              geo_type = c("county", "hrr", "msa", "dma", "state"),
                              geo_values = "*", as_of = NULL, issues = NULL,
                              lag = NULL) {
  N <- nrow(signals)
  dfs <- vector("list", N)
  metas <- vector("list", N)

  for (row in seq_len(N)) {
    df <- covidcast_signal(signals$data_source[row], signals$signal[row],
                           start_day, end_day, geo_type, geo_values, as_of,
                           issues, lag)
    dfs[[row]] <- df
    metas[[row]] <- attributes(df)$metadata
  }

  df_out <- dplyr::bind_rows(dfs)
  meta_out <- dplyr::bind_rows(metas)

  class(df_out) <- c("covidcast_signals", "data.frame")
  attributes(df_out)$metadata <- meta_out
  attributes(df_out)$geo_type <- geo_type

  return(df_out)
}

# TODO add S3 functions for covidcast_signals
# TODO add covidcast_rbind function or something like that, which rbind's but 
# preserves attributes appropriately? And doesn't allow you to rbind (or warns,
# at least), if the geo_type's don't match?

##########

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
#'   \item{max_issue}{The most recent issue date for this signal.}
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
    stop("Failed to obtain metadata: ", meta$message)
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
#' frame, for objects returned by `covidcast_meta()`.
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
#' Prints a summary of the metadata returned by `covidcast_meta()`.
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

# Helper function, not user-facing, to fetch a single geo-value.
# covidcast_signal can then loop over multiple geos to produce its result.
single_geo <- function(data_source, signal, start_day, end_day, geo_type,
                       geo_value, as_of, issues, lag) {
  ndays <- as.numeric(end_day - start_day)
  dat <- list()

  # The API limits the number of rows that can be returned at once, so we query
  # each day separately.
  for (i in seq(ndays + 1)) {
    day <- date_to_string(start_day + i - 1)
    dat[[i]] <- covidcast(data_source = data_source,
                          signal = signal,
                          time_type = "day",
                          geo_type = geo_type,
                          time_values = day,
                          geo_value = geo_value,
                          as_of = as_of,
                          issues = issues,
                          lag = lag)
    message(sprintf("Fetched day %s: %s, %s, num_entries = %s",
                    day,
                    dat[[i]]$result,
                    dat[[i]]$message,
                    nrow(dat[[i]]$epidata)))

    if (dat[[i]]$message != "success") {
      warn(paste0("Fetching ", signal, " from ", data_source, " for ", day,
                  " in geography '", geo_value, "': ", dat[[i]]$message),
           data_source = data_source,
           signal = signal,
           day = day,
           geo_value = geo_value,
           msg = dat[[i]]$message,
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
  }

  return(df)
}

# Fetch Delphi's COVID-19 Surveillance Streams
covidcast <- function(data_source, signal, time_type, geo_type, time_values,
                      geo_value, as_of, issues, lag) {
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
      stop("`issues` must be either a single date or a date interval")
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

# e.g. date_to_string(ymd("20200506")) gives "20200506"; this is the format
# expected by the API
date_to_string <- function(mydate) {
  format(mydate, "%Y%m%d")
}
