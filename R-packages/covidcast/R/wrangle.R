#' Apply time-shifts to the values in a `covidcast_signal` data frame 
#'
#' Applies time-shifts to the values in a `covidcast_signal` data frame. (Only
#' the latest issue from each data frame is retained.) 
#'
#' @param x Single `covidcast_signal` data frame, or a list of such data
#'   frames.
#' @param dt Vector of shifts to apply to the values in the data frame `x`.
#'   Negative shifts translate into in a lag value and positive shifts into a
#'   lead value; for example, if `dt = -1`, then the value on June 2 that gets
#'   reported is the original value on June 1; if `dt = 0`, then the values are
#'   left as is. When `x` is a list of data frames, `dt` can either be a single
#'   vector of shifts or a list of vectors of shifts, this list having the same
#'   length as `x` (in order to apply, respectively, the same shifts or a
#'   different set of shifts to each data frame in `x`).
#' 
#' @return When `x` is a data frame, the return value is a column-augmented
#'   version of `x` with one new column per value of `dt`. The new column names
#'   reflect the values of `dt`, as in `value+1` and `value-1` for a time shift
#'   forward 1 day and backward 1 day, respectively. When `x` is a list of data
#'   frames, the return value is a list of column-augmented data frames.
#'
#' @noRd
apply_shifts = function(x, dt) {
  # If we're passed a single covidcast_signal data frame
  if (inherits(x, "covidcast_signal")) return(apply_shifts_one(x, dt))
  
  # If we're passed a list of covidcast_signal data frames
  else if (is.list(x) &&
             all(sapply(x, function(v) {
               inherits(v, "covidcast_signal")
             }))) {
    # If dt is a vector 
    if (!is.list(dt)) dt = rep(list(dt), length(x))

    # If dt is a list
    else if (length(dt) != length(x)) {
      stop("If `dt` is a list, it must have the same length as `x`.")
    }
    
    return(mapply(apply_shifts_one, x, dt, SIMPLIFY = FALSE))
  }

  # Else throw an error
  else {
    stop("`x` must be a `covidcast_signal` data frame or a list of such data ",
         "frames.")
  }
}

# Function to apply shifts for one covidcast_signal data frame

apply_shifts_one = function(x, dt) {
  x = latest_issue(x)
  attrs = attributes(x)
  attrs = attrs[!(names(attrs) %in% c("row.names", "names"))]
  
  # Make sure that we have a complete record of dates for each geo_value (fill
  # with NAs as necessary)
  x_all = x %>% dplyr::group_by(geo_value) %>%
    dplyr::summarize(time_value = seq.Date(as.Date(min(time_value)),
                                           as.Date(max(time_value)),
                                           by = "day")) %>%
    dplyr::ungroup()
  x = dplyr::full_join(x, x_all, by = c("geo_value", "time_value"))
  
  # Group by geo value, sort rows by increasing time
  x = x %>% dplyr::group_by(geo_value) %>% dplyr::arrange(time_value) 
  
  # Loop over dt, and time shift the value column
  for (n in dt) {
    varname = sprintf("value%+d", n)
    x = x %>% dplyr::mutate(!!varname := shift(value, n))
  }
  
  # Remove value column, restore attributes, and return
  x = dplyr::select(x, -value)
  attributes(x) = c(attributes(x), attrs)
  return(x)
}

##########

#' Aggregate `covidcast_signal` objects into one data frame
#'
#' Aggregates `covidcast_signal` objects into one data frame, in either "wide"
#' or "long" format. (In "wide" aggregation, only the latest issue from each
#' data frame is retained, and several columns, including `data_source` and
#' `signal` are dropped; see details below). See the [multiple signals
#' vignette](https://cmu-delphi.github.io/covidcast/covidcastR/articles/multi-signals.html)
#' for examples.
#' 
#' @param x Single `covidcast_signal` data frame, or a list of such data
#'   frames, such as is returned by `covidcast_signals()`.
#' @param dt Vector of shifts to apply to the values in the data frame `x`.
#'   Negative shifts translate into in a lag value and positive shifts into a
#'   lead value; for example, if `dt = -1`, then the value on June 2 that gets
#'   reported is the original value on June 1; if `dt = 0`, then the values are
#'   left as is. When `x` is a list of data frames, `dt` can either be a single
#'   vector of shifts or a list of vectors of shifts, this list having the same
#'   length as `x` (in order to apply, respectively, the same shifts or a
#'   different set of shifts to each data frame in `x`).
#' @param format One of either "wide" or "long". The default is "wide".
#'
#' @details This function can be thought of having three use cases. In all three
#'   cases, the result will be a new data frame in either "wide" or "long"
#'   format, depending on `format`.
#'
#'   The first use case is to apply time-shifts to the values in a given
#'   `covidcast_signal` object. In this use case, `x` is a `covidcast_signal`
#'   data frame and `dt` is a vector of shifts. 
#'
#'   The second use case is to bind together, into one data frame, signals that
#'   are returned by `covidcast_signals()`. In this use case, `x` is a list of
#'   `covidcast_signal` data frames, and `dt` is `NULL`. 
#'
#'   The third use case is a combination of the first two: to bind together
#'   signals returned by `covidcast_signals()`, and simultaneously, apply
#'   time-shifts to their values. In this use case, `x` is a list of
#'   `covidcast_signal` data frames, and `dt` is either a vector of shifts---to
#'   apply the same shifts for each signal in `x`, or a list of vector of
#'   shifts---to apply different shifts for each signal in `x`.
#'
#' @return Data frame of aggregated signals in "wide" or "long" form, depending 
#'   on `format`. In "long" form, an extra column `dt` is appended to indicate
#'   the value of the time-shift. In "wide" form, only the latest issue of data
#'   is retained; the returned data frame is formed via full joins of the input
#'   data frames (on `geo_value` and `time_value` as the join key), and the
#'   columns `data_source`, `signal`, `issue`, `lag`, `stderr`, `sample_size`
#'   are all dropped from the output. Each unique signal---defined by a
#'   combination of data source name, signal name, and time-shift---is given its
#'   own column, whose name indicates its defining quantities. For example, the
#'   column name "value+2:usa-facts_confirmed_incidence_num" corresponds to a
#'   signal defined by `data_source = "usa-facts"`, `signal =
#'   "confirmed_incidence_num"`, and `dt = 2`.
#'
#' @seealso [covidcast_wider()], [covidcast_longer()]
#'
#' @export
aggregate_signals = function(x, dt = NULL, format = c("wide", "long")) {
  # If we're passed a single covidcast_signal data frame
  if (inherits(x, "covidcast_signal")) {
    # If dt is missing, then set it to 0
    if (is.null(dt)) dt = 0
    
    # If dt is a list, then throw an error
    if (is.list(dt)) {
      stop("If `x` is a `covidcast_signal` data frame, then `dt` must be a ",
           "vector.")
    }

    # Convert both x and dt to lists
    x = list(x); dt = list(dt)
  }
  
  # If we're passed a list of covidcast_signal data frames
  else if (is.list(x) &&
             all(sapply(x, function(v) {
               inherits(v, "covidcast_signal")
             }))) {
    # If dt is missing, then set it to a list of 0s
    if (is.null(dt)) dt = rep(list(0), length(x))
   
    # If dt is a vector, then recycle it into a list
    if (!is.list(dt)) dt = rep(list(dt), length(x))

    # If dt is a list, then check its length
    if (!is.list(dt)) {
      stop("If `dt` is a list, then it must have the same length as `x`.")
    }
  }

  # If we have neither a covidcast_signal data frame or a list of them
  else {
    stop("`x` must be a `covidcast_data` frame or a list of them.")
  }

  format = match.arg(format)
  N = length(x)
  x = apply_shifts(x, dt) # Apply shifts and overwrite x with the result!
  meta_list = vector("list", length = N)
  for (i in 1:N) {
    meta_list[[i]] = attributes(x[[i]])$metadata
  }
  meta_all = do.call(rbind, meta_list)
  
  # Issue a warning if there's more than one geo type present
  if (length(unique(meta_all$geo_type)) > 1) {
    warn(paste("More than one `geo_type` present. Are you sure you want to",
               "aggregate?"), class = "aggregate_signals_geo_type")
  }

  # Wide format
  if (format == "wide") {
    x = lapply(x, latest_issue) # Grab only the latest issue in wide form!
    
    # Rename value columns according to data source and signal combos, and drop
    # a bunch of columns 
    for (i in 1:N) {
      src = x[[i]]$data_source[1]
      sig = x[[i]]$signal[1]
      x[[i]] = x[[i]] %>%
        dplyr::rename_with(~ paste0(.x, ":", src, "_", sig),
                           dplyr::starts_with("value")) %>%
        dplyr::select(-c(data_source, signal, issue, lag, stderr, sample_size)) 
    }
    
    # This is the wide data frame that we will eventually return
    y = x[[1]]

    # Pairwise full joins (if there's any pairs to join at all)
    if (length(x) > 1) {
      for (i in 2:length(x)) {
        y = dplyr::full_join(y, x[[i]], by = c("geo_value", "time_value"))
      }
    }

    # Set covidcast_signal_wide class, attributes, and return
    class(y) = c("covidcast_signal_wide", "data.frame")
    attributes(y)$metadata = meta_all
    return(y)
  }

  # Long format
  if (format == "long") {
    # Pivot time-shifted value columns to long format, add column dt 
    for (i in 1:N) {
      x[[i]] = x[[i]] %>%
        tidyr::pivot_longer(dplyr::starts_with("value"),
                            names_to = "dt", values_to = "value") %>%
        dplyr::mutate(dt = as.numeric(sub("value", "", dt)))
    }

    # Bind all rows together from the various data frames
    y = do.call(dplyr::bind_rows, x)

    # Set covidcast_signal_wide class, attributes, and return
    class(y) = c("covidcast_signal_long", "data.frame")
    attributes(y)$metadata = meta_all
    return(y)
  }
}

#' Pivot aggregated signals between "wide" and "long" formats
#'
#' These functions take signals returned from `aggregate_signals()` and convert
#' between formats. `covidcast_longer()` takes the output of
#' `aggregate_signals(..., format = "wide")` and converts it to "long" format,
#' while `covidcast_wider()` takes the output of `aggregate_signals(..., format
#' = "long")` and converts it to "wide" format.
#'
#' @param x A `covidcast_signal_wide` or `covidcast_signal_long` object, as
#'   returned from `aggregate_signals()` with the respective `format` argument.
#'
#' @return The object pivoted into the opposite form, i.e. as if
#'   `aggregate_signals()` had been called in the first place with that
#'   `format` argument.
#'
#' @export
covidcast_longer = function(x) {
  if (!inherits(x, "covidcast_signal_wide")) {
    stop("`x` must be a `covidcast_signal_wide` object.")
  }

  # First pivot into long format
  x = x %>%
    tidyr::pivot_longer(dplyr::starts_with("value"),
                        names_to = "dt_data_source_signal",
                        values_to = "value") %>%
    tidyr::separate(col = "dt_data_source_signal",
                    into = c("dt", "data_source_signal"),
                    sep = ":") %>%
    tidyr::separate(col = "data_source_signal",
                    into = c("data_source", "signal"),
                    sep = "_", extra = "merge") 

  # Now add dt column, and reorder columns a bit
  x = x %>% dplyr::mutate(dt = as.numeric(sub("value", "", dt))) %>%
    dplyr::relocate(data_source, signal, geo_value, time_value) %>%
    dplyr::relocate(dt, .before = value)

  # Change class and return
  class(x) = c("covidcast_signal_long", "data.frame")
  return(x)
}

#' @rdname covidcast_longer
#' @export
covidcast_wider = function(x) {
  if (!inherits(x, "covidcast_signal_long")) {
    stop("`x` must be a `covidcast_signal_long` object.")
  }
  
  # First drop various columns
  x = dplyr::select(x, -c(issue, lag, stderr, sample_size))  

  # Renamer function (bit ugly)
  renamer = Vectorize(function(name) {
    k = regexpr("_", name)
    n = as.numeric(strsplit(substr(name, 1, k-1), ":")[[1]][2])
    return(sprintf("value%+d:%s", n, substr(name, k+1, nchar(name))))
  })
  
  # Now deal with dt column
  x = x %>%
    dplyr::group_by(geo_value, time_value) %>% 
    tidyr::pivot_wider(names_from = c("dt", "data_source", "signal"),
                       names_prefix = "value:",
                       names_sep = "_",
                       values_from = "value") %>%
    dplyr::rename_with(renamer, dplyr::starts_with("value"))

  # Change class and return
  class(x) = c("covidcast_signal_wide", "data.frame")
  return(x)
}
