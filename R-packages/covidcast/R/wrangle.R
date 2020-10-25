#' Append time-shifted values to a `covidcast_signal` data frame
#'
#' Appends time-shifted values, lagging or leading, to a `covidcast_signal` data
#' frame. (Only the latest issue from each data frame is retained.)
#'
#' @param x The `covidcast_signal` data frame, or a list of such data frames.
#' @param dt Vector of shifts to append for the values in the data frame `x`.
#'   Positive values are interpreted as shifts forward in time, and negative
#'   values are shifts backward in time. For example, if `dt = 1`, then the
#'   values are shifted forward 1 day in time (so, data on June 1 becomes data
#'   on June 2, and so on). When `x` is a list of data frames, `dt` can either
#'   be a single vector of shifts or a list of shift vectors of the same length
#'   as `x` (in order to apply, respectively, the same shifts or a different set
#'   of shifts to each data frame in `x`).
#' 
#' @return When `x` is a data frame, the return value is a column-augmented
#'   version of `x` with one new column per value of `dt`. The new column names
#'   reflect the values of `dt`, as in `value+1` and `value-1` for a time shift
#'   forward 1 day and backward 1 day, respectively. When `x` is a list of data
#'   frames, the return value is a list of column-augmented data frames.
#'
#' @export
append_shifts = function(x, dt) {
  # If we're passed a single covidcast_signal data frame
  if ("covidcast_signal" %in% class(x)) return(append_shifts_one(x, dt))
  
  # If we're passed a list of covidcast_signal data frames
  else if (is.list(x) && all(sapply(lapply(x, class), function(v) {
    "covidcast_signal" %in% v }))) {
    # If dt is a vector 
    if (!is.list(dt)) dt = rep(list(dt), length(x))

    # If dt is a list
    else if (length(dt) != length(x)) {
      stop("If `dt` is a list, it must have the same length as `x`.")
    }

    return(mapply(append_shifts_one, x, dt, SIMPLIFY = FALSE))
  }

  # Else throw an error
  else {
    stop("`x` must be a `covidcast_signal` data frame or a list of such data ",
         "frames.")
  }
}

# Function to append shifts for one covidcast_signal data frame

append_shifts_one = function(x, dt) {
  x = latest_issue(x)
  attrs = attributes(x)
  attrs = attrs[!(names(attrs) %in% c("row.names", "names"))]
  
  # Make sure that we have a complete record of dates for each geo_value (fill
  # with NAs as necessary)
  x_all = x %>% group_by(geo_value) %>%
    summarize(time_value = seq.Date(as.Date(min(time_value)),
                                    as.Date(max(time_value)),
                                    by = "day")) %>% ungroup()
  x = dplyr::full_join(x, x_all, by = c("geo_value", "time_value"))
  
  # Group by geo value, sort rows by increasing time
  x = x %>% dplyr::group_by(geo_value) %>% dplyr::arrange(time_value) 
  
  # Loop over dt, and append lag value or lead value
  for (n in dt) {
    fun = ifelse(n < 0, lag, lead)
    varname = sprintf("value%+d", n)
    x = x %>% dplyr::mutate(!!varname := fun(value, n = abs(n)))
  }
  
  # Ungroup, restore attributes, and return
  x = ungroup(x)
  attributes(x) = c(attributes(x), attrs)
  return(x)
}

##########

#' Aggregate list of `covidcast_signal` objects into one data frame
#'
#' Aggregates a list of `covidcast_signal` objects into one data frame, in
#' either "wide" or "long" format. (In "wide" aggregation, only the latest issue
#' from each data frame is retained, and several columns are dropped.) 
#' 
#' @param x List of `covidcast_signal` data frames.
#' @param format One of either "wide" or "long". The default is "wide".
#'
#' @return Data frame of aggregated signals in "wide" or "long" form, depending 
#'   on `format`. Important: in "wide" aggregation, only the latest issue from
#'   each data frame is retained, and the columns `data_source`, `signal`,
#'   `issue`, `lag`, `stderr`, `sample_size` are all dropped from the returned
#'   data frame.
#'
#'   For convenience, the function `aggregate_signals()` can operate on a list
#'   of `covidcast_signal` data frames with time-shifted values (so one can
#'   first call `append_shifts()`, and then `aggregate_signals()`). When there
#'   are time-shifted values, and `format` is "tall", then a `dt` column gets
#'   appended to the output data frame.
#'
#' @export
aggregate_signals = function(x, format = c("wide", "long")) {
  if (!(is.list(x) && all(sapply(lapply(x, class), function(v) {
    "covidcast_signal" %in% v })))) {
    stop("`x` must be a list of `covidcast_signal` data frames.")
  }
  format = match.arg(format)
  N = length(x)
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
    x = lapply(x, latest_issue)
    
    # Rename value columns according to data source and signal combos (note: 
    # there could be time-shifted value columns present), and drop a bunch of
    # columns 
    for (i in 1:N) {
      src = x[[i]]$data_source[1]
      sig = x[[i]]$signal[1]
      x[[i]] = x[[i]] %>%
        dplyr::rename_with(~ paste0(.x, ":", src, "_", sig),
                           dplyr::starts_with("value")) %>%
        dplyr::select(-c(data_source, signal, direction, issue, lag, stderr,
                         sample_size)) 
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
    # First check if there are time-shifted value columns present, and if so,
    # then pivot them to long format, then add a column dt
    for (i in 1:N) {
      if (length(base::grep("^value", colnames(x[[i]]))) > 1) {
        x[[i]] = x[[i]] %>%
          tidyr::pivot_longer(dplyr::starts_with("value"),
                              names_to = "dt", values_to = "value") %>%
          dplyr::mutate(dt = as.numeric(sub("value", "", dt)))
      }
    }

    # Now bind all rows together from the various data frames
    y = do.call(dplyr::bind_rows, x)

    # Replace NA values in dt column with 0s (if this column exists)
    if ("dt" %in% colnames(y)) y = y %>% tidyr::replace_na(list(dt = 0))

    # Set covidcast_signal_wide class, attributes, and return
    class(y) = c("covidcast_signal_long", "data.frame")
    attributes(y)$metadata = meta_all
    return(y)
  }
}

#' @export
covidcast_longer = function(x) {
  if (!("covidcast_signal_wide" %in% class(x))) {
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

  # Now check dt; if there's no need, drop it, otherwise convert it
  if (all(x$dt == "value")) {
    x = x %>% select(-dt)
  }
  else {
    x = x %>%
      dplyr::mutate(dt = as.numeric(sub("value", "", dt))) %>%
      tidyr::replace_na(list(dt = 0))
  }

  # Change class and return
  class(x) = c("covicast_signal_long", "data.frame")
  return(x)
}

#' @export
covidcast_wider = function(x) {
  if (!("covidcast_signal_long" %in% class(x))) {
    stop("`x` must be a `covidcast_signal_long` object.")
  }

  # First drop various columns
  x = x %>% dplyr::select(-c(direction, issue, lag, stderr, sample_size)) 

  # No dt column is present
  if (!("dt" %in% colnames(x))) {
    x = x %>%
      dplyr::group_by(geo_value, time_value) %>% 
      tidyr::pivot_wider(names_from = c("data_source", "signal"),
                         names_prefix = "value:",
                         names_sep = "_",
                         values_from = "value")
  }

  # Yes dt column is present
  else {
    x = x %>%
      dplyr::group_by(geo_value, time_value) %>% 
      tidyr::pivot_wider(names_from = c("dt", "data_source", "signal"),
                         names_prefix = "value:",
                         names_sep = "_",
                         values_from = "value") 
  }

  # Change class and return
  class(x) = c("covicast_signal_wide", "data.frame")
  return(x)
}
