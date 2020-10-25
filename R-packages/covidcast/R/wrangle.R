#' Append time-shifted values to a `covidcast_signal` data frame
#'
#' Appends time-shifted values, lagging or leading, to a `covidcast_signal` data
#' frame. (Only the latest issue from each data frame is preserved.)
#'
#' @param x The `covidcast_signal` data frame.
#' @param dt Vector of shifts to append for the values in `x`. Positive values
#'   are taken to mean shifts forward in time, and negative values mean shifts
#'   backward in time. For example, if `dt = 1`, then the values are shifted
#'   forward 1 day in time (so, data on June 1 becomes data on June 2, and so 
#'   on). Default is 0 for both.
#' 
#' @return A data frame one new column per value of `dt`. The new column names
#'   reflect the values of `dt`, as in `value+1` and `value-1` for a time shift 
#'   forward 1 day and backward 1 day, respectively.
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
  
  # Make sure that we have a complete record of dates for each geo_value (fill
  # with NAs as necessary)
  x_all = x %>% group_by(geo_value) %>%
    summarize(time_value = seq.Date(as.Date(min(time_value)),
                                    as.Date(max(time_value)),
                                    by = "day")) %>% ungroup()
  x = full_join(x, x_all, by = c("geo_value", "time_value"))
  
  # Group by geo value, sort rows by increasing time
  x = x %>% group_by(geo_value) %>% arrange(time_value) 
  
  # Loop over dt, and append lag value or lead value
  for (n in dt) {
    fun = ifelse(n < 0, lag, lead)
    varname = sprintf("value%+d", n)
    x = mutate(x, !!varname := fun(value, n = abs(n)))
  }
  
  # Ungroup and return
  return(ungroup(x))
}

# TODO add covidcast_rbind function or something like that, which rbind's but 
# preserves attributes appropriately? And doesn't allow you to rbind (or warns,
# at least), if the geo_type's don't match?
