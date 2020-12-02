#' Download signal from covidcast
#'
#' This is a simple wrapper to [covidcast::covidcast_signal()] that is less verbose.
#'
#' @param ... Arguments that are passed to [covidcast::covidcast_signal()].
download_signal <- function(...) {
  args <- list(...)
  if (is.null(args$start_day)) {
    msg <- stringr::str_glue(
      "Downloading {signal} from covidcast through {end}.",
      start = args$start_day,
      end = args$end_day,
      signal = args$signal,
      .sep = " "
    )
  }
  else {
    msg <- stringr::str_glue(
      "Downloading {signal} from covidcast for period from",
      "{start} to {end}.",
      start = args$start_day,
      end = args$end_day,
      signal = args$signal,
      .sep = " "
    )
  }
  message(msg)
  
  out <- base::suppressMessages({covidcast_signal(...)})
  if (args$geo_type == "state") {
    out$geo_value = substr(abbr_2_fips(out$geo_value), 1, 2)
  }
  out %>% rename(location = .data$geo_value)
}

#' Download multiple signals from covidcast
#'
#' This is a simple wrapper to [covidcast::covidcast_signals()] that is less verbose.
#'
#' @param ... Arguments that are passed to [covidcast::covidcast_signal()].
download_signals <- function(..., 
                             signal_aggregation = "long", 
                             signal_aggregation_dt = NULL) {
  args <- list(...)
  if (signal_aggregation != "list" ) {
    if(!is.null(signal_aggregation_dt) && is.list(signal_aggregation_dt)){
      assert_that(length(args$signals) == length(signal_aggregation_dt),
                  msg = paste("In download_signals(): to grab multiple signals",
                              "from covidcast and simultaneously apply",
                              "aggregation options, the number of signals",
                              "must match the length of the list of",
                              "transformations. See",
                              "?covidcast::aggregate_signals for details."))
    }
  }
  if (is.null(args$start_day)) {
    msg <- stringr::str_glue(
      "Downloading {signal} from covidcast through {end}.",
      start = args$start_day,
      end = args$end_day,
      signal = args$signal,
      .sep = " "
    )
  }
  else {
    msg <- stringr::str_glue(
      "Downloading {signal} from covidcast for period from",
      "{start} to {end}.",
      start = args$start_day,
      end = args$end_day,
      signal = args$signal,
      .sep = " "
    )
  }
  for (i in seq_along(msg)) message(msg[i])
  
  out <- base::suppressMessages({covidcast_signals(...)})
  if (signal_aggregation != "list") {
    out <- covidcast::aggregate_signals(out, dt = signal_aggregation_dt,
                                        format = signal_aggregation)
  }
  if (args$geo_type == "state") {
    if (signal_aggregation == "list") {
      out <- out %>% map(~ { 
        mutate(.x, geo_value = substr(abbr_2_fips(.data$geo_value), 1, 2)) %>%
          rename(location = .data$geo_value)
        })
    } else {
      out <- out %>%
        mutate(geo_value = substr(abbr_2_fips(.data$geo_value), 1, 2)) %>%
        rename(location = .data$geo_value)
    }
  }
  out 
}

