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
  out 
}


download_signals <- function(..., 
                             signal_aggregation = "long", 
                             signal_aggregation_dt = NULL) {
  # This is just a wrapper around covidcast::covidcast_signals()
  args <- list(...)
  if (signal_aggregation != "list" ) {
    if(!is.null(signal_aggregation_dt) && is.list(signal_aggregation_dt)){
      assert_that(length(args$signal) == length(signal_aggregation_dt),
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
    out <- covidcast::aggregate_signals(out, 
                                        dt = signal_aggregation_dt,
                                        format = signal_aggregation)
  }
  out 
}

