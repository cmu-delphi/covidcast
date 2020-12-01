#' Download signal from covidcast
#'
#' This is a simple wrapper to [covidcast::covidcast_signal()] that is less verbose.
#'
#' @param ... Arguments that are passed to [covidcast::covidcast_signal()].
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom covidcast covidcast_signal
#' @importFrom utils data
#' @importFrom stringr str_glue str_sub
#' @importFrom dplyr select mutate distinct rename
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
    out$geo_value = substr(covidcast::abbr_to_fips(out$geo_value), 1, 2)
  }
  out %>% rename(location = .data$geo_value)
}

