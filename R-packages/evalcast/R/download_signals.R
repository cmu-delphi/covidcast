#' Download signal from covidcast
#'
#' This is a simple wrapper to \code{\link[covidcast]{covidcast_signal}} that is less verbose.
#'
#' @param ... the arguments that are passed to [covidcast::covidcast_signal()]
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom covidcast covidcast_signal
#' @importFrom utils data
#' @importFrom stringr str_glue str_sub
#' @importFrom dplyr select mutate distinct left_join rename
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
  ## Why two calls below?? Commented out one
  ## base::suppressMessages({covidcast_signal(...)})
  out <- base::suppressMessages({covidcast_signal(...)})
  if (args$geo_type == "state") {
      e  <- new.env(parent = emptyenv())
      county_geo  <- utils::data("county_geo", package = "covidcast", envir = e)
      state_fips <- e$county_geo %>%
          select(.data$STATE, .data$COUNTYNAME, .data$FIPS) %>%
          mutate(fips = str_sub(.data$FIPS, end = 2),
                 geo_value = tolower(.data$STATE)) %>%
          distinct(.data$geo_value, .data$fips)
    out <- out %>%
      left_join(state_fips, by = "geo_value") %>%
      mutate(geo_value = .data$fips) %>%
      select(-.data$fips)
  }
  out %>% rename(location = .data$geo_value)
}

