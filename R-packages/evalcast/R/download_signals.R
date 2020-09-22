#' Download signal from covidcast
#'
#' This is a simple wrapper to \code{\link[covidcast]{covidcast_signal}} that is less verbose.
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
  base::suppressMessages({covidcast_signal(...)})
  out <- base::suppressMessages({covidcast_signal(...)})
  if (args$geo_type == "state") {
    state_fips <- covidcast::county_geo %>%
      select(STATE, COUNTYNAME, FIPS) %>%
      mutate(fips = str_sub(FIPS, end = 2),
             geo_value = tolower(STATE)) %>%
      distinct(geo_value, fips)
    out <- out %>%
      left_join(state_fips, by = "geo_value") %>%
      mutate(geo_value = fips) %>%
      select(-fips)
  }
  out %>% rename(location = geo_value)
}

