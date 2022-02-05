#' Wrapper around `covidcast::covidcast_signal()` with additional logging and caching.
#' @param ... arguments to be passed to `covidcast::covidcast_signal()`.
#' @param offline_signal_dir the directory that stores the cached data for each
#' (signal, forecast day) pair. If this is null, no caching is done and the data is
#' downloaded from covidcast.
#' @return `covidcast_signal` data frame.
#'
#' @importFrom fs dir_create
#' @importFrom rlang warn inform
#'
#' @export
download_signal <- function(..., offline_signal_dir=NULL) {
  args <- list(...)
  # Mirror covidcast_signal defaults
  if (is.null(args$as_of)) args$as_of <- Sys.Date()
  if (is.null(args$geo_type)) args$geo_type <- "county"
  if (!is.null(offline_signal_dir) & is.null(args$end_day)) stop("end_day must be set.")

  if (is.null(offline_signal_dir)) {
    if (is.null(args$start_day)) {
      msg <- stringr::str_glue(
        "Downloading {signal} from covidcast through {end}.",
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
    df <- suppressMessages({covidcast_signal_wrapper(...)})
  } else {
    signal_fpath <- file.path(offline_signal_dir, args$geo_type, sprintf("%s_%s_%s_%s.RDS", args$data_source, args$signal, args$end_day, args$as_of))
    inform(
      "Using API data caching mechanism. Be warned that a stale cache could cause issues (for safety, you can remove the 'offline_signal_dir' argument or clean the cache directory).",
      "cache_info"
    )

    if (file.exists(signal_fpath)) {
      inform(
        sprintf("Reading signal from disk: %s", signal_fpath),
        "cache_info"
      )
      df <- readRDS(signal_fpath)
      read_from_disk <- TRUE
    }
    else {
      inform(
        sprintf("Downloading signal from API and storing in: %s", signal_fpath),
        "cache_info"
      )
      df <- suppressMessages({covidcast_signal_wrapper(...)})
      dir_create(dirname(signal_fpath), recurse = TRUE)
      saveRDS(df, signal_fpath)
      read_from_disk <- FALSE
    }

    max_time_value <- (df %>% summarize(max(time_value)))[[1]]
    if (read_from_disk && (max_time_value < args$end_day)) warn("Data in cache ends earlier than `end_day`.", "cache_warning")

    if (is.null(args$start_day)) {
      df <- df %>% filter(time_value <= args$end_day)
    } else {
      min_time_value <- (df %>% summarize(min(time_value)))[[1]]
      if (read_from_disk && (min_time_value > args$start_day)) warn("Data in cache starts later than `start_day`.", "cache_warning")
      df <- df %>% filter(time_value >= args$start_day, time_value <= args$end_day)
    }

    if (!"*" %in% args$geo_values) {
      df <- df %>% filter(geo_value %in% args$geo_values)
    }
  }

  return(df)
}

# The thinnest wrapper possible to allow mock testing see:
# https://krlmlr.github.io/mockr/articles/mockr.html#write-wrapper-functions
covidcast_signal_wrapper <- function(...) {
  return(covidcast_signal(...))
}
