#' Get predictions from forecasters on the COVID Hub
#'
#' Simply converts the predictions of forecasters submitting to the [COVID
#' Hub](https://github.com/reichlab/covid19-forecast-hub/) to the format of a
#' predictions card, so it can be easily evaluated and compared.
#'
#' @param covidhub_forecaster_name A vector of strings indicating the
#'   forecasters (matching what it is called on the COVID Hub).
#' @param forecast_dates Vector of Date objects (or strings of the form
#'   "YYYY-MM-DD") indicating dates on which forecasts will be made. If `NULL`,
#'   the default, then all currently available forecast dates from the given
#'   forecaster in the COVID Hub will be used.
#' @param geo_values vector of character strings containing FIPS codes of
#'   counties, or lower case state abbreviations (or "us" for national). The
#'   default "*" fetches all available locations
#' @param forecast_type "quantile", "point" or both (the default)
#' @param ahead number of periods ahead for which the forecast is required.
#'   NULL will fetch all available aheads
#' @param incidence_period one of "epiweek" or "day". NULL will attempt to
#'   return both
#' @param signal this function supports only "confirmed_incidence_num",
#'   "deaths_incidence_num", "deaths_cumulative_num", and/or
#'   "confirmed_admissions_covid_1d". For other types, use one of the
#'   alternatives mentioned above
#' @param predictions_cards An object of class `predicitions_cards` that
#'   contains previously retrieved predictions. If provided, files will not be
#'   retrieved from Reichlab for any forecaster / forecast_date combos that are
#'   present in the file.
#' @param start_date The earliest date for which to retrieve predictions
#' @param end_date The latest date for which to retrieve predictions
#' @param date_filtering_function A function which takes a list, where each
#'   element is a vector of dates and returns a list where each element is a
#'   subset of those in the original list. Used for custom filtering of dates
#'   (e.g. only forecasts from Mondays, where all forecasters made a forecast,
#'   etc.)
#' @param verbose If TRUE, prints additional details about progress. FALSE by
#'   default.
#' @param ... Additional named arguments. Intended for expert users only.
#' @seealso [get_predictions()]
#' @seealso [get_zoltar_predictions()]
#' @template predictions_cards-template
#' @return For more flexible processing of COVID Hub data, try
#'   using [zoltr](https://docs.zoltardata.com/zoltr/)
#'
#' @importFrom stringr str_interp
#'
#' @export

get_covidhub_predictions <- function(
  covidhub_forecaster_name = get_covidhub_forecaster_names(),
  forecast_dates = NULL,
  geo_values = "*",
  forecast_type = c("point", "quantile"),
  ahead = 1:4,
  incidence_period = c("epiweek", "day"),
  signal = c("confirmed_incidence_num",
             "deaths_incidence_num",
             "deaths_cumulative_num",
             "confirmed_admissions_covid_1d"),
  predictions_cards = NULL,
  start_date = NULL,
  end_date = NULL,
  date_filtering_function = NULL,
  verbose = FALSE,
  ...) {
  extra_args <- list(...)
  forecast_dates <- as_date(forecast_dates)
  forecast_dates <- get_forecast_dates(covidhub_forecaster_name,
                                       forecast_dates,
                                       start_date,
                                       end_date,
                                       date_filtering_function)
  # Load known predictions so we don't have to re-ingest / process it. This
  # data could end up out of date if a forecast is retrospectively updated, but
  # in that case it's no longer a true prediction.
  if (!is.null(predictions_cards)) {
    seen_dates <- predictions_cards %>%
      distinct(.data$forecast_date, .data$forecaster)
    for (i in seq_along(covidhub_forecaster_name)) {
      if (covidhub_forecaster_name[[i]] %in% seen_dates$forecaster) {
        seen_forecaster_dates <- seen_dates %>%
          filter(.data$forecaster == covidhub_forecaster_name[[i]]) %>%
          pull(.data$forecast_date)
        forecast_dates[[i]] <- lubridate::as_date(setdiff(
                                                    forecast_dates[[i]],
                                                    seen_forecaster_dates))
      }
    }
  }

  num_forecasters = length(covidhub_forecaster_name)
  predictions_cards_list <- vector("list",
                                   length = num_forecasters)
  if (verbose){
    cat(str_interp("Getting forecasts for ${num_forecasters} forecasters.\n"))
  }
  if ("use_disk" %in% names(extra_args) && extra_args$use_disk) {
    get_forecaster_preds_fn <- get_forecaster_predictions_alt
  } else {
    get_forecaster_preds_fn <- get_forecaster_predictions
  }
  for (i in seq_along(covidhub_forecaster_name)) {
    if (verbose){
      cat(str_interp("${i}/${num_forecasters}: ${covidhub_forecaster_name[i]}...\n"))
    }
    if (length(forecast_dates[[i]] > 0)) {
      predictions_cards_list[[i]] <- tryCatch({
        get_forecaster_preds_fn(covidhub_forecaster_name[i],
                                rev(forecast_dates[[i]]),
                                geo_values = geo_values,
                                forecast_type = forecast_type,
                                ahead = ahead,
                                incidence_period = incidence_period,
                                signal = signal)
      }, error = function(e) cat(e$message))
    }
  }
  predictions_cards_new <- bind_rows(predictions_cards_list)

  # Combine old and new predictions cards
  if (is.null(predictions_cards)) {
    predictions_cards <- predictions_cards_new
  } else {
    predictions_cards <- rbind(predictions_cards, predictions_cards_new)
  }
  return(predictions_cards)
}

#' Get forecast dates per forecaster given select filtering criteria
#'
#' @param forecasters vector of forecaster names
#' @param start_date The earliest date for which to retrieve predictions
#' @param end_date The latest date for which to retrieve predictions
#' @param date_filtering_function A function which takes a list, where each
#'   element is a vector of dates and returns a list where each element is a
#'   subset of those in the original list. Used for custom filtering of dates
#'   (e.g. only forecasts from Mondays, where all forecasters made a forecast,
#'   etc.)
#' @param forecast_dates A vector of date objects. If provided, only dates
#'   within this set will be returned for each forecaster (subject to all other
#'   provided criteria)
#' @return A list of the same length as `forecasters`, where each entry is the
#'   vector at the same index in `forecast_dates` after being filtered according
#'   to the provided criteria
#'
get_forecast_dates <- function(forecasters,
                               forecast_dates,
                               start_date,
                               end_date,
                               date_filtering_function) {
  forecast_dates <- as_date(forecast_dates)
  forecaster_dates <- vector("list", length = length(forecasters))
  for (i in seq_len(length(forecasters))) {
    forecaster_dates[[i]] <- tryCatch({
      lubridate::as_date(get_covidhub_forecast_dates(forecasters[i]))
    },
    error = function(e) cat(sprintf("%i. %s\n", i, e$message))
    )
  }
  if (length(forecast_dates) != 0) {
    # Intersect acts oddly with dates. If foo = as_date(bar), then foo == bar is
    # true, but (foo %in% bar) is false and intersect(foo, bar) is an empty
    # vector. Additionally, intersect returns a numeric object instead of a
    # date.
    forecaster_dates <- lapply(forecaster_dates,
                               function(dates)
                                 as_date(intersect(dates, forecast_dates)))
  }
  if (!is.null(start_date)) {
    forecaster_dates <- lapply(forecaster_dates,
                               function(date) date[date >= start_date])
  }
  if (!is.null(end_date)) {
    forecaster_dates <- lapply(forecaster_dates,
                               function(date) date[date <= end_date])
  }
  if (!is.null(date_filtering_function)) {
    forecaster_dates <- date_filtering_function(forecaster_dates)
  }
  return(forecaster_dates)
}

#' Get predictions from a forecaster on the COVID Hub
#'
#' Simply converts the predictions of forecasters submitting to the [COVID
#' Hub](https://github.com/reichlab/covid19-forecast-hub/) to the format of a
#' predictions card, so it can be easily evaluated and compared.
#'
#'
#' @param covidhub_forecaster_name String indicating of the forecaster
#'   (matching what it is called on the COVID Hub).
#' @param forecast_dates Vector of Date objects (or strings of the form
#'   "YYYY-MM-DD") indicating dates on which forecasts will be made. If `NULL`,
#'   the default, then all currently available forecast dates from the given
#'   forecaster in the COVID Hub will be used.
#' @param geo_values vector of character strings containing FIPS codes of
#'   counties, or lower case state abbreviations (or "us" for national). The
#'   default "*" fetches all available locations
#' @param forecast_type "quantile", "point" or both (the default)
#' @param ahead number of periods ahead for which the forecast is required.
#'   NULL will fetch all available aheads
#' @param incidence_period one of "epiweek" or "day". NULL will attempt to
#'   return both
#' @param signal this function supports only "confirmed_incidence_num",
#'   "deaths_incidence_num", "deaths_cumulative_num", and/or
#'   "confirmed_admissions_covid_1d". For other types, use one of the
#'   alternatives mentioned above
#'
#' @template predictions_cards-template
#'
#' @seealso [get_predictions()]
#' @seealso [get_zoltar_predictions()]
#' @return Predictions card. For more flexible processing of COVID Hub data, try
#'   using [zoltr](https://docs.zoltardata.com/zoltr/)
#'
#' @importFrom data.table fread
get_forecaster_predictions <- function(covidhub_forecaster_name,
                                       forecast_dates = NULL,
                                       geo_values = "*",
                                       forecast_type = c("point","quantile"),
                                       ahead = 1:4,
                                       incidence_period = c("epiweek", "day"),
                                       signal = c("confirmed_incidence_num",
                                                  "deaths_incidence_num",
                                                  "deaths_cumulative_num",
                                                  "confirmed_admissions_covid_1d")
) {
  url <- "https://raw.githubusercontent.com/reichlab/covid19-forecast-hub/master/data-processed"
  pcards <- list()
  if (is.null(forecast_dates))
    forecast_dates <- get_covidhub_forecast_dates(covidhub_forecaster_name)
  forecast_dates <- as.character(forecast_dates)
  for (forecast_date in forecast_dates) {
    filename <- sprintf("%s/%s/%s-%s.csv",
                        url,
                        covidhub_forecaster_name,
                        forecast_date,
                        covidhub_forecaster_name)
    pred <- fread(filename,
                  na.strings = c("\"NA\"", "NA"),
                  colClasses = c(location = "character",
                                 quantile = "double",
                                 value = "double",
                                 target = "character",
                                 type = "character"),
                  data.table = FALSE,
                  showProgress = FALSE)
    # Specifying the date conversion after significantly speeds up loading
    # (~3x faster) for some reason
    pred$target_end_date = as.Date(pred$target_end_date)
    pred$forecast_date = as.Date(pred$forecast_date)

    pcards[[forecast_date]] <- pred %>%
      process_target(remove = TRUE) %>%
      mutate(forecaster = covidhub_forecaster_name) %>%
      filter_predictions(forecast_type, incidence_period, signal) %>%
      select_pcard_cols()
  }
  pcards <- bind_rows(pcards) %>%
    location_2_geo_value()
  if (!identical(geo_values, "*")) {
    pcards <- filter(pcards, .data$geo_value %in% geo_values)
  }
  if (!is.null(ahead)) {
    pcards <- filter(pcards, .data$ahead %in% !!ahead)
  }
  pcards <- filter(pcards, .data$signal %in% !!signal)
  class(pcards) = c("predictions_cards", class(pcards))
  pcards
}

#' Get predictions from a forecaster on the COVID Hub (Alternate version)
#'
#' Similar to get_forecast_predictions(), but downloads CSVs to disk to process
#' them in a more memory-efficient manner. Stores the CSVs into folder "./data".
#'
#'
#' @param covidhub_forecaster_name String indicating of the forecaster
#'   (matching what it is called on the COVID Hub).
#' @param forecast_dates Vector of Date objects (or strings of the form
#'   "YYYY-MM-DD") indicating dates on which forecasts will be made. If `NULL`,
#'   the default, then all currently available forecast dates from the given
#'   forecaster in the COVID Hub will be used.
#' @param geo_values vector of character strings containing FIPS codes of
#'   counties, or lower case state abbreviations (or "us" for national). The
#'   default "*" fetches all available locations
#' @param forecast_type "quantile", "point" or both (the default)
#' @param ahead number of periods ahead for which the forecast is required.
#'   NULL will fetch all available aheads
#' @param incidence_period one of "epiweek" or "day". NULL will attempt to
#'   return both
#' @param signal this function supports only "confirmed_incidence_num",
#'   "deaths_incidence_num", "deaths_cumulative_num", and/or
#'   "confirmed_admissions_covid_1d". For other types, use one of the
#'   alternatives mentioned above
#'
#' @template predictions_cards-template
#'
#' @seealso [get_predictions()]
#' @seealso [get_zoltar_predictions()]
#' @return Predictions card. For more flexible processing of COVID Hub data, try
#'   using [zoltr](https://docs.zoltardata.com/zoltr/)
#'
#' @importFrom arrow open_dataset schema string
#' @importFrom utils download.file
#' @importFrom readr read_csv write_csv col_double cols col_character
#' @importFrom dplyr relocate
#' @importFrom stringr str_replace_all
get_forecaster_predictions_alt <- function(covidhub_forecaster_name,
                                           forecast_dates = NULL,
                                           geo_values = "*",
                                           forecast_type = c("point","quantile"),
                                           ahead = 1:4,
                                           incidence_period = c("epiweek", "day"),
                                           signal = c("confirmed_incidence_num",
                                                      "deaths_incidence_num",
                                                      "deaths_cumulative_num",
                                                      "confirmed_admissions_covid_1d")
) {
  url <- "https://raw.githubusercontent.com/reichlab/covid19-forecast-hub/master/data-processed"
  if (is.null(forecast_dates))
    forecast_dates <- get_covidhub_forecast_dates(covidhub_forecaster_name)
  forecast_dates <- as.character(forecast_dates)
  # Download files to disk first
  # File layout is data/<covidhub_forecaster_name>/<forecast_date>/data.csv
  for (forecast_date in forecast_dates) {
    output_dir <- file.path("data", covidhub_forecaster_name, forecast_date)
    output_file <- file.path("data", covidhub_forecaster_name, forecast_date, "data.csv")
    filename <- sprintf("%s/%s/%s-%s.csv",
                        url,
                        covidhub_forecaster_name,
                        forecast_date,
                        covidhub_forecaster_name)

    dir.create(output_dir, recursive = TRUE)

    # Download file. Re-attempt up to 8 times (max 2 min wait).
    attempt <- 0
    n_max_attempt <- 8
    base_wait <- 1 # second

    while (attempt < n_max_attempt) {
      attempt <- attempt + 1
      # Increase time between download attempts in exponential backoff
      wait <- base_wait * 2 ^ (attempt - 1)
      download_status <- try({
        download.file(filename, output_file, mode="w", quiet=TRUE)
      })

      if (download_status != 0) {
        if (attempt < n_max_attempt) { message("retrying...") }
        Sys.sleep(wait)
        next
      } else {
        break
      }
    }

    if (attempt == n_max_attempt & download_status != 0) {
      warning(filename, " could not be downloaded")
      # Delete dir. We expect it to be empty, but double check.
      if (length(list.files(output_dir)) == 0) {
        unlink(output_dir, recursive = TRUE)
      }
    } else if (attempt > 1 & download_status == 0) {
      message("succeeded after ", attempt, " attempts")
    }

    # Check if header order is correct and matches the `arrow` schema; newer
    # versions of `arrow` (> 4.0.1) don't reorder columns by name on load.
    # Most of the time columns are ordered correctly, so the
    # read-reorder-write step won't happen that often.
    #
    # The motivating examples here are:
    # - `JHUAPL-Gecko`, which uses a non-standard column ordering
    #   (forecast_date, target, target_end_date, quantile, value, location,
    #   type).
    # - `COVIDhub_CDC-ensemble`, which switched to a non-standard column
    #   ordering as of 2021-11-15.
    expected_headers <- c(
      "forecast_date",
      "target",
      "target_end_date",
      "location",
      "type",
      "quantile",
      "value"
    )
    header <- (readLines(output_file, n=1) %>% strsplit(",", fixed = TRUE))[[1]] %>%
      str_replace_all(fixed("\""), "")
    if (!all(header == expected_headers)) {
      message(filename, " columns are not ordered correctly, reordering")
      # Re-save file with columns in the correct order.
      classes <- cols(
        .default = col_character(), quantile = col_double(), value = col_double()
      )
      read_csv(output_file, col_types = classes) %>%
        relocate(expected_headers) %>%
        write_csv(output_file)
    }
  }

  sch <- schema(forecast_date=string(),
                target=string(),
                target_end_date=string(),
                location=string(),
                type=string(),
                quantile=string(),
                value=string())

  ds <- open_dataset(file.path("data", covidhub_forecaster_name),
                     format = "csv", schema = sch, skip_rows = 1)

  # Create all derived columns from target first to join later
  # Works with just the distinct values of target for efficiency
  target_separated <- ds %>%
    select(.data$target) %>%
    collect() %>%
    distinct() %>%
    process_target(remove = FALSE)

  pcards <- ds %>%
      collect() %>%
      left_join(target_separated, by = "target") %>%
      select(-.data$target) %>%
      mutate(forecaster = covidhub_forecaster_name,
             forecast_date = lubridate::ymd(.data$forecast_date),
             target_end_date = lubridate::ymd(.data$target_end_date),
             quantile = as.double(.data$quantile),
             value = as.double(.data$value)) %>%
      filter_predictions(forecast_type, incidence_period, signal) %>%
      select_pcard_cols()

  pcards <- pcards %>%
    location_2_geo_value()
  if (!identical(geo_values, "*")) {
    pcards <- filter(pcards, .data$geo_value %in% geo_values)
  }
  if (!is.null(ahead)) {
    pcards <- filter(pcards, .data$ahead %in% !!ahead)
  }
  pcards <- filter(pcards, .data$signal %in% !!signal)
  class(pcards) = c("predictions_cards", class(pcards))

  # Cleanup, delete downloaded CSVs from disk
  unlink(file.path("data", covidhub_forecaster_name), recursive = TRUE)

  pcards
}

#' Get available forecast dates for a forecaster on the COVID Hub
#'
#' Retrieves the forecast dates that a forecaster submitted to
#' the [COVID Hub](https://github.com/reichlab/covid19-forecast-hub/).
#'
#' @param forecaster_name String indicating of the forecaster
#'   (matching what it is called on the COVID Hub).
#'
#' @return vector of forecast dates
#'
#' @export
get_covidhub_forecast_dates <- function(forecaster_name) {
  url <- "https://github.com/reichlab/covid19-forecast-hub/tree/master/data-processed/"
  out <- xml2::read_html(paste0(url, forecaster_name)) %>%
    # In main element (identified by id), look for a `grid` object that has `row` children.
    # Avoid using the full xpath due to fragility.
    rvest::html_nodes(xpath = "//*[@id=\"js-repo-pjax-container\"]//div[@role=\"grid\" and .//@role=\"row\"]") %>%
    rvest::html_text() %>%
    stringr::str_remove_all("\\n") %>%
    stringr::str_match_all(sprintf("(20\\d{2}-\\d{2}-\\d{2})-%s.csv",
                                   forecaster_name))
  lubridate::as_date(out[[1]][, 2])
}


#' List all COVID forecast models available
#'
#' Utility function to list all forecasters submitting COVID-19 forecasts to
#' the [COVID 19 Forecast Hub](http://covid19forecasthub.org/).
#'
#' @param repo character string either "zoltar" indicating the
#'   [Zoltar](https://zoltardata.com) Forecast Archive or "covid19forecast_repo"
#'   which lists those available at the
#'   [Reich Lab](https://github.com/reichlab/covid19-forecast-hub)
#'   Github submission repo.
#' @param designations vector of character strings representing acceptable
#'   designation types for models. If `"*"` (the default), models of all
#'   designations will be returned. See
#'   [Reich Lab's Documentation](https://github.com/reichlab/covid19-forecast-hub/blob/master/data-processed/METADATA.md#team_model_designation)
#'   for allowed designations and their meanings.
#' @return character vector of all available forecasters matching given criteria
#' @export
#'
#'
get_covidhub_forecaster_names <- function(
  repo = c("zoltar", "covid19forecast_repo"),
  designations = "*") {

  if (identical(designations, "*")) {
    repo <- match.arg(repo, c("zoltar", "covid19forecast_repo"))
    if (repo == "covid19forecast_repo") repo <- "remote_hub_repo"
    forecaster_names <- covidHubUtils::get_all_models(source = repo)
  } else {
    models <- covidHubUtils::get_model_designations(source = "zoltar")
    forecaster_names <- models %>%
                         filter(.data$designation %in% designations) %>%
                         pull(.data$model)
  }
  return(forecaster_names)
}




#' Vector of quantiles used for submission to the COVID 19 Forecast Hub
#'
#' See the [Forecast Hub Documentation](https://github.com/reichlab/covid19-forecast-hub/blob/master/data-processed/README.md#quantile)
#' for more details.
#'
#' @param type defaults to the "standard" set of 23 quantiles. Setting
#'   `type = "inc_case"` will return a set of 7 quantiles used for incident
#'   cases.
#'
#' @return a numeric vector
#' @export
#'
#' @examples
#' covidhub_probs()
covidhub_probs <- function(type = c("standard", "inc_case")) {
  type = match.arg(type)
  switch(type,
         standard = c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99),
         inc_case = c(0.025, 0.100, 0.250, 0.500, 0.750, 0.900, 0.975)
  )
}
