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
#'   "deaths_incidence_num", and/or "deaths_cumulative_num" (those currently
#'   forecast by the COVIDhub-ensemble). For other types, use one of the
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
#' @seealso [get_predictions()]
#' @seealso [get_zoltar_predictions()]
#' @template predictions_cards-template
#' @return For more flexible processing of COVID Hub data, try
#'   using [zoltr](https://docs.zoltardata.com/zoltr/)
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
             "deaths_cumulative_num"),
  predictions_cards = NULL,
  start_date = NULL,
  end_date = NULL,
  date_filtering_function = NULL) {
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
      distinct(forecast_date, forecaster)
    for (i in seq_len(length(covidhub_forecaster_name))) {
      if (covidhub_forecaster_name[[i]] %in% seen_dates$forecaster) {
        seen_forecaster_dates <- seen_dates %>%
          filter(forecaster == covidhub_forecaster_name[[i]]) %>%
          pull(forecast_date)
        forecast_dates[[i]] <- lubridate::as_date(setdiff(
                                                    forecast_dates[[i]],
                                                    seen_forecaster_dates))
      }
    }
  }

  predictions_cards_list <- vector("list",
                                   length = length(covidhub_forecaster_name))
  for (i in seq_len(length(covidhub_forecaster_name))) {
    if (length(forecast_dates[[i]] > 0)) {
      predictions_cards_list[[i]] <- tryCatch({
        get_forecaster_predictions(covidhub_forecaster_name[i],
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
#'   "deaths_incidence_num", and/or "deaths_cumulative_num" (those currently
#'   forecast by the COVIDhub-ensemble). For other types, use one of the 
#'   alternatives mentioned above
#'   
#' @template predictions_cards-template
#' 
#' @seealso [get_predictions()]
#' @seealso [get_zoltar_predictions()]
#' @return Predictions card. For more flexible processing of COVID Hub data, try
#'   using [zoltr](https://docs.zoltardata.com/zoltr/)
#' 
#' @importFrom readr read_csv
get_forecaster_predictions <- function(covidhub_forecaster_name,
                                     forecast_dates = NULL,
                                     geo_values = "*",
                                     forecast_type = c("point","quantile"),
                                     ahead = 1:4,
                                     incidence_period = c("epiweek", "day"),
                                     signal = c("confirmed_incidence_num",
                                                "deaths_incidence_num",
                                                "deaths_cumulative_num")
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
    pred <- read_csv(filename,
                     col_types = cols(
                       location = col_character(),
                       forecast_date = col_date(format = ""),
                       quantile = col_double(),
                       value = col_double(),
                       target = col_character(),
                       target_end_date = col_date(format = ""),
                       type = col_character()
                     ))
    pcards[[forecast_date]] <- pred %>%
      separate(.data$target,
               into = c("ahead", "incidence_period", NA, "inc", "response"),
               remove = TRUE) %>%
      mutate(forecaster = covidhub_forecaster_name, 
             incidence_period = if_else(
               .data$incidence_period == "wk", "epiweek","day"),
             inc = if_else(.data$inc == "inc", "incidence", "cumulative"),
             response = case_when(.data$response == "death" ~ "deaths", 
                                  .data$response == "case" ~ "confirmed",
                                  TRUE ~ "drop",),
             signal = paste(response, inc, "num", sep="_"),
             data_source = if_else(.data$response=="drop", "drop", "jhu-csse"),
             ahead = as.integer(.data$ahead)) %>%
      filter(.data$response != "drop", .data$type %in% forecast_type,
             .data$incidence_period %in% incidence_period,
             .data$signal %in% signal) %>%
      select(.data$ahead, .data$location, .data$quantile, .data$value,
               .data$forecaster, .data$forecast_date, .data$data_source,
               .data$signal, .data$target_end_date, 
               .data$incidence_period)
  }
  pcards <- bind_rows(pcards) %>%
    mutate(geo_value = if_else(nchar(.data$location)==2,
                               fips_2_abbr(.data$location),
                               .data$location),
           location = NULL) %>%
    relocate(.data$geo_value, .after = .data$ahead)
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
    rvest::html_nodes(xpath = "//*[@id=\"js-repo-pjax-container\"]/div[2]/div/div/div[3]") %>%
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
#' @param repo character strinng either "zoltar" indicating the 
#' [Zoltar](https://zoltardata.com) Forecast Archive or "covid19forecast_repo"
#' which lists those available at the 
#' [Reich Lab](https://github.com/reichlab/covid19-forecast-hub)
#' Github submission repo.
#'
#' @return character vector of all available forecasters
#' @export
#'
#'
get_covidhub_forecaster_names <- function(
  repo = c("zoltar", "covid19forecast_repo")) {
  
  repo <- match.arg(repo, c("zoltar","covid19forecast_repo"))
  if (repo == "covid19forecast_repo") repo = "remote_hub_repo"
  
  covidHubUtils::get_all_models(source = repo)
}

covidhub_probs <- function() {
  c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)
}
