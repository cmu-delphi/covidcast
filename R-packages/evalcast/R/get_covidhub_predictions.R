#' Get predictions from a COVID forecaster in [Zoltar](https://www.zoltardata.com)
#'
#' Simply converts the predictions of forecasters submitting to the [COVID
#' Hub](https://github.com/reichlab/covid19-forecast-hub/) to the format of a
#' predictions card, so it can be easily evaluated and compared.
#'
#' Note: For greater flexibility, use [zoltr::do_zoltar_query()] or perhaps
#' [covidHubUtils::load_forecasts()].
#'
#' @param forecaster_names Vector of strings indicating of the forecaster(s)
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
#' @param incidence_period one of "epiweek" or "day". NULL will attempt to fetch
#'   both
#' @param signal this function supports only "confirmed_incidence_num",
#'   "deaths_incidence_num", and/or "deaths_cumulative_num" (those currently)
#'   forecast by the COVIDhub-ensemble). For other types, use one of the 
#'   alternatives mentioned above
#' @param as_of only forecasts available as of this date will be retrieved.
#'   Default (NULL) is effectively as of today
#'   
#' @template predictions_cards-template
#' 
#' @seealso [get_predictions()]
#' @seealso [get_covidhub_predictions()]
#' @export
get_zoltar_predictions <- function(forecaster_names = NULL,
                                   forecast_dates = NULL,
                                   geo_values = "*",
                                   forecast_type = c("point","quantile"),
                                   ahead = 1:4,
                                   incidence_period = c("epiweek", "day"),
                                   signal = c("confirmed_incidence_num",
                                              "deaths_incidence_num",
                                              "deaths_cumulative_num"),
                                   as_of = NULL){
  if (is.null(geo_values) || geo_values == "*"){
    geo_values <- NULL
  } else {
    gt_fips <- grepl("[a-z]", geo_values) & geo_values != "us"
    geo_values[gt_fips] <- substr(abbr_2_fips(geo_values[gt_fips]), 1, 2)
    geo_values[geo_values == "us"] <- "US"
  }
  if (!is.null(forecast_type)) {
    forecast_type <- match.arg(forecast_type, c("point","quantile"), TRUE)
  }
  if (ext_filter <- is.null(ahead) || is.null(signal)) {
    targets <- NULL
  } else {
    sig <- match.arg(signal, c("confirmed_incidence_num", 
                                  "deaths_incidence_num",
                                  "deaths_cumulative_num"), TRUE)
    cd <- ifelse(substr(sig, 1, 1)=="c", "case", "death")
    ic <- ifelse(str_extract(sig,"(?<=_)[ci]") == "c", "cum", "inc")
    incidence_period <- match.arg(incidence_period, c("epiweek","day"))
    dw <- ifelse(incidence_period == "epiweek", "wk", "day")
    targets <- paste(dw, "ahead", ic, cd)
    targets <- outer(ahead, targets, paste) %>% c()
  }
  
  # set up Zoltar connection
  zoltar_connection <- zoltr::new_connection()
  if(Sys.getenv("Z_USERNAME") == "" | Sys.getenv("Z_PASSWORD") == "") {
    zoltr::zoltar_authenticate(zoltar_connection, "zoltar_demo","Dq65&aP0nIlG")
  } else {
    zoltr::zoltar_authenticate(zoltar_connection, Sys.getenv("Z_USERNAME"),Sys.getenv("Z_PASSWORD"))
  }
  
  # construct Zoltar project url
  the_projects <- zoltr::projects(zoltar_connection)
  project_url <- the_projects[the_projects$name == "COVID-19 Forecasts", "url"]
  
  # get all valid timezeros in project
  all_valid_timezeros <- zoltr::timezeros(
    zoltar_connection = zoltar_connection,
    project_url = project_url)$timezero_date
  
  if (!missing(forecast_dates)){
    valid_forecast_dates <- intersect(as.character(forecast_dates), 
                                      as.character(all_valid_timezeros))
  } else {
    valid_forecast_dates <- all_valid_timezeros
  }
  
  print("Grabbing forecasts from Zoltar...")
  forecasts <- suppressMessages(
    zoltr::do_zoltar_query(
      zoltar_connection = zoltar_connection, project_url = project_url,
      query_type = "forecasts", units = geo_values, 
      timezeros = valid_forecast_dates, models = forecaster_names,
      targets = targets, types = forecast_type, as_of = as_of, verbose = FALSE))
  if (nrow(forecasts) ==0){
    warning(paste("Warning in do_zotar_query: Forecasts are not available.\n", 
                  "Please check your parameters."))
  }
  
  forecasts <- forecasts %>%
    dplyr::select(.data$model, .data$timezero, .data$unit, 
                  .data$target, .data$quantile, .data$value) %>%
    rename(forecaster = .data$model, forecast_date = .data$timezero,
           geo_value = .data$unit) %>%
    separate(.data$target,
             into = c("ahead", "incidence_period", NA, "inc", "response"),
             remove = TRUE) %>%
    mutate(incidence_period = if_else(
      .data$incidence_period == "wk", "epiweek","day"),
      ahead = as.numeric(.data$ahead),
      inc = if_else(.data$inc == "inc", "incidence", "cumulative"),
      response = case_when(.data$response == "death" ~ "deaths", 
                           .data$response == "case" ~ "confirmed",
                           TRUE ~ "drop",),
      signal = paste(response, inc, "num", sep="_"),
      data_source = if_else(.data$response != "drop", "jhu-csse", "drop"),
      forecast_date = ymd(.data$forecast_date),
      target_end_date = .data$forecast_date + .data$ahead)
  epw <- forecasts$incidence_period == "epiweek"
  forecasts$target_end_date[epw] <- get_target_period(
    forecasts$forecast_date[epw], "epiweek", forecasts$ahead[epw])$end
  
  forecasts <- forecasts %>%
    dplyr::filter(.data$response != "hosp") %>%
    dplyr::select(.data$ahead, .data$geo_value, .data$quantile, .data$value,
                  .data$forecaster, .data$forecast_date, .data$data_source,
                  .data$signal, .data$target_end_date, 
                  .data$incidence_period) %>%
    mutate(geo_value = case_when(
      .data$geo_value == "US" ~ "us",
      nchar(.data$geo_value) == 2 ~ fips_2_abbr(paste0(.data$geo_value,"000")),
      TRUE ~.data$geo_value))
      
  if (is.null(signal) && !is.null(ahead)) {
    forecasts <- filter(forecasts, .data$ahead %in% ahead)
  }
  if (!is.null(signal) && is.null(ahead)) {
    forecasts <- filter(forecasts, .data$signal %in% signal)
  }
  class(forecasts) <- c("predictions_cards", class(forecasts))
  forecasts
}




#' Get predictions from a forecaster on the COVID Hub
#'
#' Simply converts the predictions of forecasters submitting to the [COVID
#' Hub](https://github.com/reichlab/covid19-forecast-hub/) to the format of a
#' predictions card, so it can be easily evaluated and compared.
#'
# For now, this function only supports (i) incident not cumulative predictions
# and (ii) epiweek not daily incidence period predictions.
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
#'   "deaths_incidence_num", and/or "deaths_cumulative_num" (those currently)
#'   forecast by the COVIDhub-ensemble). For other types, use one of the 
#'   alternatives mentioned above
#'   
#' @template predictions_cards-template
#' 
#' @seealso [get_predictions()]
#' @seealso [get_zoltar_predictions()]

# @param ... Additional parameters to be passed to [filter_predictions()].
#' @return tibble of predictions cards. Only incident predictions are returned.
#'   For more flexible processing of COVID Hub data, try using 
#'   [zoltr](https://docs.zoltardata.com/zoltr/)
#' 
#' @seealso [get_predictions()]
#' @importFrom readr read_csv
#' @export
get_covidhub_predictions <- function(covidhub_forecaster_name,
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
             data_source = if_else(.data$response != "drop", 
                                    "jhu-csse", "drop"),
             ahead = as.integer(ahead)) %>%
      filter(.data$response != "drop", .data$type %in% forecast_type,
             .data$incidence_period %in% incidence_period,
             .data$signal %in% signal) %>%
      select(.data$ahead, .data$location, .data$quantile, .data$value,
               .data$forecaster, .data$forecast_date, .data$data_source,
               .data$signal, .data$target_end_date, 
               .data$incidence_period)
    if (!is.null(ahead)) {
      pcards[[forecast_date]] <- filter(pcards[[forecast_date]], 
                                        .data$ahead %in% ahead)
    }
  }
  pcards <- bind_rows(pcards) %>%
    mutate(geo_value = if_else(nchar(.data$location)==2,
                               fips_2_abbr(paste0(.data$location,"000")),
                               .data$location),
           location = NULL) %>%
    relocate(.data$geo_value, .after = .data$ahead)
  if (geo_values != "*") {
    pcards <- filter(pcards, .data$geo_value %in% geo_values)
  }
  class(pcards) = c("predictions_cards", class(pcards))
  pcards
}

#' Get available forecast dates for a forecaster on the COVID Hub
#'
#' Retrieves the forecast dates that a forecaster submitted to
#' the [COVID Hub](https://github.com/reichlab/covid19-forecast-hub/). 
#' Currently no way to query Zoltar for this information.
#'
#' @param covidhub_forecaster_name String indicating of the forecaster
#'   (matching what it is called on the COVID Hub).
#' 
#' @export
get_covidhub_forecast_dates <- function(covidhub_forecaster_name) {
  url <- "https://github.com/reichlab/covid19-forecast-hub/tree/master/data-processed/"
  out <- xml2::read_html(paste0(url, covidhub_forecaster_name)) %>%
    rvest::html_nodes(xpath = "//*[@id=\"js-repo-pjax-container\"]/div[2]/div/div[3]") %>%
    rvest::html_text() %>%
    stringr::str_remove_all("\\n") %>%
    stringr::str_match_all(sprintf("(20\\d{2}-\\d{2}-\\d{2})-%s.csv",
                                   covidhub_forecaster_name))
  return(lubridate::as_date(out[[1]][, 2]))
}



#' List all COVID forecast models available
#'
#' Utility function to list all forecasters submitting COVID-19 forecasts to
#' the [COVID 19 Forecast Hub](http://covid19forecasthub.org/). 
#'
#' @param repo character strinng either "zoltar" indicating the 
#' [Zoltar](https://zoltardata.com) Forecast Archive or "covid19forecast_repo"
#' which lists those available at the 
#' [Reichlab](https://github.com/reichlab/covid19-forecast-hub)
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

