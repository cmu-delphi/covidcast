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
#'   "deaths_incidence_num", "deaths_cumulative_num", and/or
#'   "confirmed_admissions_covid_1d". For other types, use one of the 
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
                                              "deaths_cumulative_num",
                                              "confirmed_admissions_covid_1d"),
                                   as_of = NULL){
  if (is.null(geo_values) || geo_values == "*"){
    geo_values <- NULL
  } else {
    gt_fips <- grepl("[a-z]", geo_values) & geo_values != "us"
    geo_values[gt_fips] <- abbr_2_fips(geo_values[gt_fips])
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
                               "deaths_cumulative_num",
                               "confirmed_admissions_covid_1d"), TRUE)
    cd <- ifelse(startsWith(sig, "deaths"), "death",
                 ifelse(sig == "confirmed_incidence_num", "case", "hosp"))
    ic <- ifelse(str_detect(sig, "cum"), "cum", "inc")
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
    zoltr::zoltar_authenticate(zoltar_connection, Sys.getenv("Z_USERNAME"),
                               Sys.getenv("Z_PASSWORD"))
  }
  
  # construct Zoltar project url
  the_projects <- zoltr::projects(zoltar_connection)
  project_url <- the_projects[the_projects$name == "COVID-19 Forecasts", "url"]
  
  # get all valid timezeros in project
  all_valid_timezeros <- zoltr::timezeros(
    zoltar_connection = zoltar_connection,
    project_url = project_url
    )$timezero_date
  
  if (missing(forecast_dates)){
    valid_forecast_dates <- all_valid_timezeros
  } else {
    valid_forecast_dates <- intersect(as.character(forecast_dates), 
                                      as.character(all_valid_timezeros))
  }
  
  print("Grabbing forecasts from Zoltar...")
  forecasts <- suppressMessages(
    zoltr::do_zoltar_query(
      zoltar_connection = zoltar_connection, project_url = project_url,
      query_type = "forecasts", units = geo_values, 
      timezeros = valid_forecast_dates, models = forecaster_names,
      targets = targets, types = forecast_type, as_of = as_of, verbose = FALSE))
  if (nrow(forecasts) == 0) {
    warning(paste("Warning in do_zoltar_query: Forecasts are not available.\n", 
                  "Please check your parameters."))
  }
  
  forecasts <- forecasts %>%
    select(.data$model, .data$timezero, .data$unit, 
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
                           .data$response == "hosp" ~ "hosp",
                           TRUE ~ "drop"),
      data_source = case_when(.data$response == "deaths" ~ "jhu-csse",
                              .data$response == "confirmed" ~ "jhu-csse",
                              .data$response == "hosp" ~ "hhs",
                              TRUE ~ "drop"),
      signal = case_when(
        .data$data_source == "jhu-csse" ~ paste(.data$response, .data$inc, "num", sep="_"),
        .data$data_source == "hhs" & .data$inc == "incidence" ~ "confirmed_admissions_covid_1d",
        TRUE ~ "drop"),
      forecast_date = lubridate::ymd(.data$forecast_date),
      target_end_date = .data$forecast_date + .data$ahead)
  epw <- forecasts$incidence_period == "epiweek"
  forecasts$target_end_date[epw] <- get_target_period(
    forecasts$forecast_date[epw], "epiweek", forecasts$ahead[epw])$end
  
  forecasts <- forecasts %>%
    filter(.data$response != "drop") %>%
    select(.data$ahead, .data$geo_value, .data$quantile, .data$value,
                  .data$forecaster, .data$forecast_date, .data$data_source,
                  .data$signal, .data$target_end_date, 
                  .data$incidence_period) %>%
    mutate(geo_value = if_else(
      nchar(.data$geo_value) == 2,
      fips_2_abbr(.data$geo_value),
      tolower(.data$geo_value)))
  
  if (is.null(signal) && !is.null(ahead)) {
    forecasts <- filter(forecasts, .data$ahead %in% !!ahead)
  }
  if (!is.null(signal) && is.null(ahead)) {
    forecasts <- filter(forecasts, .data$signal %in% !!signal)
  }
  if (!is.null(signal) && !is.null(ahead)) {
    forecasts <- filter(forecasts, .data$signal %in% !!signal, 
                        .data$ahead %in% !!ahead)
  }
  class(forecasts) <- c("predictions_cards", class(forecasts))
  forecasts
}




#' Get available forecast dates for a COVID forecaster on Zoltar
#'
#' Retrieves the forecast dates that a forecaster submitted to
#'  [Zoltar](https://www.zoltardata.com). 
#'
#' @param forecaster_name String indicating of the forecaster
#'   (matching what it is called on the COVID Hub).
#'   
#' @return vector of forecast dates
#' 
#' @export
get_zoltar_forecast_dates <- function(forecaster_name) {
  # set up Zoltar connection
  zoltar_connection <- zoltr::new_connection()
  if(Sys.getenv("Z_USERNAME") == "" | Sys.getenv("Z_PASSWORD") == "") {
    zoltr::zoltar_authenticate(zoltar_connection, "zoltar_demo","Dq65&aP0nIlG")
  } else {
    zoltr::zoltar_authenticate(zoltar_connection, Sys.getenv("Z_USERNAME"),
                               Sys.getenv("Z_PASSWORD"))
  }
  the_projects <- zoltr::projects(zoltar_connection)
  project_url <- the_projects[the_projects$name == "COVID-19 Forecasts", "url"]
  the_models <- zoltr::models(zoltar_connection, project_url)
  model_url <- the_models[the_models$model_abbr == forecaster_name, "url"]
  the_forecasts <- zoltr::forecasts(zoltar_connection, model_url)
  
  sort(the_forecasts$timezero_date)
}
