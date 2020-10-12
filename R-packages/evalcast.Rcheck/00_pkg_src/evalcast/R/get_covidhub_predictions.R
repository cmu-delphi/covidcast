#' Get predictions from a forecaster on COVIDHub
#'
#' This function simply converts the predictions of forecasters submitting to
#' the COVID Hub https://github.com/reichlab/covid19-forecast-hub/
#' to the format of a predictions card, so it can be easily evaluated and
#' compared.
#'
#' For now, this function only supports (i) incident not cumulative predictions
#' and (ii) epiweek not daily incidence_period predictions.
#'
#' @param covid_hub_forecaster_name the name of the forecaster matching what it is called on covid hub
#' @param forecast_dates a vector of class Date on which forecasts will be made.
#'   e.g. \code{c(lubridate::ymd("2020-09-07"), lubridate::ymd("2020-09-14"))}
#' @param ... additional parameters to be passed to \code{\link{filter_predictions}}.
#' @return a list of predictions cards
#' @export
#' @importFrom readr read_csv
get_covidhub_predictions <- function(covid_hub_forecaster_name,
                                     forecast_dates = NULL, ...) {
  url <- "https://raw.githubusercontent.com/reichlab/covid19-forecast-hub/master/data-processed"
  pcards <- list()
  if (is.null(forecast_dates))
    forecast_dates <- get_forecast_dates(covid_hub_forecaster_name)
  forecast_dates <- as.character(forecast_dates)
  for (forecast_date in forecast_dates) {
    filename <- sprintf("%s/%s/%s-%s.csv",
                        url,
                        covid_hub_forecaster_name,
                        forecast_date,
                        covid_hub_forecaster_name)
    cat("Downloading", filename, fill = TRUE)
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
      rename(probs = quantile, quantiles = value) %>%
      filter(str_detect(target, "wk ahead inc")) %>%
      filter(type == "quantile") %>%
      separate(target,
               into = c("ahead", NA, NA, NA, "response"),
               remove = TRUE) %>%
      select(-forecast_date, -type, -target_end_date) %>%
      mutate(geo_type = case_when(nchar(location) == 2 ~ "state",
                                  nchar(location) == 5 ~ "county")) %>%
      group_by(ahead, response, location, geo_type) %>%
      group_modify(~ tibble(forecast_distribution = list(.))) %>%
      group_by(ahead, response, geo_type) %>%
      group_map(~ {
        if (.y$response == "death") {
          signals <- tibble(data_source = "jhu-csse",
                            signal = "deaths_incidence_num")
        } else if (.y$response == "case") {
          signals <- tibble(data_source = "jhu-csse",
                            signal = "confirmed_incidence_num")
        }
        attributes(.x) <- c(
          attributes(.x),
          list(forecaster = NA,
                                 name_of_forecaster = covid_hub_forecaster_name,
                                 signals = signals,
                                 forecast_date = lubridate::ymd(forecast_date),
                                 incidence_period = "epiweek",
                                 ahead = as.numeric(.y$ahead),
                                 geo_type = .y$geo_type,
                                 geo_values = NA,
                                 from_covidhub = TRUE)
        )

        return(.x)
      })
  }
  flatten(pcards) %>% filter_predictions(...)
}

#' Get Available Forecast Dates for Forecaster on COVID Hub
#'
#' Retrieves the forecast dates that a forecaster submitted to
#' the COVID Hub https://github.com/reichlab/covid19-forecast-hub/
#'
#' @param covid_hub_forecaster_name the name of a forecaster on the COVID Hub.
#' @importFrom rvest html_nodes html_text
#' @importFrom xml2 read_html
#' @importFrom stringr str_remove_all str_match_all
#' @export
get_forecast_dates <- function(covid_hub_forecaster_name) {
  url <- "https://github.com/reichlab/covid19-forecast-hub/tree/master/data-processed/"
  out <- xml2::read_html(paste0(url, covid_hub_forecaster_name)) %>%
    rvest::html_nodes(xpath = "//*[@id=\"js-repo-pjax-container\"]/div[2]/div/div[3]") %>%
    rvest::html_text() %>%
    stringr::str_remove_all("\\n") %>%
    stringr::str_match_all(sprintf("(20\\d{2}-\\d{2}-\\d{2})-%s.csv",
                                   covid_hub_forecaster_name))
  out[[1]][, 2]
}

#' @importFrom rvest html_nodes html_text
#' @importFrom xml2 read_html
#' @importFrom stringr str_subset
get_covid_hub_forecaster_names <- function() {
  warning("This should be done with the github API.")
  url <- "https://github.com/reichlab/covid19-forecast-hub/tree/master/data-processed/"
  xml2::read_html(url) %>%
    rvest::html_nodes(xpath="//*[@id=\"js-repo-pjax-container\"]/div[2]/div/div[3]//a[@class='js-navigation-open link-gray-dark']") %>%
    rvest::html_text() %>%
    stringr::str_subset("\\.[md|Rmd|txt|png|R]", negate = TRUE)
}

