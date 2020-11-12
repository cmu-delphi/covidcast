#' Get predictions from a forecaster on the COVID Hub
#'
#' Simply converts the predictions of forecasters submitting to the [COVID
#' Hub](https://github.com/reichlab/covid19-forecast-hub/) to the format of a
#' predictions card, so it can be easily evaluated and compared.
#'
#' For now, this function only supports (i) incident not cumulative predictions
#' and (ii) epiweek not daily incidence period predictions.
#'
#' @param covid_hub_forecaster_name String indicating of the forecaster
#'   (matching what it is called on the COVID Hub).
#' @param forecast_dates Vector of Date objects (or strings of the form
#'   "YYYY-MM-DD") indicating dates on which forecasts will be made. If `NULL`,
#'   the default, then all currently available forecast dates from the given
#'   forecaster in the COVID Hub will be used.
#' @param ... Additional parameters to be passed to [filter_predictions()].
#' @return List of predictions cards.
#' 
#' @seealso [get_predictions()]
#' @importFrom readr read_csv
#' @importFrom rlang .data
#' @importFrom purrr flatten
#' @importFrom dplyr rename filter select mutate group_by group_modify group_map summarize case_when
#' @importFrom tidyr separate
#' @importFrom tibble tibble
#' @importFrom stringr str_detect
#' @export
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
    message("Downloading ", filename)
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
      rename(probs = .data$quantile, quantiles = .data$value) %>%
      filter(str_detect(.data$target, "wk ahead inc")) %>%
      filter(.data$type == "quantile") %>%
      separate(.data$target,
               into = c("ahead", NA, NA, NA, "response"),
               remove = TRUE) %>%
      select(-.data$forecast_date, -.data$type, -.data$target_end_date) %>%
      mutate(geo_type = case_when(nchar(.data$location) == 2 ~ "state",
                                  nchar(.data$location) == 5 ~ "county")) %>%
      group_by(.data$ahead, .data$response, .data$location, .data$geo_type) %>%
      group_modify(~ tibble(forecast_distribution = list(.))) %>%
      group_by(.data$ahead, .data$response, .data$geo_type) %>%
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
        class(.x) <- c("prediction_card", class(.x))
        return(.x)
      })
  }
  flatten(pcards) %>% filter_predictions(...)
}

#' Get available forecast dates for a forecaster on the COVID Hub
#'
#' Retrieves the forecast dates that a forecaster submitted to
#' the [COVID Hub](https://github.com/reichlab/covid19-forecast-hub/).
#'
#' @param covid_hub_forecaster_name String indicating of the forecaster
#'   (matching what it is called on the COVID Hub).
#' 
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
  warning("This should be reimplemented using the Zoltar API ... coming soon.")
  url <- "https://github.com/reichlab/covid19-forecast-hub/tree/master/data-processed/"
  xml2::read_html(url) %>%
    rvest::html_nodes(xpath="//*[@id=\"js-repo-pjax-container\"]/div[2]/div/div[3]//a[@class='js-navigation-open link-gray-dark']") %>%
    rvest::html_text() %>%
    stringr::str_subset("\\.[md|Rmd|txt|png|R]", negate = TRUE)
}

