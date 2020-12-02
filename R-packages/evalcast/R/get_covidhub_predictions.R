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
# @param ... Additional parameters to be passed to [filter_predictions()].
#' @return tibble of predictions cards. Only incident predictions are returned.
#'   For more flexible processing of COVID Hub data, try using 
#'   [zoltr](https://docs.zoltardata.com/zoltr/)
#' 
#' @seealso [get_predictions()]
#' @importFrom readr read_csv
#' @export
get_covidhub_predictions <- function(covidhub_forecaster_name,
                                     forecast_dates = NULL) {
  url <- "https://raw.githubusercontent.com/reichlab/covid19-forecast-hub/master/data-processed"
  pcards <- list()
  if (is.null(forecast_dates))
    forecast_dates <- get_forecast_dates(covidhub_forecaster_name)
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
      filter(str_detect(.data$target, "wk ahead inc")) %>%
      separate(.data$target,
               into = c("ahead", NA, NA, NA, "response"),
               remove = TRUE) %>%
      mutate(forecaster = covidhub_forecaster_name, 
             incidence_period = 'epiweek',
             data_source = "jhu-csse",
             ahead = as.integer(ahead),
             signal = if_else(.data$response=="death",
                                     "deaths_incidence_num",
                                     "confirmed_incidence_num")) %>%
      select(-.data$response, -.data$type) %>%
      relocate(.data$ahead, .data$location, .data$quantile, .data$value,
               .data$forecaster, .data$forecast_date, .data$data_source,
               .data$signal, .data$target_end_date, 
               .data$incidence_period)
  }
  bind_rows(pcards) #%>% filter_predictions(...)
}

#' Get available forecast dates for a forecaster on the COVID Hub
#'
#' Retrieves the forecast dates that a forecaster submitted to
#' the [COVID Hub](https://github.com/reichlab/covid19-forecast-hub/).
#'
#' @param covidhub_forecaster_name String indicating of the forecaster
#'   (matching what it is called on the COVID Hub).
#' 
#' @importFrom rvest html_nodes html_text
#' @importFrom xml2 read_html
#' @importFrom stringr str_remove_all str_match_all
#' @importFrom lubridate as_date
#' @export
get_forecast_dates <- function(covidhub_forecaster_name) {
  url <- "https://github.com/reichlab/covid19-forecast-hub/tree/master/data-processed/"
  out <- xml2::read_html(paste0(url, covidhub_forecaster_name)) %>%
    rvest::html_nodes(xpath = "//*[@id=\"js-repo-pjax-container\"]/div[2]/div/div[3]") %>%
    rvest::html_text() %>%
    stringr::str_remove_all("\\n") %>%
    stringr::str_match_all(sprintf("(20\\d{2}-\\d{2}-\\d{2})-%s.csv",
                                   covidhub_forecaster_name))
  return(lubridate::as_date(out[[1]][, 2]))
}

#' @importFrom rvest html_nodes html_text
#' @importFrom xml2 read_html
#' @importFrom stringr str_subset
get_covidhub_forecaster_names <- function() {
  # warning("This should be reimplemented using the Zoltar API ... coming soon.")
  url <- "https://github.com/reichlab/covid19-forecast-hub/tree/master/data-processed/"
  xml2::read_html(url) %>%
    rvest::html_nodes(xpath="//*[@id=\"js-repo-pjax-container\"]/div[2]/div/div[3]//a[@class='js-navigation-open link-gray-dark']") %>%
    rvest::html_text() %>%
    stringr::str_subset("\\.[md|Rmd|txt|png|R]", negate = TRUE)
}

