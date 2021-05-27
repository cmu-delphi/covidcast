#' Separate fields of target col and create derived fields
#'
#' This function supports only "confirmed_incidence_num",
#' "deaths_incidence_num", "deaths_cumulative_num", and/or
#' "confirmed_admissions_covid_1d".
#'
#' @param predictions Dataframe with `target` column containing information
#'   about time period and signal forecast.
#' @param remove 	If TRUE, remove input `target` column from output data frame.
#'   See `dplyr::separate`.
#'
#' @return predictions dataframe with new `ahead`, `incidence_period`, `inc`,
#'   `response`, `data_source`, and `signal` fields
process_target <- function(predictions,
                           remove) {
  predictions %>%
    separate(.data$target,
             into = c("ahead", "incidence_period", NA, "inc", "response"),
             remove = remove, sep = " ") %>%
    mutate(incidence_period = if_else(
      .data$incidence_period == "wk", "epiweek","day"),
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
      ahead = as.integer(.data$ahead))
}

#' Drop irrelevant predictions
#'
#' @param predictions Dataframe as produced by `process_target`.
#' @param forecast_type "quantile", "point", or both
#' @param incidence_period "epiweek", "day", or both
#' @param signal Any combination of "confirmed_incidence_num",
#'   "deaths_incidence_num", "deaths_cumulative_num", and/or
#'   "confirmed_admissions_covid_1d".
#'
#' @return predictions dataframe with irrelevant rows removed
filter_predictions <- function(predictions,
                               forecast_type,
                               incidence_period,
                               signal) {
  # Filter does not use variables in global environment when variable names is
  # the same as a column name, so do a workaround.
  forecast_type <- enquo(forecast_type)
  incidence_period <- enquo(incidence_period)
  signal <- enquo(signal)
  
  predictions %>%
    filter(.data$response != "drop",
           .data$type %in% (!!forecast_type),
           .data$incidence_period %in% (!!incidence_period),
           .data$signal %in% (!!signal))
}

#' Select and reorder desired columns for the pcard format
#'
#' @param predictions Dataframe as produced by `filter_predictions`.
#'
#' @return predictions dataframe with irrelevant columns removed
select_pcard_cols <- function(predictions) {
  predictions %>%
    select(.data$ahead, .data$location, .data$quantile, .data$value,
           .data$forecaster, .data$forecast_date, .data$data_source,
           .data$signal, .data$target_end_date,
           .data$incidence_period)
}

#' Convert location FIPS to abbreviations as needed for the pcard format
#'
#' @param predictions Dataframe with `location` column.
#' @param default_process_fn Function to use to reformat the `location` column
#'   if already in abbreviation format. Defaults to `as.character`.
#'
#' @return predictions dataframe with irrelevant columns removed
location_2_geo_value <- function(predictions,
                              default_process_fn = as.character) {
  predictions %>%
    mutate(geo_value = if_else(nchar(.data$location) == 2,
                               fips_2_abbr(.data$location),
                               default_process_fn(.data$location)),
           location = NULL) %>%
    relocate(.data$geo_value, .after = .data$ahead)
}
