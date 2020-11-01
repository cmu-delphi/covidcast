#' Create a list of score cards
#'
#' Performs backtesting, through the following steps:
#' \enumerate{
#'   \item Takes a list of predictions cards (as created by
#'   [get_predictions()]). These should be from a single forecaster, each card 
#'   corresponding to a different forecast date.
#'   \item Downloads from the COVIDcast API the latest available data to compute
#'   what actually occurred (summing the response over the incidence period).
#'   \item Computes various user-specified error measures.
#' }
#' The result is a list of "score cards", where each list element corresponds to
#' a distinct ahead value. A score card is a data frame in which each row
#' corresponds to a location-day pair, and the columns give the values of the
#' error measures (along with other information including the forecast
#' distributions and the actual response values).
#'
#' Backfill refers to the process by which some data sources go back in time
#' updating previously reported values. Suppose it is September 14 and we are 
#' evaluating our predictions for what happened in the previous epiweek
#' (September 6 through 12). Although we may be able to calculate a value for
#' "actual", we might not trust this value since on September 16, backfill may
#' occur changing what is known about the period September 6 through 12.  There are
#' two consequences of this phenomenon.  First, running this function on
#' different dates may result in different estimates of the error. Second, we
#' may not trust the evaluations we get that are too recent.  The parameter
#' `backfill_buffer` specifies how long of a buffer period we should
#' enforce.  This will be dependent on the data source and signal and is left
#' to the user to determine.  If backfill is not relevant for the particular
#' signal you are predicting, then you can set `backfill_buffer` to 0.
#'
#' @param predictions_cards List of predictions cards from the same forecaster 
#'   that are all for the same response, incidence period, and geo type. Each 
#'   should be from a different forecast date or for a different ahead.  A 
#'   predictions card is created by the function [get_predictions()].
#' @param err_measures Named list of one or more functions, where each function
#'   takes a data frame with two columns `probs` and `quantiles` and an actual
#'   (i.e., observed) scalar value and returns some measure of error.  If empty,
#'   returns the scorecard without any error measure columns.
#' @param backfill_buffer How many days until response is deemed trustworthy
#'   enough to be taken as correct? See details for more.
#' 
#' @return List of "score cards", with one element per ahead value. 
#'
#' @importFrom purrr map
#' @importFrom magrittr %>%
#' @importFrom assertthat assert_that
#' @export
evaluate_predictions <- function(
  predictions_cards,
  err_measures = list(wis = weighted_interval_score,
                      ae = absolute_error,
                      coverage_80 = interval_coverage(alpha = 0.2)),
  backfill_buffer = 10) {
  responses <- all_attr(predictions_cards, "signals") %>%
    map(~ .x[1, names(.x) != "start_day"]) # RJT: I added this part. I don't
  # think start dates need to match here. Now that I'm allowing start_day to be 
  # a function, it can depend on the forecast_date, so the next assertion would
  # (unintentionally) fail in that case.
  assert_that(length(unique(responses)) <= 1,
              msg="All predictions cards should have the same response.")
  unique_attr(predictions_cards, "incidence_period")
  unique_attr(predictions_cards, "geo_type")

  ahead <- unlist(all_attr(predictions_cards, "ahead"))
  unique_ahead <- unique(ahead)
  scorecards <- list()
  for (i in seq_along(unique_ahead)) {
    cat("ahead =", unique_ahead[i], fill = TRUE)
    scorecards[[i]] <- evaluate_predictions_single_ahead(
      predictions_cards = predictions_cards[ahead == unique_ahead[i]],
      err_measures = err_measures,
      backfill_buffer = backfill_buffer)
  }
  return(scorecards)
}

#' Create score cards for one ahead
#' 
#' @param predictions_cards List of predictions cards from the same forecaster 
#'   that are all for the same prediction task, meaning they are for the same
#'   response, incidence period, ahead, and geo type. Each should be from a
#'   different forecast date.  A predictions card is created by the function
#'   [get_predictions()].
#' @param err_measures Named list of one or more functions, where each function
#'   takes a data frame with two columns `probs` and `quantiles` and an actual
#'   (i.e., observed) scalar value and returns some measure of error.  If empty,
#'   returns the scorecard without any error measure columns.
#' @param backfill_buffer How many days until response is deemed trustworthy
#'   enough to be taken as correct? See details for more.
#'   
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom purrr map2_dfr map
#' @importFrom dplyr filter select inner_join mutate rowwise ungroup
#' @importFrom stringr str_glue_data
#' @importFrom rlang :=
#' @importFrom assertthat assert_that
evaluate_predictions_single_ahead <- function(predictions_cards,
                                              err_measures,
                                              backfill_buffer) {
  # get information from predictions cards' attributes and check:
  att <- get_and_check_pc_attributes(predictions_cards)
  # calculate the actual value we're trying to predict:
  target_response <- get_target_response(att$signals,
                                         att$forecast_dates,
                                         att$incidence_period,
                                         att$ahead,
                                         att$geo_type)
  as_of <- attr(target_response, "as_of")
  . <- "got this idea from https://github.com/tidyverse/magrittr/issues/29"
  assert_that(as_of > max(target_response$end) + backfill_buffer,
              msg=(target_response %>% filter(.data$end == max(.data$end)) %>%
              stringr::str_glue_data(
              "Reliable data for evaluation is not yet available for ",
              "`forecast_date` of {forecast_date} because target period ",
              "extends to {end} which is too recent to be reliable ",
              "according to the provided `backfill_buffer` of ",
              "{backfill_buffer}.", forecast_date=.$forecast_date[1],
              end=.$end[1], backfill_buffer=backfill_buffer)))
  # combine all predictions cards into a single data frame with an additional
  # column called forecast_date:
  predicted <- map2_dfr(predictions_cards, att$forecast_dates,
                        ~ mutate(.x, forecast_date = .y))
  invalid <- check_valid_forecaster_output(predicted)
  assert_that(nrow(invalid) == 0,
              msg=paste0("The following `forecast_date`, `location` pairs have invalid ",
                         "forecasts:\n", invalid %>%
                                         stringr::str_glue_data("({forecast_date}, {location})") %>%
                                         paste(collapse = "\n"), "."))
  # join together the data frames target_response and predicted:
  score_card <- target_response %>%
    inner_join(predicted, by = c("location", "forecast_date")) %>%
    select(.data$location,
           .data$forecast_date,
           .data$start,
           .data$end,
           .data$actual,
           .data$forecast_distribution)
  # compute the error
  for (err in names(err_measures)) {
    score_card <- score_card %>%
      rowwise() %>%
      mutate(!!err := err_measures[[err]](.data$forecast_distribution, .data$actual)) %>%
      ungroup()
  }
  pc_attributes_list <- predictions_cards %>%
    map(~ attributes(.x)[-(1:3)])
  attributes(score_card) <- c(
    attributes(score_card),
    list(pc_attributes_list = pc_attributes_list,
         name_of_forecaster = att$name_of_forecaster,
         response = att$signals[1, 1:2],
         forecast_dates = att$forecast_dates,
         incidence_period = att$incidence_period,
         ahead = att$ahead,
         geo_type = att$geo_type,
         err_measures = err_measures,
         backfill_buffer = backfill_buffer,
         as_of = as_of)
  )
  return(score_card)
}

#' Check that a forecaster's output is valid
#' @param pred_card Prediction card, in the form created by [get_predictions()].
#' @export
check_valid_forecaster_output <- function(pred_card) {
  null_forecasts <- pred_card$forecast_distribution %>%
    map_lgl(is.null)
  covidhub_probs <- c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)
  wrong_format <- pred_card$forecast_distribution %>%
    map_lgl(~ any(names(.x) != c("probs", "quantiles")))
  wrong_probs <- pred_card$forecast_distribution %>%
    map_lgl(~ all(abs(.x$probs - covidhub_probs) > 1e-8))
  bad_quantiles <- pred_card$forecast_distribution %>%
    map_lgl(~ all(diff(.x$quantiles) < -1e-8))
  pred_card %>%
    mutate(null_forecasts = null_forecasts,
           wrong_probs = wrong_probs,
           bad_quantiles = bad_quantiles) %>%
    filter(null_forecasts | wrong_probs | bad_quantiles | wrong_format)
}
