#' Create a score card data frame
#'
#' Evaluates the performance of a forecaster, through the following steps:
#' \enumerate{
#'   \item Takes a prediction card (as created by
#'   [get_predictions()]).
#'   \item Computes various user-specified error measures.
#' }
#' The result is a "score card" data frame, where each row corresponds to
#' a prediction-result pair, where the columns define the prediction
#' task, give the observed value, and give the calculated values of the provided
#' error measures.
#'
#' @param predictions_cards tibble of quantile forecasts, which contains at
#'   least `quantile` and `value` columns, as well as any other prediction task
#'   identifiers. For covid data, a predictions card may be created by the
#'   function [get_predictions()], downloaded with [get_covidhub_predictions()]
#'   or created manually.
#' @param err_measures Named list of one or more functions, where each function
#'   takes a data frame with three columns `quantile`, `value` and `actual`
#'   (i.e., observed) and returns a scalar measure of error. Null or an empty
#'   list may be provided if scoring is not desired.
#' @param truth_data truth data (observed). This should be a data frame that
#'   will be joined to `predictions_cards` by all available columns. The
#'   observed data column should be named `actual`.
#' @param grp_vars character vector of named columns in `predictions_cards`
#'  such that the combination gives a unique (quantile) prediction.
#'
#' @return tibble of "score cards". Contains the same information as the
#'   `predictions_cards()` with additional columns for each `err_measure` and
#'   for the truth (named `actual`).
#'
#' @export
evaluate_predictions <- function(
  predictions_cards,
  truth_data,
  err_measures = list(wis = weighted_interval_score,
                      ae = absolute_error,
                      coverage_80 = interval_coverage(coverage = 0.8)),
  grp_vars = c("forecaster",
               intersect(colnames(predictions_cards), colnames(truth_data)))) {

  assert_that(is.data.frame(truth_data),
              msg = paste("In evaluate_predictions: `truth_data` must be a
                          data frame"))
  assert_that("actual" %in% names(truth_data),
              msg = paste("`truth_data` must contain a column named `actual`"))
  predictions_cards <- left_join(predictions_cards, truth_data)
  if (is.null(err_measures) || length(err_measures) == 0) {
    score_card <- predictions_cards
  } else {
    score_card <- predictions_cards %>% group_by(across(all_of(grp_vars)))
    sc_keys <- score_card %>% group_keys()
    score_card <- score_card %>%
      group_split() %>%
      lapply(erm, err_measures = err_measures) %>%
      bind_rows() %>%
      bind_cols(sc_keys) %>%
      inner_join(predictions_cards, by = grp_vars)
  }
  class(score_card) <- c("score_cards", class(score_card))
  attributes(score_card) <- c(attributes(score_card),
                              as_of = lubridate::as_date(Sys.Date()))
  score_card <- collapse_cards(score_card)
  score_card <- score_card %>%
    select(-.data$quantile, -.data$value)

  score_card <- score_card %>%
                 relocate(attr(err_measures, "name"), .after = last_col())
  return(score_card)
}

#' Create a score card data frame based on covid forecasts
#'
#' Evaluates the performance of a covid forecaster, through the following steps:
#' \enumerate{
#'   \item Takes a prediction card (as created by
#'   [get_predictions()]).
#'   \item Downloads from the COVIDcast API the latest available data to compute
#'   what actually occurred (summing the response over the incidence period).
#'   \item Computes various user-specified error measures.
#' }
#'   Backfill refers to the process by which some data sources go back in time
#' updating previously reported values. Suppose it is September 14 and we are
#' evaluating our predictions for what happened in the previous epiweek
#' (September 6 through 12). Although we may be able to calculate a value for
#' "actual", we might not trust this value since on September 16, backfill may
#' occur changing what is known about the period September 6 through 12. There
#' are two consequences of this phenomenon.  First, running this function on
#' different dates may result in different estimates of the error. Second, we
#' may not trust the evaluations we get that are too recent.  The parameter
#' `backfill_buffer` specifies how long of a buffer period we should
#' enforce.  This will be dependent on the data source and signal and is left
#' to the user to determine.  If backfill is not relevant for the particular
#' signal you are predicting, then you can set `backfill_buffer` to 0.
#'
#' @param predictions_cards tibble of quantile forecasts, which contains at
#'   least `quantile` and `value` columns, as well as any other prediction task
#'   identifiers. Must be of class "predictions_cards". Covid predictions card
#'   may be created by the function [get_predictions()], downloaded with
#'   [get_covidhub_predictions()] or potentially created manually.
#' @param err_measures Named list of one or more functions, where each function
#'   takes a data frame with three columns `quantile`, `value` and `actual`
#'   (i.e., observed) returns a scalar measure of error. Null or an empty list
#'   may be provided if scoring is not desired.
#' @param backfill_buffer How many days until response is deemed trustworthy
#'   enough to be taken as correct? See details for more.
#' @template geo_type-template
#' @return tibble of "score cards". Contains the same information as the
#'   `predictions_cards()` with additional columns for each `err_measure` and
#'   for the truth (named `actual`).
#'
#' @export
evaluate_covid_predictions <- function(predictions_cards,
                                        err_measures = list(wis = weighted_interval_score,
                                                            ae = absolute_error,
                                                            coverage_80 = interval_coverage(coverage = 0.8)),
                                        backfill_buffer = 0,
                                        geo_type = c("county", "hrr", "msa",
                                                     "dma", "state", "hhs",
                                                     "nation")) {
  assert_that("predictions_cards" %in% class(predictions_cards),
              msg = "predictions_cards must be of class `predictions_cards`")
  geo_type <- match.arg(geo_type)
  grp_vars <- c("data_source",
               "signal",
               "incidence_period",
               "geo_value",
               "forecast_date",
               "ahead")
  actual_data <- get_covidcast_data(predictions_cards, backfill_buffer,
                                    geo_type)
  predictions_cards <- left_join(predictions_cards,
                                 actual_data,
                                 by = grp_vars)
  score_card <- evaluate_predictions(predictions_cards,
                                     actual_data,
                                     err_measures)
  score_card <- score_card %>%
    relocate(.data$ahead, .data$geo_value, .data$forecaster,
             .data$forecast_date, .data$data_source,
             .data$signal, .data$target_end_date,
             .data$incidence_period, .data$actual)
  return(score_card)
}

get_covidcast_data <- function(predictions_cards,
                               backfill_buffer, 
                               geo_type) {
  grouped_preds <- predictions_cards %>%
                     group_split(data_source, signal, ahead, incidence_period)
  actuals <- list()
  for (i in seq_len(length(grouped_preds))) {
    preds = grouped_preds[[i]]
    if (nrow(preds) > 0){
      data_source = preds$data_source[1]
      signal = preds$signal[1]
      ahead = preds$ahead[1]
      incidence_period = preds$incidence_period[1]
      message("data_source = ", data_source, 
              " signal = ", signal, 
              " incidence_period = ", incidence_period,
              " ahead = ", ahead)
      
      response = tibble(data_source = data_source,
                        signal = signal)
      forecast_dates <- select(predictions_cards, .data$forecast_date) %>%
        distinct() %>%
        pull()
      geo_values <- select(predictions_cards, .data$geo_value) %>%
        distinct() %>%
        pull()
      # calculate the actual value we're trying to predict:
      target_response <- get_target_response(response,
                                             forecast_dates,
                                             incidence_period,
                                             ahead,
                                             geo_type,
                                             geo_values)
      target_response <- target_response %>%
                          add_column(data_source = data_source,
                                     signal = signal,
                                     ahead = ahead,
                                     incidence_period = incidence_period,
                                     .before = TRUE)
      as_of <- attr(target_response, "as_of")
      . <- "got this idea from https://github.com/tidyverse/magrittr/issues/29"
      if (as_of < max(target_response$end) + backfill_buffer) {
        warning(target_response %>% filter(.data$end == max(.data$end)) %>%
                  stringr::str_glue_data(
                    "Reliable data for evaluation is not yet available for ",
                    "`forecast_date` of {forecast_date} because target period ",
                    "extends to {end} which is too recent to be reliable ",
                    "according to the provided `backfill_buffer` of ",
                    "{backfill_buffer}.", forecast_date = .$forecast_date[1],
                    end = .$end[1], backfill_buffer = backfill_buffer))
      }
      actuals[[i]] <- target_response
    }
  }
  response <- bind_rows(actuals) %>% select(-.data$start, -.data$end)
  return(response)
}

#' Retrieve actual results for provided forecasts
#'
#' @param predictions_cards tibble of predictions
#'   that are all for the same prediction task, meaning they are for the same
#'   response, incidence period and geo_type. Forecasts may be for a
#'   different forecast date or forecaster.
#'   A predictions card may be created by the function
#'   [get_predictions()], downloaded with [get_covidhub_predictions()] or
#'   possibly created manually.
#' @template geo_type-template 
#' @return 'predictions_cards' with an added column `actual`, which represents
#'   the observed value on the date. The `quantile` and `value` columns are
#'   dropped, as the actual value does not depend on the quantile predictions.
#' @export
get_actuals <- function(predictions_cards,
                        geo_type = c("county", "hrr", "msa", "dma", "state",
                                     "hhs", "nation")) {
  geo_type = match.arg(geo_type)
  assert_that("predictions_cards" %in% class(predictions_cards),
              msg = paste("predictions_cards",
                          "must be of class `predictions_cards`."))
  actuals <- evaluate_covid_predictions(predictions_cards,
                                        err_measures = NULL,
                                        geo_type = geo_type)
  return(actuals)
}
