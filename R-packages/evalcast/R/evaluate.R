#' Create a data frame of score cards
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
#' @param predictions_cards tibble of predictions 
#'   that are all for the same prediction task, meaning they are for the same
#'   response, incidence period, ahead, and geo type. Forecasts may be for a
#'   different forecast date or forecaster.  
#'   A predictions card may be created by the function
#'   [get_predictions()], downloaded with [get_covidhub_predictions()] or
#'   possibly created manually.
#' @param err_measures Named list of one or more functions, where each function
#'   takes a data frame with three columns `quantile`, `value` and `actual`
#'   (i.e., observed) returns a scalar measure of error.
#' @param backfill_buffer How many days until response is deemed trustworthy
#'   enough to be taken as correct? See details for more.
#' @param side_truth you can optionally provide your own truth data (observed).
#'   Adding this column bypasses all checks to get appropriate data as 
#'   reported at the time of the forecast. This column should either be a vector
#'   the same length as `predictions_cards` or a data frame that will be 
#'   joined to `predictions_cards`. `grp_vars` and `avg_vars` must be present
#'   and joining will be performed on all available columns. If a data frame,
#'   the observed data should be named `actual` 
#' @param grp_vars character vector of named columns in the predictions_cards
#'  such that the combination gives a unique (quantile) prediction. Ignored if
#'  `predictions_cards` is of the type returned by [get_covidhub_predictions()]
#'  or [get_predictions()] (class is `predictions_cards`)
#' 
#' @return tibble of "score cards" with additional columns for each err_measure
#'
#' @export
evaluate_predictions <- function(
  predictions_cards,
  err_measures = list(wis = weighted_interval_score,
                      ae = absolute_error,
                      coverage_80 = interval_coverage(alpha = 0.2)),
  backfill_buffer = 10,
  side_truth = NULL,
  grp_vars = c("forecaster", "forecast_date", "ahead", "location")) {
  
  
  assert_that(class(predictions_cards)[1] == "predictions_cards" ||
                !is.null(side_truth),
              msg = paste("In evaluate_predictions: either predictions_cards",
                          "must be of class `predictions_cards` so that",
                          "appropriate responses can be downloaded from",
                          "covidcast, or you must provide your own",
                          "ground truth."))
  erm <- function(x){
    out <- double(length(err_measures))
    for (i in seq_along(err_measures)) {
      out[i] <- err_measures[[i]](x$quantile, x$value, x$actual)
    }
    names(out) <- names(err_measures)
    bind_rows(out)
  }
  
  ## Computations if actuals are provided by the user
  if (!is.null(side_truth)) {
    if (is.data.frame(side_truth)) {
      predictions_cards <- left_join(predictions_cards, side_truth)
      assert_that("actual" %in% names(predictions_cards),
                  msg = paste("When providing your own truth data as a data",
                              "frame, one column must be named `actual`"))
    } else {
      predictions_cards <- bind_cols(predictions_cards, actual = side_truth)
    }
    if (class(predictions_cards)[1] == "predictions_cards") {
      grp_vars = c("forecaster", "forecast_date", "ahead", "location")
    }
    score_card <- predictions_cards %>% group_by(across(all_of(grp_vars)))
    sc_keys <- score_card %>% group_keys()
    score_card <- score_card %>% group_split() %>% lapply(erm) %>% bind_rows()
    score_card <- bind_cols(score_card, sc_keys)
    score_card <- inner_join(score_card, predictions_cards, by=grp_vars)
    return(score_card)
  }
  
  
  ## more heavy lifting if we are grabbing data from covidcast
  unique_ahead <- select(predictions_cards, .data$ahead) %>% 
    distinct() %>% pull()
  scorecards <- list()
  for (iter in seq_along(unique_ahead)) {
    message("ahead = ", unique_ahead[iter])
    scorecards[[iter]] <- evaluate_predictions_single_ahead(
      filter(predictions_cards, .data$ahead == !!iter) ,
      backfill_buffer = backfill_buffer)
  }
  scorecards <- bind_rows(scorecards)
  class(scorecards) <- c("score_card", class(scorecards))
}

#' Create score cards for one ahead
#' 
#' @param predictions_cards tibble of predictions 
#'   that are all for the same prediction task, meaning they are for the same
#'   response, incidence period, ahead, and geo type. Forecasts may be for a
#'   different forecast date or forecaster.  
#'   A predictions card may be created by the function
#'   [get_predictions()], downloaded with [get_covidhub_predictions()] or
#'   possibly created manually.
#' @param backfill_buffer How many days until response is deemed trustworthy
#'   enough to be taken as correct? See details for more.
evaluate_predictions_single_ahead <- function(predictions_cards,
                                              backfill_buffer) {
  
  response <- predictions_cards %>% 
    select(.data$data_source, .data$signal) %>% distinct()
  assert_that(nrow(response) == 1,
              msg="All predictions cards should have the same response.")
  incidence_period <- unique_for_ahead(predictions_cards, "incidence_period")
  geo_type <- unique_for_ahead(
    select(predictions_cards, .data$location, .data$ahead) %>%
      mutate(geo_type = nchar(.data$location)), 
    "geo_type")
  geo_type <- ifelse(geo_type == 2L, "state", "county")
  forecast_dates <- select(predictions_cards, forecast_date) %>%
    distinct() %>% pull()
  ahead <- predictions_cards$ahead[1]
  locations <- select(predictions_cards, location) %>%
    distinct() %>% pull()
  
  # get information from predictions cards' attributes and check:
  # att <- get_and_check_pc_attributes(predictions_cards)
  # calculate the actual value we're trying to predict:
  target_response <- get_target_response(response,
                                         forecast_dates,
                                         incidence_period,
                                         ahead,
                                         geo_type,
                                         locations)
  as_of <- attr(target_response, "as_of")
  . <- "got this idea from https://github.com/tidyverse/magrittr/issues/29"
  if (as_of < max(target_response$end) + backfill_buffer) {
    warning(target_response %>% filter(.data$end == max(.data$end)) %>%
              stringr::str_glue_data(
              "Reliable data for evaluation is not yet available for ",
              "`forecast_date` of {forecast_date} because target period ",
              "extends to {end} which is too recent to be reliable ",
              "according to the provided `backfill_buffer` of ",
              "{backfill_buffer}.", forecast_date=.$forecast_date[1],
              end=.$end[1], backfill_buffer=backfill_buffer))
  }
  # combine all predictions cards into a single data frame with an additional
  # column called forecast_date:
  
  # join together the data frames target_response and predicted:
  score_card <- target_response %>%
    inner_join(predictions_cards, by = c("location", "forecast_date")) %>%
    select(.data$location,
           .data$forecast_date,
           .data$actual,
           .data$quantile,
           .data$value,
           .data$forecaster)
  # compute the error
  
  score_card <- score_card %>%
    group_by(.data$forecaster, .data$location, .data$forecast_date)
  sc_keys <- score_card %>% group_keys()
  score_card <- score_card %>% group_split() %>% lapply(erm) %>% bind_rows()
  score_card <- bind_cols(score_card, sc_keys)
  
  score_card <- left_join(score_card, target_response, 
                          by=c("location", "forecast_date")) %>%
    select(-.data$start, -.data$end)
  score_card <- inner_join(score_card, predictions_cards,
                           by=c("forecaster", "location", "forecast_date"))
  score_card <- score_card %>% relocate(
    .data$ahead, .data$location, .data$quantile, .data$value, .data$forecaster,
    .data$forecast_date, .data$data_source, .data$signal, .data$target_end_date,
    .data$incidence_period, .data$actual)
    
  attributes(score_card) <- c(attributes(score_card), as_of = as_of)
  class(score_card) <- c("score_card", class(score_card))
  return(score_card)
}

#' Check that a forecaster's output is valid
#' @param pred_card Prediction card, in the form created by [get_predictions()].
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
