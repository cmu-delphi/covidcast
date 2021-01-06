#' Create a data frame of score cards
#'
#' Performs backtesting, through the following steps:
#' \enumerate{
#'   \item Takes a list of predictions cards (as created by
#'   [get_predictions()]). 
#'   \item Downloads from the COVIDcast API the latest available data to compute
#'   what actually occurred (summing the response over the incidence period).
#'   \item Computes various user-specified error measures.
#' }
#' The result is a data frame of "score cards", where each row corresponds to
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
#'   response, incidence period,and geo type. Forecasts may be for a
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
#'   joined to `predictions_cards` by all available columns. If a data frame,
#'   the observed data should be named `actual`
#' @param grp_vars character vector of named columns in the predictions_cards
#'  such that the combination gives a unique (quantile) prediction. Ignored if
#'  `predictions_cards` is of the type returned by [get_covidhub_predictions()]
#'  or [get_predictions()] (class is `predictions_cards`)
#'
#' @return tibble of "score cards". Contains the same information as the
#'   `predictions_cards()` with additional columns for each err_measure and
#'   for the truth (named `actual`). All the columns may be necessary for
#'   some types of plotting, but it may be useful to shrink this down
#'
#' @export
evaluate_predictions <- function(
  predictions_cards,
  err_measures = list(wis = weighted_interval_score,
                      ae = absolute_error,
                      coverage_80 = interval_coverage(coverage = 0.8)),
  backfill_buffer = 10,
  side_truth = NULL,
  grp_vars = c("forecaster", "forecast_date", "ahead", "geo_value")) {

  assert_that("predictions_cards" %in% class(predictions_cards) ||
                !is.null(side_truth),
              msg = paste("In evaluate_predictions: either predictions_cards",
                          "must be of class `predictions_cards` so that",
                          "appropriate responses can be downloaded from",
                          "covidcast, or you must provide your own",
                          "ground truth."))

  # Construct predictions joined with real data
  if (is.null(side_truth)) {
    actual_data <- get_covidcast_data(predictions_cards, backfill_buffer)
    predictions_cards <- left_join(predictions_cards,
                                   actual_data,
                                   by = c("geo_value",
                                          "forecast_date",
                                          "ahead"))
  } else {
    # Computations if actuals are provided by the user
    if (is.data.frame(side_truth)) {
      actual_data <- side_truth
      assert_that("actual" %in% names(side_truth),
                  msg = paste("When providing your own truth data as a data",
                              "frame, one column must be named `actual`"))
      predictions_cards <- left_join(predictions_cards, side_truth)
    } else {
      predictions_cards <- bind_cols(predictions_cards, actual = side_truth)
    }
  }

  score_card <- predictions_cards %>% group_by(across(all_of(grp_vars)))
  sc_keys <- score_card %>% group_keys()
  score_card <- score_card %>%
    group_split() %>%
    lapply(erm, err_measures = err_measures) %>%
    bind_rows() %>%
    bind_cols(sc_keys) %>%
    inner_join(predictions_cards, by = grp_vars)
  class(score_card) <- c("score_cards", class(score_card))
  attributes(score_card) <- c(attributes(score_card),
                              as_of = lubridate::as_date(Sys.Date()))
  score_card <- collapse_cards(score_card)
  score_card <- score_card %>%
                 select(-c(quantile, value))
  if (is.null(side_truth)) {
    score_card <- score_card %>%
                    relocate(.data$ahead, .data$geo_value, .data$forecaster,
                             .data$forecast_date, .data$data_source,
                             .data$signal, .data$target_end_date,
                             .data$incidence_period, .data$actual)
  }
  score_card <- score_card %>%
                 relocate(attr(err_measures, "name"), .after = last_col())
  return(score_card)
}

get_covidcast_data <- function(predictions_cards,
                               backfill_buffer) {
  response <- predictions_cards %>%
                select(.data$data_source, .data$signal) %>%
                distinct()
  assert_that(nrow(response) == 1,
              msg = "All predictions cards should have the same response.")
  incidence_period <- unique(predictions_cards$incidence_period)
  assert_that(length(incidence_period) == 1,
              msg = "All predictions cards should have the same incidence
                     period.")
  geo_type_len <- predictions_cards %>%
    mutate(type_len = nchar(geo_value)) %>%
    distinct(type_len)
  assert_that(nrow(geo_type_len) == 1,
              msg = "All predictions cards should have the same geo_type.")
  geo_type <- ifelse(geo_type_len$type_len == 2L, "state", "county")

  unique_ahead <- select(predictions_cards, .data$ahead) %>%
                    distinct() %>%
                    pull()
  actuals <- list()
  for (i in seq_along(unique_ahead)) {
    message("ahead = ", unique_ahead[i])
    ahead <- unique_ahead[i]
    predictions_cards_ahead <- filter(predictions_cards, .data$ahead == ahead)
    forecast_dates <- select(predictions_cards_ahead, .data$forecast_date) %>%
                        distinct() %>%
                        pull()
    geo_values <- select(predictions_cards_ahead, .data$geo_value) %>%
                    distinct() %>%
                    pull()

    # calculate the actual value we're trying to predict:
    target_response <- get_target_response(response,
                                           forecast_dates,
                                           incidence_period,
                                           ahead,
                                           geo_type,
                                           geo_values)
    target_response$ahead <- ahead
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
  response <- bind_rows(actuals) %>% select(-c(start, end))
  return(response)
}

#' @importFrom rlang :=
empty_score_card <- function(pcards, err_measures){
  # Creates a score card with nothing in it in case there's no available data
  # to evaluate some particular forecast task. Avoids errors later on.
  out <- pcards[0,]
  out$actual <- double(0)
  for(iter in names(err_measures)){
    out <- bind_cols(out, tibble(!!iter := double(0)))
  }
}

erm <- function(x, err_measures){
  # just binds up any error measure functions for an lapply
  # I'm sure there's a better way to do this, but I couldn't think of one
  out <- double(length(err_measures))
  for (i in seq_along(err_measures)) {
    out[i] <- err_measures[[i]](x$quantile, x$value, x$actual)
  }
  names(out) <- names(err_measures)
  bind_rows(out)
}
