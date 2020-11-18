#' @importFrom assertthat assert_that
get_and_check_pc_attributes <- function(predictions_cards) {
  # using do.call in the next line to keep these of class Date:
  # https://stackoverflow.com/questions/15659783/why-does-unlist-kill-dates-in-r
  forecast_dates <- do.call("c",
                            predictions_cards %>%
                              map(~ attr(.x, "forecast_date")))
  assert_that(length(unique(forecast_dates)) == length(forecast_dates),
              msg="Each predictions card must have a distinct forecast date.")
  list(
    forecast_dates = forecast_dates,
    name_of_forecaster = unique_attr(predictions_cards, "name_of_forecaster"),
    signals = unique_attr(predictions_cards, "signals"),
    ahead = as.numeric(unique_attr(predictions_cards, "ahead")),
    incidence_period = unique_attr(predictions_cards, "incidence_period"),
    geo_type = unique_attr(predictions_cards, "geo_type")
  )
}

#' Return unique value of attribute or throw error
#'
#' If TRUE, returns the unique value; if FALSE, throws an error.
#' @param cards List of predictions cards or a list of score cards.
#' @param attribute Name of attribute.
#' @importFrom assertthat assert_that
unique_attr <- function(cards, attribute) {
  attr_list <- all_attr(cards, attribute)
  if (attribute == "signals")
    attr_list <- attr_list %>%
      map(~ .x[1, names(.x) != "start_day"]) # RJT: I added this part. I don't
  # think start dates need to match here. Now that I'm allowing start_day to be 
  # a function, it can depend on the forecast_date, so the next assertion would
  # (unintentionally) fail in that case. Note: this if statement on "signals" is
  # a bit of a hack and there might be a cleaner way to do it but it works for
  # now.  
  assert_that(length(unique(attr_list)) <= 1,
              msg=sprintf("These cards do not all have the same %s.",
                          attribute))
  return(attr_list[[1]])
}

#' Return list of attributes
#'
#' Given a list of cards, returns a list of the same length giving the values
#' of that attribute across all cards.
#'
#' @param cards List of predictions cards or a list of score cards.
#' @param attribute Name of attribute.
all_attr <- function(cards, attribute) {
  return(cards %>% map(~ attr(.x, attribute)))
}

#' Remove locations that are not in all cards
#'
#' @param cards List of predictions cards or a list of score cards.
intersect_locations <- function(cards) {
  locations_list <- cards %>% map(~ unique(.x$location))
  intersected_locations <- Reduce(intersect, locations_list)
  for (i in seq_along(locations_list)) {
    removed <- setdiff(locations_list[[i]], intersected_locations)
    if (length(removed) > 0)
      message(sprintf("Removed the following locations for cards[[%s]]: %s",
                      i,
                      paste(removed, collapse = ", ")))
  }
  cards %>% map(~ .x %>% filter(location %in% intersected_locations))
}

#' Aggregate cards from a list into a single unnested data frame
#'
#' @param list_of_cards List of prediction or evaluation cards.  See documentation for the outputs
#'   of `evalcast::get_predictions()` and `evalcast::evaluate_predictions()` for the required
#'   format.
#' @return Data frame such that:
#'   \item There is a one-to-one correspondence between rows of the output and the rows of
#'     `card$forecast_distribution` across each `card` in `list_of_cards`.  That is to say, if each
#'     card has 7 rows in its `forecast_distribution` and there are 3 such cards in
#'     `list_of_cards`, the output will have 21 rows.
#'   \item The columns of the output correspond to:
#'     \itemize{
#'       \item Columns of `card$forecast_distribution`
#'       \item Columns of `card`
#'       \item Attributes of `card`
#'     }
#' @export
aggregate_cards <- function(list_of_cards) {
  list_of_cards %>% purrr::map_dfr(unpack_single_card)
}

#' Unpack a single prediction or evaluation card into an unnested tibble
#'
#' This is a generic method for dispatching to specific calls for evaluation and prediction cards.
#' @param card Evaluation or prediction card.
#' @return See `aggregate_cards`.
unpack_single_card <- function(card){
  UseMethod("unpack_single_card", card)
}

#' Unpack a single prediction card into an unnested tibble
unpack_single_card.prediction_card <- function(card) {
  card_attr <- attributes(card)
  card %>%
  tidyr::unnest(.data$forecast_distribution) %>%
  dplyr::mutate(
    ahead = card_attr$ahead,
    data_source = card_attr$signals$data_source,
    forecast_date = card_attr$forecast_date,
    geo_type = card_attr$geo_type,
    geo_values = card_attr$geo_values,
    incidence_period = card_attr$incidence_period,
    name_of_forecaster = card_attr$name_of_forecaster,
    signal = card_attr$signals$signal
  )
}

#' Unpack a single evaluation card into an unnested tibble
unpack_single_card.evaluation_card <- function(card) {
  card_attr <- attributes(card)
  card %>%
  tidyr::unnest(.data$forecast_distribution) %>%
  dplyr::mutate(
    ahead = card_attr$ahead,
    as_of = card_attr$as_of,
    backfill_buffer = card_attr$backfill_buffer,
    data_source = card_attr$response$data_source,
    geo_type = card_attr$geo_type,
    incidence_period = card_attr$incidence_period,
    name_of_forecaster = card_attr$name_of_forecaster,
    signal = card_attr$response$signal
  )
}
