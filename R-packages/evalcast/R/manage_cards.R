
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

unique_for_ahead <- function(cards, attribute) {
  att <- select(cards, !!attribute) %>% distinct()
  ahead <- cards$ahead[1]
  assert_that(nrow(att) == 1,
              msg=sprintf("These cards do not all have the same %s for ahead %i.",
                          attribute, ahead))
  invisible(pull(att))
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



#' Print a single prediction card.
#' @param card Prediction card.
#' @export
print.prediction_card <- function(card, ...) {
  card_attr <- attributes(card)
  cat("Attributes:\n")
  cat("  Ahead:", card_attr$ahead, "\n")
  cat("  Data source:", card_attr$signals$data_source, "\n")
  cat("  Forecast date:", as.character(card_attr$forecast_date), "\n")
  cat("  Geo type:", card_attr$geo_type, "\n")
  cat("  Geo values:", card_attr$geo_values, "\n")
  cat("  Incidence period:", card_attr$incidence_period, "\n")
  cat("  Name of forecaster:", card_attr$name_of_forecaster, "\n")
  cat("  Signal:", card_attr$signals$signal, "\n")
  print(as_tibble(card), ...)
}


#' Print a single score card.
#' @param card Score card.
#' @export
print.score_card <- function(card, ...) {
  card_attr <- attributes(card)
  cat("Attributes:\n")
  cat("  Ahead:", card_attr$ahead, "\n")
  cat("  As of:", as.character(card_attr$as_of), "\n")
  cat("  Backfill buffer:", card_attr$backfill_buffer, "\n")
  cat("  Data source:", card_attr$response$data_source, "\n")
  cat("  Geo type:", card_attr$geo_type, "\n")
  cat("  Incidence period:", card_attr$incidence_period, "\n")
  cat("  Name of forecaster:", card_attr$name_of_forecaster, "\n")
  cat("  Signal:", card_attr$response$signal, "\n")
  print(as_tibble(card), ...)
}
