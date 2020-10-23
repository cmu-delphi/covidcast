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
#' @param cards a list of predictions cards or a list of scorecards
#' @param attribute name of attribute
#' @importFrom assertthat assert_that
unique_attr <- function(cards, attribute) {
  attr_list <- all_attr(cards, attribute)
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
#' @param cards a list of predictions cards or a list of scorecards
#' @param attribute name of attribute
all_attr <- function(cards, attribute) {
  return(cards %>% map(~ attr(.x, attribute)))
}


#' Remove locations that are not in all cards
#'
#' @param cards a list of predictions cards or a list of scorecards
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









