#' Remove all quantile forecasts
#'
#' @param cards either predictions_cards or scorecards
#'
#' @return cards of the same class but with only one row for each
#'   geo_value/forecast_date/ahead/forecaster (the point estimate)
#'
#' @importFrom tidyr pivot_wider
#'
#' @export
collapse_cards <- function(cards){
  cls <- class(cards)[1]
  assert_that(cls %in% c("predictions_cards", "score_cards"),
              msg=paste("This function is only appropriate for",
                        "predictions_cards or score_cards classes."))
  cards <- cards %>%
    filter(abs(.data$quantile - 0.5) < 1e-8 | is.na(.data$quantile)) %>%
    mutate(quantile = ifelse(is.na(.data$quantile), "p","m"))
  if (n_distinct(cards$quantile) == 1) {
    cards <- cards %>%
      mutate(quantile = ifelse(.data$quantile == "p", NA, 0.5))
  } else {
    cards <- cards %>%
      pivot_wider(names_from = .data$quantile, values_from = .data$value) %>%
      mutate(quantile = ifelse(is.na(.data$p), 0.5, NA),
             value = ifelse(is.na(.data$p), .data$m, .data$p)) %>%
      select(-.data$p, -.data$m)
  }
  if ("geo_value" %in% colnames(cards)) {
    cards <- cards %>%
      relocate(.data$quantile:.data$value, .after = .data$geo_value)
  }
  class(cards) = c(cls, class(cards))
  cards
}
