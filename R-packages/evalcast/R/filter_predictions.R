#' Filter a list of predictions cards based on attributes
#'
#' @param predictions_cards a list of predictions cards
#' @export
filter_predictions <- function(predictions_cards,
                               name_of_forecaster = NULL,
                               response_data_source = NULL,
                               response_signal = NULL,
                               forecast_date = NULL,
                               incidence_period = NULL,
                               ahead = NULL,
                               geo_type = NULL) {
  include <- rep(TRUE, length(predictions_cards))
  if (!is.null(forecast_date)) {
    # using do.call in the next line to keep these of class Date:
    # https://stackoverflow.com/questions/15659783/why-does-unlist-kill-dates-in-r
    .forecast_date <- do.call("c",
                              predictions_cards %>%
                                map(~ attr(.x, "forecast_date")))
    include[!(.forecast_date %in% forecast_date)] <- FALSE
  }
  if (!is.null(name_of_forecaster)) {
    .name_of_forecaster <- predictions_cards %>%
      map_chr(~ attr(.x, "name_of_forecaster"))
    include[!(.name_of_forecaster %in% name_of_forecaster)] <- FALSE
  }
  if (!is.null(ahead)) {
    .ahead <- predictions_cards %>% map_dbl(~ attr(.x, "ahead"))
    include[!(.ahead %in% ahead)] <- FALSE
  }
  if (!is.null(incidence_period)) {
    .incidence_period <- predictions_cards %>%
      map_chr(~ attr(.x, "incidence_period"))
    include[!(.incidence_period %in% incidence_period)] <- FALSE
  }
  if (!is.null(geo_type)) {
    .geo_type <- predictions_cards %>% map_chr(~ attr(.x, "geo_type"))
    include[!(.geo_type %in% geo_type)] <- FALSE
  }
  .signals <- predictions_cards %>% map_dfr(~ attr(.x, "signals")[1, ])
  if (!is.null(response_data_source)) {
    dsource <- .signals$data_source %in% response_data_source
    include[!dsource] <- FALSE
  }
  if (!is.null(response_signal)) {
    sig <- .signals$signal %in% response_signal
    include[!sig] <- FALSE
  }
  predictions_cards[include]
}
