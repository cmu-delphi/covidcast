


#' Convert forecasts into a format compatible with all `predictions_cards`
#'
#' Several methods are provided to convert external forecasts given as a
#' data.frame into `predictions_cards` objects compatible with the various
#' evaluation and plotting functions. 
#' 
#'
#' @param x Object to be converted. See Methods section below for details on
#'   formatting of each input type.
#' @param forecaster The name of the forecaster that produced this data.
#' @template forecast_date-template
#' @template incidence_period-template
#' @param data_source The name of the data_source for the forecast target.
#' @param signal The name of the signal for the forecast target.
#' @param target_end_date The date the forecast target is observed (combines
#'   `forecast_date`, `ahead`, and `incidence_period`). This is determined 
#'   automatically if `NULL` (the default).
#' @param ... Additional arguments passed to methods.
#'
#' @template predictions_cards-template
#' @export
as.predictions_cards <- function(x, ...) {
  UseMethod("as.predictions_cards")
}

#' @method as.predictions_cards predictions_cards
#' @describeIn as.predictions_cards Simply returns the `predictions_cards` object
#'   unchanged.
#' @export
as.predictions_cards.predictions_cards <- function(x, ...) {
  return(x)
}

#' @method as.predictions_cards data.frame
#' @describeIn as.predictions_cards The input data frame `x` must contain the
#'   columns `ahead`, `geo_value`, `quantile` and `value`. Addional columns
#'   `forecaster` `forecast_date`, `incidence_period`, `data_source`, 
#'   `signal`, and `target_end_date` will be created if missing.
#'    Other columns will be preserved as-is.
#' @export
as.predictions_cards.data.frame <- function(
  x, 
  forecaster = "forecaster",
  forecast_date = lubridate::today(),
  incidence_period = c("epiweek","day"),
  data_source = "data_source",
  signal = "signal",
  target_end_date = NULL
) {
  provided_cols <- names(x)
  req_cols <- c("ahead", "geo_value", "quantile", "value")
  out_cols <- c(req_cols, "forecaster", "forecast_date", "signal", 
                "data_source", "incidence_period", "target_end_date")
  assert_that(all(req_cols %in% provided_cols),
              msg = cat("If `x` is a data.frame, it must contain the columns",
                          req_cols))
  missing_cols <- setdiff(out_cols, provided_cols)
  if ("incidence_period" %in% missing_cols) {
    incidence_period <- match.arg(incidence_period)
    x$incidence_period <- incidence_period
  } else {
    assert_that(length(unique(x$incidence_period)) == 1,
               x$incidence_period[1] %in% c("epiweek", "day"),
               msg = paste("Predictions must be for the same incidence period.",
                           "Either `epiweek` or `day`."))
  }
  if ("forecast_date" %in% missing_cols) {
    x$forecast_date <- forecast_date
  }
  if ("target_end_date" %in% missing_cols) {
    if (is.null(target_end_date)) {
      x$target_end_date <- get_target_period(
        x$forecast_date, x$incidence_period[1], x$ahead)$end
    } else {
      x$target_end_date <- target_end_date
    }
  }
  if ("forecaster" %in% missing_cols) x$forecaster <- forecaster
  if ("data_source" %in% missing_cols) x$data_source <- data_source
  if ("signal" %in% missing_cols) x$signal <- signal
  
  return(x)
}