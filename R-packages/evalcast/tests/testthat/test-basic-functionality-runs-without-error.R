library(tibble)
library(lubridate)
library(covidcast)
signals <- tibble(data_source = "jhu-csse",
       signal = c("deaths_incidence_num", "confirmed_incidence_num"),
       start_day = "2020-06-15")
forecast_dates <- ymd(c("2020-07-20", "2020-08-10"))
test_that("get_predictions and evaluate_predictions on baseline_forecaster works", {
  predictions_cards <- get_predictions(baseline_forecaster,
                                       name_of_forecaster = "baseline",
                                       signals = signals,
                                       forecast_dates = forecast_dates,
                                       incidence_period = "epiweek",
                                       ahead = 3,
                                       geo_type = "state")
  expect_equal(length(predictions_cards), 2)
  err_measures <- list(wis = weighted_interval_score,
                       ae = absolute_error,
                       coverage_80 = interval_coverage(alpha = 0.2))
  scorecards <- evaluate_predictions(filter_predictions(predictions_cards,
                                                        ahead = 3),
                                     err_measures,
                                     backfill_buffer = 10)
})
