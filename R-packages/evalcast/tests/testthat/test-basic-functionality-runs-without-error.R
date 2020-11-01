library(tibble)
library(lubridate)
signals <- tibble(data_source = "jhu-csse",
                  signal = c("deaths_incidence_num", "confirmed_incidence_num"),
                  start_day = "2020-06-15")
forecast_dates <- ymd(c("2020-07-20", "2020-08-10"))

test_that("get_predictions and evaluate_predictions on baseline_forecaster works", {
  # in addition to making sure these functions run, we will also use
  # the generated pc and sc in other tests.
  pc <<- get_predictions(baseline_forecaster,
                         name_of_forecaster = "baseline",
                         signals = signals,
                         forecast_dates = forecast_dates,
                         incidence_period = "epiweek",
                         ahead = 3,
                         geo_type = "state")
  expect_equal(length(pc), 2)
  err_measures <- list(wis = weighted_interval_score,
                       ae = absolute_error,
                       coverage_80 = interval_coverage(alpha = 0.2))
  sc <<- evaluate_predictions(filter_predictions(pc, ahead = 3),
                              err_measures,
                              backfill_buffer = 10)
})

test_that("geo_values argument to get_predictions works", {
  # if we run get_predictions on a subset of locations, the predictions
  # of baseline_forecaster should not change on those locations
  geo_values <- c("ca", "pa", "al")
  pc2 <- get_predictions(baseline_forecaster,
                         name_of_forecaster = "baseline",
                         signals = signals,
                         forecast_dates = forecast_dates,
                         incidence_period = "epiweek",
                         ahead = 3,
                         geo_type = "state",
                         geo_values = geo_values)
  both <- pc2[[1]] %>% left_join(pc[[1]], by = "location")
  expect_identical(both$forecast_distribution.x, both$forecast_distribution.y)
})

test_that("backfill_buffer works", {
  # how long has it been since the last target period ends in the scorecards?
  days_elapsed <- sc %>% map_dbl(~ today() - max(.x$end)) %>% unlist()
  # too soon should give error:
  expect_error(evaluate_predictions(pc, backfill_buffer = days_elapsed),
               "backfill_buffer")
  # waited long enough should have no error:
  evaluate_predictions(pc, backfill_buffer = days_elapsed - 1)
})

test_that("get_predictions works when forecaster has additional arguments", {
  forecaster_with_args <- function(df,
                                   forecast_date,
                                   signals,
                                   incidence_period,
                                   ahead,
                                   geo_type,
                                   symmetrize = TRUE,
                                   arg1,
                                   arg2) {
    # this forecaster adds arg1 * arg2 to the predictions of the baseline 
    # forecaster
    out <- baseline_forecaster(df = df,
                               forecast_date = forecast_date,
                               signals = signals,
                               incidence_period = incidence_period,
                               ahead = ahead,
                               geo_type = geo_type,
                               symmetrize = symmetrize)
    out$quantiles <- out$quantiles + arg1 * arg2
    out
  }
  pc2 <- get_predictions(forecaster_with_args,
                         name_of_forecaster = "forecaster_with_args",
                         signals = signals,
                         forecast_dates = forecast_dates,
                         incidence_period = "epiweek",
                         ahead = 3,
                         geo_type = "state",
                         arg1 = 2,
                         arg2 = 5)
  baseline_pred <- pc[[1]]$forecast_distribution[[1]]$quantiles
  with_args_pred <- pc2[[1]]$forecast_distribution[[1]]$quantiles
  expect_equal(baseline_pred + 10, with_args_pred)
})