library(tibble)
library(lubridate)
signals <- tibble(data_source = "jhu-csse",
                  signal = c("deaths_incidence_num", "confirmed_incidence_num"),
                  start_day = "2020-06-15")
forecast_dates <- ymd(c("2020-07-20", "2020-08-10"))
common_objects <- new.env(parent = emptyenv())

test_that("get_predictions and evaluate_predictions on baseline_forecaster works", {
  # in addition to making sure these functions run, we will also use
  # the generated pc and sc in other tests.
  pc <- get_predictions(baseline_forecaster,
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
  sc <- evaluate_predictions(filter_predictions(pc, ahead = 3),
                             err_measures,
                             backfill_buffer = 10)
  # let's save these for other tests:
  common_objects$pc <- pc
  common_objects$sc <- sc
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
  both <- pc2[[1]] %>% left_join(common_objects$pc[[1]], by = "location")
  expect_identical(both$forecast_distribution.x, both$forecast_distribution.y)
})

test_that("backfill_buffer works", {
  # how long has it been since the last target period ends in the scorecards?
  days_elapsed <- common_objects$sc %>%
    map_dbl(~ today() - max(.x$end)) %>% 
    unlist()
  # too soon should give error:
  expect_error(evaluate_predictions(common_objects$pc,
                                    backfill_buffer = days_elapsed),
               "backfill_buffer")
  # waited long enough should have no error:
  evaluate_predictions(common_objects$pc, backfill_buffer = days_elapsed - 1)
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
  baseline_pred <- common_objects$pc[[1]]$forecast_distribution[[1]]$quantiles
  with_args_pred <- pc2[[1]]$forecast_distribution[[1]]$quantiles
  expect_equal(baseline_pred + 10, with_args_pred)
})

test_that("start_day within signals works", {
  # if signals does not have a start_day, does all still run?
  signals_no_start_day <- tibble(data_source = "jhu-csse",
                                 signal = c("deaths_incidence_num",
                                            "confirmed_incidence_num"))
  forecast_dates <- ymd(c("2020-07-20", "2020-07-27"))
  pc_no_start_day <- get_predictions(baseline_forecaster,
                                     name_of_forecaster = "baseline",
                                     signals = signals_no_start_day,
                                     forecast_dates = forecast_dates,
                                     incidence_period = "epiweek",
                                     ahead = 3,
                                     geo_type = "state")
  sc_no_start_day <- evaluate_predictions(pc_no_start_day)
  
  # if signals has a start_day, does all still run?
  signals <- signals_no_start_day %>% mutate(start_day = "2020-06-15")
  forecast_date <- ymd("2020-07-20")
  pc1 <- get_predictions(baseline_forecaster,
                         name_of_forecaster = "baseline",
                         signals = signals,
                         forecast_dates = forecast_date,
                         incidence_period = "epiweek",
                         ahead = 3,
                         geo_type = "state")
  # the median should be the same for both for the
  # baseline_forecaster in the symmetrized case:
  med_pred_no_start_day <- pc_no_start_day[[1]]$forecast_distribution %>%
    map_dbl(~ .x %>% filter(probs == 0.5) %>% pull(quantiles))
  med_pred1 <- pc1[[1]]$forecast_distribution %>%
    map_dbl(~ .x %>% filter(probs == 0.5) %>% pull(quantiles))
  expect_equal(med_pred1, med_pred_no_start_day)

  # if signals has a start_day that is a function, does all still run?
  signals_f <- signals_no_start_day %>% 
    mutate(start_day = list(function(forecast_date) forecast_date - 40))
  pc_f <- get_predictions(baseline_forecaster,
                          name_of_forecaster = "baseline",
                          signals = signals_f,
                          forecast_dates = forecast_date,
                          incidence_period = "epiweek",
                          ahead = 3,
                          geo_type = "state")
  med_pred_f <- pc_f[[1]]$forecast_distribution %>%
    map_dbl(~ .x %>% filter(probs == 0.5) %>% pull(quantiles))
  expect_equal(med_pred1, med_pred_f)
  
  # what about a mix of start_day types? (currently fails)
  signals_mix <- signals_f
  signals_mix$start_day[[1]] <- "2020-06-15"
  pc_mix <- get_predictions(baseline_forecaster,
                            name_of_forecaster = "baseline",
                            signals = signals_mix,
                            forecast_dates = forecast_date,
                            incidence_period = "epiweek",
                            ahead = 3,
                            geo_type = "state")
  med_pred_mix <- pc_mix[[1]]$forecast_distribution %>%
    map_dbl(~ .x %>% filter(probs == 0.5) %>% pull(quantiles))
  expect_equal(med_pred1, med_pred_mix)
})
