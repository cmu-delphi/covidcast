library(mockery)

create_fake_downloaded_signal <- function(geo_value) {
  list(tibble(data_source = "jhu-csse",
              signal =  c("deaths_incidence_num", "confirmed_incidence_num"),
              geo_value = geo_value,
              time_value = as.Date("2020-01-01"),
              issue = as.Date("2020-01-02"),
              lag = 1L,
              value = c(1, 2),
              stderr = c(0.1, 0.2),
              sample_size = c(2, 2)
        )
  )  
}

create_fake_forecast <- function(ahead, geo_value) {
  quantiles <- c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)
  ahead_list <- c(ahead)
  tibble(
    ahead = rep(ahead_list, each=length(quantiles)),
    geo_value = geo_value,
    quantile = rep(quantiles, length(ahead_list)),
    value = rep(100 * quantiles, length(ahead_list))
  )
}

test_that("get_predictions and evaluate_predictions on baseline_forecaster works", {
  fake_downloaded_signals <- c(create_fake_downloaded_signal("in"),
                               create_fake_downloaded_signal("nc"))
  mock_download_signals <- do.call(mock, c(fake_downloaded_signals))
  with_mock(download_signals = mock_download_signals, {
    mock_forecaster <- mock(create_fake_forecast(3, "in"),
                            create_fake_forecast(3, "nc"))

    signals <- tibble(data_source = "jhu-csse",
                      signal = c("deaths_incidence_num", "confirmed_incidence_num"),
                      start_day = "2020-01-01")
    forecast_dates <- as.Date(c("2020-01-01", "2020-01-02"))

    pcard <- get_predictions(mock_forecaster,
                          name_of_forecaster = "fake",
                          signals = signals,
                          forecast_dates = forecast_dates,
                          incidence_period = "epiweek",
                          ahead = 3,
                          geo_type = "state")

    expect_called(mock_download_signals, 2)
    expect_called(mock_forecaster, 2)
    expect_equal(mock_args(mock_forecaster),
                 list(list(fake_downloaded_signals[[1]],
                           as.Date("2020-01-01"),
                           signals,
                           "epiweek",
                           3,
                           "state"),
                      list(fake_downloaded_signals[[2]],
                           as.Date("2020-01-02"),
                           signals,
                           "epiweek",
                           3,
                           "state")))
    expect_equal(colnames(pcard),
      c("ahead", "geo_value", "quantile", "value", "forecaster", "forecast_date", "data_source",
        "signal", "target_end_date", "incidence_period"))
    n <- 46
    expect_equal(nrow(pcard), n)
    expect_equal(pcard$ahead, rep(3, n))
    expect_equal(pcard$geo_value, rep(c("in", "nc"), each=n/2))
    expect_equal(pcard$forecaster, rep("fake", n))
    expect_equal(pcard$forecast_date, rep(forecast_dates, each=n/2))
    expect_equal(pcard$data_source, rep("jhu-csse", n))
    expect_equal(pcard$signal, rep("deaths_incidence_num", n))
    expect_equal(pcard$target_end_date, rep(as.Date("2020-01-25"), n))
    expect_equal(pcard$incidence_period, rep("epiweek", n))
  })
})

test_that("geo_values argument to get_predictions works", {
  # if we run get_predictions on a subset of locations, the predictions
  # of baseline_forecaster should not change on those locations
  skip("To be revised...")
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
  skip("To be revised...")
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
  skip("To be revised...")
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
  skip("To be revised...")
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
