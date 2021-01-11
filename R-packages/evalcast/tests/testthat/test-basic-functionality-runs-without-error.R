library(mockery)

# Create a fake result from the covidcast API as returned by the `evalcast::download_signals()`
# function.
create_fake_downloaded_signal <- function(geo_value) {
  tibble(data_source = "jhu-csse",
         signal =  c("deaths_incidence_num", "confirmed_incidence_num"),
         geo_value = geo_value,
         time_value = as.Date("2020-01-01"),
         issue = as.Date("2020-01-02"),
         lag = 1L,
         value = c(1, 2),
         stderr = c(0.1, 0.2),
         sample_size = c(2, 2)) 
}

# Create a fake forecast output with arbitrary predictions for each quantile.
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

create_pcard <- function(card) {
  class(card) <- c("predictions_cards", class(card))
  return(card)
}

test_that("get_predictions on baseline_forecaster works", {
  # Set up mocks for the following functions:
  # - `evalcast::download_signals()` to avoid dependencies on the covidcast API.
  # - the forecaster to avoid dependencies on its internal prediction algorithm.
  fake_downloaded_signals <- c(list(create_fake_downloaded_signal("in")),
                               list(create_fake_downloaded_signal("nc")))
  mock_download_signals <- do.call(mock, fake_downloaded_signals)
  # Ideally we should use `mockery::stub` here but there is bug that persists the mock across tests.
  # See https://github.com/r-lib/mockery/issues/20.
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
  # Set up mocks for the following functions:
  # - `evalcast::download_signals()` to avoid dependencies on the covidcast API.
  # - the forecaster to avoid dependencies on its internal prediction algorithm.
  fake_downloaded_signals <- list(
    bind_rows(create_fake_downloaded_signal("nd"),
              create_fake_downloaded_signal("ia"),
              create_fake_downloaded_signal("sd")))
  mock_download_signals <- mock(fake_downloaded_signals)
  # Ideally we should use `mockery::stub` here but there is bug that persists the mock across tests.
  # See https://github.com/r-lib/mockery/issues/20.
  with_mock(download_signals = mock_download_signals, {
    mock_forecaster <- mock(bind_rows(
      create_fake_forecast(3, "nd"),
      create_fake_forecast(3, "sd")))

    signals <- tibble(data_source = "jhu-csse",
                      signal = c("deaths_incidence_num", "confirmed_incidence_num"),
                      start_day = "2020-01-01")
    forecast_dates <- as.Date(c("2020-01-01"))

    pcard <- get_predictions(mock_forecaster,
                             name_of_forecaster = "fake",
                             signals = signals,
                             forecast_dates = forecast_dates,
                             incidence_period = "epiweek",
                             ahead = 3,
                             geo_type = "state",
                             geo_values = c("nd", "sd"))

    expect_called(mock_download_signals, 1)
    expect_called(mock_forecaster, 1)
    expect_equal(mock_args(mock_forecaster),
                 list(list(list(bind_rows(create_fake_downloaded_signal("nd"),
                                          create_fake_downloaded_signal("sd"))),
                           as.Date("2020-01-01"),
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
    expect_equal(pcard$geo_value, rep(c("nd", "sd"), each=n/2))
    expect_equal(pcard$forecaster, rep("fake", n))
    expect_equal(pcard$forecast_date, rep(forecast_dates, each=n))
    expect_equal(pcard$data_source, rep("jhu-csse", n))
    expect_equal(pcard$signal, rep("deaths_incidence_num", n))
    expect_equal(pcard$target_end_date, rep(as.Date("2020-01-25"), n))
    expect_equal(pcard$incidence_period, rep("epiweek", n))
  })
})

test_that("get_predictions works when forecaster has additional arguments", {
  # Set up mocks for the following functions:
  # - `evalcast::download_signals()` to avoid dependencies on the covidcast API.
  # - the forecaster to avoid dependencies on its internal prediction algorithm.
  fake_downloaded_signals <- c(list(create_fake_downloaded_signal("in")),
                               list(create_fake_downloaded_signal("nc")))
  mock_download_signals <- do.call(mock, fake_downloaded_signals)
  # Ideally we should use `mockery::stub` here but there is bug that persists the mock across tests.
  # See https://github.com/r-lib/mockery/issues/20.
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
                          geo_type = "state",
                          forecaster_arg1 = 1,
                          forecaster_arg2 = "2")

    expect_called(mock_download_signals, 2)
    expect_called(mock_forecaster, 2)
    expect_equal(mock_args(mock_forecaster),
                 list(list(fake_downloaded_signals[[1]],
                           as.Date("2020-01-01"),
                           signals,
                           "epiweek",
                           3,
                           "state",
                           forecaster_arg1 = 1,
                           forecaster_arg2 = "2"),
                      list(fake_downloaded_signals[[2]],
                           as.Date("2020-01-02"),
                           signals,
                           "epiweek",
                           3,
                           "state",
                           forecaster_arg1 = 1,
                           forecaster_arg2 = "2")))
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

test_that("backfill_buffer works", {
  skip("To be revised...")
  mock_download_signal <- mock(create_fake_downloaded_signal("al"), cycle=TRUE)
  with_mock(download_signal = mock_download_signal, {
    pcard <- create_pcard(tibble(
      ahead = 1,
      geo_value = rep(c("al", "wy"), each=3),
      quantile = c(0.1, 0.5, 0.9, 0.1, 0.5, 0.9),
      value = seq(1, 6),
      forecaster = "a",
      forecast_date = rep(as.Date(c("2020-01-02", "2020-01-03")), each=3),
      data_source = "source",
      signal = "signal",
      target_end_date = as.Date("2020-01-05"),
      incidence_period = "epiweek"
    ))
    expect_warning(evaluate_predictions(pcard, backfill_buffer = 4),
                "backfill_buffer")
    # waited long enough should have no error:
    evaluate_predictions(pcard, backfill_buffer = 2)
  })
})
