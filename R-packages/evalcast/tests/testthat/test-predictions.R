library(mockr)
library(mockery)

# Create a fake result from the covidcast API as returned by the `evalcast::download_signals()`
# function.
create_fake_downloaded_signal <- function(geo_value, data_source, signal) {
  tibble(data_source = data_source,
         signal = signal,
         geo_value = geo_value,
         time_value = as.Date(c("2020-01-01", "2020-01-02")),
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

test_that("get_predictions works", {
  # Set up mocks for the following functions:
  # - `evalcast::download_signals()` to avoid dependencies on the covidcast API.
  # - the forecaster to avoid dependencies on its internal prediction algorithm.
  fake_downloaded_signals <- list(
    create_fake_downloaded_signal("in", "jhu_csse", "deaths_incidence_num"),
    create_fake_downloaded_signal("in", "jhu_csse", "confirmed_incidence_num")
  )
  mock_download_signal <- mock(fake_downloaded_signals[[1]],
                               fake_downloaded_signals[[2]],
                               cycle=TRUE)
  mockr::with_mock(download_signal = mock_download_signal, {
    mock_forecaster <- mock(create_fake_forecast(3, "in"),
                            create_fake_forecast(3, "nc"))

    signals <- tibble(data_source = "jhu-csse",
                      signal = c("deaths_incidence_num", "confirmed_incidence_num"),
                      start_day = as.Date("2020-01-01"))
    forecast_dates <- as.Date(c("2020-01-01", "2020-01-02"))

    pcard <- get_predictions(mock_forecaster,
                             name_of_forecaster = "fake",
                             signals = signals,
                             forecast_dates = forecast_dates,
                             incidence_period = "epiweek",
                             ahead = 3,
                             geo_type = "state")

    expect_called(mock_download_signal, 4)
    expect_equal(mock_args(mock_download_signal),
                 list(list(data_source = "jhu-csse",
                           signal = "deaths_incidence_num",
                           start_day = as.Date("2020-01-01"),
                           end_day = as.Date("2020-01-01"),
                           as_of = as.Date("2020-01-01"),
                           geo_type = "state",
                           geo_values = "*"),
                      list(data_source = "jhu-csse",
                           signal = "confirmed_incidence_num",
                           start_day = as.Date("2020-01-01"),
                           end_day = as.Date("2020-01-01"),
                           as_of = as.Date("2020-01-01"),
                           geo_type = "state",
                           geo_values = "*"),
                      list(data_source = "jhu-csse",
                           signal = "deaths_incidence_num",
                           start_day = as.Date("2020-01-01"),
                           end_day = as.Date("2020-01-02"),
                           as_of = as.Date("2020-01-02"),
                           geo_type = "state",
                           geo_values = "*"),
                      list(data_source = "jhu-csse",
                           signal = "confirmed_incidence_num",
                           start_day = as.Date("2020-01-01"),
                           end_day = as.Date("2020-01-02"),
                           as_of = as.Date("2020-01-02"),
                           geo_type = "state",
                           geo_values = "*"))
    )
    expect_called(mock_forecaster, 2)
    expect_equal(mock_args(mock_forecaster),
                 list(list(fake_downloaded_signals,
                           as.Date("2020-01-01"),
                           signals,
                           "epiweek",
                           3,
                           "state"),
                      list(fake_downloaded_signals,
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

test_that("get_predictions works when forecaster has additional arguments", {
  # Set up mocks for the following functions:
  # - `evalcast::download_signals()` to avoid dependencies on the covidcast API.
  # - the forecaster to avoid dependencies on its internal prediction algorithm.
  fake_downloaded_signals <- list(
    create_fake_downloaded_signal("in", "jhu_csse", "deaths_incidence_num"),
    create_fake_downloaded_signal("in", "jhu_csse", "confirmed_incidence_num")
  )
  mock_download_signal <- mock(fake_downloaded_signals[[1]],
                               fake_downloaded_signals[[2]],
                               cycle=TRUE)
  mockr::with_mock(download_signal = mock_download_signal, {
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

    expect_called(mock_download_signal, 4)
    expect_called(mock_forecaster, 2)
    expect_equal(mock_args(mock_forecaster),
                 list(list(fake_downloaded_signals,
                           as.Date("2020-01-01"),
                           signals,
                           "epiweek",
                           3,
                           "state",
                           forecaster_arg1 = 1,
                           forecaster_arg2 = "2"),
                      list(fake_downloaded_signals,
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

test_that("no start_day within signals raises warning but works", {
  # Set up mocks for the following functions:
  # - `evalcast::download_signals()` to avoid dependencies on the covidcast API.
  # - the forecaster to avoid dependencies on its internal prediction algorithm.
  fake_downloaded_signals <- list(
    create_fake_downloaded_signal("fl", "jhu_csse", "deaths_incidence_num"),
    create_fake_downloaded_signal("fl", "jhu_csse", "confirmed_incidence_num")
  )
  mock_download_signal <- do.call(mock, fake_downloaded_signals)
  mockr::with_mock(download_signal = mock_download_signal, {
    mock_forecaster <- mock(create_fake_forecast(2, "fl"))

    signals_no_start_day <- tibble(
                              data_source = "jhu-csse",
                              signal = c("deaths_incidence_num", "confirmed_incidence_num"))
    forecast_dates <- as.Date(c("2020-01-01"))

    expect_warning(
      pcard <- get_predictions(mock_forecaster,
                               name_of_forecaster = "fake",
                               signals = signals_no_start_day,
                               forecast_dates = forecast_dates,
                               incidence_period = "epiweek",
                               ahead = 2,
                               geo_type = "state"),
      "Unknown or uninitialised column: `start_day`.")

    expect_called(mock_download_signal, 2)
    expect_called(mock_forecaster, 1)
    expect_equal(mock_args(mock_forecaster),
                 list(list(fake_downloaded_signals,
                           as.Date("2020-01-01"),
                           signals_no_start_day,
                           "epiweek",
                           2,
                           "state")))
    expect_equal(colnames(pcard),
      c("ahead", "geo_value", "quantile", "value", "forecaster", "forecast_date", "data_source",
        "signal", "target_end_date", "incidence_period"))
    n <- 23
    expect_equal(nrow(pcard), n)
    expect_equal(pcard$ahead, rep(2, n))
    expect_equal(pcard$geo_value, rep("fl", n))
    expect_equal(pcard$forecaster, rep("fake", n))
    expect_equal(pcard$forecast_date, rep(forecast_dates, each=n))
    expect_equal(pcard$data_source, rep("jhu-csse", n))
    expect_equal(pcard$signal, rep("deaths_incidence_num", n))
    expect_equal(pcard$target_end_date, rep(as.Date("2020-01-18"), n))
    expect_equal(pcard$incidence_period, rep("epiweek", n))
  })
})

test_that("start_day function within signals works", {
  # Set up mocks for the following functions:
  # - `evalcast::download_signals()` to avoid dependencies on the covidcast API.
  # - the forecaster to avoid dependencies on its internal prediction algorithm.
  fake_downloaded_signals <- list(
    create_fake_downloaded_signal("fl", "jhu_csse", "deaths_incidence_num"),
    create_fake_downloaded_signal("fl", "jhu_csse", "confirmed_incidence_num")
  )
  mock_download_signal <- mock(fake_downloaded_signals[[1]],
                               fake_downloaded_signals[[2]],
                               cycle=TRUE)

  mockr::with_mock(download_signal = mock_download_signal, {
    mock_forecaster <- mock(create_fake_forecast(2, "fl"),
                            create_fake_forecast(2, "ga"))

    signals_with_start_day_fn <- tibble(
                                  data_source = "jhu-csse",
                                  signal = c("deaths_incidence_num", "confirmed_incidence_num"),
                                  start_day = list(function(forecast_date) forecast_date - 10)
                                 )
    forecast_dates <- as.Date(c("2020-12-11", "2020-12-12"))

    pcard <- get_predictions(mock_forecaster,
                             name_of_forecaster = "fake",
                             signals = signals_with_start_day_fn,
                             forecast_dates = forecast_dates,
                             incidence_period = "epiweek",
                             ahead = 2,
                             geo_type = "state")

    expect_called(mock_download_signal, 4)
    expect_called(mock_forecaster, 2)
    expect_equal(mock_args(mock_forecaster),
                 list(list(fake_downloaded_signals,
                           as.Date("2020-12-11"),
                           tibble(data_source = "jhu-csse",
                                  signal = c("deaths_incidence_num", "confirmed_incidence_num"),
                                  start_day = as.Date("2020-12-01")),
                           "epiweek",
                           2,
                           "state"),
                      list(fake_downloaded_signals,
                           as.Date("2020-12-12"),
                           tibble(data_source = "jhu-csse",
                                  signal = c("deaths_incidence_num", "confirmed_incidence_num"),
                              start_day = as.Date("2020-12-02")),
                           "epiweek",
                           2,
                           "state")))
    expect_equal(colnames(pcard),
      c("ahead", "geo_value", "quantile", "value", "forecaster", "forecast_date", "data_source",
        "signal", "target_end_date", "incidence_period"))
    n <- 46
    expect_equal(nrow(pcard), n)
    expect_equal(pcard$ahead, rep(2, n))
    expect_equal(pcard$geo_value, rep(c("fl", "ga"), each=n/2))
    expect_equal(pcard$forecaster, rep("fake", n))
    expect_equal(pcard$forecast_date, rep(forecast_dates, each=n/2))
    expect_equal(pcard$data_source, rep("jhu-csse", n))
    expect_equal(pcard$signal, rep("deaths_incidence_num", n))
    expect_equal(pcard$target_end_date, rep(as.Date("2020-12-26"), n))
    expect_equal(pcard$incidence_period, rep("epiweek", n))
  })
})

test_that("start_day function and date mix within signals works", {
  skip("To be revised...")
  # Set up mocks for the following functions:
  # - `evalcast::download_signals()` to avoid dependencies on the covidcast API.
  # - the forecaster to avoid dependencies on its internal prediction algorithm.
  fake_downloaded_signals <- list(create_fake_downloaded_signal("fl"),
                                  create_fake_downloaded_signal("ga"))
  mock_download_signal <- mock(fake_downloaded_signals[[1]],
                               fake_downloaded_signals[[2]],
                               cycle = TRUE)

  mockr::with_mock(download_signal = mock_download_signal, {
    mock_forecaster <- mock(create_fake_forecast(2, "fl"),
                            create_fake_forecast(2, "ga"))

    signals_with_start_day_fn <- tibble(
                                  data_source = "jhu-csse",
                                  signal = c("deaths_incidence_num", "confirmed_incidence_num"),
                                  start_day = c("2020-11-07",
                                                list(function(forecast_date) forecast_date - 10))
                                 )
    forecast_dates <- as.Date(c("2020-12-11", "2020-12-12"))

    pcard <- get_predictions(mock_forecaster,
                             name_of_forecaster = "fake",
                             signals = signals_with_start_day_fn,
                             forecast_dates = forecast_dates,
                             incidence_period = "epiweek",
                             ahead = 2,
                             geo_type = "state")

    expect_called(mock_download_signal, 4)
    expect_called(mock_forecaster, 2)
    expect_equal(mock_args(mock_forecaster),
                 list(list(fake_downloaded_signals,
                           as.Date("2020-12-11"),
                           tibble(data_source = "jhu-csse",
                                  signal = c("deaths_incidence_num", "confirmed_incidence_num"),
                                  start_day = as.Date(c("2020-11-07", "2020-12-01"))),
                           "epiweek",
                           2,
                           "state"),
                      list(fake_downloaded_signals,
                           as.Date("2020-12-12"),
                           tibble(data_source = "jhu-csse",
                                  signal = c("deaths_incidence_num", "confirmed_incidence_num"),
                                  start_day = as.Date(c("2020-11-07", "2020-12-02"))),
                           "epiweek",
                           2,
                           "state")))
    expect_equal(colnames(pcard),
      c("ahead", "geo_value", "quantile", "value", "forecaster", "forecast_date", "data_source",
        "signal", "target_end_date", "incidence_period"))
    n <- 46
    expect_equal(nrow(pcard), n)
    expect_equal(pcard$ahead, rep(2, n))
    expect_equal(pcard$geo_value, rep(c("fl", "ga"), each=n/2))
    expect_equal(pcard$forecaster, rep("fake", n))
    expect_equal(pcard$forecast_date, rep(forecast_dates, each=n/2))
    expect_equal(pcard$data_source, rep("jhu-csse", n))
    expect_equal(pcard$signal, rep("deaths_incidence_num", n))
    expect_equal(pcard$target_end_date, rep(as.Date("2020-12-26"), n))
    expect_equal(pcard$incidence_period, rep("epiweek", n))
  })
})
