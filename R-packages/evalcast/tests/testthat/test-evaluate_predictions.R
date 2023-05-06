library(mockr)
library(mockery)

# Create a fake result from the covidcast API as returned by the `evalcast::download_signals()`
# function.
create_fake_downloaded_signal <- function(geo_value, value) {
  tibble(data_source = "source",
         signal =  "signal",
         geo_value = geo_value,
         time_value = as.Date("2020-01-01"),
         issue = as.Date("2020-01-02"),
         lag = 1L,
         value = value,
         stderr = 1,
         sample_size = 2) 
}

# Create a predictions card with the proper type from the data in `card`.
create_pcard <- function(card) {
  class(card) <- c("predictions_cards", class(card))
  return(card)
}

test_that("evaluate_covid_predictions evaluates against downloaded data", {
  # Mock out the call to `download_signals()`.
  mock_download_signal <- mock(create_fake_downloaded_signal(c("al", "wy"), 5),
                               create_fake_downloaded_signal(c("al", "wy"), 10))
  mockr::with_mock(download_signal = mock_download_signal, {
    pcard <- create_pcard(tibble(
      ahead = 1,
      geo_value = rep(c("al", "wy"), each=3),
      quantile = c(0.1, 0.5, 0.9, 0.1, 0.5, 0.9),
      value = seq(1, 6),
      forecaster = "a",
      forecast_date = rep(as.Date(c("2020-01-02", "2020-01-09")), each=3),
      data_source = "source",
      signal = "signal",
      target_end_date = as.Date("2020-01-21"),
      incidence_period = "epiweek"
    ))
    score_card <- evaluate_covid_predictions(pcard, geo_type = "state")
    expect_called(mock_download_signal, 2)
    expect_equal(mock_args(mock_download_signal),
                 list(list(start_day = as.Date("2020-01-05"),
                           end_day = as.Date("2020-01-11"),
                           data_source = "source",
                           signal = "signal",
                           geo_type = "state",
                           geo_values = c("al", "wy"),
                           offline_signal_dir = NULL),
                      list(start_day = as.Date("2020-01-12"),
                           end_day = as.Date("2020-01-18"),
                           data_source = "source",
                           signal = "signal",
                           geo_type = "state",
                           geo_values = c("al", "wy"),
                           offline_signal_dir = NULL
                           )))

    expect_equal(colnames(score_card),
                  c("ahead", "geo_value", "forecaster", "forecast_date", "data_source", "signal",
                    "target_end_date", "incidence_period", "actual", "wis", "ae", "coverage_80"))
    n <- 2
    expect_equal(score_card$ahead, rep(1, n))
    expect_equal(score_card$geo_value, rep(c("al", "wy"), each=n/2))
    expect_equal(score_card$forecaster, rep("a", n))
    expect_equal(score_card$forecast_date, rep(as.Date(c("2020-01-02", "2020-01-09")), each=n/2))
    expect_equal(score_card$data_source, rep("source", n))
    expect_equal(score_card$signal, rep("signal", n))
    expect_equal(score_card$target_end_date, rep(as.Date("2020-01-21"), n))
    expect_equal(score_card$incidence_period, rep("epiweek", n))
    expect_equal(score_card$actual, rep(c(5, 10), each=n/2))
    expect_equal(score_card$wis, rep(c(7.4/3, 13.4/3), each=n/2)) # computed by hand
    expect_equal(score_card$ae, rep(c(3, 5), each=n/2))
    expect_equal(score_card$coverage_80, rep(FALSE, n))
  })
})

test_that("evaluate_covid_predictions fails on non-predictions cards data frames", {
  expect_error(
    evaluate_covid_predictions(
      tibble(
        ahead = 1,
        geo_value = rep(c("al", "wy"), each=3),
        quantile = c(0.1, 0.5, 0.9, 0.1, 0.5, 0.9),
        value = seq(1, 6),
        forecaster = "a",
        forecast_date = rep(as.Date(c("2020-01-02", "2020-01-09")), each=3),
        data_source = "source",
        signal = "signal",
        target_end_date = as.Date("2020-01-21"),
        incidence_period = "epiweek"
      )),
    "must be of class `predictions_cards`")
})

test_that("evaluate_predictions evaluates against truth_data", {
  # Mock out `download_signal` to verify it isn't getting called.
  mock_download_signal <- mock()
  mockr::with_mock(download_signal = mock_download_signal, {
    pcard <- create_pcard(tibble(
      ahead = 1,
      geo_value = rep(c("al", "wy"), each=3),
      quantile = c(0.1, 0.5, 0.9, 0.1, 0.5, 0.9),
      value = seq(1, 6),
      forecaster = "a",
      forecast_date = rep(as.Date(c("2020-01-02", "2020-01-09")), each=3),
      data_source = "source",
      signal = "signal",
      target_end_date = as.Date("2020-01-21"),
      incidence_period = "epiweek"))

    truth_data <- tibble(
      forecast_date = as.Date(c("2020-01-02", "2020-01-09")),
      ahead = 1,
      geo_value = c("al", "wy"),
      actual = c(5, 10)
    )
    score_card <- evaluate_predictions(pcard, truth_data = truth_data)

    expect_called(mock_download_signal, 0)

    expect_equal(colnames(score_card),
                  c("forecaster", "ahead", "geo_value", "forecast_date", "actual", "data_source", "signal",
                    "target_end_date", "incidence_period", "wis", "ae", "coverage_80"))
    n <- 2
    expect_equal(score_card$ahead, rep(1, n))
    expect_equal(score_card$geo_value, rep(c("al", "wy"), each=n/2))
    expect_equal(score_card$forecaster, rep("a", n))
    expect_equal(score_card$forecast_date, rep(as.Date(c("2020-01-02", "2020-01-09")), each=n/2))
    expect_equal(score_card$data_source, rep("source", n))
    expect_equal(score_card$signal, rep("signal", n))
    expect_equal(score_card$target_end_date, rep(as.Date("2020-01-21"), n))
    expect_equal(score_card$incidence_period, rep("epiweek", n))
    expect_equal(score_card$actual, rep(c(5, 10), each=n/2))
    expect_equal(score_card$wis, rep(c(7.4/3, 13.4/3), each=n/2)) # computed by hand
    expect_equal(score_card$ae, rep(c(3, 5), each=n/2))
    expect_equal(score_card$coverage_80, rep(FALSE, n))
  })
})

test_that("evaluate_predictions uses alternate grouping variables", {
  # Mock out `download_signal` to verify it isn't getting called.
  mock_download_signal <- mock()
  mockr::with_mock(download_signal = mock_download_signal, {
    pcard <- create_pcard(tibble(
      ahead = 1,
      geo_value = rep(c("al", "wy"), each=3),
      quantile = c(0.1, 0.5, 0.9, 0.1, 0.5, 0.9),
      value = seq(1, 6),
      forecaster = rep(c("a", "b"), each=3),
      forecast_date = rep(as.Date(c("2020-01-02", "2020-01-09")), each=3),
      data_source = "source",
      signal = "signal",
      target_end_date = as.Date("2020-01-21"),
      incidence_period = "epiweek"))

    truth_data <- tibble(
      forecaster = c("a", "b"),
      actual = c(5, 10)
    )
    score_card <- evaluate_predictions(pcard, truth_data = truth_data,
                                       grp_vars=c("forecaster"))

    expect_called(mock_download_signal, 0)

    expect_equal(colnames(score_card),
                  c("forecaster", "ahead", "geo_value", "forecast_date", "data_source", "signal",
                    "target_end_date", "incidence_period", "actual", "wis", "ae", "coverage_80"))
    n <- 2
    expect_equal(score_card$ahead, rep(1, n))
    expect_equal(score_card$geo_value, rep(c("al", "wy"), each=n/2))
    expect_equal(score_card$forecaster, rep(c("a", "b"), each=n/2))
    expect_equal(score_card$forecast_date, rep(as.Date(c("2020-01-02", "2020-01-09")), each=n/2))
    expect_equal(score_card$data_source, rep("source", n))
    expect_equal(score_card$signal, rep("signal", n))
    expect_equal(score_card$target_end_date, rep(as.Date("2020-01-21"), n))
    expect_equal(score_card$incidence_period, rep("epiweek", n))
    expect_equal(score_card$actual, rep(c(5, 10), each=n/2))
    expect_equal(score_card$wis, rep(c(7.4/3, 13.4/3), each=n/2)) # computed by hand
    expect_equal(score_card$ae, rep(c(3, 5), each=n/2))
    expect_equal(score_card$coverage_80, rep(FALSE, n))
  })  
})

test_that("evaluate_covid_predictions backfill_buffer works", {
  # Mock `download_signals()` so we don't call to the covidcast API.
  mock_download_signal <- mock(create_fake_downloaded_signal(c("al", "wy"), 10), cycle=TRUE)
  mockr::with_mock(download_signal = mock_download_signal, {
    pcard <- create_pcard(tibble(
      ahead = 1,
      geo_value = rep(c("al", "wy"), each=3),
      quantile = c(0.1, 0.5, 0.9, 0.1, 0.5, 0.9),
      value = seq(1, 6),
      forecaster = "a",
      # The backfill buffer compares against today's date, so we need to set the forecast date
      # dynamically here or else after n days these tests will stop working.
      forecast_date = Sys.Date() - 21,
      data_source = "source",
      signal = "signal",
      target_end_date = as.Date("2020-01-05"),
      incidence_period = "epiweek"
    ))
    expect_warning(evaluate_covid_predictions(pcard, backfill_buffer = 22),
                   "backfill_buffer")
    # waited long enough should have no warning:
    expect_warning(evaluate_covid_predictions(pcard, backfill_buffer = 7), NA)
  })
})
