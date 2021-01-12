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

test_that("evaluate_predictions evaluates against downloaded data", {
  # Mock out the call to `download_signals()`.
  mock_download_signal <- mock(create_fake_downloaded_signal(c("al", "wy"), 5),
                               create_fake_downloaded_signal(c("al", "wy"), 10))
  with_mock(download_signal = mock_download_signal, {
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
    score_card <- evaluate_predictions(pcard)

    expect_called(mock_download_signal, 2)
    expect_equal(mock_args(mock_download_signal),
                 list(list(start_day = as.Date("2020-01-05"),
                           end_day = as.Date("2020-01-11"),
                           data_source = "source",
                           signal = "signal",
                           geo_type = "state",
                           geo_values = c("al", "wy")),
                      list(start_day = as.Date("2020-01-12"),
                           end_day = as.Date("2020-01-18"),
                           data_source = "source",
                           signal = "signal",
                           geo_type = "state",
                           geo_values = c("al", "wy"))))

    expect_equal(colnames(score_card),
                  c("ahead", "geo_value", "quantile", "value", "forecaster", "forecast_date",
                    "data_source", "signal", "target_end_date", "incidence_period", "actual",
                    "wis", "ae", "coverage_80"))
    n <- 6
    expect_equal(score_card$ahead, rep(1, n))
    expect_equal(score_card$geo_value, rep(c("al", "wy"), each=n/2))
    expect_equal(score_card$quantile, rep(c(0.1, 0.5, 0.9), n/3))
    expect_equal(score_card$value, seq(1, 6))
    expect_equal(score_card$forecaster, rep("a", n))
    expect_equal(score_card$forecast_date, rep(as.Date(c("2020-01-02", "2020-01-09")), each=n/2))
    expect_equal(score_card$data_source, rep("source", n))
    expect_equal(score_card$signal, rep("signal", n))
    expect_equal(score_card$target_end_date, rep(as.Date("2020-01-21"), n))
    expect_equal(score_card$incidence_period, rep("epiweek", n))
    expect_equal(score_card$actual, rep(c(5, 10), each=n/2))
    expect_equal(score_card$wis, rep(c(7.4/3, 13.4/3), each=n/2)) # computed by hand
    expect_equal(score_card$ae, rep(c(3, 5), each=n/2))
    expect_equal(score_card$coverage_80, rep(0, n))
  })
})

test_that("evaluate_predictions fails on non-predictions cards data frames", {
  expect_error(
    evaluate_predictions(
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

test_that("evaluate_predictions evaluates against side truth", {
  # Mock out `download_signal` to verify it isn't getting called.
  mock_download_signal <- mock()
  with_mock(download_signal = mock_download_signal, {
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

    side_truth <- tibble(
      forecaster = "a",
      forecast_date = as.Date(c("2020-01-02", "2020-01-09")),
      ahead = 1,
      geo_value = c("al", "wy"),
      actual = c(5, 10)
    )
    score_card <- evaluate_predictions(pcard, side_truth=side_truth)

    expect_called(mock_download_signal, 0)

    expect_equal(colnames(score_card),
                  c("wis", "ae", "coverage_80", "forecaster", "forecast_date", "ahead", 
                    "geo_value", "quantile", "value", "data_source", "signal", "target_end_date",
                    "incidence_period", "actual"))
    n <- 6
    expect_equal(score_card$ahead, rep(1, n))
    expect_equal(score_card$geo_value, rep(c("al", "wy"), each=n/2))
    expect_equal(score_card$quantile, rep(c(0.1, 0.5, 0.9), n/3))
    expect_equal(score_card$value, seq(1, 6))
    expect_equal(score_card$forecaster, rep("a", n))
    expect_equal(score_card$forecast_date, rep(as.Date(c("2020-01-02", "2020-01-09")), each=n/2))
    expect_equal(score_card$data_source, rep("source", n))
    expect_equal(score_card$signal, rep("signal", n))
    expect_equal(score_card$target_end_date, rep(as.Date("2020-01-21"), n))
    expect_equal(score_card$incidence_period, rep("epiweek", n))
    expect_equal(score_card$actual, rep(c(5, 10), each=n/2))
    expect_equal(score_card$wis, rep(c(7.4/3, 13.4/3), each=n/2)) # computed by hand
    expect_equal(score_card$ae, rep(c(3, 5), each=n/2))
    expect_equal(score_card$coverage_80, rep(0, n))
  })
})

test_that("evaluate_predictions uses alternate grouping variables", {
  # Mock out `download_signal` to verify it isn't getting called.
  mock_download_signal <- mock()
  with_mock(download_signal = mock_download_signal, {
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

    side_truth <- tibble(
      forecaster = c("a", "b"),
      actual = c(5, 10)
    )
    score_card <- evaluate_predictions(pcard, side_truth=side_truth, grp_vars=c("forecaster"))

    expect_called(mock_download_signal, 0)

    expect_equal(colnames(score_card),
                  c("wis", "ae", "coverage_80", "forecaster",  "ahead", "geo_value",
                    "quantile", "value", "forecast_date", "data_source", "signal",
                    "target_end_date", "incidence_period", "actual"))
    n <- 6
    expect_equal(score_card$ahead, rep(1, n))
    expect_equal(score_card$geo_value, rep(c("al", "wy"), each=n/2))
    expect_equal(score_card$quantile, rep(c(0.1, 0.5, 0.9), n/3))
    expect_equal(score_card$value, seq(1, 6))
    expect_equal(score_card$forecaster, rep(c("a", "b"), each=n/2))
    expect_equal(score_card$forecast_date, rep(as.Date(c("2020-01-02", "2020-01-09")), each=n/2))
    expect_equal(score_card$data_source, rep("source", n))
    expect_equal(score_card$signal, rep("signal", n))
    expect_equal(score_card$target_end_date, rep(as.Date("2020-01-21"), n))
    expect_equal(score_card$incidence_period, rep("epiweek", n))
    expect_equal(score_card$actual, rep(c(5, 10), each=n/2))
    expect_equal(score_card$wis, rep(c(7.4/3, 13.4/3), each=n/2)) # computed by hand
    expect_equal(score_card$ae, rep(c(3, 5), each=n/2))
    expect_equal(score_card$coverage_80, rep(0, n))
  })  
})

test_that("evaluate_predictions backfill_buffer works", {
  # Mock out the following functions:
  # - `download_signals()` so we don't call to the covidcast API.
  # - `Sys.Date()` so we can control "today's" date to compare against the forecasts.
  mock_download_signal <- mock(create_fake_downloaded_signal(c("al", "wy"), 10), cycle=TRUE)
  mock_sys_date <- mock(as.Date("2020-01-14"), cycle=TRUE)
  with_mock(download_signal = mock_download_signal, Sys.Date = mock_sys_date, {
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
    # waited long enough should have no warning:
    evaluate_predictions(pcard, backfill_buffer = 2)
  })
})