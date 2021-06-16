library(vdiffr)
library(dplyr)
library(mockr)
library(mockery)

## Plotting tests.
# Since it is difficult to test specific output of plots, these tests instead
# use vdiffr to ensure that plots match a saved reference version. The tests
# below simple build simple graphs of each type, thus exercising the different
# plotting options and ensuring they all function.

# Contexts are no longer required or recommended in testthat, but vdiffr still
# wants one to place the figure files correctly. See
# https://github.com/r-lib/vdiffr/issues/71
context("plot_coverage")

# Get fake predictions_cards for use in tests
fake_predictions_cards <- readRDS(
  test_path('data/fake_plot_predictions_cards.rds'))

# Create a fake result from the covidcast API as returned by the `evalcast::download_signals()`
# function.
create_fake_downloaded_signal <- function(geo_value, value, date_string) {
  tibble(data_source = "source",
         signal =  "signal",
         geo_value = geo_value,
         time_value = as.Date(date_string),
         issue = as.Date(date_string) + 1,
         lag = 1L,
         value = value,
         stderr = 1,
         sample_size = 2) 
}


test_that("plot_coverage default plotting args type all", {
  # Mock out the call to `download_signals()`.
  mock_download_signal = mock(
    create_fake_downloaded_signal(c('pa', 'wv'), c(4, 6), '2020-10-20'),
    create_fake_downloaded_signal(c('pa', 'wv'), c(7, 3), '2020-10-26'))
  mockr::with_mock(download_signal = mock_download_signal, {
    expect_doppelganger("plot default args type all",
                        plot_coverage(fake_predictions_cards, 
                                      type = "all",
                                      geo_type = "state")
      )
  })
  
})


test_that("plot_coverage custom facets type all", {
  # Mock out the call to `download_signals()`.
  mock_download_signal = mock(
    create_fake_downloaded_signal(c('pa', 'wv'), c(4, 6), '2020-10-20'),
    create_fake_downloaded_signal(c('pa', 'wv'), c(7, 3), '2020-10-26'))
  mockr::with_mock(download_signal = mock_download_signal, {
    expect_doppelganger(
      "plot custom facets one type all",
      plot_coverage(
        fake_predictions_cards, type = "all", geo_type = 'state',
        facet_rows = "forecaster", facet_cols = NULL,
        grp_vars = c("forecaster", "ahead"),
        avg_vars = c("forecast_date", "geo_value"))
    )
  })
  
  mock_download_signal = mock(
    create_fake_downloaded_signal(c('pa', 'wv'), c(4, 6), '2020-10-20'),
    create_fake_downloaded_signal(c('pa', 'wv'), c(7, 3), '2020-10-26'))
  mockr::with_mock(download_signal = mock_download_signal, {
    expect_doppelganger(
      "plot custom facets two type all",
      plot_coverage(
        fake_predictions_cards, type = "all", geo_type = 'state',
        facet_rows = "forecaster", facet_cols = "ahead",
        grp_vars = c("forecaster", "geo_value", "ahead"),
        avg_vars = c("forecast_date"))
    )
  })
  
})


test_that("compute_coverage default grps and avg", {
  # Mock out the call to `download_signals()`.
  mock_download_signal = mock(
    create_fake_downloaded_signal(c('pa', 'wv'), c(4, 6), '2020-10-20'),
    create_fake_downloaded_signal(c('pa', 'wv'), c(7, 3), '2020-10-26'))
  mockr::with_mock(download_signal = mock_download_signal, {
    score_card <- compute_coverage(fake_predictions_cards, geo_type = 'state')
    expect_equal(score_card$forecaster,
                 as.factor(rep(c('fancy', 'simple'), each = 6)))
    expect_equal(score_card$forecast_date, 
                 rep(c(as.Date('2020-10-17'), as.Date('2020-10-23'),
                       as.Date('2020-10-17'), as.Date('2020-10-23')),
                     each = 3))
    expect_equal(score_card$ahead, rep(3, 12))
    expect_equal(score_card$nominal_prob, rep(c(0.5, 0.8, 0.95), times = 4))
    expect_equal(
      score_card$prop_below, c(0, 0, 0, 0.5, 0.5, 0, 0, 0, 0, 0, 0, 0))
    expect_equal(
      score_card$prop_above, c(0.5, 0.5, 0, 0.5, 0, 0, 0.5, 0, 0, 0, 0, 0))
    expect_equal(
      score_card$prop_covered, c(0.5, 0.5, 1, 0, 0.5, 1, 0.5, 1, 1, 1, 1, 1))
  })
})

test_that("compute_coverage avg all", {
  # Mock out the call to `download_signals()`.
  mock_download_signal = mock(
    create_fake_downloaded_signal(c('pa', 'wv'), c(4, 6), '2020-10-20'),
    create_fake_downloaded_signal(c('pa', 'wv'), c(7, 3), '2020-10-26'))
  mockr::with_mock(download_signal = mock_download_signal, {
    score_card <- compute_coverage(
      fake_predictions_cards, geo_type = 'state', grp_vars = NULL,
      avg_vars = c("forecaster", "forecast_date", "ahead", "geo_value"))
    expect_equal(score_card$nominal_prob, c(0.5, 0.8, 0.95))
    expect_equal(
      score_card$prop_below, c(0.125, 0.125, 0))
    expect_equal(
      score_card$prop_above, c(0.375, 0.125, 0))
    expect_equal(
      score_card$prop_covered, c(0.5, 0.75, 1))
  })
})

test_that("compute_coverage custom grp and avg", {
  # Mock out the call to `download_signals()`.
  mock_download_signal = mock(
    create_fake_downloaded_signal(c('pa', 'wv'), c(4, 6), '2020-10-20'),
    create_fake_downloaded_signal(c('pa', 'wv'), c(7, 3), '2020-10-26'))
  mockr::with_mock(download_signal = mock_download_signal, {
    score_card <- compute_coverage(
      fake_predictions_cards, geo_type = 'state',
      grp_vars = c("forecaster", "ahead"),
      avg_vars = c("forecast_date", "geo_value"))
    expect_equal(score_card$forecaster,
                 as.factor(rep(c('fancy', 'simple'), each = 3)))
    expect_equal(score_card$ahead, rep(3, 6))
    expect_equal(score_card$nominal_prob, rep(c(0.5, 0.8, 0.95), times = 2))
    expect_equal(
      score_card$prop_below, c(0.25, 0.25, 0, 0, 0, 0))
    expect_equal(
      score_card$prop_above, c(0.5, 0.25, 0, 0.25, 0, 0))
    expect_equal(
      score_card$prop_covered, c(0.25, 0.5, 1, 0.75, 1, 1))
  })
})

test_that("compute_coverage errors", {
  predictions_cards_without_grp <- fake_predictions_cards %>% 
    select(-.data$forecaster)
  expect_error(
    compute_coverage(predictions_cards_without_grp, geo_type = 'state'),
    paste("In compute_coverage:",
          "grp_vars and avg_vars must be present in the predictions_card.",
          "See details."))
  expect_error(
    compute_coverage(predictions_cards_without_grp, geo_type = 'state',
                     grp_vars = 'geo_value', avg_vars = 'geo_value'),
    paste("In compute_coverage:",
          "grp_vars and avg_vars must have empty intersection.",
          "See details."))
})



