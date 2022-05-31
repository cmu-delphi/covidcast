library(mockr)
library(mockery)
library(covidcast)
library(fs)

# Create a fake result from the covidcast API as returned by the `evalcast::download_signals()`
# function.
create_fake_downloaded_signal <- function(geo_value, value) {
  tibble(
    data_source = "source",
    signal = "signal",
    geo_value = geo_value,
    time_value = as.Date("2020-01-01"),
    issue = as.Date("2020-01-02"),
    lag = 1L,
    value = value,
    stderr = 1,
    sample_size = 2
  )
}

test_that("download_signals runs as normal without cache dir", {
  # Mock out the call to `covidcast_signal_wrapper()`.
  mock_covidcast_signal <- mock(
    create_fake_downloaded_signal(c("al", "wy"), 5),
    create_fake_downloaded_signal(c("al", "wy"), 10)
  )

  # Call once.
  mockr::with_mock(covidcast_signal_wrapper = mock_covidcast_signal, {
    downloaded_data <- download_signal(
      data_source = "source",
      signal = "signal",
      geo_type = "state",
      geo_value = c("al", "wy"),
      end_day = as.Date("2020-01-01"),
      issue = as.Date("2020-01-02")
    )
  })

  expect_called(mock_covidcast_signal, 1)
  expect_equal(downloaded_data, create_fake_downloaded_signal(c("al", "wy"), 5))

  # Call a second time.
  mockr::with_mock(covidcast_signal_wrapper = mock_covidcast_signal, {
    downloaded_data <- download_signal(
      data_source = "source",
      signal = "signal",
      geo_type = "state",
      geo_value = c("al", "wy"),
      end_day = as.Date("2020-01-01"),
      issue = as.Date("2020-01-02")
    )
  })

  expect_called(mock_covidcast_signal, 2)
  expect_equal(downloaded_data, create_fake_downloaded_signal(c("al", "wy"), 10))
})


test_that("download_signals caches when called with cache dir", {
  # Mock out the call to `covidcast_signal_wrapper()`.
  mock_covidcast_signal <- mock(
    create_fake_downloaded_signal(c("al", "wy"), 5),
    create_fake_downloaded_signal(c("al", "wy"), 10)
  )

  temp_offline_signal_dir <- fs::dir_create(withr::local_file(tempfile()))

  # Call once.
  mockr::with_mock(covidcast_signal_wrapper = mock_covidcast_signal, {
    downloaded_data <- download_signal(
      data_source = "source",
      signal = "signal",
      geo_type = "state",
      geo_values = c("al", "wy"),
      end_day = as.Date("2020-01-01"),
      issue = as.Date("2020-01-02"),
      offline_signal_dir = temp_offline_signal_dir
    )
  })

  expect_called(mock_covidcast_signal, 1)
  expect_equal(downloaded_data, create_fake_downloaded_signal(c("al", "wy"), 5))

  # Call a second time.
  mockr::with_mock(covidcast_signal_wrapper = mock_covidcast_signal, {
    downloaded_data <- download_signal(
      data_source = "source",
      signal = "signal",
      geo_type = "state",
      geo_values = c("al", "wy"),
      end_day = as.Date("2020-01-01"),
      issue = as.Date("2020-01-02"),
      offline_signal_dir = temp_offline_signal_dir
    )
  })

  expect_called(mock_covidcast_signal, 1)
  expect_equal(downloaded_data, create_fake_downloaded_signal(c("al", "wy"), 5))
})


test_that("populate_cache calls mock_covidcast_signal appropriately", {
  # Mock out the call to `covidcast_signal_wrapper()`.
  mock_covidcast_signal <- mock(
    create_fake_downloaded_signal(c("al", "wy"), 5),
    create_fake_downloaded_signal(c("al", "wy"), 10)
  )

  temp_offline_signal_dir <- fs::dir_create(withr::local_file(tempfile()))

  # Call once.
  mockr::with_mock(covidcast_signal_wrapper = mock_covidcast_signal, {
    populate_cache(tibble(
      data_source = c("hhs", "jhu-csse"),
      signal = c("confirmed_admissions_covid_1d", "confirmed_incidence_num"),
      start_day = c("2020-08-01", "2020-08-01"),
      as_of = c("2022-01-01", "2022-01-01"),
      geo_type = c("state", "state")
    ), offline_signal_dir = temp_offline_signal_dir)
  })

  expect_called(mock_covidcast_signal, 2)
  expect_equal(mock_args(mock_covidcast_signal), list(
      list(
        data_source = "hhs",
        signal = "confirmed_admissions_covid_1d",
        start_day = "2020-08-01",
        end_day = "2022-01-01",
        geo_type = "state",
        geo_values = "*",
        as_of = "2022-01-01"
      ),
      list(
        data_source = "jhu-csse",
        signal = "confirmed_incidence_num",
        start_day = "2020-08-01",
        end_day = "2022-01-01",
        geo_type = "state",
        geo_values = "*",
        as_of = "2022-01-01"
      )
    )
  )
})