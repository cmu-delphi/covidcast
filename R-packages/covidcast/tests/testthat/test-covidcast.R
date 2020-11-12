library(covidcast)
library(httptest)
library(mockery)
library(dplyr)

# Many of these tests use mockery::with_mock_api. This replaces calls to the
# live API server, instead returning static JSON files from disk. Three
# weaknesses to be aware of:
#
# 1. The HTTP URL and parameters are hashed into a single filename to use. This
# means that you can't have the query return one thing, then have the same query
# return something else, for instance to test how the client handles different
# types of errors.
#
# 2. Once you've written a test, it can be difficult to find the file storing
# the JSON or CSV needed for that test. We hence store the filename in comments
# adjacent to every call. (Note that we request CSVs from the API for
# covidcast_signal, and httptest suggests filenames ending in .json by default.
# You can use .csv instead and httptest will correctly locate those files.)
#
# 3. covidcast_signal() calls covidcast_meta() unconditionally. We hence need a
# single meta file that suffices for all tests that call covidcast_signal().
#
# Other tests use mockery::stub and mockery::mock, which come with their own
# pros and cons.
#
# with_mock_api:
#   Replace all calls to a specified function that use specified arguments
#   with the result in the corresponding file.
# mock:
#   Replace all calls to a specified function with the arguments of the mock, in
#   order (i.e. The n-th call to the function returns the n-th argument of the
#   mock).
# stub:
#   Replaces all calls from a specified function to another specified function
#   with a specified result (without regard for arguments). The specified
#   result can be a function, so using stub with a mock object allows you to
#   return different values for each call.


with_mock_api({
  test_that("covidcast_meta formats result correctly", {
    # api.php-dd024f.csv
    expect_equal(covidcast_meta(),
                 structure(
                   data.frame(
                     data_source = "foo",
                     signal = c("bar", "bar2"),
                     min_time = as.Date(c("2020-01-01", "2020-10-02")),
                     max_time = as.Date(c("2020-01-02", "2020-10-03")),
                     min_value = 0,
                     max_value = 10,
                     num_locations = 100,
                     time_type = "day",
                     geo_type = c("county", "state"),
                     max_issue = as.Date(c("2020-04-04", "2020-11-01"))
                   ),
                   class = c("covidcast_meta", "data.frame")
                 ))
  })
})

test_that("covidcast_meta raises error when API signals one", {
  stub(covidcast_meta, ".request", "")

  expect_error(covidcast_meta(),
               class = "covidcast_meta_fetch_failed")
})

with_mock_api({
  ## covidcast_signal() tests
  test_that("covidcast_signal warns when requested geo_values are unavailable", {
    # api.php-3e1dc3.csv
    expect_warning(covidcast_signal("foo", "bar", "2020-01-01", "2020-01-01",
                                    geo_values = c("pa", "tx", "DUCKS")),
                   class = "covidcast_missing_geo_values")

    # ...but not when they *are* available.
    # api.php-f666a2.csv
    expect_silent(suppressMessages(
      covidcast_signal("foo", "bar", "2020-01-01", "2020-01-01",
                       geo_values = c("pa", "tx"))))
  })

  test_that("covidcast_signal warns when requested dates are unavailable", {
    # with geo_values = "*".
    # api.php-b6e478.csv
    expect_warning(covidcast_signal("foo", "bar", "2020-01-02", "2020-01-02"),
                   class = "covidcast_fetch_failed")

    # and with geo_values = "pa"
    # api.php-d707dc.csv
    expect_warning(covidcast_signal("foo", "bar", "2020-01-02", "2020-01-02",
                                    geo_values = "pa"),
                   class = "covidcast_fetch_failed")
  })

  test_that("covidcast_signal aborts when meta not found", {
    expect_error(covidcast_signal("foo", "bar-not-found"),
                 class = "covidcast_meta_not_found")
  })

  test_that("covidcast_signal works for signals with no meta", {
    # when no meta is available, we must provide start_day and end_day.
    # api.php-1d9b5c.csv
    expect_equal(
      covidcast_signal("foo", "bar-not-found",
                       "2020-01-01", "2020-01-01"),
      structure(data.frame(
        data_source = "foo",
        signal = "bar-not-found",
        geo_value = "01000",
        time_value = as.Date("2020-01-01"),
        issue = as.Date("2020-01-02"),
        lag = 1L,
        value = 1,
        stderr = 0.1,
        sample_size = 2
      ),
      class = c("covidcast_signal", "data.frame"),
      metadata = list(geo_type = "county", num_locations = 100)
      )
    )
  })

  test_that("covidcast_signal works across multiple days with gaps", {
    # If we request 3 days, we'll get 3 API queries. If the middle day is
    # missing, we should get an appropriate warning, but still get the right
    # data frame.

    # day 1: api.php-32641f.csv
    # day 2: api.php-b85d44.csv (empty)
    # day 3: api.php-f49e8f.csv
    expect_warning(covidcast_signal("foo", "bar", "2020-01-10", "2020-01-12",
                                    geo_type = "county"),
                   class = "covidcast_fetch_failed")

    res <- suppressWarnings(
      covidcast_signal("foo", "bar", "2020-01-10", "2020-01-12",
                       geo_type = "county"))
    expect_equal(
      res,
      structure(data.frame(
        data_source = "foo",
        signal = "bar",
        geo_value = c("01001", "01002", "31001", "31002"),
        time_value = as.Date(c("2020-01-10", "2020-01-10",
                               "2020-01-12", "2020-01-12")),
        issue = as.Date(c("2020-01-11", "2020-01-11",
                          "2020-01-13", "2020-01-13")),
        lag = 1,
        value = c(91.2, 99.1, 81.2, 89.1),
        stderr = c(0.8, 0.2, 0.8, 0.2),
        sample_size = c(114.2, 217.8, 314.2, 417.8)),
        class = c("covidcast_signal", "data.frame"),
        metadata = structure(data.frame(
          data_source = "foo",
          signal = "bar",
          time_type = "day",
          geo_type = "county",
          min_time = as.Date("2020-01-01"),
          max_time = as.Date("2020-01-02"),
          min_value = 0,
          max_value = 10,
          max_issue = as.Date("2020-04-04")),
          class = c("covidcast_meta", "data.frame"))))
  })

  test_that("covidcast_signal stops when end_day < start_day", {
    # reusing api.php-dd024f.csv for metadata
    expect_error(covidcast_signal("foo", "bar", "2020-01-02", "2020-01-01"))
  })

  ## covidcast_signals()

  test_that("covidcast_signals rejects incorrect start/end_day length", {
    expect_error(covidcast_signals("foo", "bar",
                                   start_day = c("2020-01-01", "2020-01-02")))
    expect_error(covidcast_signals("foo", "bar",
                                   end_day = c("2020-01-01", "2020-01-02")))
  })
})

test_that("covidcast_days does not treat \"*\" as a missing geo_value", {
  stub(covidcast_days, "covidcast",
       list(message = "success", epidata = data.frame(
         geo_value = c("geoa", "geob"),
         signal = "signal",
         time_value = c(20201030, 20201031),
         direction = NA,
         issue = as.Date("2020-11-04"),
         lag = 2,
         value = 3,
         stderr = NA,
         sample_size = NA
       ), result = 1))
  # Expect no warning
  expect_warning(
    covidcast_days(
      data_source = "fb-survey",
      signal = "raw_cli",
      start_day = as.Date("2020-10-30"),
      end_day = as.Date("2020-10-31"),
      geo_type = "county",
      geo_value = c("*"),
      as_of = NULL,
      issues = NULL,
      lag = NULL,
      max_geos = 1),
    regexp = NA)
})

test_that("covidcast_days does not raise warnings for full response", {
  stub(covidcast_days, "covidcast",
       list(message = "success", epidata = data.frame(
         geo_value = c("geoa"),
         signal = "signal",
         time_value = c(20201030, 20201031),
         direction = NA,
         issue = as.Date("2020-11-04"),
         lag = 2,
         value = 3,
         stderr = NA,
         sample_size = NA
       ), result = 1))
  # Expect no warning
  expect_warning(
    covidcast_days(
      data_source = "fb-survey",
      signal = "raw_cli",
      start_day = as.Date("2020-10-30"),
      end_day = as.Date("2020-10-31"),
      geo_type = "county",
      geo_value = c("geoa"),
      as_of = NULL,
      issues = NULL,
      lag = NULL,
      max_geos = 1),
    regexp = NA)
})

test_that("covidcast_days batches calls to covidcast", {
  covidcast_returns <- rep(list(list(message = "success", epidata = data.frame(
    geo_value = c("geoa"),
    signal = "signal",
    time_value = rep(NA, 3),
    direction = NA,
    issue = as.Date("2020-11-04"),
    lag = 2,
    value = 3,
    stderr = NA,
    sample_size = NA
  ), result = 1)), 10)
  covidcast_returns[[1]]$epidata$time_value <- 20101001:20101003
  covidcast_returns[[2]]$epidata$time_value <- 20101004:20101006

  m <- mock(covidcast_returns[[1]], covidcast_returns[[2]])
  stub(covidcast_days, "covidcast", m)
    expect_warning(
      covidcast_days(
        data_source = "fb-survey",
        signal = "raw_cli",
        start_day = as.Date("2020-10-01"),
        end_day = as.Date("2020-10-06"),
        geo_type = "county",
        geo_value = "*",
        as_of = NULL,
        issues = NULL,
        lag = NULL,
        max_geos = 1000
      ),
      regexp = NA)
  expect_called(m, 2)
})
