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
#   return different values for each call. By default, only direct calls to a
#   function are stubbed. There is a depth argument to allow stubbing
#   indirect calls as well (i.e. You call f() -> g() -> h() and need to stub g's
#   call to h), but there is a bug in mockery that should be fixed with the next
#   release. See https://github.com/r-lib/mockery/pull/39.


with_mock_api({
  test_that("covidcast_meta formats result correctly", {
    # api.php-dd024f.csv
    expect_equal(covidcast_meta(),
                 structure(
                   data.frame(
                     data_source = "foo",
                     signal = c("bar", "bar2"),
                     time_type = "day",
                     geo_type = "county",
                     min_time = as.Date(c("2020-01-01", "2020-10-02")),
                     max_time = as.Date(c("2020-01-02", "2020-10-03")),
                     min_value = 0,
                     max_value = 10,
                     num_locations = 100,
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
    # api.php-dd024f.csv
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
      metadata = data.frame(geo_type = "county", num_locations = 100,
                            data_source = "foo", signal = "bar-not-found")
      )
    )
  })

  test_that("covidcast_signal stops when end_day < start_day", {
    # reusing api.php-dd024f.csv for metadata
    expect_error(covidcast_signal("foo", "bar", "2020-01-02", "2020-01-01"))
  })

  test_that("covidcast_signals rejects incorrect start/end_day length", {
    expect_error(covidcast_signals("foo", "bar",
                                   start_day = c("2020-01-01", "2020-01-02")))
    expect_error(covidcast_signals("foo", "bar",
                                   end_day = c("2020-01-01", "2020-01-02")))
  })
})

test_that("covidcast_days does not treat \"*\" as a missing geo_value", {
  stub(covidcast_days, "covidcast",
       data.frame(
         geo_value = c("geoa", "geob"),
         signal = "signal",
         time_value = c(20201030, 20201031),
         direction = NA,
         issue = as.Date("2020-11-04"),
         lag = 2,
         value = 3,
         stderr = NA,
         sample_size = NA
       ))

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
      time_type = "day",
      max_geos = 1),
    regexp = NA)
})

test_that("covidcast_days does not raise warnings for full response", {
  stub(covidcast_days, "covidcast",
       data.frame(
         geo_value = c("geoa"),
         signal = "signal",
         time_value = c(20201030, 20201031),
         direction = NA,
         issue = as.Date("2020-11-04"),
         lag = 2,
         value = 3,
         stderr = NA,
         sample_size = NA
       ))

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
      time_type = "day",
      max_geos = 1),
    regexp = NA)
})

test_that("covidcast_days batches calls to covidcast", {
  covidcast_returns <- rep(
    list(
      data.frame(
        geo_value = c("geoa"),
        signal = "signal",
        time_value = rep(NA, 3),
        direction = NA,
        issue = as.Date("2020-11-04"),
        lag = 2,
        value = 3,
        stderr = NA,
        sample_size = NA
      )),
    2)
  covidcast_returns[[1]]$time_value <- 20101001:20101003
  covidcast_returns[[2]]$time_value <- 20101004:20101006

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
        time_type = "day",
        lag = NULL,
        max_geos = 1000
      ),
      regexp = NA)
  expect_called(m, 2)
})

# This test requires the use of stub's depth parameter, but there is a bug in
# mockery's current release (0.4.2) which doesn't handle locked bindings
# properly, so it errors when running devtools::check(). The test can be run
# using testthat::test_package("covidcast"), but overwrites some functions, so
# devtools:load_all() should be run after. This should be fixed with the next
# release of mockery, at which point we can uncomment the test.
# See: https://github.com/r-lib/mockery/pull/39

# test_that("covidcast_days batches calls with few geo_values", {
#   covidcast_returns <-  data.frame(
#                           geo_value = c("geoa"),
#                           signal = "signal",
#                           time_value = 20101001:20101006,
#                           direction = NA,
#                           issue = as.Date("2020-11-07"),
#                           lag = 2,
#                           value = 3,
#                           stderr = NA,
#                           sample_size = NA
#                         )
# 
#   m <- mock(covidcast_returns)
#   stub(covidcast_days, "covidcast", m, depth = 2)
#   expect_warning(
#     covidcast_signal(
#       data_source = "fb-survey",
#       signal = "raw_cli",
#       start_day = as.Date("2020-10-01"),
#       end_day = as.Date("2020-10-06"),
#       geo_type = "county",
#       geo_values = "geoa"
#     ),
#     regexp = NA)
#   expect_called(m, 1)
# })

test_that("as.covidcast_signal produces valid covidcast_signal objects", {
  foo <- data.frame(
    geo_value = "01000",
    time_value = as.Date("2020-01-01"),
    value = 1
  )

  expected <- structure(
    data.frame(
      data_source = "user",
      signal = "foo",
      geo_value = "01000",
      time_value = as.Date("2020-01-01"),
      value = 1,
      issue = as.Date("2020-10-01")
    ),
    class = c("covidcast_signal", "data.frame"),
    metadata = data.frame(data_source = "user", signal = "foo",
                          geo_type = "county")
  )

  expect_equal(as.covidcast_signal(foo,
                                   signal = "foo",
                                   issue = as.Date("2020-10-01")),
               expected)

  expected <- structure(
    data.frame(
      data_source = "some-source",
      signal = "some-signal",
      geo_value = "01000",
      time_value = as.Date("2020-01-01"),
      value = 1,
      issue = as.Date("2020-10-01")
    ),
    class = c("covidcast_signal", "data.frame"),
    metadata = data.frame(data_source = "some-source", signal = "some-signal",
                          geo_type = "county")
  )

  expect_equal(
    as.covidcast_signal(
      foo,
      data_source = "some-source",
      signal = "some-signal",
      issue = as.Date("2020-10-01")
    ),
    expected
  )
})

test_that("as.covidcast_signal throws errors when input is unsuitable", {
  foo <- data.frame(
    geo_value = "01000",
    time_value = as.Date("2020-10-01"),
    ducks = "rabbit"
  )

  expect_error(as.covidcast_signal(foo, signal = "foo"),
               class = "covidcast_coerce_value")

  foo <- data.frame(
    county = "01000",
    time_value = as.Date("2020-10-01"),
    value = 1
  )

  expect_error(as.covidcast_signal(foo, signal = "foo"),
               class = "covidcast_coerce_geo_value")

  foo <- data.frame(
    geo_value = "01000",
    day = as.Date("2020-10-01"),
    value = 1
  )

  expect_error(as.covidcast_signal(foo, signal = "foo"),
               class = "covidcast_coerce_time_value")
})

test_that("as.covidcast_signal does not affect covidcast_signal objects", {
  expected <- structure(
    data.frame(
      data_source = "some-source",
      signal = "some-signal",
      geo_value = "01000",
      time_value = as.Date("2020-01-01"),
      value = 1,
      issue = as.Date("2020-10-01")
    ),
    class = c("covidcast_signal", "data.frame"),
    metadata = list(geo_type = "county")
  )

  expect_equal(as.covidcast_signal(expected), expected)
})
