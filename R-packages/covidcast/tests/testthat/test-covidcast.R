library(covidcast)
library(httptest)
library(mockery)

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
# the JSON needed for that test. We hence store the filename in comments
# adjacent to every call.
#
# 3. covidcast_signal() calls covidcast_meta() unconditionally. We hence need a
# single meta file that suffices for all tests that call covidcast_signal().

with_mock_api({
  test_that("covidcast_meta formats result correctly", {
    # api.php-d2e163.json
    expect_equal(covidcast_meta(),
                 structure(
                   data.frame(
                     data_source = "foo",
                     signal = "bar",
                     min_time = as.Date(c("2020-01-01", "2020-10-02")),
                     max_time = as.Date(c("2020-01-02", "2020-10-03")),
                     max_issue = as.Date(c("2020-04-04", "2020-11-01")),
                     min_value = 0,
                     max_value = 10,
                     time_type = "day",
                     geo_type = "county"
                   ),
                   class = c("covidcast_meta", "data.frame")
                 ))
  })
})

test_that("covidcast_meta raises error when API signals one", {
  stub(covidcast_meta, ".request",
       list(message = "argle-bargle"))

  expect_error(covidcast_meta(),
               class = "covidcast_meta_fetch_failed")
})

with_mock_api({
  ## covidcast_signal() tests

  test_that("covidcast_signal warns when requested geo_values are unavailable", {
    # api.php-6a5814.json
    expect_warning(covidcast_signal("foo", "bar", "2020-01-01", "2020-01-01",
                                    geo_values = c("pa", "tx", "DUCKS")),
                   class = "covidcast_missing_geo_values")

    # ...but not when they *are* available.
    # api.php-64a69c.json
    expect_silent(suppressMessages(
      covidcast_signal("foo", "bar", "2020-01-01", "2020-01-01",
                       geo_values = c("pa", "tx"))))
  })

  test_that("covidcast_signal warns when requested dates are unavailable", {
    # with geo_values = "*".
    # api.php-96f6a5.json
    expect_warning(covidcast_signal("foo", "bar", "2020-01-02", "2020-01-02"),
                   class = "covidcast_fetch_failed")

    # and with geo_values = "pa"
    # api.php-da6974.json
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
    # api.php-cb89ad.json
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
      metadata = list(geo_type = "county")
      )
    )
  })

  test_that("covidcast_signal stops when end_day < start_day", {
    # reusing api.php-da6974.json
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
