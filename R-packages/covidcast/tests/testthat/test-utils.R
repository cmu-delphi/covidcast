# Internal utility functions.

test_that("latest_issue & earliest_issue give only the correct issue", {
  # metadata is included to ensure attributes are preserved
  foo <- structure(
    data.frame(
      data_source = "foo",
      signal = "bar",
      geo_value = c(rep("pa", 3), rep("tx", 3)),
      issue = c(3, 2, 1, 1, 2, 3),
      time_value = 1,
      value = c(4, 5, 6, 7, 8, 9)),
    class = c("covidcast_signal", "data.frame"),
    metadata = list(geo_type = "state")
  )

  latest <- structure(
    data.frame(
      data_source = "foo",
      signal = "bar",
      geo_value = c("pa", "tx"),
      issue = 3,
      time_value = 1,
      value = c(4, 9)),
    class = c("covidcast_signal", "data.frame"),
    metadata = list(geo_type = "state")
  )

  expect_equal(latest_issue(foo), latest)

  earliest <- structure(
    data.frame(
      data_source = "foo",
      signal = "bar",
      geo_value = c("pa", "tx"),
      issue = 1,
      time_value = 1,
      value = c(6, 7)),
    class = c("covidcast_signal", "data.frame"),
    metadata = list(geo_type = "state")
  )

  expect_equal(earliest_issue(foo), earliest)
})

test_that("latest_issue and earliest_issue error on wrong input class", {
  # neither supports covidcast_signal_wide
  foo <- structure(
    data.frame(
      data_source = "foo",
      signal = "bar",
      geo_value = c("pa", "tx"),
      issue = 3,
      time_value = 1,
      value = c(4, 9)),
    class = c("covidcast_signal_wide", "data.frame")
  )

  expect_error(latest_issue(foo))
  expect_error(earliest_issue(foo))
})

test_that("latest_issue & earliest_issue take covidcast_signal_long data frames", {
  foo <- structure(
    data.frame(
      data_source = "foo",
      signal = c(rep("bar", 3), rep("baz", 3)),
      geo_value = c("pa", "tx"),
      issue = 1:6,
      time_value = 1,
      value = 7:12),
    class = c("covidcast_signal_long", "data.frame")
  )

  latest <- structure(
    data.frame(
      data_source = "foo",
      signal = c("baz", "baz", "bar", "bar"),
      geo_value = c("tx", "pa", "pa", "tx"),
      issue = c(6, 5, 3, 2),
      time_value = 1,
      value = c(12, 11, 9, 8)),
    class = c("covidcast_signal_long", "data.frame")
  )

  expect_equal(latest_issue(foo), latest)

  earliest <- structure(
    data.frame(
      data_source = "foo",
      signal = c("bar", "bar", "baz", "baz"),
      geo_value = c("pa", "tx", "tx", "pa"),
      issue = c(1, 2, 4, 5),
      time_value = 1,
      value = c(7, 8, 10, 11)),
    class = c("covidcast_signal_long", "data.frame")
  )

  expect_equal(earliest_issue(foo), earliest)
})
