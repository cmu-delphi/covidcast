library(covidcast)
library(tibble)

test_that("simple correlations", {
  # Two data frames with perfect correlation, except on a day that does not
  # match between the two of them
  foo <- data.frame(
    time_value = as.Date(c("2020-01-01", "2020-01-02",
                           "2020-01-03", "2020-01-05")),
    geo_value = "pa",
    issue = 1,
    value = c(1, 2, 3, 5)
  )

  bar <- data.frame(
    time_value = as.Date(c("2020-01-01", "2020-01-02",
                           "2020-01-03", "2020-01-04")),
    geo_value = "pa",
    issue = 1,
    value = c(1, 2, 3, -5)
  )

  expected <- tibble(geo_value = "pa", value = 1)
  expect_equal(covidcast_cor(foo, bar, by = "geo_value"),
               expected)

  # Try permuting order
  expect_equal(covidcast_cor(foo, bar[c(2, 1, 4, 3), ],
                             by = "geo_value"),
               expected)
})

test_that("basic lagged correlations", {
  foo <- data.frame(
    time_value = as.Date(c("2020-01-01", "2020-01-02",
                           "2020-01-03", "2020-01-04")),
    geo_value = "pa",
    issue = 1,
    value = c(1, 2, 1, 2)
  )

  bar <- data.frame(
    time_value = as.Date(c("2020-01-01", "2020-01-02",
                           "2020-01-03", "2020-01-04")),
    geo_value = "pa",
    issue = 1,
    value = c(2, 1, 2, 1)
  )

  expected <- tibble(
    geo_value = "pa",
    value = 1
  )
  expect_equal(covidcast_cor(foo, bar, dt_x = 1),
               expected)
  expect_equal(covidcast_cor(foo, bar, dt_y = 1),
               expected)
})

test_that("lags are in correct direction", {
  foo <- data.frame(
    time_value = seq.Date(as.Date("2020-01-01"), as.Date("2020-01-10"),
                          "day"),
    geo_value = "pa",
    issue = 1,
    value = 1:10
  )

  bar <- data.frame(
    time_value = seq.Date(as.Date("2020-01-01"), as.Date("2020-01-10"),
                          "day"),
    geo_value = "pa",
    issue = 1,
    value = c(rep(0, 5), 1:5)
  )

  expected <- tibble(
    geo_value = "pa",
    value = 1
  )
  expect_equal(covidcast_cor(foo, bar, dt_x = -4),
               expected)
  expect_equal(covidcast_cor(foo, bar, dt_y = 4),
               expected)
})
