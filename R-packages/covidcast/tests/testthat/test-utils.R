# Internal utility functions.

test_that("latest_issue gives only the latest issue", {
  foo <- data.frame(
    geo_value = c(rep("pa", 3), rep("tx", 3)),
    issue = c(3, 2, 1, 1, 2, 3),
    time_value = 1,
    value = c(4, 5, 6, 7, 8, 9))

  latest <- data.frame(
    geo_value = c("pa", "tx"),
    issue = 3,
    time_value = 1,
    value = c(4, 9))

  expect_equal(latest_issue(foo), latest)
})

test_that("earliest_issue gives only the earliest issue", {
  foo <- data.frame(
    geo_value = c(rep("pa", 3), rep("tx", 3)),
    issue = c(3, 2, 1, 1, 2, 3),
    time_value = 1,
    value = c(4, 5, 6, 7, 8, 9))

  earliest <- data.frame(
    geo_value = c("pa", "tx"),
    issue = 1,
    time_value = 1,
    value = c(6, 7))

  expect_equal(earliest_issue(foo), earliest)
})
