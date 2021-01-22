test_that("coverage calulations are accurate", {
  quantiles <- c(0.01, 0.05, 0.1, 0.3, 0.5, 0.7, 0.9, 0.95, 0.99)
  value <- c(1, 5, 10, 30, 50, 70, 90, 95, 99)

  expect_false(interval_coverage(0.8)(quantiles, value, 9.9))
  expect_true(interval_coverage(0.8)(quantiles, value, 10.1))
  expect_true(interval_coverage(0.8)(quantiles, value, 89.9))
  expect_false(interval_coverage(0.8)(quantiles, value, 90.1))

  expect_false(interval_coverage(0.4)(quantiles, value, 29.9))
  expect_true(interval_coverage(0.4)(quantiles, value, 30.1))
  expect_true(interval_coverage(0.4)(quantiles, value, 69.9))
  expect_false(interval_coverage(0.4)(quantiles, value, 70.1))
})

test_that("coverage calculations can handle point estimate", {
  quantiles <- c(0.01, 0.05, 0.1, 0.3, 0.5, 0.7, 0.9, 0.95, 0.99, NA)
  value <- c(1, 5, 10, 30, 50, 70, 90, 95, 99, 20)

  expect_false(interval_coverage(0.8)(quantiles, value, 9.9))
})

test_that("absolute_error calculations are correct", {
  quantiles <- c(0.01, 0.05, 0.1, 0.3, 0.5, 0.7, 0.9, 0.95, 0.99)
  value <- c(1, 5, 10, 30, 50, 70, 90, 95, 99)

  expect_equal(absolute_error(quantiles, value, 35), 15)
})

test_that("absolute_error calculations prefer point estimate over median", {
  quantiles <- c(0.01, 0.05, 0.1, 0.3, 0.5, 0.7, 0.9, 0.95, 0.99, NA)
  value <- c(1, 5, 10, 30, 50, 70, 90, 95, 99, 30)

  expect_equal(absolute_error(quantiles, value, 35), 5)
})

test_that("WIS is correct", {
  l <- 1:11
  u <- 23:13
  w <- covidhub_probs()[1:11]
  wis <- (sum(w * (u - l) + (l - 10) * (10 < l) + (10 - u) * (10 > u)) + 1) / 11.5
  expect_equal(weighted_interval_score(covidhub_probs(), 1:23, 10), wis)
})

test_that("Warning when appropriate quantiles don't exist", {
  quantiles <- c(0.01, 0.05, 0.1, 0.3, 0.5, 0.7, 0.9, 0.95, 0.99)
  value <- c(1, 5, 10, 30, 50, 70, 90, 95, 99)

  expect_warning(interval_coverage(0.75)(quantiles, value, 9.9))
})

test_that("Error when duplicate quantiles", {
  quantiles <- c(0.01, 0.05, 0.1, 0.3, 0.3, 0.7, 0.9, 0.95, 0.99)
  value <- c(1, 5, 10, 30, 50, 70, 90, 95, 99)

  expect_error(interval_coverage(0.8)(quantiles, value, 9.9),
               regexp = "quantiles")
})

test_that("Error when values is the wrong length", {
  quantiles <- c(0.01, 0.05, 0.1, 0.3, 0.5, 0.7, 0.9, 0.95, 0.99)
  value <- c(1, 5, 10, 30, 50, 70, 95, 99)
  actual_value <- c(2)

  expect_error(absolute_error(quantiles, value, actual_value),
               regexp = "values")
})

test_that("Error when actual_value is the wrong length", {
  quantiles <- c(0.01, 0.05, 0.1, 0.3, 0.5, 0.7, 0.9, 0.95, 0.99)
  value <- c(1, 5, 10, 30, 50, 70, 90, 95, 99)
  actual_value <- c(2, 2)

  expect_error(absolute_error(quantiles, value, actual_value),
               regexp = "actual_value")
})
