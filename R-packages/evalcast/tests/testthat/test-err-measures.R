test_that("coverage calulations are accurate", {
  quantiles = c(0.01, 0.05, 0.1, 0.3, 0.5, 0.7, 0.9, 0.95, 0.99)
  value = c(1, 5, 10, 30, 50, 70, 90, 95, 99)
  
  expect_false(interval_coverage(0.8)(quantiles, value, 9.9))
  expect_true(interval_coverage(0.8)(quantiles, value, 10.1))
  expect_true(interval_coverage(0.8)(quantiles, value, 89.9))
  expect_false(interval_coverage(0.8)(quantiles, value, 90.1))
  
  expect_false(interval_coverage(0.4)(quantiles, value, 29.9))
  expect_true(interval_coverage(0.4)(quantiles, value, 30.1))
  expect_true(interval_coverage(0.4)(quantiles, value, 69.9))
  expect_false(interval_coverage(0.4)(quantiles, value, 70.1))
})

test_that("Warning when appropriate quantiles don't exist", {
  quantiles = c(0.01, 0.05, 0.1, 0.3, 0.5, 0.7, 0.9, 0.95, 0.99)
  value = c(1, 5, 10, 30, 50, 70, 90, 95, 99)
  
  expect_warning(interval_coverage(0.75)(quantiles, value, 9.9))
})

test_that("Warning when duplicate quantiles", {
  quantiles = c(0.01, 0.05, 0.1, 0.3, 0.3, 0.7, 0.9, 0.95, 0.99)
  value = c(1, 5, 10, 30, 50, 70, 90, 95, 99)
  
  expect_warning(interval_coverage(0.8)(quantiles, value, 9.9))
})
