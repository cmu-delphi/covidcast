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

## These next few wis tests come from github.com/epiforecasts/scoringutils
## They ensure that we match the covidHubUtils calculations 
## (implemented be scoringutils::interval_score())
test_that("wis works, median only", {
  y <- c(1, -15, 22)
  m <- c(1, 2, 3)
  quantile_probs <- 0.5
  
  actual <- map2_dbl(y, m, weighted_interval_score, quantile = .5)
  expected <- abs(y - m)
  
  
  expect_identical(actual, expected)
})

test_that("wis works, 1 interval only", {
  y <- c(1, -15, 22)
  alpha <- .5
  
  test_data <- tibble(loc = rep(letters[1:3], each = 2), 
                   value = c(0,2,1,2,0,3),
                   quantile = rep(c(.25,.75), times = 3),
                   actual = rep(c(1,-15,22), each = 2))
  
  ours <- test_data %>% group_by(loc) %>% 
    summarise(wis = weighted_interval_score(quantile, value, actual)) %>%
    pull()
  
  expected <- (c(2,2,3) - c(0,1,0)) * (alpha/2) + c(0, 1-(-15), 22-3)
  
  expect_identical(ours, expected)
})


test_that("wis works, 1 interval and median", {
  test_data <- data.frame(actual =   rep(c(1, -15, 22), times = 3),
                          quantile = rep(c(0.25, 0.5, 0.75), each = 3),
                          value = c(c(0, 1, 0), c(1, 2, 3), c(2, 2, 3)),
                          loc = rep(letters[1:3], times = 3))
  
  ours <- test_data %>% group_by(loc) %>% 
    summarise(wis = weighted_interval_score(quantile, value, actual)) %>%
    pull()
  
  test_data <- data.frame(true_value =   rep(c(1, -15, 22), times = 3),
                          quantile = rep(c(0.25, 0.5, 0.75), each = 3),
                          prediction = c(c(0, 1, 0), c(1, 2, 3), c(2, 2, 3)),
                          model = c("model1"),
                          date = rep(1:3, times = 3))

  eval <- scoringutils::eval_forecasts(
    test_data,
    interval_score_arguments = list(count_median_twice = FALSE))
  theirs <- eval$interval_score
  
  expected <- c(1/3, 16.5, 19.5)
  expect_identical(ours, expected)
  expect_identical(theirs, expected) # will fail if their package changes
})



test_that("over/under prediction work, 1 interval and median", {
  
  test_data <- data.frame(actual =   rep(c(1, -15, 22), times = 4),
                          quantile = rep(c(0.25, 0.5, 0.75, NA), each = 3),
                          value = c(c(0, 1, 0), c(1, 2, 3),
                                    c(2, 2, 3), c(0.9, 2.1, 3.1)),
                          loc = rep(letters[1:3], times = 4))
  
  over <- test_data %>% group_by(loc) %>% 
    summarise(over = overprediction(quantile, value, actual)) %>%
    pull()
  
  under <- test_data %>% group_by(loc) %>% 
    summarise(under = underprediction(quantile, value, actual)) %>%
    pull()
  
  expect_identical(over, c(0, 16 + 1/3, 0))
  expect_identical(under, c(0, 0, 19))
  
})

test_that("wis over/under works, 2 intervals and median", {
  
  test_data <- data.frame(true_value =   rep(c(1, -15, 22), times = 6),
                          quantile = rep(c(0.1, 0.25, 0.5, 0.75, 0.9, NA),
                                         each = 3),
                          prediction = c(c(-1, -2, -2), c(0, 1, 0), c(1, 2, 3),
                                         c(2, 2, 3), c(3, 4, 4),
                                         c(0.9, 1.9, 2.9)),
                          model = c("model1"),
                          date = rep(1:3, times = 6))
  
  eval <- scoringutils::eval_forecasts(
    test_data,
    interval_score_arguments = list(count_median_twice = FALSE))
   
  wis <- test_data %>% group_by(date) %>% 
    summarise(wis = weighted_interval_score(quantile, prediction, true_value)) %>%
    pull()
  over <- test_data %>% group_by(date) %>% 
    summarise(wis = overprediction(quantile, prediction, true_value)) %>%
    pull()
  under <- test_data %>% group_by(date) %>% 
    summarise(wis = underprediction(quantile, prediction, true_value)) %>%
    pull()
  
  expect_identical(wis, c(.36, 15.34, 19.14))
  expect_identical(over, c(0,15,0))
  expect_identical(under, c(0,0,18.6))
  
  expect_identical(wis, eval$interval_score)
  expect_identical(over, eval$overprediction)
  expect_identical(under, eval$underprediction)
})


