test_that("check_valid_coverage_probs finds mistakes", {
  tib <- tibble(fct = rep(letters[1:2], each = 5),
                quantile = rep(c(.1,.25,.5,.75,.9), times=2))
  expect_true(check_valid_coverage_probs(tib, "fct"))
  tib2 <- tib
  tib2$quantile[2] <- .2
  expect_false(check_valid_coverage_probs(tib2, "fct"))
  tib2 <- tib
  tib2$quantile <- tib2$quantile[c(2,1,3:10)]
  expect_false(check_valid_coverage_probs(tib2, "fct"))
  tib2 <- tib
  tib2$quantile <- rev(tib2$quantile)
  expect_false(check_valid_coverage_probs(tib2, "fct"))
  big_tib <- bind_rows(tib, tib, .id="tib")
  expect_true(check_valid_coverage_probs(big_tib, c("fct","tib")))
})

test_that("averaging_checks finds mistakes", {
  tib <- tibble(fct = rep(letters[1:2], each = 5),
                quantile = rep(c(.1,.25,.5,.75,.9), times=2))
  big_tib <- bind_rows(tib, tib, .id="tib")
  expect_identical(averaging_checks(big_tib, "tib", "fct"), big_tib)
  big_tib2 <- big_tib
  big_tib2$quantile[8] <- .2
  expect_warning(averaging_checks(big_tib2, "tib", "fct"))
  big_tib2$quantile[12] <- .2
  expect_error(averaging_checks(big_tib2, "tib", "fct"))
  tib2 <- tib
  tib2$quantile <- rev(tib2$quantile)
  big_tib2 <- bind_rows(tib, tib2, .id="tib")
  expect_warning(ch <- averaging_checks(big_tib2, "tib", "fct"))
  expect_equal(nrow(ch), 10L)
  biggest_tib <- bind_rows(big_tib, big_tib, .id = "bigger")
  expect_identical(averaging_checks(biggest_tib, "bigger", c("tib","fct")),
                   biggest_tib)
  expect_identical(averaging_checks(biggest_tib, c("bigger","tib"), "fct"),
                   biggest_tib)
})

