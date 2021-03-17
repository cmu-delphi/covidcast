library(tibble)

test_that("slide_by_geo works", {
    df <- tibble(
        geo_value = rep(c("a", "b"), each=5),
        time_value = rep(seq.Date(as.Date("2021-01-01"), as.Date("2021-01-05"), "day"), 2),
        value = 1:10
    )
    out <- slide_by_geo(df, ~ Sum(.x$value), n = 3, col_name = "summed_value")
    expect_equal(names(out), c("geo_value", "time_value", "value", "summed_value"))
    expect_equal(out$geo_value, df$geo_value)
    expect_equal(out$time_value, df$time_value)
    expect_equal(out$value, df$value)
    expect_equal(out$summed_value, c(1, 3, 6, 9, 12, 6, 13, 21, 24, 27))
})


test_that("slide_by_geo works with interleaved geo_values", {
    df <- tibble(
        geo_value = rep(c("a", "b"), 5),
        time_value = rep(seq.Date(as.Date("2021-01-01"), as.Date("2021-01-05"), "day"), each=2),
        # keep the individual rows the same as the previous test so the summed values are identical
        value = c(1, 6, 2, 7, 3, 8, 4, 9, 5, 10)
    )
    out <- slide_by_geo(df, ~ Sum(.x$value), n = 3, col_name = "summed_value")
    expect_equal(names(out), c("geo_value", "time_value", "value", "summed_value"))
    expect_equal(out$geo_value, rep(c("a", "b"), each=5))
    expect_equal(out$time_value,
                 rep(seq.Date(as.Date("2021-01-01"), as.Date("2021-01-05"), "day"), 2))
    expect_equal(out$value, 1:10)
    expect_equal(out$summed_value, c(1, 3, 6, 9, 12, 6, 13, 21, 24, 27))
})

test_that("slide_by_geo overwrites column when specified", {
    df <- tibble(
        geo_value = rep(c("a", "b"), each=5),
        time_value = rep(seq.Date(as.Date("2021-01-01"), as.Date("2021-01-05"), "day"), 2),
        value = 1:10
    )
    out <- slide_by_geo(df, ~ Sum(.x$value), n = 3, col_name = "value")
    expect_equal(names(out), c("geo_value", "time_value", "value"))
    expect_equal(out$geo_value, df$geo_value)
    expect_equal(out$time_value, df$time_value)
    expect_equal(out$value, c(1, 3, 6, 9, 12, 6, 13, 21, 24, 27))
})


test_that("slide_by_geo works with positive shift", {
    df <- tibble(
        geo_value = rep(c("a", "b"), each=5),
        time_value = rep(seq.Date(as.Date("2021-01-01"), as.Date("2021-01-05"), "day"), 2),
        value = 1:10
    )
    out <- slide_by_geo(df, ~ Sum(.x$value), n = 2, shift = 1, col_name = "summed_value")
    expect_equal(names(out), c("geo_value", "time_value", "value", "summed_value"))
    expect_equal(out$time_value, df$time_value)
    expect_equal(out$geo_value, df$geo_value)
    expect_equal(out$value, df$value)
    expect_equal(out$summed_value, c(3, 5, 7, 9, 5, 13, 15, 17, 19, 10))
})

test_that("slide_by_geo works with negative shift", {
    df <- tibble(
        geo_value = rep(c("a", "b"), each=5),
        time_value = rep(seq.Date(as.Date("2021-01-01"), as.Date("2021-01-05"), "day"), 2),
        value = 1:10
    )
    out <- slide_by_geo(df, ~ Sum(.x$value), n = 2, shift = -1, col_name = "summed_value")
    expect_equal(names(out), c("geo_value", "time_value", "value", "summed_value"))
    expect_equal(out$time_value, df$time_value)
    expect_equal(out$geo_value, df$geo_value)
    expect_equal(out$value, df$value)
    expect_equal(out$summed_value, c(0, 1, 3, 5, 7, 0, 6, 13, 15, 17))
})

test_that("slide_by_geo works with positive shift past current value", {
    df <- tibble(
        geo_value = rep(c("a", "b"), each=10),
        time_value = rep(seq.Date(as.Date("2021-01-01"), as.Date("2021-01-10"), "day"), 2),
        value = 1:20
    )
    out <- slide_by_geo(df, ~ Sum(.x$value), n = 2, shift = 3, col_name = "summed_value")
    expect_equal(names(out), c("geo_value", "time_value", "value", "summed_value"))
    expect_equal(out$time_value, df$time_value)
    expect_equal(out$geo_value, df$geo_value)
    expect_equal(out$value, df$value)
    expect_equal(out$summed_value, c(7, 9, 11, 13, 15, 17, 19, 10, 0, 0,
                                     27, 29, 31, 33, 35, 37, 39, 20, 0, 0))
})
