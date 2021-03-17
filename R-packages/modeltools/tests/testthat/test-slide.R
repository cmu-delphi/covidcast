library(tibble)

test_that("slide_by_geo works", {
    df <- tibble(
        geo_value = rep(c("a", "b"), each=5),
        time_value = rep(seq.Date(as.Date("2021-01-01"), as.Date("2021-01-05"), "day"), 2),
        value = 1:10
    )
    out <- slide_by_geo(df, ~ Sum(.x$value), n = 3, col_name = "summed_value")
    expect_equal(names(out), c("geo_value", "time_value", "value", "summed_value"))
    expect_equal(out$time_value, df$time_value)
    expect_equal(out$geo_value, df$geo_value)
    expect_equal(out$value, df$value)
    expect_equal(out$summed_value, c(1, 3, 6, 9, 12, 6, 13, 21, 24, 27))
})
