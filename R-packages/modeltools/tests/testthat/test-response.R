library(tibble)


test_that("make_response works for epiweeks", {
    df <- tibble(
        geo_value = "oh",
        time_value = seq.Date(as.Date("2021-01-01"), as.Date("2021-01-31"), "day"),
        `value-0:my_response` = 1:31
    )
    out <- make_response(df, "my_response", as.Date("2021-01-30"), "epiweek", 1:2)

    expect_equal(names(out),
                 c("geo_value", "time_value", "value-0:my_response",
                   "value+1:my_response", "value+2:my_response"))
    expect_equal(out$geo_value, df$geo_value)
    expect_equal(out$time_value, df$time_value)
    expect_equal(out$`value-0:my_response`, df$`value-0:my_response`)
    expect_equal(out$`value+1:my_response`,
                 c(seq(35, 196, 7), seq(199.5, 217, 3.5), NA))
    expect_equal(out$`value+2:my_response`,
                 c(seq(84, 196, 7), seq(199.5, 217, 3.5), rep(NA, 8)))
})


test_that("make_response works for days", {
    df <- tibble(
        geo_value = "oh",
        time_value = seq.Date(as.Date("2021-01-01"), as.Date("2021-01-31"), "day"),
        `value-0:my_response` = 1:31
    )
    out <- make_response(df, "my_response", as.Date("2021-01-30"), "day", 1:4)

    expect_equal(names(out),
                 c("geo_value", "time_value", "value-0:my_response",
                   "value+1:my_response", "value+2:my_response", "value+3:my_response", "value+4:my_response"))
    expect_equal(out$geo_value, df$geo_value)
    expect_equal(out$time_value, df$time_value)
    expect_equal(out$`value-0:my_response`, df$`value-0:my_response`)
    expect_equal(out$`value+1:my_response`, c(seq(2, 31), NA))
    expect_equal(out$`value+2:my_response`, c(seq(3, 31), rep(NA, 2)))
    expect_equal(out$`value+3:my_response`, c(seq(4, 31), rep(NA, 3)))
    expect_equal(out$`value+4:my_response`, c(seq(5, 31), rep(NA, 4)))
})
