library(testthat)
library(vdiffr)

# Contexts are no longer required or recommended in testthat, but vdiffr still
# wants one to place the figure files correctly. See
# https://github.com/r-lib/vdiffr/issues/71
context("plot")

test_that("line graphs", {
  fake_data <- structure(data.frame(
    value = 1:10,
    time_value = seq.Date(as.Date("2020-01-01"), as.Date("2020-01-10"),
                          by = "day"),
    issue = as.Date("2020-02-01"),
    geo_value = "pa",
    stderr = 0.5),
  class = c("covidcast_signal", "data.frame")
  )

  expect_doppelganger("simple line graph", plot(
    fake_data,
    plot_type = "line",
    range = c(-1, 11),
    title = "Penguins!",
    line_params = list(
      xlab = "Day",
      ylab = "Penguinocity",
      stderr_bands = TRUE,
      stderr_alpha = 0.3
    )
  ))
})
