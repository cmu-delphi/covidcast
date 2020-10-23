if(memoise::is.memoised(download_signal)) memoise::forget(download_signal)

t1 = system.time(
  suppressMessages(
    get_predictions(
      baseline_forecaster, "baby",
      tibble(
        data_source=c("jhu-csse", "usa-facts"),
        signal = c("deaths_incidence_num","confirmed_incidence_num"),
        start_day=lubridate::ymd("2020-09-15")),
        lubridate::ymd("2020-10-01"),"epiweek", 1L, "state", "mi"
    )
  )
)

t2 = system.time(
  suppressMessages(
    get_predictions(
      baseline_forecaster, "baby",
      tibble(
        data_source=c("jhu-csse", "usa-facts"),
        signal = c("deaths_incidence_num","confirmed_incidence_num"),
        start_day=lubridate::ymd("2020-09-15")),
      lubridate::ymd("2020-10-01"),"epiweek", 1L, "state", "mi"
    )
  )
)


test_that("we are caching COVIDCAST calls", {
  expect_true(t1[1] > t2[1])
})
