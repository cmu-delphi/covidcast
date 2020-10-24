if(memoise::is.memoised(download_signal)) memoise::forget(download_signal)

t1 <- system.time(
  out1 <- suppressMessages(
    get_predictions(
      baseline_forecaster, "baby",
      tibble::tibble(
        data_source=c("jhu-csse", "usa-facts"),
        signal = c("deaths_incidence_num","confirmed_incidence_num"),
        start_day=lubridate::ymd("2020-09-15")),
        lubridate::ymd("2020-10-01"),"epiweek", 1L, "state", "mi"
    )
  )
)

test_that("we memoised the download_signals function", {
  expect_true(memoise::is.memoised(evalcast:::download_signal))
})

# test_that("our first call was actually cached",{
#   expect_true(memoise::has_cache(evalcast:::download_signal)(
#     "jhu-csse","deaths_incidence_num", start_day=lubridate::ymd("2020-09-15"),
#     end_day = lubridate::ymd("2020-10-01"), geo_type="state", geo_values="mi"))
# })
#
# test_that("our second call was actually cached",{
#   expect_true(memoise::has_cache(evalcast:::download_signal)(
#     "usa-facts","confirmed_incidence_num", start_day=lubridate::ymd("2020-09-15"),
#     end_day = lubridate::ymd("2020-10-01"), geo_type="state", geo_values="mi"))
# })


t2 <- system.time(
  suppressMessages(
    out2 <- get_predictions(
      baseline_forecaster, "baby",
      tibble::tibble(
        data_source=c("jhu-csse", "usa-facts"),
        signal = c("deaths_incidence_num","confirmed_incidence_num"),
        start_day=lubridate::ymd("2020-09-15")),
      lubridate::ymd("2020-10-01"),"epiweek", 1L, "state", "mi"
    )
  )
)

test_that("original and subsequent predictions return prediction cards", {
  # when prediction cards are given an S3 class, we can test those instead
  expect_identical(attributes(out1[[1]]), attributes(out2[[1]]))
})

test_that("predictions are the same for original and cached versions", {
  expect_equal(out1[[1]]$forecast_distribution[[1]],
               out2[[1]]$forecast_distribution[[1]])
})

test_that("we are caching COVIDCAST calls", {
  expect_true(t1[1] > t2[1])
})
