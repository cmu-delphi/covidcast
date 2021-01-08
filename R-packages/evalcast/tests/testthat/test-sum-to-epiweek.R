test_that("sum to epiweek returns the right df", {
  response_df <- suppressMessages(covidcast::covidcast_signal(
    "jhu-csse","deaths_incidence_num", "2020-07-03", "2020-10-10", 
    geo_type = "state"))
  
  summed <- sum_to_epiweek(response_df)
  expect_equal(nrow(summed), 780L)
  expect_equal(summed[0,], 
               tibble::tibble(
                 geo_value = character(0),
                 time_value = lubridate::ymd(),
                 value = integer(0)))
})
