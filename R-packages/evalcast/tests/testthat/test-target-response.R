test_that("grabbing response works", {
  
  # when called, signals, incidence_period, ahead, and geo_type are unique
  # by design. Forecast_dates and locations should be vectors
  signals <- tibble(data_source="jhu-csse",signal="deaths_incidence_num")
  forecast_dates <-  c("2020-11-01","2020-11-05")
  incidence_period <- "epiweek"
  ahead <- 1L
  geo_type <- "state"
  locations <- "01"
  out <- get_target_response(signals,forecast_dates,incidence_period,ahead,
                             geo_type,locations)
  expect_equal(nrow(out), 2)
  expect_identical(out[0,] %>% select(-actual), 
                   empty_actual() %>% select(-actual))
  forecast_dates = c(forecast_dates[1], 
                     as.character(lubridate::ymd(Sys.Date())+5))
  expect_warning(out <- get_target_response(
    signals,forecast_dates,incidence_period,ahead,geo_type,locations))
  expect_equal(nrow(out), 1)
  expect_identical(out[0,] %>% select(-actual), 
                   empty_actual() %>% select(-actual))
  forecast_dates = as.character(lubridate::ymd(Sys.Date())+5)
  expect_warning(out <- get_target_response(
    signals,forecast_dates,incidence_period,ahead,geo_type,locations))
  expect_identical(out, empty_actual())
})
