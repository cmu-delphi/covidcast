test_that("response df for baseline works for covidcast_signals()", {
  skip("To be revised...")
  
  signals = tibble(data_source = "jhu-csse", signal = "confirmed_incidence_num")
  
  good_df <- download_signals(data_source = "jhu-csse",
                              signal = "confirmed_incidence_num",
                              start_day = "2020-12-01", 
                              end_day = "2020-12-14",
                              geo_type = "state",
                              geo_values = "ny")
  long_df <- download_signals(data_source = "jhu-csse",
                              signal = "confirmed_incidence_num",
                              start_day = "2020-12-01", 
                              end_day = "2020-12-14",
                              geo_type = "state",
                              geo_values = "ny",
                              signal_aggregation_dt = c(0,-1),
                              signal_aggregation = "long")
  long1 <- get_response_df(long_df, signals)
  expect_identical(good_df, long1)
  
  wide1 <- covidcast::covidcast_wider(long_df)
  wide1 <- get_response_df(wide1, signals)
  expect_equal(nrow(wide1), nrow(good_df))
  expect_true(all(abs(wide1$dt) < 1e-8))  
  
  listy <- download_signals(data_source = "jhu-csse",
                            signal = c("confirmed_incidence_num","deaths_incidence_num"),
                            start_day = "2020-12-01", 
                            end_day = "2020-12-14",
                            geo_type = "state",
                            geo_values = "ny",
                            signal_aggregation = "list")
  list_df <- get_response_df(listy, signals)
  expect_equal(nrow(list_df), nrow(good_df))
  expect_true(all(list_df$data_source == signals$data_source))
  expect_true(all(list_df$signal == signals$signal))
  
  ugly <- download_signals(data_source = "jhu-csse",
                           signal = "confirmed_incidence_num",
                           start_day = "2020-12-01", 
                           end_day = "2020-12-14",
                           geo_type = "state",
                           geo_values = "ny",
                           signal_aggregation_dt = c(1,-1),
                           signal_aggregation = "long")
  expect_error(get_response_df(ugly, signals))
})
