test_that("download_signals() handles long, wide, list", {
  data_source = c("jhu-csse","usa-facts")
  signal = c("deaths_incidence_num","confirmed_incidence_num")
  start_day = c("2020-10-01")
  end_day = "2020-10-02"
  as_of = "2020-10-31"
  geo_type = "state"
  geo_values = c("mi","fl")
  l1 <- download_signals(data_source=data_source, signal=signal, start_day=start_day,
                   end_day=end_day, as_of=as_of, geo_type=geo_type, 
                   geo_values=geo_values)
  expect_equal(class(l1), c("covidcast_signal_long","data.frame"))
  l2 <- download_signals(data_source=data_source, signal=signal, start_day=start_day,
                         end_day=end_day, as_of=as_of, geo_type=geo_type, 
                         geo_values=geo_values, signal_aggregation_dt = c(-1,1))
  expect_equal(nrow(l2), 16L)
  l3 <- download_signals(data_source=data_source, signal=signal, start_day=start_day,
                         end_day=end_day, as_of=as_of, geo_type=geo_type, 
                         geo_values=geo_values, signal_aggregation_dt = c(-1,1),
                         signal_aggregation = "list")
  expect_equal(length(l3), 2L)
  expect_error(download_signals(data_source=data_source, signal=signal, start_day=start_day,
                                end_day=end_day, as_of=as_of, geo_type=geo_type, 
                                geo_values=geo_values, 
                                signal_aggregation_dt = list(c(-1,1),1,1:3),
                                signal_aggregation = "wide"))
})
