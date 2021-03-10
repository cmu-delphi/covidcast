create_fake_downloaded_signal <- function() {
  t1 <- tibble(data_source = "jhu-csse",
               signal =  c("deaths_incidence_num"),
               geo_value = "mi",
               time_value = seq(as.Date("2020-01-01"), 
                                as.Date("2020-02-01"), by = 1),
               issue = as.Date("2020-02-02"),
               lag = 1L,
               value = 1:32,
               stderr = .1,
               sample_size = 2) 
  t2 <- tibble(data_source = "jhu-csse",
               signal =  c("deaths_incidence_num"),
               geo_value = "pa",
               time_value = seq(as.Date("2020-01-05"), 
                                as.Date("2020-02-01"), by = 1),
               issue = as.Date("2020-02-02"),
               lag = 1L,
               value = 5:32,
               stderr = .1,
               sample_size = 2) 
  bind_rows(t1, t2)
}


test_that("sum to epiweek returns the right df", {
  response_df <- create_fake_downloaded_signal()
  
  summed <- sum_to_epiweek(response_df)
  expect_equal(nrow(summed), 9L)
  expect_equal(summed[0,], 
               tibble::tibble(
                 geo_value = character(0),
                 time_value = lubridate::ymd(),
                 value = integer(0)))
  episums <- c(10, rep(c(56,105,154,203), 2))
  expect_equal(summed$value, episums)
})
