test_that("character columns work", {
  tib <- tibble(data_source = "jhu", signal = c("conf","death"))
  fd <- "2020-01-01"
  expect_identical(signal_listcols(tib, fd), 
                   tib %>% mutate(geo_values = "*", geo_type = "county"))
  expect_identical(signal_listcols(tib %>% mutate(geo_values = "mi"), fd), 
                   tib %>% mutate(geo_values = "mi", geo_type = "county"))
  expect_identical(signal_listcols(tib %>% mutate(geo_type = "state"), fd), 
                   tib %>% mutate(geo_type = "state", geo_values = "*"))
})


test_that("fun columns work", {
  tib <- tibble(data_source = "jhu", signal = c("conf","death"), 
                start_day = list(function(x) x))
  fd <- "2020-01-01"
  out <- tib %>% mutate(start_day = fd, geo_values = "*", geo_type = "county")
  expect_identical(signal_listcols(tib, fd), out)
  
  tib$start_day = list(function(x) x, function(x) as.character(as.Date(x)+2))
  out$start_day[2] = "2020-01-03"
  expect_identical(signal_listcols(tib, fd), out)
})
