test_that("character columns work", {
  tib <- tibble(data_source = "jhu", signal = c("conf","death"))
  fd <- as.Date("2020-01-01")
  expect_identical(signal_listcols(tib, fd), 
                   tib %>% 
                     mutate(as_of = fd, geo_values = "*", geo_type = "county"))
  expect_identical(signal_listcols(tib %>% mutate(geo_values = "mi"), fd), 
                   tib %>% mutate(geo_values = "mi", 
                                  as_of = fd, geo_type = "county"))
  expect_identical(signal_listcols(tib %>% mutate(geo_type = "state"), fd), 
                   tib %>% mutate(geo_type = "state", as_of = fd, geo_values = "*"))
})


test_that("fun columns work", {
  tib <- tibble(data_source = "jhu", signal = c("conf","death"), 
                start_day = list(function(x) x))
  fd <- as.Date("2020-01-01")
  out <- tib %>% mutate(start_day = fd, as_of = fd, 
                        geo_values = "*", geo_type = "county")
  expect_identical(signal_listcols(tib, fd), out)
  
  tib$start_day = list(function(x) x, function(x) as.character(as.Date(x)+2))
  out$start_day[2] = "2020-01-03"
  expect_identical(signal_listcols(tib, fd), out)
})
