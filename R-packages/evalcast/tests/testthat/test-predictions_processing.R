library(dplyr)

test_that("target field processing works as expected", {
  preds_in <- tibble(
    target = c("1 wk ahead inc death", "3 wk ahead cum death", "2 day ahead inc case", "1 day ahead inc hosp", "1 wk ahead inc other")
  )
  
  preds_out_removed <- process_target(preds_in, remove = TRUE)
  preds_out_notremoved <- process_target(preds_in, remove = FALSE)
  
  expected <- tibble(
    target = c("1 wk ahead inc death", "3 wk ahead cum death", "2 day ahead inc case", "1 day ahead inc hosp", "1 wk ahead inc other"),
    ahead = as.integer(c(1, 3, 2, 1, 1)),
    incidence_period = c("epiweek", "epiweek", "day", "day", "epiweek"),
    inc = c("incidence", "cumulative", "incidence", "incidence", "incidence"),
    response = c("deaths", "deaths", "confirmed", "hosp", "drop"),
    data_source = c("jhu-csse", "jhu-csse", "jhu-csse", "hhs", "drop"),
    signal = c("deaths_incidence_num", "deaths_cumulative_num", "confirmed_incidence_num", "confirmed_admissions_covid_1d", "drop")
  )
  
  expect_identical(preds_out_notremoved, expected)
  expect_identical(preds_out_removed, expected %>% select(-target))
})


test_that("valid predictions filter works as expected", {
  preds_in <- tibble(
    target = c("1 wk ahead inc death", "3 wk ahead cum death", "2 day ahead inc case", "1 day ahead inc hosp", "1 wk ahead inc other"),
    ahead = as.integer(c(1, 3, 2, 1, 1)),
    incidence_period = c("epiweek", "epiweek", "day", "day", "epiweek"),
    inc = c("incidence", "cumulative", "incidence", "incidence", "incidence"),
    response = c("deaths", "deaths", "confirmed", "hosp", "drop"),
    data_source = c("jhu-csse", "jhu-csse", "jhu-csse", "hhs", "drop"),
    signal = c("deaths_incidence_num", "deaths_cumulative_num", "confirmed_incidence_num", "confirmed_admissions_covid_1d", "drop"),
    type = "quantile"
  )
  
  preds_out <- filter_predictions(
    preds_in,
    forecast_type = c("point", "quantile"),
    incidence_period = "day",
    signal = c("deaths_incidence_num", "deaths_cumulative_num", "confirmed_incidence_num", "confirmed_admissions_covid_1d")
  )
  preds_out_empty <- filter_predictions(
    preds_in,
    forecast_type = c("point", "quantile"),
    incidence_period = "day",
    signal = NULL
  )
  
  expected <- tibble(
    target = c("2 day ahead inc case", "1 day ahead inc hosp"),
    ahead = as.integer(c(2, 1)),
    incidence_period = c("day", "day"),
    inc = c("incidence", "incidence"),
    response = c("confirmed", "hosp"),
    data_source = c("jhu-csse", "hhs"),
    signal = c("confirmed_incidence_num", "confirmed_admissions_covid_1d"),
    type = "quantile"
  )
  
  expect_identical(preds_out, expected)
  expect_identical(preds_out_empty, expected[FALSE, ])
})

test_that("desired prediction card columns are kept and reordered", {
  preds_in <- tibble(
    target_end_date = NA,
    ahead = as.integer(c(1, 3, 2, 1, 1)),
    incidence_period = c("epiweek", "epiweek", "day", "day", "epiweek"),
    inc = c("incidence", "cumulative", "incidence", "incidence", "incidence"),
    response = c("deaths", "deaths", "confirmed", "hosp", "drop"),
    data_source = c("jhu-csse", "jhu-csse", "jhu-csse", "hhs", "drop"),
    quantile = NA,
    signal = c("deaths_incidence_num", "deaths_cumulative_num", "confirmed_incidence_num", "confirmed_admissions_covid_1d", "drop"),
    type = "quantile",
    location = NA,
    value = NA,
    forecaster = NA,
    forecast_date = NA
  )
  
  preds_out <- select_pcard_cols(preds_in)
  
  expected <- tibble(
    ahead = as.integer(c(1, 3, 2, 1, 1)),
    location = NA,
    quantile = NA,
    value = NA,
    forecaster = NA,
    forecast_date = NA,
    data_source = c("jhu-csse", "jhu-csse", "jhu-csse", "hhs", "drop"),
    signal = c("deaths_incidence_num", "deaths_cumulative_num", "confirmed_incidence_num", "confirmed_admissions_covid_1d", "drop"),
    target_end_date = NA,
    incidence_period = c("epiweek", "epiweek", "day", "day", "epiweek"),
  )
  
  expect_identical(preds_out, expected)
})


test_that("location field is renamed and reformated in place", {
  preds_in <- tibble(
    ahead = as.integer(c(1, 3, 2, 1, 1, 6)),
    location = c("05", "60", "TX", "01011", "01039", "California"),
    quantile = NA,
    value = NA
  )
  
  preds_out <- location_2_geo_value(preds_in)
  preds_out_lower <- location_2_geo_value(preds_in, tolower)
  
  expected <- tibble(
    ahead = as.integer(c(1, 3, 2, 1, 1, 6)),
    geo_value = c("ar", "as", NA, "01011", "01039", "California"),
    quantile = NA,
    value = NA
  )
  
  expect_identical(preds_out, expected)
  expect_identical(preds_out_lower, expected %>% mutate(geo_value = tolower(geo_value)))
})
