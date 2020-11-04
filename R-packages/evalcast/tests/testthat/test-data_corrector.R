signals = tibble::tibble(
  data_source=c("jhu-csse", "usa-facts"),
  signal = c("deaths_incidence_num","confirmed_incidence_num"),
  start_day=lubridate::ymd("2020-09-15"))

df <- suppressMessages(signals %>%
  pmap_dfr(function(...) {
    args <- list(...)
    download_signal(data_source=args$data_source,
                    signal = args$signal,
                    start_day = args$start_day,
                    end_day = lubridate::ymd("2020-10-01"),
                    as_of = lubridate::ymd("2020-10-01"),
                    geo_type = "state",
                    geo_values = c("mi","mo"))
  }))

fn <- function(x) dplyr::mutate(x, corrected=value)

out1 <- suppressMessages(
  get_predictions(
    baseline_forecaster, "baby",
    tibble::tibble(
      data_source=c("jhu-csse", "usa-facts"),
      signal = c("deaths_incidence_num","confirmed_incidence_num"),
      start_day=lubridate::ymd("2020-09-15")),
    lubridate::ymd("2020-10-01"),"epiweek", 1L, "state", "mi",
    apply_corrections = fn
  )
)

test_that("data_corrector returns the right format", {
  expect_equal(df,
               data_corrector(
                 df, function(x) dplyr::mutate(x, corrected=value)
                 )
               )
})

test_that("data_corrector handles partial corrections", {
  expect_equal(df,
               data_corrector(
                 df, function(x) {
                   x = filter(x, location=="29")
                   dplyr::mutate(x, corrected=value)
                 }
               )
  )
})

test_that("data_corrector refuses to delete important variables", {
  expect_error(
    data_corrector(df,
                   function(x) {
                     x %>% mutate(corrected=value) %>%
                       select(c("location","corrected"))
                     }
               )
  )
})

test_that("get_predictions has the correction function as an attribute",{
  expect_identical(attributes(out1[[1]])$corrections_applied, fn)
})
