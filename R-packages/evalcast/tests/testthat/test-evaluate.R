library(mockery)

# Create a fake result from the covidcast API as returned by the `evalcast::download_signals()`
# function.
create_fake_downloaded_signal <- function(geo_value) {
  tibble(data_source = "jhu-csse",
         signal =  c("deaths_incidence_num", "confirmed_incidence_num"),
         geo_value = geo_value,
         time_value = as.Date("2020-01-01"),
         issue = as.Date("2020-01-02"),
         lag = 1L,
         value = c(1, 2),
         stderr = c(0.1, 0.2),
         sample_size = c(2, 2)) 
}

create_pcard <- function(card) {
  class(card) <- c("predictions_cards", class(card))
  return(card)
}

test_that("backfill_buffer works", {
  skip("To be revised...")
  mock_download_signal <- mock(create_fake_downloaded_signal("al"), cycle=TRUE)
  with_mock(download_signal = mock_download_signal, {
    pcard <- create_pcard(tibble(
      ahead = 1,
      geo_value = rep(c("al", "wy"), each=3),
      quantile = c(0.1, 0.5, 0.9, 0.1, 0.5, 0.9),
      value = seq(1, 6),
      forecaster = "a",
      forecast_date = rep(as.Date(c("2020-01-02", "2020-01-03")), each=3),
      data_source = "source",
      signal = "signal",
      target_end_date = as.Date("2020-01-05"),
      incidence_period = "epiweek"
    ))
    expect_warning(evaluate_predictions(pcard, backfill_buffer = 4),
                "backfill_buffer")
    # waited long enough should have no error:
    evaluate_predictions(pcard, backfill_buffer = 2)
  })
})