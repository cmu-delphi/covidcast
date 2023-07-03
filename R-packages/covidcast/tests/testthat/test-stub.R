library(httptest)
library(mockery)
library(MMWRweek)
library(dplyr)

with_mock_api({
  test_that("with_mock_api supports httr::rerequest", {
    # the first call to covidcast_meta is routed through httr::request
    mdf1 <- covidcast_meta()
    
    # the second call goes through httr::rerequest
    mdf2 <- covidcast_meta()
    expect_equals(nrow(mdf2), 3)
  })
})

with_mock_api({
  test_that("mockery::stub takes effect even when httptest::with_mock_api is active", {
    mock_identical <- mock(TRUE)
    stub(.request_meta, "identical", mock_identical)
    tryCatch(
      covidcast_meta(),
      finally={
        print(mock_calls(mock_identical))
        expect_called(mock_identical, 1)
      })
  })
})
