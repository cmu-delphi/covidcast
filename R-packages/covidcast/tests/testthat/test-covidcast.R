library(covidcast)
library(mockery)

test_that("covidcast_meta", {
  stub(covidcast_meta, ".request",
       list(message = "success",
            epidata = data.frame(
              min_time = c("20200101", "20201002"),
              max_time = c("20200102", "20201003"),
              max_issue = c("20200404", "20201101"))))

  expect_equal(covidcast_meta(),
               structure(
                 data.frame(
                   min_time = as.Date(c("2020-01-01", "2020-10-02")),
                   max_time = as.Date(c("2020-01-02", "2020-10-03")),
                   max_issue = as.Date(c("2020-04-04", "2020-11-01"))
                 ),
                 class = c("covidcast_meta", "data.frame")
               ))

  stub(covidcast_meta, ".request",
       list(message = "argle-bargle"))

  expect_error(covidcast_meta())
})
