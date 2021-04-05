test_that("test_legal_faceting checks", {
  expect_equal(test_legal_faceting(NULL, NULL, c("foo", "bar")), TRUE)
  expect_equal(test_legal_faceting("foo", NULL, c("foo", "bar")), TRUE)
  expect_equal(test_legal_faceting(NULL, "bar", c("foo", "bar")), TRUE)
  expect_equal(test_legal_faceting(NULL, "bar", c("foo", "bar")), TRUE)
  expect_equal(test_legal_faceting("foo", "bar", c("foo", "bar")), TRUE)
  
  expect_error(
    test_legal_faceting("foo", "bar", "bar"),
    "Variables must be grouped in order to be faceted in rows: foo")
  expect_error(
    test_legal_faceting("foo", "bar", "foo"),
    "Variables must be grouped in order to be faceted in cols: bar")
})
