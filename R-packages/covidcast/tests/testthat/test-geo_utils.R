suppressPackageStartupMessages(library(covidcast))

test_that("name_to_fips", {
  # Basic function
  expect_equal(name_to_fips("Allegheny"),
               c("Allegheny County" = "42003"))

  # state argument
  expect_equal(name_to_fips("Allegheny", state = "pa"),
               c("Allegheny County" = "42003"))
  expect_equal(name_to_fips("Allegheny", state = "PA"),
               c("Allegheny County" = "42003"))
  expect_equal(name_to_fips("Miami", state = "fl"),
               c("Miami-Dade County" = "12086"))

  # In case of duplicates
  expect_warning(name_to_fips("Miami"))
})

test_that("fips_to_name", {
  # Basic function
  expect_equal(fips_to_name("42003"),
               c("42003" = "Allegheny County"))
  expect_equal(fips_to_name("42000"),
               c("42000" = "Pennsylvania"))
})
