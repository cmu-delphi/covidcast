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

test_that("county_fips_to_name", {
  # Basic function
  expect_equal(county_fips_to_name("42003"),
               c("42003" = "Allegheny County"))
  expect_equal(county_fips_to_name("42000"),
               c("42000" = "Pennsylvania"))

  # Repeats in vector
  expect_equal(county_fips_to_name(c("42003", "42003")),
               c("42003" = "Allegheny County",
                 "42003" = "Allegheny County"))
})

test_that("state_fips_to_name", {

  # Basic function
  expect_equal(state_fips_to_name("42"),
               c("42" = "Pennsylvania"))
  expect_equal(state_fips_to_name("10"),
               c("10" = "Delaware"))
  expect_equal(state_fips_to_name("01"),
               c("01" = "Alabama"))
  expect_equal(state_fips_to_name("01112"),
               c("01" = "Alabama"))
               
  # Repeats in vector
  expect_equal(state_fips_to_name(c("42", "42", "01")),
               c("42" = "Pennsylvania",
                 "42" = "Pennsylvania",
                 "01" = "Alabama"))
})
