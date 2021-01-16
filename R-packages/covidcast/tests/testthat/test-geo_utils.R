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

  # Repeats in vector
  expect_equal(fips_to_name(c("42003", "42003")),
               c("42003" = "Allegheny County",
                 "42003" = "Allegheny County"))

  # Request all ties
  expect_equal(fips_to_name("4213", ties_method = "all"),
               list(c("42131" = "Wyoming County",
                      "42133" = "York County")))

  # Correct matching of multiple ties
  expect_equal(fips_to_name(c("4213", "4019"), ties_method = "all"),
               list(c("42131" = "Wyoming County",
                      "42133" = "York County"),
                    c("04019" = "Pima County",
                      "24019" = "Dorchester County",
                      "34019" = "Hunterdon County",
                      "54019" = "Fayette County")))
})
