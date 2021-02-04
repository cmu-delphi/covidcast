test_that("we can validate geo types", {
  state_geos <- c("ak", "mn")
  expect_error(validate_geo_type("xyz", state_geos))
  expect_equal(validate_geo_type("state", state_geos), "state")
  expect_null(validate_geo_type("hrr", state_geos))

  county_geos <- c("10023", "23436")
  expect_equal(validate_geo_type("county", county_geos), "county")
  expect_equal(validate_geo_type(c("state", "county"), county_geos), "county")
  expect_equal(validate_geo_type("hrr", county_geos), "hrr")

  mix_number_geos <- c("12", "123")
  expect_equal(validate_geo_type("hrr", mix_number_geos), "hrr")
  expect_equal(validate_geo_type("msa", mix_number_geos), "msa")
  expect_equal(validate_geo_type("dma", mix_number_geos), "dma")
  expect_error(validate_geo_type("state", mix_number_geos))
  expect_error(validate_geo_type("county", mix_number_geos))

  one_letter_geos <- c("k", "q")
  expect_null(validate_geo_type("state", one_letter_geos))
  expect_null(validate_geo_type("county", one_letter_geos))

  one_digit_geos <- c("6", "7")
  expect_null(validate_geo_type("state", one_digit_geos))
  expect_null(validate_geo_type("county", one_digit_geos))

  us_geos <- c("us", "us")
  expect_equal(validate_geo_type("nation", us_geos), "nation")
  expect_equal(validate_geo_type(c("nation", "state"), state_geos), "state")
})
