## code to prepare `county_label_lookup` dataset goes here
county_label_lookup <- paste(
  covidcast::county_geo$STATE, covidcast::county_geo$COUNTYNAME)
names(county_label_lookup) = covidcast::county_geo$FIPS
usethis::use_data(county_label_lookup, overwrite = TRUE)
