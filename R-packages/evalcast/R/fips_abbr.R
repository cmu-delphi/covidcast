fips_2_abbr <- function(fips){
  # faster version of covidcast::fips_to_abbr
  ab = left_join(tibble(fips = fips), state_fips, by="fips") %>% 
    pull(.data$abbr) 
  names(ab) = NULL
  ab
}

abbr_2_fips <- function(abbr){
  # the reverse operation
  fi = left_join(tibble(abbr = abbr), state_fips, by="abbr") %>% 
    pull(.data$fips)
  names(fi) = NULL
  fi
}
