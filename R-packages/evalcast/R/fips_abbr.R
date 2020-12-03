

fips_2_abbr <- function(fips){
  df <- tibble(fi = unique(fips),
               ab = covidcast::fips_to_abbr(fi))
  
  ab = left_join(tibble(fi = fips), df, by="fi") %>% pull(ab) %>% tolower()
  names(ab) = NULL
  ab
}

abbr_2_fips <- function(abbr){
  df <- tibble(ab = unique(abbr),
               fi = covidcast::abbr_to_fips(ab))
  
  fi = left_join(tibble(ab = abbr), df, by="ab") %>% pull(fi)
  names(fi) = NULL
  fi
}