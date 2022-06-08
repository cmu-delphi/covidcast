library(tidyverse)

format_fips <- function(fips) { sprintf("%05d", fips) }

# County census data
county_census = read.csv("https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/totals/co-est2019-alldata.csv", stringsAsFactors = FALSE)
county_census$FIPS = format_fips(county_census$STATE * 1000 + county_census$COUNTY)

# County names are in latin1; convert to UTF-8
county_census$CTYNAME = iconv(county_census$CTYNAME, "latin1", "UTF-8")
Encoding(county_census$CTYNAME) = "UTF-8"

# Generate 5 digit fips column and drop unused columns
county_census = county_census %>%
  mutate(
    FIPS = str_c(str_pad(pull(county_census, STATE), 2, pad = "0"),
                 str_pad(pull(county_census, COUNTY), 3, pad = "0"))
  )
county_census = county_census %>% select(SUMLEV, REGION, DIVISION, STATE, COUNTY, STNAME, CTYNAME, POPESTIMATE2019, FIPS)

# Add PR and Territories data from local files which were constructed by hand
# from https://www.census.gov/data/tables/2010/dec/2010-island-areas.html,
# https://www.census.gov/data/tables/time-series/demo/popest/2010s-total-puerto-rico-municipios.html,
# and http://datadictionary.naaccr.org/default.aspx?c=11&Version=21#Virgin%20Islands%20of%20the%20United%20States
load("./metadata/county_territories.rda")
load("./metadata/county_pr.rda")
county_census = rbind(county_census, county_pr, county_territories)
save(county_census, file = "../covidcast/data/county_census.rda", compress = "bzip2")

# MSA census data
msa_census = read.csv("https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/metro/totals/cbsa-est2019-alldata.csv", stringsAsFactors = FALSE)
msa_census$CBSA = as.character(msa_census$CBSA)

# Convert names to UTF-8
msa_census$NAME = iconv(msa_census$NAME, "latin1", "UTF-8")
Encoding(msa_census$NAME) = "UTF-8"
msa_census$STATE = substring(msa_census$NAME, regexpr(",", msa_census$NAME) + 2)
msa_census <- msa_census %>% select(CBSA, LSAD, MDIV, NAME, POPESTIMATE2019, STATE, STCOU)

save(msa_census, file = "../covidcast/data/msa_census.rda")

# State census data
state_census = read.csv("https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/state/detail/SCPRC-EST2019-18+POP-RES.csv", stringsAsFactors = FALSE)
state_abbr = character(nrow(state_census))
names(state_abbr) = state_census$NAME
state_abbr[state.name] = state.abb
state_abbr["United States"] = "US"
state_abbr["District of Columbia"] = "DC"
state_abbr["Puerto Rico Commonwealth"] = "PR"
state_census$ABBR = state_abbr

# Add territories data like we did for county_census
load("./metadata/state_territories.rda")
state_census = rbind(state_census, state_territories)
save(state_census, file = "../covidcast/data/state_census.rda")

# County geo centroids from usmap
county_col_classes = c("numeric", "numeric", "character", "character",
                       "character", "character")
county_file = system.file("extdata", "us_counties_centroids.csv", package = "usmap")
county_geo = utils::read.csv(county_file, colClasses = county_col_classes)
#save(county_geo, file = "../covidcast/data/county_geo.rda", compress = "bzip2")

# State geo centroids from usmap
state_col_classes = c("numeric", "numeric", "character", "character", "character")
state_file = system.file("extdata", "us_states_centroids.csv", package = "usmap")
state_geo = utils::read.csv(state_file, colClasses = state_col_classes)
#save(state_geo, file = "../covidcast/data/state_geo.rda")
