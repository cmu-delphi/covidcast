format_fips <- function(fips) { sprintf("%05d", fips) }

# County census data
county_census = read.csv("https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/totals/co-est2019-alldata.csv", stringsAsFactors = FALSE)
county_census$FIPS = format_fips(county_census$STATE * 1000 + county_census$COUNTY)

# County names are in latin1; convert to UTF-8
county_census$CTYNAME = iconv(county_census$CTYNAME, "latin1", "UTF-8")
Encoding(county_census$CTYNAME) = "UTF-8"
save(county_census, file = "../covidcast/data/county_census.rda", compress = "bzip2")

# MSA census data
msa_census = read.csv("https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/metro/totals/cbsa-est2019-alldata.csv", stringsAsFactors = FALSE)
msa_census$CBSA = as.character(msa_census$CBSA)

# Convert names to UTF-8
msa_census$NAME = iconv(msa_census$NAME, "latin1", "UTF-8")
Encoding(msa_census$NAME) = "UTF-8"
msa_census$STATE = substring(msa_census$NAME, regexpr(",", msa_census$NAME) + 2)
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
save(state_census, file = "../covidcast/data/state_census.rda")

# County geo data from https://www.weather.gov/gis/Counties
library(sf)
county_geo = st_read("c_03mr20/c_03mr20.shp")
county_geo$STATE = as.character(county_geo$STATE)
county_geo$CWA = as.character(county_geo$CWA)
county_geo$FE_AREA = as.character(county_geo$FE_AREA)
county_geo$COUNTYNAME = as.character(county_geo$COUNTYNAME)
county_geo$TIME_ZONE = NULL
county_geo$geometry = NULL
county_geo = data.frame(county_geo)
save(county_geo, file = "../covidcast/data/county_geo.rda", compress = "bzip2")

county_census$FIPS[!county_census$FIPS %in% county_geo$FIPS] # Just the states themselves
county_geo[!county_geo$FIPS %in% county_census$FIPS, ] # AS, PR, VI, GU, etc.

# State geo data from https://developers.google.com/public-data/docs/canonical/states_csv
state_geo = read.table("state-geo.txt", sep = "\t", stringsAsFactors = FALSE)
colnames(state_geo) = c("STATE", "LAT", "LON", "NAME")
save(state_geo, file = "../covidcast/data/state_geo.rda")
