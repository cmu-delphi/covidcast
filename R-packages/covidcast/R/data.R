#' County census population data
#'
#' Data set on county populations, from the 2019 US Census.
#'
#' @format A data frame with 3193 rows, each representing one county. There are
#'   many columns. The most crucial are:
#'
#' \describe{
#'   \item{FIPS}{5-digit county FIPS codes. These are unique identifiers
#'   used, for example, as the `geo_values` argument to `covidcast_signal()` to
#'   request data from a specific county.}
#'   \item{CTYNAME}{County name, to help find counties by name.}
#'   \item{STNAME}{Name of the state in which this county belongs.}
#'   \item{POPESTIMATE2019}{Estimate of the county's resident population as of
#'   July 1, 2019.}
#' }
#'
#' @references Census Bureau documentation of all columns and their meaning:
#'   \url{https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/totals/co-est2019-alldata.pdf}
#'
#' @source
#' United States Census Bureau, at
#'   \url{https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/totals/co-est2019-alldata.csv}
"county_census"

#' Metro area population data
#'
#' Data set on metropolitan area populations, from the 2019 US Census. This
#' includes metropolitan and micropolitan statistical areas, although the
#' COVIDcast API only supports fetching data from metropolitan statistical
#' areas.
#'
#' @format A data frame with 2797 rows, each representing one core-based
#'   statistical area (including metropolitan and micropolitan statistical
#'   areas). There are many columns. The most crucial are:
#'
#' \describe{
#'   \item{CBSA}{Core Based Statistical Area code. These are unique identifiers
#'   used, for example, as the `geo_values` argument to `covidcast_signal()`
#'   when requesting data from specific metro areas (with `geo_type = 'msa'`).}
#'   \item{NAME}{Name or title of the area.}
#'   \item{LSAD}{Legal/Statistical Area Description, identifying if this is a
#'   metropolitan or micropolitan area, a metropolitan division, or a county.}
#'   \item{POPESTIMATE2019}{Estimate of the area's resident population as of
#' July 1, 2019.}
#' }
#'
#' @references Census Bureau documentation of all columns and their meaning:
#'   \url{https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/metro/totals/cbsa-est2019-alldata.pdf}
#'
#' @source
#'   \url{https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/metro/totals/cbsa-est2019-alldata.csv}
"msa_census"

#' State population data
#'
#' Data set on state populations, from the 2019 US Census.
#'
#' @format Data frame with 53 rows (including one for the United States as a
#'   whole, plus the District of Columbia and the Puerto Rico Commonwealth).
#'   Important columns:
#'
#' \describe{
#'   \item{NAME}{Name of the state.}
#'   \item{POPESTIMATE2019}{Estimate of the state's resident population in 2019.}
#' }
#'
#' @source
#'   \url{https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/state/detail/SCPRC-EST2019-18+POP-RES.csv}
"state_census"

#' County latitudes and longitudes
#'
#' Data set on latitudes and longitudes of county centroids, from the National
#' Weather Service.
#'
#' @format Data frame with 3331 rows, each representing one county. Columns:
#'
#' \describe{
#'   \item{COUNTYNAME}{Name of the county.}
#'   \item{FIPS}{5-digit county FIPS code.}
#'   \item{STATE}{Two-letter state abbreviation.}
#'   \item{LON}{Longitude of county centroid.}
#'   \item{LAT}{Latitude of county centroid.}
#' }
#'
#' @source
#'   \url{https://www.weather.gov/gis/Counties}
"county_geo"

#' State latitudes and longitudes
#'
#' Data set on latitudes and longitudes of state centroids, from Google's DSPL.
#'
#' @format Data frame with 52 rows, each representing one state (including
#'   Puerto Rico and the District of Columbia). Columns:
#'
#' \describe{
#'   \item{STATE}{Two-letter state abbreviation.}
#'   \item{LAT}{Latitude of state centroid.}
#'   \item{LON}{Longitude of state centroid.}
#'   \item{NAME}{Name of state.}
#' }
#'
#' @source
#'   \url{https://developers.google.com/public-data/docs/canonical/states_csv}
"state_geo"
