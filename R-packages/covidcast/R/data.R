#' County census population data
#'
#' Data set on county populations, from the 2019 US Census.
#'
#' @format A data frame with 3193 rows, one for each county (along with the 50
#'   states and DC). There are many columns. The most crucial are:  
#'
#' \describe{
#'   \item{FIPS}{Five-digit county FIPS codes. These are unique identifiers
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
#' @source United States Census Bureau, at
#'   \url{https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/totals/co-est2019-alldata.csv}
#'
#' @seealso [fips_to_name()], [name_to_fips()]
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
#'   areas, county or county equivalents, and metropolitan divisions). There are
#'   many columns. The most crucial are: 
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
#' @source United States Census Bureau, at
#'   \url{https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/metro/totals/cbsa-est2019-alldata.csv}
#'
#' @seealso [cbsa_to_name()], [name_to_cbsa()]
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
#' @source United States Census Bureau, at
#'   \url{https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/state/detail/SCPRC-EST2019-18+POP-RES.csv}
#'
#' @seealso [abbr_to_name()], [name_to_abbr()], [abbr_to_fips()], [fips_to_abbr()]
"state_census"

#' County latitudes and longitudes
#'
#' Data set on latitudes and longitudes of county centroids, from the `usmap`
#' package.
#'
#' @format Data frame with 3142 rows, each representing one county. Columns:
#'
#' \describe{
#'   \item{x}{Longitude of county centroid.}
#'   \item{y}{Latitude of county centroid.}
#'   \item{fips}{Five-digit county FIPS code.}
#'   \item{abbr}{Two-letter state abbreviation.}
#'   \item{full}{State name.}
#'   \item{county}{County name.}
#' }
#'
#' @source `usmap`
"county_geo"

#' State latitudes and longitudes
#'
#' Data set on latitudes and longitudes of state centroids, from the `usmap`
#' package.
#'
#' @format Data frame with 51 rows, each representing one state (including the
#'   District of Columbia). Columns:
#'
#' \describe{
#'   \item{x}{Longitude of state centroid.}
#'   \item{y}{Latitude of state centroid.}
#'   \item{fips}{Five-digit county FIPS code.}
#'   \item{abbr}{Two-letter state abbreviation.}
#'   \item{full}{State name.}
#' }
#'
#' @source `usmap`
"state_geo"
