#' Fetch only the latest issue for each observation in a data frame.
#'
#' Since `covidcast_signal()` can, with the right options, return multiple
#' issues for a single observation in a single geo, we may want only the most
#' recent for plotting, mapping, or other purposes.
#'
#' @param df A `covidcast_signal` data frame
#' @return The same `covidcast_signal` data frame, but with only the latest
#'     issue of every observation
#' @importFrom rlang .data
#' @keywords internal
latest_issue <- function(df) {
  # Preserve the attributes, since grouping overwrites them
  attrs <- attributes(df)

  df <- df %>%
    dplyr::group_by(.data$geo_value, .data$time_value) %>%
    dplyr::filter(.data$issue == max(.data$issue)) %>%
    dplyr::ungroup()

  attributes(df) <- attrs

  return(df)
}

#' Fetch only the earliest issue for each observation in a data frame.
#'
#' Since `covidcast_signal()` can, with the right options, return multiple
#' issues for a single observation in a single geo, we may want only the most
#' recent for plotting, mapping, or other purposes.
#'
#' @param df A `covidcast_signal` data frame
#' @return The same `covidcast_signal` data frame, but with only the earliest
#'     issue of every observation
#' @importFrom rlang .data
#' @keywords internal
earliest_issue <- function(df) {
  # Preserve the attributes, since grouping overwrites them
  attrs <- attributes(df)

  df <- df %>%
    dplyr::group_by(.data$geo_value, .data$time_value) %>%
    dplyr::filter(.data$issue == min(.data$issue)) %>%
    dplyr::ungroup()

  attributes(df) <- attrs

  return(df)
}

##########

#' Get FIPS or CBSA codes from county or city names
#'
#' Look up FIPS or CBSA codes by county or city names, respectively; these
#' functions are based on `grep()`, and hence allow for regular expressions.
#'
#' @param name Vector of county or city names to look up.
#' @param ignore.case,perl,fixed Arguments to pass to `grep()`, with the same
#'   defaults as in the latter function. Hence, by default, regular expressions
#'   are used; to match against a fixed string (no regular expressions), set
#'   `fixed = TRUE`.
#' @param ties_method If "first", then only the first match for each name is
#'   returned. If "all", then all matches for each name are returned.
#'
#' @return A vector of FIPS or CBSA codes if `ties_method` equals "first", and a
#'   list of FIPS or CBSA codes otherwise.
#'
#' @examples
#' name_to_fips("Allegheny")
#' name_to_cbsa("Pittsburgh")
#' name_to_fips("Miami")
#' name_to_fips("Miami", ties_method = "all")
#' name_to_fips(c("Allegheny", "Miami", "New "), ties_method = "all")
#'
#' @seealso [fips_to_name()], [cbsa_to_name()]
#' @export
name_to_fips = function(name, ignore.case = FALSE, perl = FALSE, fixed = FALSE,
                        ties_method = c("first", "all")) {
  # First remove states from county_census
  df = county_census %>% filter(COUNTY != 0)

  # Now perform the grep-based look up
  grep_lookup(key = name, keys = df$CTYNAME, values = df$FIPS,
              ignore.case = ignore.case, perl = perl, fixed = fixed,
              ties_method = ties_method)
}

#' @rdname name_to_fips
#' @export
name_to_cbsa = function(name, ignore.case = FALSE, perl = FALSE, fixed = FALSE,
                        ties_method = c("first", "all")) {
  # First restrict msa_census to metro areas
  df = msa_census %>% filter(LSAD == "Metropolitan Statistical Area")

  # Now perform the grep-based look up
  grep_lookup(key = name, keys = df$NAME, values = df$CBSA,
              ignore.case = ignore.case, perl = perl, fixed = fixed,
              ties_method = ties_method)
}

#' Get county or city names from FIPS or CBSA codes
#'
#' Look up county or city names by FIPS or CBSA codes, respectively; these
#' functions are based on `grep()`, and hence allow for regular expressions.
#'
#' @param code Vector of FIPS or CBSA codes to look up.
#' @param ignore.case,perl,fixed Arguments to pass to `grep()`, with the same
#'   defaults as in the latter function. Hence, by default, regular expressions
#'   are used; to match against a fixed string (no regular expressions), set
#'   `fixed = TRUE`.
#' @param ties_method If "first", then only the first match for each code is
#'   returned. If "all", then all matches for each code are returned.
#'
#' @return A vector of FIPS or CBSA codes if `ties_method` equals "first", and a
#'   list of FIPS or CBSA codes otherwise.
#'
#' @examples
#' fips_to_name("42003")
#' cbsa_to_name("38300")
#' fips_to_name("4200", ties_method = "all")
#'
#' # Count the number of counties, grouped by first two digits of FIPS code:
#' unlist(lapply(fips_to_name(sprintf("%02d", 1:99), ties = "all"), length))
#'
#' @seealso [name_to_fips()], [name_to_cbsa()]
#' @export
fips_to_name = function(code, ignore.case = FALSE, perl = FALSE, fixed = FALSE,
                        ties_method = c("first", "all")) {
  # First remove states from county_census
  df = county_census %>% filter(COUNTY != 0)

  # Now perform the grep-based look up
  grep_lookup(key = code, keys = df$FIPS, values = df$CTYNAME,
              ignore.case = ignore.case, perl = perl, fixed = fixed,
              ties_method = ties_method)
}

#' @rdname fips_to_name
#' @export
cbsa_to_name = function(code, ignore.case = FALSE, perl = FALSE, fixed = FALSE,
                        ties_method = c("first", "all")) {
  # First restrict msa_census to metro areas
  df = msa_census %>% filter(LSAD == "Metropolitan Statistical Area")

  # Now perform the grep-based look up
  grep_lookup(key = code, keys = df$CBSA, values = df$NAME,
              ignore.case = ignore.case, perl = perl, fixed = fixed,
              ties_method = ties_method)
}

# This is the core lookup function
grep_lookup = function(key, keys, values, ignore.case = FALSE, perl = FALSE,
                       fixed = FALSE,  ties_method = c("first", "all")) {
  ties_method = match.arg(ties_method)
  res = vector("list", length(key))
  for (i in 1:length(key)) {
    ind = grep(key[i], keys, ignore.case = ignore.case, perl = perl,
               fixed = fixed)
    if (length(ind) == 0) { res[[i]] = NA; names(res[[i]]) = key[i] }
    else {  res[[i]] = values[ind]; names(res[[i]]) = keys[ind] }
  }

  # If they ask for all matches, then return the list
  if (ties_method == "all") return(res)

  # Otherwise, format into a vector, and warn if needed
  if (length(unlist(res)) > length(key)) {
    warning("Some inputs were not uniquely matched; returning only the first ",
            "match in each case.")
  }
  return(sapply(res, `[`, 1))
}
