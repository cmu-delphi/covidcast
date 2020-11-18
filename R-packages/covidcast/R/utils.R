#' Fetch only the latest issue for each observation in a data frame
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
  # Save the attributes, since grouping overwrites them
  attrs <- attributes(df)
  attrs <- attrs[!(names(attrs) %in% c("row.names", "names"))]

  df <- df %>%
    dplyr::arrange(dplyr::desc(.data$issue)) %>%
    dplyr::distinct(.data$geo_value, .data$time_value,
                    .keep_all = TRUE)

  attributes(df) <- c(attributes(df), attrs)

  return(df)
}

#' Fetch only the earliest issue for each observation in a data frame
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
  # Save the attributes, since grouping overwrites them
  attrs <- attributes(df)
  attrs <- attrs[!(names(attrs) %in% c("row.names", "names"))]

  df <- df %>%
    dplyr::arrange(.data$issue) %>%
    dplyr::distinct(.data$geo_value, .data$time_value,
                    .keep_all = TRUE)

  attributes(df) <- c(attributes(df), attrs)

  return(df)
}

##########

#' Get FIPS or CBSA codes from county or metropolitan area names
#'
#' Look up FIPS or CBSA codes by county or metropolitan area names,
#' respectively; these functions are based on `grep()`, and hence allow for
#' regular expressions.
#'
#' @param name Vector of county or metropolitan area names to look up.
#' @param ignore.case,perl,fixed Arguments to pass to `grep()`, with the same
#'   defaults as in the latter function. Hence, by default, regular expressions
#'   are used; to match against a fixed string (no regular expressions), set
#'   `fixed = TRUE`.
#' @param ties_method If "first", then only the first match for each name is
#'   returned. If "all", then all matches for each name are returned.
#' @param state Two letter state abbreviation (case insensitive) indicating a
#'   parent state used to restrict the search. For example, when `state = "NY"`,
#'   then `name_to_fips()` searches only over only counties lying in New York
#'   state, whereas `name_to_cbsa()` searches over the metropolitan areas lying,
#'   either fully or partially (as a metropolitan area can span several states),
#'   in New York state. If `NULL`, the default, then the search is performed
#'   US-wide (not restricted to any state in particular).
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
                        ties_method = c("first", "all"), state = NULL) {
  # Leave states in county_census (so we can find state fips)
  df = covidcast::county_census # %>% dplyr::filter(COUNTY != 0)

  # Restrict to a particular state, if we're asked to
  if (!is.null(state)) {
    df = df %>% dplyr::filter(STNAME == abbr_to_name(toupper(state)))
  }

  # Now perform the grep-based look up
  grep_lookup(key = name, keys = df$CTYNAME, values = df$FIPS,
              ignore.case = ignore.case, perl = perl, fixed = fixed,
              ties_method = ties_method)
}

#' @rdname name_to_fips
#' @export
name_to_cbsa = function(name, ignore.case = FALSE, perl = FALSE, fixed = FALSE,
                        ties_method = c("first", "all"), state = NULL) {
  # Restrict msa_census to metro areas
  df = covidcast::msa_census %>% dplyr::filter(LSAD == "Metropolitan Statistical Area")

  # Restrict to a particular state, if we're asked to
  if (!is.null(state)) {
    df = df %>% dplyr::slice(grep(toupper(state), df$STATE))
  }

  # Now perform the grep-based look up
  grep_lookup(key = name, keys = df$NAME, values = df$CBSA,
              ignore.case = ignore.case, perl = perl, fixed = fixed,
              ties_method = ties_method)
}

#' Get county or metropolitan area names from FIPS or CBSA codes
#'
#' Look up county or metropolitan area names by FIPS or CBSA codes,
#' respectively; these functions are based on `grep()`, and hence allow for
#' regular expressions.
#'
#' @param code Vector of FIPS or CBSA codes to look up.
#' @param ignore.case,perl,fixed Arguments to pass to `grep()`, with the same
#'   defaults as in the latter function. Hence, by default, regular expressions
#'   are used; to match against a fixed string (no regular expressions), set
#'   `fixed = TRUE`.
#' @param ties_method If "first", then only the first match for each code is
#'   returned. If "all", then all matches for each code are returned.
#'
#' @return A vector of county or metro names if `ties_method` equals "first",
#'   and a list of county or names otherwise.
#'
#' @examples
#' fips_to_name("42003")
#' cbsa_to_name("38300")
#' fips_to_name("4200", ties_method = "all")
#'
#' # Count the number of counties, grouped by first two digits of FIPS code
#' # (which identify states):
#' unlist(lapply(fips_to_name(sprintf("%02d", 1:99), ties = "all"), length))
#'
#' @seealso [name_to_fips()], [name_to_cbsa()]
#' @export
fips_to_name = function(code, ignore.case = FALSE, perl = FALSE, fixed = FALSE,
                        ties_method = c("first", "all")) {
  # Leave states in county_census (so we can find state fips)
  df = covidcast::county_census # %>% dplyr::filter(COUNTY != 0)

  # Now perform the grep-based look up
  grep_lookup(key = code, keys = df$FIPS, values = df$CTYNAME,
              ignore.case = ignore.case, perl = perl, fixed = fixed,
              ties_method = ties_method)
}

#' @rdname fips_to_name
#' @export
cbsa_to_name = function(code, ignore.case = FALSE, perl = FALSE, fixed = FALSE,
                        ties_method = c("first", "all")) {
  # Restrict msa_census to metro areas
  df = covidcast::msa_census %>% dplyr::filter(LSAD == "Metropolitan Statistical Area")

  # Now perform the grep-based look up
  grep_lookup(key = code, keys = df$CBSA, values = df$NAME,
              ignore.case = ignore.case, perl = perl, fixed = fixed,
              ties_method = ties_method)
}

#' Get state abbreviations from state names
#'
#' Look up state abbreviations by state names (including District of Columbia
#' and Puerto Rico); this function is based on `grep()`, and hence allows for
#' regular expressions.
#'
#' @param name Vector of state names to look up.
#' @param ignore.case,perl,fixed Arguments to pass to `grep()`, with the same
#'   defaults as in the latter function. Hence, by default, regular expressions
#'   are used; to match against a fixed string (no regular expressions), set
#'   `fixed = TRUE`.
#' @param ties_method If "first", then only the first match for each name is
#'   returned. If "all", then all matches for each name are returned.
#'
#' @return A vector of state abbreviations if `ties_method` equals "first", and
#'   a list of state abbreviations otherwise.
#'
#' @examples
#' name_to_abbr("Penn")
#' name_to_abbr(c("Penn", "New"), ties_method = "all")
#'
#' @seealso [abbr_to_name()]
#' @export
name_to_abbr = function(name, ignore.case = FALSE, perl = FALSE, fixed = FALSE,
                        ties_method = c("first", "all")) {
  # First get rid of United States from state_census
  df = covidcast::state_census %>% dplyr::filter(STATE > 0)

  # Now perform the grep-based look up
  grep_lookup(key = name, keys = df$NAME, values = df$ABBR,
              ignore.case = ignore.case, perl = perl, fixed = fixed,
              ties_method = ties_method)
}

#' Get state names from state abbreviations
#'
#' Look up state names by state abbreviations (including District of Columbia
#' and Puerto Rico); this function is based on `grep()`, and hence allows for
#' regular expressions.
#'
#' @param abbr Vector of state abbreviations to look up.
#' @param ignore.case,perl,fixed Arguments to pass to `grep()`, with the same
#'   defaults as in the latter function. Hence, by default, regular expressions
#'   are used; to match against a fixed string (no regular expressions), set
#'   `fixed = TRUE`.
#' @param ties_method If "first", then only the first match for each name is
#'   returned. If "all", then all matches for each name are returned.
#'
#' @return A vector of state names if `ties_method` equals "first", and a list
#'   of state names otherwise.
#'
#' @examples
#' abbr_to_name("PA")
#' abbr_to_name(c("PA", "PR", "DC"))
#'
#' @seealso [name_to_abbr()]
#' @export
abbr_to_name = function(abbr, ignore.case = FALSE, perl = FALSE, fixed = FALSE,
                        ties_method = c("first", "all")) {
  # First get rid of United States from state_census
  df = covidcast::state_census %>% dplyr::filter(STATE > 0)

  # Perform the grep-based look up
  grep_lookup(key = abbr, keys = df$ABBR, values = df$NAME,
              ignore.case = ignore.case, perl = perl, fixed = fixed,
              ties_method = ties_method)
}

#' Get FIPS codes from state abbreviations
#'
#' Look up FIPS codes by state abbreviations (including District of Columbia and
#' Puerto Rico); this function is based on `grep()`, and hence allows for
#' regular expressions.
#'
#' @param abbr Vector of state abbreviations to look up.
#' @param ignore.case,perl,fixed Arguments to pass to `grep()`, with the same
#'   defaults as in the latter function, except for `ignore.case = TRUE`. Hence,
#'   by default, regular expressions are used; to match against a fixed string
#'   (no regular expressions), set `fixed = TRUE`.
#' @param ties_method If "first", then only the first match for each name is
#'   returned. If "all", then all matches for each name are returned.
#'
#' @return A vector of FIPS codes if `ties_method` equals "first", and a list of
#'   FIPS codes otherwise. These FIPS codes have five digits (ending in "000").
#'
#' @examples
#' abbr_to_fips("PA")
#' abbr_to_fips(c("PA", "PR", "DC"))
#'
#' # Note that name_to_fips() works for state names too:
#' name_to_fips("^Pennsylvania$")
#'
#' @seealso [abbr_to_name()]
#' @export
abbr_to_fips = function(abbr, ignore.case = TRUE, perl = FALSE, fixed = FALSE,
                        ties_method = c("first", "all")) {
  # First get rid of United States from state_census, then convert FIPS codes to
  # appropriate character format
  df = covidcast::state_census %>% dplyr::filter(STATE > 0) %>%
    dplyr::mutate(STATE = format_state_fips(STATE))

  # Now perform the grep-based look up
  grep_lookup(key = abbr, keys = df$ABBR, values = df$STATE,
              ignore.case = ignore.case, perl = perl, fixed = fixed,
              ties_method = ties_method)
}

#' Get state abbreviations from FIPS codes
#'
#' Look up state abbreviations by FIPS codes (including District of Columbia and
#' Puerto Rico); this function is based on `grep()`, and hence allows for
#' regular expressions.
#'
#' @param code Vector of FIPS codes to look up; codes can have either two digits
#'   (as in "42") or five digits (as in "42000"), either is allowed.
#' @param ignore.case,perl,fixed Arguments to pass to `grep()`, with the same
#'   defaults as in the latter function, except for `ignore.case = TRUE`. Hence,
#'   by default, regular expressions are used; to match against a fixed string
#'   (no regular expressions), set `fixed = TRUE`.
#' @param ignore.case,perl,fixed Arguments to pass to `grep()`, with the same
#'   defaults as in the latter function. Hence, by default, regular expressions
#'   are used; to match against a fixed string (no regular expressions), set
#'   `fixed = TRUE`.
#' @param ties_method If "first", then only the first match for each code is
#'   returned. If "all", then all matches for each code are returned.
#'
#' @return A vector of state abbreviations if `ties_method` equals "first", and
#'   a list of state abbreviations otherwise.
#'
#' @examples
#' fips_to_abbr("42000")
#' fips_to_abbr(c("42", "72", "11"))
#'
#' # Note that fips_to_name() works for state names too:
#' fips_to_name("42000")
#'
#' @seealso [abbr_to_fips()]
#' @export
fips_to_abbr = function(code, ignore.case = TRUE, perl = FALSE, fixed = FALSE,
                        ties_method = c("first", "all")) {
  # First get rid of United States from state_census, then convert FIPS codes to
  # appropriate character format
  df = covidcast::state_census %>% dplyr::filter(STATE > 0) %>%
    dplyr::mutate(STATE = format_state_fips(STATE))

  # Now perform the grep-based look up
  grep_lookup(key = code, keys = df$STATE, values = df$ABBR,
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
    warn(paste("Some inputs were not uniquely matched; returning only the",
               "first match in each case."),
         res = res, key = key, class = "grep_lookup_nonunique_match")
  }
  return(sapply(res, `[`, 1))
}

# Simple convenience functions for FIPS formatting
format_fips = function(fips) { sprintf("%05d", fips) }
format_state_fips = function(fips) { sprintf("%02d000", fips) }
