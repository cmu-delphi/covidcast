#' Fetch the latest or earliest issue for each observation
#'
#' The data returned from `covidcast_signal()` or `covidcast_signals()` can, if
#' called with the `issues` argument, contain multiple issues for a single
#' observation in a single location. These functions filter the data frame to
#' contain only the earliest issue or only the latest issue.
#'
#' @param df A `covidcast_signal` or `covidcast_signal_long` data frame, such as
#'   returned from `covidcast_signal()` or the "long" format of
#'   `aggregate_signals()`.
#' @return A data frame in the same form, but with only the earliest or latest
#'   issue of every observation. Note that these functions sort the data frame
#'   as part of their filtering, so the output data frame rows may be in a
#'   different order.
#' @importFrom rlang .data
#' @export
latest_issue <- function(df) {
  return(first_or_last_issue(df, TRUE))
}

#' @rdname latest_issue
#' @export
earliest_issue <- function(df) {
  return(first_or_last_issue(df, FALSE))
}

# Helper to do either first or last issue.
first_or_last_issue <- function(df, latest) {
  if (!inherits(df, c("covidcast_signal", "covidcast_signal_long"))) {
    stop("`df` must be a `covidcast_signal` ",
         "or `covidcast_signal_long` data frame")
  }

  # Save the attributes, such as metadata, since dplyr drops them
  attrs <- attributes(df)
  attrs <- attrs[!(names(attrs) %in% c("row.names", "names"))]

  issue_sort <- function(df) {
    if (latest) {
      dplyr::arrange(df, dplyr::desc(.data$issue))
    } else {
      dplyr::arrange(df, .data$issue)
    }
  }

  df <- df %>%
    issue_sort() %>%
    dplyr::distinct(.data$data_source, .data$signal, .data$geo_value,
                    .data$time_value, .keep_all = TRUE)

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
name_to_fips <- function(name, ignore.case = FALSE, perl = FALSE, fixed = FALSE,
                        ties_method = c("first", "all"), state = NULL) {
  # Leave states in county_census (so we can find state fips)
  df <- covidcast::county_census # %>% dplyr::filter(COUNTY != 0)

  # Restrict to a particular state, if we're asked to
  if (!is.null(state)) {
    df <- df %>% dplyr::filter(.data$STNAME == abbr_to_name(toupper(state)))
  }

  # Now perform the grep-based look up
  grep_lookup(key = name, keys = df$CTYNAME, values = df$FIPS,
              ignore.case = ignore.case, perl = perl, fixed = fixed,
              ties_method = ties_method)
}

#' @rdname name_to_fips
#' @export
name_to_cbsa <- function(name, ignore.case = FALSE, perl = FALSE, fixed = FALSE,
                        ties_method = c("first", "all"), state = NULL) {
  # Restrict msa_census to metro areas
  df <- dplyr::filter(
    covidcast::msa_census, .data$LSAD == "Metropolitan Statistical Area"
  )

  # Restrict to a particular state, if we're asked to
  if (!is.null(state)) {
    df <- df %>% dplyr::slice(grep(toupper(state), df$STATE))
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
fips_to_name <- function(code, ignore.case = FALSE, perl = FALSE, fixed = FALSE,
                        ties_method = c("first", "all")) {
  # Leave states in county_census (so we can find state fips)
  df <- covidcast::county_census # %>% dplyr::filter(COUNTY != 0)

  # Now perform the grep-based look up
  grep_lookup(key = code, keys = df$FIPS, values = df$CTYNAME,
              ignore.case = ignore.case, perl = perl, fixed = fixed,
              ties_method = ties_method)
}

#' @rdname fips_to_name
#' @export
cbsa_to_name <- function(code, ignore.case = FALSE, perl = FALSE, fixed = FALSE,
                        ties_method = c("first", "all")) {
  # Restrict msa_census to metro areas
  df <- dplyr::filter(
    covidcast::msa_census, .data$LSAD == "Metropolitan Statistical Area"
  )

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
name_to_abbr <- function(name, ignore.case = FALSE, perl = FALSE, fixed = FALSE,
                        ties_method = c("first", "all")) {
  # First get rid of United States from state_census
  df <- covidcast::state_census %>% dplyr::filter(.data$STATE > 0)

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
abbr_to_name <- function(abbr, ignore.case = FALSE, perl = FALSE, fixed = FALSE,
                        ties_method = c("first", "all")) {
  # First get rid of United States from state_census
  df <- covidcast::state_census %>% dplyr::filter(.data$STATE > 0)

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
abbr_to_fips <- function(abbr, ignore.case = TRUE, perl = FALSE, fixed = FALSE,
                        ties_method = c("first", "all")) {
  # First get rid of United States from state_census, then convert FIPS codes to
  # appropriate character format
  df <- covidcast::state_census %>% dplyr::filter(.data$STATE > 0) %>%
    dplyr::mutate(STATE = format_state_fips(.data$STATE))

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
fips_to_abbr <- function(code, ignore.case = TRUE, perl = FALSE, fixed = FALSE,
                        ties_method = c("first", "all")) {
  # First get rid of United States from state_census, then convert FIPS codes
  # to appropriate character format
  df <- covidcast::state_census %>% dplyr::filter(.data$STATE > 0) %>%
    dplyr::mutate(STATE = format_state_fips(.data$STATE))

  # Now perform the grep-based look up
  grep_lookup(key = code, keys = df$STATE, values = df$ABBR,
              ignore.case = ignore.case, perl = perl, fixed = fixed,
              ties_method = ties_method)
}

# This is the core lookup function
grep_lookup <- function(key, keys, values, ignore.case = FALSE, perl = FALSE,
                       fixed = FALSE,  ties_method = c("first", "all")) {
  ties_method <- match.arg(ties_method)

  # Only do grep lookup for unique keys, to keep the look sort. It's a common
  # use case to, say, call fips_to_name on a covidcast_signal data frame over
  # many days of data with many repeat observations of the same locations.
  unique_key <- unique(key)

  res <- vector("list", length(unique_key))
  for (i in seq_along(unique_key)) {
    ind <- grep(unique_key[i], keys, ignore.case = ignore.case, perl = perl,
               fixed = fixed)
    if (length(ind) == 0) {
      res[[i]] <- NA
      names(res[[i]]) <- unique_key[i]
    } else {
      res[[i]] <- values[ind]
      names(res[[i]]) <- keys[ind]
    }
  }

  # Restore to original length, including duplicate keys.
  res <- res[match(key, unique_key)]

  # If they ask for all matches, then return the list
  if (ties_method == "all") {
    return(res)
  }

  # Otherwise, format into a vector, and warn if needed
  if (length(unlist(res)) > length(key)) {
    warn(paste("Some inputs were not uniquely matched; returning only the",
               "first match in each case."),
         res = res, key = key, class = "grep_lookup_nonunique_match")
  }
  return(unlist(lapply(res, `[`, 1)))
}

# Simple convenience functions for FIPS formatting
format_fips <- function(fips) { sprintf("%05d", fips) }
format_state_fips <- function(fips) { sprintf("%02d000", fips) }
