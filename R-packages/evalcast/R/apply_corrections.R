##-------------
## Originally in evalforecast, written by @bnaras



#' Retrieve a tibble of data corrections from the corrections database
#'
#' Reads all records in the corrections database and returns a tibble
#' with columns noted above
#'
#' @param db_path the path for the SQLite database, for example
#'     `~/data_corrections.sqlite`
#' @param geo_type the `geo_type`, one of `"state"` or `"county"`
#' @return a tibble of corrections data
#' @importFrom dplyr tbl collect mutate
#' @importFrom DBI dbConnect dbDisconnect
#' @importFrom lubridate ymd
#' @importFrom RSQLite SQLite
#' @export get_data_corrections
get_data_corrections  <- function(db_path, geo_type) {
  ## The check below is preferable to match.arg because both state and county have same
  ## schema and we don't want people to clobber state with county data or vice-versa easily!
  if (!(geo_type %in% c("state", "county"))) {
    stop("get_data_corrections: geo_type should be one of 'state' or 'county'!")
  }
  con <- DBI::dbConnect(drv = RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(conn = con))
  dplyr::tbl(src = con, geo_type) %>%
    dplyr::collect() %>%
    dplyr::mutate(reference_date = lubridate::ymd(reference_date),
                  issue_date = lubridate::ymd(issue_date),
                  correction_date = lubridate::ymd(correction_date))
}


#' Replace corrections data for a geo type with new data
#'
#' Overwrites the corrections records for a geo type with new data after checking that schemas match.
#'
#' @param db_path the path for the SQLite database, for example
#'     `~/corrections.sqlite`
#' @param geo_type the `geo_type`, one of `"state"` or `"county"`
#' @param new_df the new data to replace the old
#' @return `TRUE` invisibly
#' @importFrom dplyr tbl collect mutate
#' @importFrom DBI dbConnect dbDisconnect dbWriteTable
#' @importFrom lubridate ymd
#' @importFrom RSQLite SQLite
#'
#' @export update_corrections
update_corrections  <- function(db_path, geo_type, new_df) {
  ## The check below is preferable to match.arg because both state and county have same
  ## schema and we don't want people to clobber state with county data or vice-versa easily!
  if (!(geo_type %in% c("state", "county"))) {
    stop("get_data_corrections: geo_type should be one of 'state' or 'county'!")
  }
  con <- DBI::dbConnect(drv = RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(conn = con))
  ## Ensure the schema is correct
  dplyr::tbl(src = con, geo_type) %>%
    dplyr::collect() %>%
    dplyr::mutate(reference_date = lubridate::ymd(reference_date),
                  issue_date = lubridate::ymd(issue_date),
                  correction_date = lubridate::ymd(correction_date)) ->
    current_data
  ## Quick and dirty way to check schema match
  current_schema  <- current_data[FALSE, ] ## get a zero row tibble of existing data
  new_schema  <- new_df[FALSE, ] ## zero row tibble of new data
  if (!identical(current_schema, new_schema)) {
    stop("update_corrections: new_df does not have the same schema as old one!")
  }
  new_df %>%
    dplyr::mutate(reference_date = as.character(reference_date),
                  issue_date = as.character(issue_date),
                  correction_date = as.character(correction_date)
    ) %>%
    DBI::dbWriteTable(conn = con, name = geo_type, overwrite = TRUE)
  invisible(TRUE)
}


#' Apply corrections, if available, to upstream data frame
#'
#' Corrections data are replacement records for the original data
#' using the schema described above. This function returns a new
#' tibble by removing the matching original data, the matching being
#' done using the variables `location`, `reference_date` and
#' `variable_name`, and appending the entire corrections data at the
#' end. Ideally, this function should only make corrections that a
#' properly versioned data frame cannot account for, i.e. persistent
#' bad data rows that are likely to mess up forecasting algorithms
#' (this has the salutory effect of keeping the number of corrections
#' small).
#'
#' @param df the upstream data frame corresponding to the geo type
#' @param geo_type the geo_type corresponding to the upstream data
#'     frame
#' @param corrections_db_path the path for the SQLite database, for example
#'     `~/corrections.sqlite`
#' @return a df with corrections applied if the corrections are
#'     available, or same dataframe
#' @importFrom dplyr anti_join bind_rows select rename mutate
#' @importFrom DBI dbConnect dbDisconnect
#' @importFrom lubridate ymd
#' @importFrom RSQLite SQLite
#' @export
apply_corrections  <- function(df, geo_type, corrections_db_path) {
  ## The check below is preferable to match.arg because both state and county have same
  ## schema and we don't want people to clobber state with county data or vice-versa easily!
  if (!(geo_type %in% c("state", "county"))) {
    stop("get_data_corrections: geo_type should be one of 'state' or 'county'!")
  }

  con <- DBI::dbConnect(drv = RSQLite::SQLite(), corrections_db_path)
  on.exit(DBI::dbDisconnect(conn = con))


  dplyr::tbl(src = con, geo_type) %>%
    dplyr::collect() %>%
    dplyr::mutate(reference_date = lubridate::ymd(reference_date),
                  issue_date = lubridate::ymd(issue_date),
                  correction_date = lubridate::ymd(correction_date)) %>%
    ## Remove unneeded columns for merge
    dplyr::select(-value, -correction_date, -description) %>%
    dplyr::rename(value = new_value) %>% ## New value is now effective value
    dplyr::inner_join(x = df, by = c("location", "reference_date", "variable_name")) %>%
    ## New value in y overrides old value in x but old location_name and issue_date should prevail,
    ## so select columns in correct order and drop unneeded ones
    dplyr::select(location, location_name = location_name.x, reference_date, issue_date = issue_date.x,
                  variable_name, value = value.y, -value.x, -location_name.y, -issue_date.y) ->
    corrections

  if (nrow(corrections) == 0) {
    warning(sprintf("No corrections available for %s", geo_type))
    return(df)
  }

  ## Ensure the schema is correct. Quick and dirty way to check schema match:
  ## get a zero row tibble of existing data
  current_schema  <- corrections[FALSE, ]
  df_schema  <- df[FALSE, ] ## zero row tibble of new data
  if (!identical(current_schema, df_schema)) {
    stop("apply_corrections: df does not have the same schema as corrections!")
  }

  dplyr::anti_join(x = df, y = corrections,
                   by = c("location", "reference_date", "variable_name")) %>%
    dplyr::bind_rows(corrections)

}
