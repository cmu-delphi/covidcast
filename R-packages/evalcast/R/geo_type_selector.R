geo_type_selector <- function(geo_type, predictions_cards, auto_select = TRUE) {
  # tries to determine which geo_type we want and/or validate
  # our selection based on the content of predictions_cards
  #
  # Wraps the function below for ease of testing (we test the validator,
  # rather than this function)
  assert_that(is.null(geo_type) | auto_select,
              msg = "A geo_type must be provided when auto_select is FALSE")
  assert_that(length(geo_type) <= 1,
              msg = paste("A maximum of one geo_type may be provided to",
                          "geo_type_selector. (Set geo_type = NULL and",
                          "auto_select = TRUE to attempt auto-identification)"))
  if (is.null(geo_type)) {
    geo_types <- c("county", "hrr", "msa", "dma", "state", "nation")
  } else {
    if (auto_select) {
      if (geo_type == "county") {
        geo_types <- c("county", "state", "nation")
      } else if (geo_type == "state") {
        geo_types <- c("state", "county", "nation")
      } else if (geo_type == "nation") {
        geo_types <- c("nation", "state", "county")
      } else {
        geo_types <- geo_type
      }
    } else {
      geo_types <- geo_type
    }
  }
  found_geo_type <- validate_geo_type(geo_types,
                                     predictions_cards$geo_value,
                                     auto_select)
  if (is.null(found_geo_type)) {
    if (is.null(geo_type)) {
      stop(msg = paste("geo_type could not be auto-identified."))
    } else {
      stop(paste(
        msg = "Provided geo_type was", geo_type,
        "but format of some geo_values in predictions_cards do not match.")
      )
    }
  }
  else{
    return(found_geo_type)
  }
}

#' @param a vector of potential geo_types to check
#' @param geo_values vector of strings to validate
#'
#' @return If `geo_values` conform to a type in `geo_types`, the first
#'   conforming type is returned. Else return NULL.
validate_geo_type <- function(geo_types, geo_values) {
  assert_that(all(geo_types %in% c("county", "hrr", "msa", "dma", "state",
                                    "nation")),
              msg = "Unidentified geo_type")
  geo_type_lengths <- unique(nchar(geo_values))
  has_letters <- str_detect(geo_values, "[aA-zZ]")

  if (length(geo_type_lengths) > 1) {
    assert_that(any(geo_types %in% c("hrr", "msa", "dma")),
                msg = paste("Provided geo_values vary in length, which is not",
                            "allowed for provided geo_types:", geo_types))
  }
  return_type <- NULL
  for (geo_type in geo_types) {
    if (geo_type == "county") {
      if ((geo_type_lengths == 5) & !any(has_letters)) {
        return_type <- "county"
        break
      }
    }
    if (geo_type == "state") {
      if ((geo_type_lengths == 2) & all(has_letters)) {
        return_type <- "state"
        break
      }
    }
    if (geo_type == "nation") {
      if (all(geo_values == "us")) {
        return_type <- "nation"
        break
      }
    }
    if (geo_type %in% c("hrr", "msa", "dma")) {
      if (!any(has_letters)) {
        return_type <- geo_types
        break
      }
    }
  }
  return(return_type)
}
