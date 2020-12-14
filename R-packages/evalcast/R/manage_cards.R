#' Reduce size of cards
#'
#' @param cards either predictions_cards or scorecards
#'
#' @return cards of the same class but with only one row for each 
#'   geo_value/forecast_date/ahead/forecaster (the point estimate)
#'   
#' @export
collapse_cards <- function(cards){
  cls <- class(cards)[1]
  assert_that(cls %in% c("predictions_cards", "score_cards"),
              msg=paste("This function is only appropriate for",
                        "predictions_cards or score_cards classes."))
  cards <- cards %>% 
    filter(abs(.data$quantile - 0.5) < 1e-8 | is.na(.data$quantile)) %>%
    mutate(quantile = ifelse(is.na(.data$quantile), "p","m"))
  if (n_distinct(cards$quantile) == 1) {
    cards <- cards %>% 
      mutate(quantile = ifelse(.data$quantile == "p", NA, 0.5))
  } else {
    cards <- cards %>%
      pivot_wider(names_from = .data$quantile, values_from = .data$value) %>%
      mutate(quantile = ifelse(is.na(.data$p), 0.5, NA),
             value = ifelse(is.na(.data$p), .data$m, .data$p)) %>%
      select(-.data$p, -.data$m)
  }
  cards <- cards %>% 
    relocate(.data$quantile:.data$value, .after = .data$geo_value)
  class(cards) = c(cls, class(cards))
  cards
}



get_and_check_pc_attributes <- function(predictions_cards) {
  # using do.call in the next line to keep these of class Date:
  # https://stackoverflow.com/questions/15659783/why-does-unlist-kill-dates-in-r
  forecast_dates <- do.call("c",
                            predictions_cards %>%
                              map(~ attr(.x, "forecast_date")))
  assert_that(length(unique(forecast_dates)) == length(forecast_dates),
              msg="Each predictions card must have a distinct forecast date.")
  list(
    forecast_dates = forecast_dates,
    name_of_forecaster = unique_attr(predictions_cards, "name_of_forecaster"),
    signals = unique_attr(predictions_cards, "signals"),
    ahead = as.numeric(unique_attr(predictions_cards, "ahead")),
    incidence_period = unique_attr(predictions_cards, "incidence_period"),
    geo_type = unique_attr(predictions_cards, "geo_type")
  )
}


unique_attr <- function(cards, attribute) {
  attr_list <- all_attr(cards, attribute)
  if (attribute == "signals")
    attr_list <- attr_list %>%
      map(~ .x[1, names(.x) != "start_day"]) # RJT: I added this part. I don't
  # think start dates need to match here. Now that I'm allowing start_day to be 
  # a function, it can depend on the forecast_date, so the next assertion would
  # (unintentionally) fail in that case. Note: this if statement on "signals" is
  # a bit of a hack and there might be a cleaner way to do it but it works for
  # now.  
  assert_that(length(unique(attr_list)) <= 1,
              msg=sprintf("These cards do not all have the same %s.",
                          attribute))
  return(attr_list[[1]])
}

unique_for_ahead <- function(cards, attribute) {
  att <- select(cards, !!attribute) %>% distinct()
  ahead <- cards$ahead[1]
  assert_that(nrow(att) == 1,
              msg=sprintf("These cards do not all have the same %s for ahead %i.",
                          attribute, ahead))
  invisible(pull(att))
}



all_attr <- function(cards, attribute) {
  return(cards %>% map(~ attr(.x, attribute)))
}


intersect_averagers <- function(cards, grp_vars, avg_vars) {
  
  assert_that(length(intersect(grp_vars, avg_vars)) == 0L,
              msg = paste("In intersect_averagers: grp_vars and avg_vars",
                          "must not overlap"))
  distinct_averagers <- cards %>%
    select(all_of(avg_vars)) %>%
    distinct()
  
  averagers_intersected <- cards %>% 
    select(all_of(c(grp_vars, avg_vars))) %>%
    group_by(across(all_of(grp_vars))) %>%
    group_split(.keep = FALSE) %>% 
    lapply(distinct) %>%
    reduce(intersect)
  
  assert_that(nrow(averagers_intersected) > 1L,
              msg = paste("In intersect_averagers: there was at most one",
                          "common row to average over."))

  if (nrow(distinct_averagers) > nrow(averagers_intersected)) {
      message(paste("In intersect_averagers: Some avg_vars were not common",
                    "to all grp_vars. Only the intersection is used for",
                    "averaging."))
  }
  cards %>% right_join(averagers_intersected)
}




# Print method for predictions_cards
# 
# @param x Prediction card.
# @export
# print.predictions_cards <- function(x, ...) {
#   z = unique(x$geo_value)
#   cat("Overview:\n")
#   cat("  Name of forecaster:", x$forecaster[1], "\n")
#   cat("  Forecast date:", as.character(unique(x$forecast_date)), "\n")
#   cat("  Ahead:", unique(x$ahead), "\n")
#   cat("  Geo type:", ifelse(nchar(z[1])==2, "state", "county"), "\n")
#   if (length(z) < 4){
#     cat("  Geo values:", z, "\n")
#   } else {
#     cat("  Geo values:", "*", "\n")
#   }
#   cat("  Incidence period:", x$incidence_period[1], "\n")
#   cat("  Response Data source:", x$data_source[1], "\n")
#   cat("  Response Signal:", x$signal[1], "\n")
#   NextMethod(x, ...)
# }


# Print a single score card.
# @param card Score card.
# @export
# print.score_card <- function(x, ...) {
#   z = unique(x$geo_value)
#   zz = unique(x$forecaster)
#   cat("Overview:\n")
#   cat("  Forecaster(s):", zz, "\n")
#   cat("  Forecast date:", as.character(unique(x$forecast_date)), "\n")
#   cat("  Ahead:", unique(x$ahead), "\n")
#   cat("  Geo type:", ifelse(nchar(z[1])==2, "state", "county"), "\n")
#   if (length(z) < 4){
#     cat("  Geo values:", z, "\n")
#   } else {
#     cat("  Geo values:", "*", "\n")
#   }
#   cat("  Incidence period:", x$incidence_period[1], "\n")
#   cat("  Response Data source:", x$data_source[1], "\n")
#   cat("  Response Signal:", x$signal[1], "\n")
#   NextMethod(x, ...)
# }
