
#' Summarize value over the trailing epiweek
#'
#' @param daily_df a data frame containing the columns `time_value`, `geo_value`,
#'   and `value`. Additional columns are ignored
#' @param return_sum multiply the average by 7 to compute the total cases per 
#'   week (the default) or return the average over the week
#'
#' @return a data frame of the same type with 1 row per complete epiweek. The
#'   `time_value` entries will be modified to correspond to the last day
#'   (a Saturday) of each epiweek. 
#'   
#'   These functions vary only in the manner of the calculation. `average_*()`
#'   computes the average over the week ignoring `NA`'s and returns the result.
#'   `sum_*` sums over the epiweek. Thus in the absence of missing values, the
#'   results (when `return_sum = TRUE`) will be the same. However, if there are
#'   missing values, the result of `sum_*()` will be lower but will accurately 
#'   reflect the reported totals.
#' @export
average_to_epiweek <- function(daily_df, return_sum = TRUE) { 
  multiplier <- ifelse(return_sum, 7, 1)
  daily_df %>% 
    bind_cols(daily_df, MMWRweek::MMWRweek(daily_df$time_value)) %>%
    mutate(time_value = 
             MMWRweek::MMWRweek2Date(.data$MMWRyear, .data$MMWRweek, 7)) %>%
    group_by(.data$geo_value, .data$time_value) %>%
    summarise(value = mean(.data$value, na.rm = TRUE) * multiplier) %>%
    ungroup()
}

#' @rdname average_to_epiweek
sum_to_epiweek <- function(daily_df){
  daily_df <- bind_cols(daily_df, MMWRweek::MMWRweek(daily_df$time_value)) %>%
    mutate(time_value = 
             MMWRweek::MMWRweek2Date(.data$MMWRyear, .data$MMWRweek, 7)) %>%
    group_by(.data$geo_value, .data$time_value) %>%
    summarise(value = sum(.data$value, na.rm = TRUE)) %>%
    ungroup()
}


shift_day_to_preceding_xxxday <- function(day, xxx){
  ew_day <- MMWRweek::MMWRweek(day)
  if(ew_day$MMWRday < xxx) {
    MMWRweek::MMWRweek2Date(MMWRyear = ew_day$MMWRyear,
                            MMWRweek = ew_day$MMWRweek,
                            MMWRday = xxx) - 7
  } else {
    MMWRweek::MMWRweek2Date(MMWRyear = ew_day$MMWRyear, 
                            MMWRweek = ew_day$MMWRweek, 
                            MMWRday = xxx)
  }
}

shift_day_to_following_xxxday <- function(day, xxx){
  ew_day <- MMWRweek::MMWRweek(day)
  if(ew_day$MMWRday > xxx) {
    MMWRweek::MMWRweek2Date(MMWRyear = ew_day$MMWRyear,
                            MMWRweek = ew_day$MMWRweek,
                            MMWRday = xxx) + 7
  } else {
    MMWRweek::MMWRweek2Date(MMWRyear = ew_day$MMWRyear, 
                            MMWRweek = ew_day$MMWRweek, 
                            MMWRday = xxx)
  }
}
