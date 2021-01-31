
sum_to_epiweek <- function(daily_df){
  daily_df <- bind_cols(daily_df, MMWRweek::MMWRweek(daily_df$time_value)) %>%
    mutate(time_value = 
             MMWRweek::MMWRweek2Date(.data$MMWRyear, .data$MMWRweek, 7)) %>%
    group_by(.data$geo_value, .data$time_value) %>%
    summarise(value = sum(.data$value)) %>%
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
