make_covidcast_signal <- function(destination, source, geo_type) {
  destination$time_value = source$time_value[1]
  destination$issue = source$issue[1]
  attributes(destination)$metadata$geo_type = geo_type
  class(destination) = c("covidcast_signal", "data.frame")
  return(destination)
}

plot_28_day_frequency_state  <- function(df_to_plot) {
  states_present = df_to_plot %>%
    group_by(geo_value) %>%
    summarize(value = n())
  
  covidcast_signal_to_plot = make_covidcast_signal(states_present, df_to_plot, "state")
  
  plot(covidcast_signal_to_plot,
       title = sprintf(
         "State frequency in last 28 days (start date: %s)",
         covidcast_signal_to_plot$time_value[1]
       ),
       range = c(0, 28))
}

plot_28_day_frequency_county  <- function(df_to_plot) {
  counties_present = df_to_plot %>%
    group_by(geo_value) %>%
    summarize(value = n()) %>% ungroup() %>%
    filter(substr(geo_value, 3, 5) != "000")
  
  covidcast_signal_to_plot = make_covidcast_signal(counties_present, df_to_plot, "county")
  
  plot(
    covidcast_signal_to_plot,
    title = sprintf(
      "County frequency in last 28 days (start date: %s)",
      covidcast_signal_to_plot$time_value[1]
    ),
    range = c(0, 28)
  )
}
