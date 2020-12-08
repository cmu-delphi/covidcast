#Shared libraries
library(covidcast)
library(dplyr)
library(ggplot2)

#Shared config and constants
knitr::opts_chunk$set(message = FALSE, warning = FALSE)

date_scale <-
  scale_x_date(date_breaks = "1 week", date_labels = "%b %d")

twenty_eight_days_ago = Sys.Date() - 28

# Utility methods for rendering plots
make_covidcast_signal <- function(destination, source, geo_type) {
  destination$time_value = source$time_value[1]
  destination$issue = source$issue[1]
  attributes(destination)$metadata$geo_type = geo_type
  class(destination) = c("covidcast_signal", "data.frame")
  return(destination)
}

plot_28_day_unique <- function(df_to_plot, plural_geo_title) {
  samples_per_day = df_to_plot %>%
    group_by(time_value) %>%
    summarize(n = n())
  
  ggplot(samples_per_day, aes(x = time_value, y = n)) +
    geom_line() + geom_point() + theme_bw() +
    labs(
      x = "Date",
      y = sprintf("Number of %s", plural_geo_title),
      title = sprintf(
        "Unique %s present in signal (last 28 days): %i, mean per day: %i",
        plural_geo_title,
        length(unique(df_to_plot$geo_value)),
        round(mean(samples_per_day$n))
      )
    ) + date_scale
}

plot_28_day_sample_volume <- function(df_to_plot, y_axis_title) {
  n_per_day = df_to_plot %>%
    group_by(time_value) %>%
    summarize(n = sum(value))
  
  ggplot(n_per_day, aes(x = time_value, y = n)) +
    geom_line() + geom_point() + theme_bw() +
    labs(
      x = "Date",
      y = y_axis_title,
      title = sprintf("Mean per day: %i",
                      round(sum(n_per_day$n)), round(mean(n_per_day$n)))
    ) +
    date_scale
}

plot_28_day_frequency_state  <- function(df_to_plot) {
  states_present = df_to_plot %>%
    group_by(geo_value) %>%
    summarize(value = n())
  
  covidcast_signal_to_plot = make_covidcast_signal(states_present, df_to_plot, "state")
  
  plot(
    covidcast_signal_to_plot,
    title = sprintf(
      "# of times state appears in data in last 28 days (start date: %s); grey = no data for state.",
      covidcast_signal_to_plot$time_value[1]
    ),
    range = c(1, 28),
    choro_params = list(legend_digits = 0)
  )
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
      "# of times county appears in data in last 28 days (start date: %s); grey = no data for county.",
      covidcast_signal_to_plot$time_value[1]
    ),
    range = c(1, 28),
    choro_params = list(legend_digits = 0)
  )
}
