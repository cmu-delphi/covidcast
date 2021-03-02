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

plot_unique_geo_types_present <- function(df_to_plot, plural_geo_title) {
  samples_per_day = df_to_plot %>%
    group_by(time_value) %>%
    summarize(n = n())
  
  day_diff =
    samples_per_day %>% 
    summarise(difftime(max(time_value), min(time_value), units = "days"))
  
  ggplot(samples_per_day, aes(x = time_value, y = n)) +
    geom_line() + geom_point() + theme_bw() +
    labs(
      x = "Date",
      y = sprintf("Number of %s", plural_geo_title),
      title = sprintf(
        "Unique %s present in signal (last %s days of data): %i, mean per day: %i",
        plural_geo_title,
        as.numeric(day_diff),
        length(unique(df_to_plot$geo_value)),
        round(mean(samples_per_day$n))
      )
    ) + date_scale
}

plot_sample_volume <- function(df_to_plot, y_axis_title) {
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

plot_data_frequency_choro <- function(df_to_plot, geo_type) {
  if (geo_type == "county") {
    geo_types_present = df_to_plot %>%
      group_by(geo_value) %>%
      summarize(value = n()) %>% ungroup() %>%
      filter(substr(geo_value, 3, 5) != "000")
  } else if (geo_type == "state") {
    geo_types_present = df_to_plot %>%
      group_by(geo_value) %>%
      summarize(value = n())
  } else {
    return()
  }
  
  day_diff = as.numeric(df_to_plot %>% 
                          summarise(difftime(max(time_value), min(time_value), units="days")))
  covidcast_signal_to_plot = make_covidcast_signal(geo_types_present, df_to_plot, geo_type)
  
  plot(
    covidcast_signal_to_plot,
    title = sprintf(
      "# of times %s appears in data in last %i days of data (start date: %s); grey = no data for %s.",
      geo_type,
      day_diff,
      covidcast_signal_to_plot$time_value[1],
      geo_type
    ),
    range = c(1, day_diff),
    choro_params = list(legend_digits = 0)
  )
}

