plot_28_day_frequency <- function(df_to_plot, geo_type) {
  # These are all required for rendering using covidcast.plot
  df_to_plot$time_value = "2020-04-15"
  df_to_plot$issue = "2020-04-15"
  attributes(df_to_plot)$geo_type = geo_type
  class(df_to_plot) = c("covidcast_signal", "data.frame")
  
  plot(df_to_plot, range=c(0,28))
}