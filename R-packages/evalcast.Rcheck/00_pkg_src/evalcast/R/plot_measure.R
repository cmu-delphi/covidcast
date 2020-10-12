#' Plot a measure
#'
#' @param scorecards a list of scorecards
#' @param err_name the name of a column appearing in all scorecards
#' @export
plot_measure <- function(scorecards, err_name, type = "boxplot") {
  # make sure scorecards are comparable:
  unique_attr(scorecards, "ahead")
  unique_attr(scorecards, "as_of")
  geo_type <- unique_attr(scorecards, "geo_type")
  unique_attr(scorecards, "incidence_period")
  unique_attr(scorecards, "backfill_buffer")
  unique_attr(scorecards, "response")

  # combine scorecards:
  names(scorecards) <- scorecards %>% map_chr(~ attr(.x, "name_of_forecaster"))
  all <- bind_rows(scorecards, .id = "forecaster")

  # make plot:
  err_name2 <- rlang::sym(err_name)
  if (type == "boxplot") {
    all %>%
      ggplot(aes(x = forecaster, y = !!err_name2)) +
      geom_boxplot() +
      facet_wrap(~ forecast_date) +
      scale_y_log10()
  }
  else if (type == "dotplot") {
    if (geo_type == "state") {
      nams <- covidcast::fips_to_name(paste0(all$location, "000"))
      all <- all %>% mutate(location = if_else(is.na(nams), location, nams))
    }
    if (geo_type == "county") {
      nams <- covidcast::fips_to_name(all$location)
      all <- all %>% mutate(location = if_else(is.na(nams), location, nams))
    }
    ordered_levels <- all %>%
      group_by(location) %>%
      summarize(avg_err = mean(!!err_name2)) %>%
      arrange(avg_err)
    all$location <- factor(all$location, levels = ordered_levels$location)
    all %>%
      mutate(forecast_date = factor(forecast_date)) %>%
      ggplot(aes(x = !!err_name2,
                 y = location,
                 color = forecaster,
                 pch = forecast_date)) +
      geom_point()
  }
}
