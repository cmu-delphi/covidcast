#' Plot error measure (deprecated)
#'
#' @param scorecards List of different score cards, all on the same forecasting
#'   task (i.e., same ahead, etc.).
#' @param err_name Name of a column appearing in all score cards.
#' @param type One of "boxplot" or "dotplot".
#'
#' @export
plot_measure <- function(scorecards, err_name, type = "boxplot") {
  stop("This function has been deprecated. Pass the scorecards to ggplot.")
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
  ## err_name2 <- rlang::sym(err_name)
  if (type == "boxplot") {
    all %>%
      ##  ggplot(aes(x = forecaster, y = !!err_name2)) +
      ggplot(aes(x = .data$forecaster, y = .data[[err_name]])) +
      geom_boxplot() +
      facet_wrap(~ forecast_date) +
      scale_y_log10() + 
      theme_bw() 
  }
  else if (type == "dotplot") {
    if (geo_type == "state") {
      nams <- covidcast::fips_to_name(paste0(all$location, "000"))
      all <- all %>% mutate(location = if_else(is.na(nams), .data$location, nams))
    }
    if (geo_type == "county") {
      nams <- covidcast::fips_to_name(all$location)
      all <- all %>% mutate(location = if_else(is.na(nams), .data$location, nams))
    }
    ordered_levels <- all %>%
      group_by(.data$location) %>%
      ## summarize(avg_err = mean(!!err_name2)) %>%
      summarize(avg_err = mean(.data[[err_name]])) %>%
      arrange(.data$avg_err)
    all$location <- factor(all$location, levels = ordered_levels$location)
    all %>%
      mutate(forecast_date = factor(.data$forecast_date)) %>%
      ## ggplot(aes(x = !!err_name2,
      ##            y = location,
      ##            color = forecaster,
      ##            pch = forecast_date)) +
      ggplot(aes(x = .data[[err_name]],
                 y = .data$location,
                 color = .data$forecaster,
                 pch = .data$forecast_date)) +
      geom_point() +
      theme_bw()
  }
}
