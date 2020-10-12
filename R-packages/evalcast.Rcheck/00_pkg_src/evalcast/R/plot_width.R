#' Plot the interval width
#'
#' Interval width does not depend on the actual outcome, so this function can
#' be called on predictions cards in addition to scorecards.
#'
#' @param cards a list of different forecasters scorecards (or predictions
#'  cards), all on the same forecasting task (i.e., same ahead, etc.)
#' @param location of vertical line
#' @export
plot_width <- function(cards, alpha = 0.2) {
  unique_attr(scorecards, "ahead")
  unique_attr(scorecards, "geo_type")
  unique_attr(scorecards, "incidence_period")
  unique_attr(scorecards, "response")
  scorecards <- intersect_locations(scorecards)
  df <- scorecards %>%
    set_names(all_attr(scorecards, "name_of_forecaster")) %>%
    bind_rows(.id = "forecaster")
  df$coverage <- df$forecast_distribution %>%
    map(compute_width_single_distribution)
  df %>%
    select(forecaster, location, forecast_date, coverage) %>%
    unnest(coverage) %>%
    group_by(forecaster, nominal, forecast_date) %>%
    summarize(median = median(width),
              max = max(width)) %>%
    pivot_longer(cols = median:max,
                 names_to = "summary",
                 values_to = "width") %>%
    ggplot(aes(x = nominal,
               y = width,
               color = forecaster,
               lty = summary)) +
    geom_line() +
    geom_vline(xintercept = 1 - alpha, lty = 2) +
    facet_wrap(~ forecast_date) +
    labs(x = "Nominal coverage", y = "Interval Width")
}

compute_width_single_distribution <- function(forecast_distribution) {
  lower <- forecast_distribution %>% mutate(probs = round(probs, 4))
  upper <- forecast_distribution %>% mutate(probs = round(1 - probs, 4))
  lower %>%
    left_join(upper, by = "probs") %>%
    filter(probs < 0.5) %>%
    transmute(nominal = 1 - 2 * probs,
              width = quantiles.y - quantiles.x)
}
