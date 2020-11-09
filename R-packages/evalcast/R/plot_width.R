#' Plot interval width
#'
#' @param cards List of different score cards (or predictions cards), all on the
#'   same forecasting task (i.e., same ahead, etc.).
#' @param alpha Deprecated parameter to be removed soon.
#' @param levels Quantile levels for the summary of interval width, to be
#'   plotted. For example, `levels = c(0.5, 0.7, 0.9)`, the default, plots the
#'   median, 70% and 90% quantiles of interval widths.
#' @param legend_position Legend position, the default being "bottom".
#' 
#' @details Interval width does not depend on the actual outcome, so this
#'   function can be called on predictions cards in addition to score cards.
#' 
#' @importFrom rlang .data set_names
#' @importFrom purrr map
#' @importFrom dplyr group_by summarize select bind_rows
#' @importFrom tidyr pivot_longer unnest
#' @importFrom ggplot2 ggplot aes geom_line geom_vline facet_wrap labs theme
#' @importFrom stats median
#' @export
plot_width <- function(cards, alpha = 0.2, levels = c(0.5, 0.7, 0.9),
                       legend_position = "bottom") {
  # make sure scorecards are comparable:
  unique_attr(cards, "ahead")
  unique_attr(cards, "geo_type") 
  unique_attr(cards, "incidence_period")
  unique_attr(cards, "response")
  cards <- intersect_locations(cards)
  df <- cards %>%
    set_names(all_attr(cards, "name_of_forecaster")) %>%
    bind_rows(.id = "forecaster")
  df$coverage <- df$forecast_distribution %>%
    map(compute_width_single_distribution)
  df %>%
    select(.data$forecaster, .data$location, .data$forecast_date, .data$coverage) %>%
    unnest(.data$coverage) %>%
    group_by(.data$forecaster, .data$nominal, .data$forecast_date) %>%
    summarize(level = as.factor(levels),
              width = quantile(.data$width, probs = levels)) %>%
    ggplot(aes(x = .data$nominal,
               y = .data$width,
               color = .data$forecaster,
               lty = .data$level)) +
    geom_line() +
    facet_wrap(~ .data$forecast_date) +
    labs(x = "Nominal coverage", y = "Interval width") + 
    theme_bw() + theme(legend.position = legend_position)
}

#' @importFrom rlang .data
#' @importFrom dplyr left_join filter transmute
compute_width_single_distribution <- function(forecast_distribution) {
  lower <- forecast_distribution %>% mutate(probs = round(.data$probs, 4))
  upper <- forecast_distribution %>% mutate(probs = round(1 - .data$probs, 4))
  lower %>%
    left_join(upper, by = "probs") %>%
    filter(.data$probs < 0.5) %>%
    transmute(nominal = 1 - 2 * .data$probs,
              width = .data$quantiles.y - .data$quantiles.x)
}
