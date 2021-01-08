#' Plot interval width
#'
#' @param cards scorecards or predictions cards (or a data frame with columns
#'   `quantile`, `value` and any grouping vars and averaging vars)
#' @param levels Quantile levels for the summary of interval width, to be
#'   plotted. For example, `levels = c(0.5, 0.7, 0.9)`, the default, plots the
#'   median, 70% and 90% quantiles of interval widths.
#' @param grp_vars variables over which to compare widths The first 
#'   determines the color of the lines while the rest will be faceted over
#' @param avg_vars variables over which we compute quantiles.
#' @param legend_position Legend position, the default being "bottom".
#' 
#' @details Interval width does not depend on the actual outcome, so this
#'   function can be called on predictions cards in addition to score cards.
#' 
#' @export
plot_width <- function(cards, 
                       levels = c(0.5, 0.7, 0.9),
                       grp_vars = c("forecaster", "forecast_date", "ahead"),
                       avg_vars = c("geo_value"),
                       legend_position = "bottom") {
  
  grps <- grp_processing_for_facets(cards, grp_vars, 4, "interval width plots")
  ngrps <- length(grps)
  cards <- compute_width(cards, levels, grp_vars, avg_vars)
  
  g <- cards %>%
    ggplot(aes(x = .data$nominal_prob,
               y = .data$width,
               lty = .data$level)) +
    labs(x = "Nominal coverage", y = "Interval width") + 
    theme_bw() + theme(legend.position = legend_position)
    
  if (ngrps == 3L) {
    g <- g + geom_line(aes(color = !!sym(grps[1]))) +
      scale_color_viridis_d() +
      facet_grid(stats::as.formula(paste(grps[2:3], collapse = "~")))
  }
  if (ngrps == 2L) {
    g <- g + geom_line(aes(color = !!sym(grps[1]))) +
      scale_color_viridis_d() +
      facet_wrap(stats::as.formula(paste0("~", grps[2])))
  }
  if (ngrps == 1L) {
    g <- g + geom_line(color="orange") + geom_point() + 
      facet_wrap(stats::as.formula(paste0("~", grps)))
  }
  g
}


#' Summarize forecast interval widths
#'
#' @param cards scorecards or predictions cards (or a data frame with columns
#'   `quantile`, `value` and any grouping vars and averaging vars)
#' @param levels Quantile levels to summarize the interval widths. For example, 
#'   `levels = c(0.5, 0.7, 0.9)`, the default, computes the
#'   median, 70% and 90% quantiles of interval widths within each combination
#'   of `grp_vars` over the `avg_vars`
#' @param grp_vars variables over which to compare widths 
#' @param avg_vars variables over which we compute quantiles.
#'
#' @return a summarized data frame (by grp_vars)
#' @export
compute_width <- function(cards, 
                          levels = c(0.5, 0.7, 0.9),
                          grp_vars = c("forecaster", "forecast_date", "ahead"),
                          avg_vars = c("geo_value")) {
  
  assert_that(all(c(grp_vars, avg_vars, "quantile", "value") %in% names(cards)),
              msg = paste("In compute_width: quantile, value,",
                          "grp_vars and avg_vars must be present in the cards.",
                          "See details."))
  # make sure cards are comparable:
  cards <- intersect_averagers(cards, grp_vars, avg_vars)
  cards <- cards %>% 
    filter(!is.na(.data$quantile)) %>%
    select(all_of(grp_vars), all_of(avg_vars),
           .data$quantile, .data$value)
  
  cards <- averaging_checks(cards, grp_vars, avg_vars) %>%
    group_by(across(all_of(c(grp_vars,avg_vars)))) %>%
    mutate(rev_value = rev(.data$value)) %>%
    filter(.data$quantile < 0.5) %>%
    mutate(nominal_prob = 1 - 2 * .data$quantile,
           width = .data$rev_value - .data$value) %>%
    ungroup() %>%
    select(all_of(grp_vars), .data$nominal_prob, .data$width) %>%
    group_by(across(all_of(grp_vars)), .data$nominal_prob) %>%
    summarise(level = as.factor(levels),
              width = stats::quantile(.data$width, probs = levels))
}
