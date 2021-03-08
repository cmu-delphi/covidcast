#' Plot interval width
#'
#' @param predictions_cards tibble of predictions
#'   that are all for the same prediction task, meaning they are for the same
#'   response, incidence period,and geo type. Forecasts may be for a
#'   different forecast date or forecaster.
#'   A predictions card may be created by the function
#'   [get_predictions()], downloaded with [get_covidhub_predictions()] or
#'   possibly created manually. Alternatively, any data frame with columns
#'   `quantile`, `value` and any grouping vars and averaging vars.
#' @param facet_rows A variable name to facet data over. Creates a
#'   separate row of plots for each value of specified variable. Can be used
#'   with `facet_cols` to create a grid of plots.  Should be passed to 
#'   plot_calibration when customized.
#' @param facet_cols Same as `facet_rows`, but with columns.
#' @param legend_position Legend position, the default being "bottom".
#' @param grp_vars variables over which to compare widths The first 
#'   determines the color of the lines while the rest will be faceted over
#' @param avg_vars variables over which we compute quantiles.
#' @param levels Quantile levels for the summary of interval width, to be
#'   plotted. For example, `levels = c(0.5, 0.7, 0.9)`, the default, plots the
#'   median, 70% and 90% quantiles of interval widths.
#' 
#' @details Interval width does not depend on the actual outcome, so this
#'   function can be called on predictions cards in addition to score cards.
#' 
#' @export
plot_width <- function(predictions_cards,
                       facet_rows = "forecast_date",
                       facet_cols = "forecaster",
                       legend_position = "bottom",
                       grp_vars = c("forecaster", "forecast_date", "ahead"),
                       avg_vars = c("geo_value"),
                       levels = c(0.5, 0.7, 0.9)) {
  
  if (!is.null(facet_rows)) {
    non_grouped_facet <- setdiff(facet_rows, grp_vars)
    assert_that(length(non_grouped_facet) == 0,
                msg = paste("Variables must be grouped in order to be faceted in rows:",
                            non_grouped_facet))
  }
  if (!is.null(facet_cols)) {
    non_grouped_facet <- setdiff(facet_cols, grp_vars)
    assert_that(length(non_grouped_facet) == 0,
                msg = paste("Variables must be grouped in order to be faceted in cols:",
                            non_grouped_facet))
  }
  
  facet_layer <- facet_grid(rows = vars(!!!syms(facet_rows)),
                            cols = vars(!!!syms(facet_cols)))
  
  color_vars <- setdiff(grp_vars, c(facet_rows, facet_cols))
  
  predictions_cards <- compute_width(
    predictions_cards, grp_vars, avg_vars, levels)
  
  g <- predictions_cards %>%
    mutate(color = as.factor(Interaction(!!!syms(color_vars)))) %>%
    ggplot(aes(x = .data$nominal_prob,
               y = .data$width,
               lty = .data$level)) +
    labs(x = "Nominal coverage", y = "Interval width") + 
    geom_line(aes(color = .data$color)) +
    scale_color_viridis_d() +
    facet_layer +
    theme_bw() + theme(legend.position = legend_position)
    
  return(g)
}


#' Summarize forecast interval widths
#'
#' @param predictions_cards predictions cards (or a data frame with columns
#'   `quantile`, `value` and any grouping vars and averaging vars)
#' @param grp_vars variables over which to compare widths 
#' @param avg_vars variables over which we compute quantiles.
#' @param levels Quantile levels to summarize the interval widths. For example, 
#'   `levels = c(0.5, 0.7, 0.9)`, the default, computes the
#'   median, 70% and 90% quantiles of interval widths within each combination
#'   of `grp_vars` over the `avg_vars`
#'
#' @return a summarized data frame (by grp_vars)
#' @export
compute_width <- function(predictions_cards,
                          grp_vars = c("forecaster", "forecast_date", "ahead"),
                          avg_vars = c("geo_value"),
                          levels = c(0.5, 0.7, 0.9)) {
  
  assert_that(all(c(grp_vars, avg_vars, "quantile", "value") 
                  %in% names(predictions_cards)),
              msg = paste("In compute_width: quantile, value,",
                          "grp_vars and avg_vars must be present in the cards.",
                          "See details."))
  # make sure cards are comparable:
  predictions_cards <- intersect_averagers(predictions_cards, grp_vars, avg_vars)

  predictions_cards <- predictions_cards %>% 
    filter(!is.na(.data$quantile)) %>%
    select(all_of(grp_vars), all_of(avg_vars),
           .data$quantile, .data$value)
  
  if(any(is.na(predictions_cards$value))) {
    message("Some quantiles values have NA values; these will be removed for
            the interval width plots.")
    predictions-cards <- predictions_cards %>% filter(!is.na(.data$value))
  }
  
  predictions_cards <- averaging_checks(
      predictions_cards, grp_vars, avg_vars) %>%
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
