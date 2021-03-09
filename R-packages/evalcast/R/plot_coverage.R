
#' Plot interval coverage
#'
#' @param predictions_cards tibble of predictions
#'   that are all for the same prediction task, meaning they are for the same
#'   response, incidence period,and geo type. Forecasts may be for a
#'   different forecast date or forecaster.
#'   A predictions card may be created by the function
#'   [get_predictions()], downloaded with [get_covidhub_predictions()] or
#'   possibly created manually.
#' @param facet_rows A variable name to facet data over. Creates a
#'   separate row of plots for each value of specified variable. Can be used
#'   with `facet_cols` to create a grid of plots.
#' @param facet_cols Same as `facet_rows`, but with columns.
#' @param legend_position Legend position, the default being "bottom".
#' @param grp_vars variables over which to compare coverage
#' @param avg_vars variables over which we average to determine the proportion
#'   of coverage.
#' @template geo_type-template
#' @param type One of "all" or "none", indicating whether to show coverage
#'   across all nominal levels (in which case averaging is performed across
#'   `avg_vars`) or whether to show it for one specific alpha
#'   value.
#' @param backfill_buffer How many days until response is deemed trustworthy
#'   enough to be taken as correct?
#' @param coverage If `type = "one"`, then coverage is the nominal interval 
#'   coverage shown.
#' 
#' @export 
plot_coverage <- function(predictions_cards,
                          facet_rows = c("forecaster"),
                          facet_cols = c("forecast_date"),
                          legend_position = "bottom",
                          grp_vars = c("forecaster", "forecast_date", "ahead"),
                          avg_vars = c("geo_value"),
                          geo_type = c("county", "hrr", "msa", "dma", "state",
                                       "hhs", "nation"),
                          type = c("all", "one"), 
                          backfill_buffer = 10,
                          coverage = 0.8) {
  geo_type <- match.arg(geo_type)
  type <- match.arg(type)
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

  scorecards <- left_join(predictions_cards,
                          get_covidcast_data(predictions_cards,
                                             backfill_buffer,
                                             geo_type),
                          by = c("geo_value",
                                 "forecast_date",
                                 "ahead"))
  
  # make sure scorecards are comparable:
  grps <- grp_processing_for_facets(scorecards, grp_vars)
  cover <- compute_coverage(scorecards, grp_vars, avg_vars)

  facet_layer <- facet_grid(rows = vars(!!!syms(facet_rows)),
                            cols = vars(!!!syms(facet_cols)))
  color_vars <- setdiff(grp_vars, c(facet_rows, facet_cols))

  if (type == "all") {
    g <- cover %>%
      mutate(color = Interaction(!!!syms(color_vars))) %>%
      ggplot(aes(x = .data$nominal_prob, y = .data$prop_covered)) +
      geom_abline(slope = 1, intercept = 0) +
      xlim(0, 1) +
      ylim(0, 1) +
      labs(x = "Nominal coverage", y = "Empirical coverage")
  } else {
    g <- cover %>%
      filter(.data$nominal_prob == coverage) %>%
      group_by(across(all_of(grp_vars))) %>%
      summarize(prop_covered = mean(.data$prop_covered, na.rm = TRUE)) %>%
      mutate(color = Interaction(!!!syms(color_vars))) %>%
      ggplot(aes(x = !! sym(grps[1]),
                 y = .data$prop_covered)) +
      geom_hline(yintercept = 1 - coverage, lty = 2) +
      labs(x = grps[1], y = "Empirical coverage") + 
      geom_point(aes(color = .data$color, group = .data$color))     
  }
  return(
    g +
    geom_line(aes(color = .data$color, group = .data$color)) +
    scale_color_viridis_d() +
    facet_layer +
    theme_bw() +
    theme(legend.position = legend_position))
}
