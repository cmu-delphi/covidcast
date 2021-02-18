
#' Plot interval coverage
#'
#' @param predictions_cards tibble of predictions
#'   that are all for the same prediction task, meaning they are for the same
#'   response, incidence period,and geo type. Forecasts may be for a
#'   different forecast date or forecaster.
#'   A predictions card may be created by the function
#'   [get_predictions()], downloaded with [get_covidhub_predictions()] or
#'   possibly created manually.
#' @param backfill_buffer How many days until response is deemed trustworthy
#'   enough to be taken as correct?
#' @param type One of "all" or "none", indicating whether to show coverage
#'   across all nominal levels (in which case averaging is performed across
#'   `avg_vars`) or whether to show it for one specific alpha
#'   value.
#' @param grp_vars variables over which to compare coverage. The first 
#'   determines the color of the lines while the rest will be faceted over
#' @param avg_vars variables over which we average to determine the proportion
#'   of coverage. If `type = "one"`, 
#' @param coverage If `type = "one"`, then coverage is the nominal interval 
#'   coverage shown.
#' @param legend_position Legend position, the default being "bottom".
#' 
#' @export 
plot_coverage <- function(predictions_cards,
                          geo_type,
                          backfill_buffer = 10,
                          type = c("all", "one"), 
                          grp_vars = c("forecaster", "forecast_date", "ahead"),
                          avg_vars = c("geo_value"),
                          coverage = 0.8, 
                          legend_position = "bottom") {
  type <- match.arg(type)

  scorecards <- left_join(predictions_cards,
                          get_covidcast_data(predictions_cards,
                                             backfill_buffer,
                                             geo_type),
                          by = c("geo_value",
                                 "forecast_date",
                                 "ahead"))
  
  # make sure scorecards are comparable:
    
  if (type == "all") {
    grps <- grp_processing_for_facets(scorecards, grp_vars, 4, 
                                      "`all` coverage plots")
    ngrps <- length(grps)
    cover <- compute_coverage(scorecards, grp_vars, avg_vars)
    
    g <- cover %>%
      ggplot(aes(x = .data$nominal_prob, y = .data$prop_covered)) +
      geom_abline(slope = 1, intercept = 0) +
      xlim(0, 1) +
      ylim(0, 1) +
      labs(x = "Nominal coverage", y = "Empirical coverage")
    
    
    if (ngrps == 3L) {
      g <- g + geom_line(aes(color = as.factor(!!sym(grps[1])))) +
        scale_color_viridis_d() +
        facet_grid(stats::as.formula(paste(grps[2:3], collapse = "~")))
    }
    if (ngrps == 2L) {
      g <- g + geom_line(aes(color = as.factor(!!sym(grps[1])))) +
        scale_color_viridis_d() +
        facet_wrap(stats::as.formula(paste0("~", grps[2])))
    }
    if (ngrps == 1L) {
      g <- g + geom_line(aes(color = as.factor(!!sym(grps))))
    }
    if (ngrps == 0L) {
      g <- g + geom_line(color = "orange") + geom_point(color="orange")
    }
  } else {
    grps <- grp_processing_for_facets(scorecards, grp_vars, 5, 
                                      "`all` coverage plots")
    ngrps <- length(grps)
    cover <- compute_coverage(scorecards, grp_vars, avg_vars)
    
    g <- cover %>%
      filter(.data$nominal_prob == coverage) %>%
      group_by(across(all_of(grp_vars))) %>%
      summarize(prop_covered = mean(.data$prop_covered, na.rm = TRUE)) %>%
      ggplot(aes(x = !! sym(grps[1]),
                 y = .data$prop_covered)) +
      geom_hline(yintercept = 1 - coverage, lty = 2) +
      labs(x = grps[1], y = "Empirical coverage")
    if (ngrps > 1L) {
      g <- g + 
        geom_line(aes(color = as.factor(!!sym(grps[2])))) +
        geom_point(aes(color = as.factor(!!sym(grps[2])))) +
        scale_color_viridis_d()
    }
    if (ngrps == 4L) {
      g <- g + facet_grid(stats::as.formula(paste(grps[3:4], collapse = "~")))
    }
    if (ngrps == 3L) {
      g <- g + facet_wrap(stats::as.formula(paste0("~", grps[3])))
    }
    if (ngrps == 1L) {
      g <- g + 
        geom_line(color = "orange") +
        geom_point(color = "orange")
    }
  }
  g + theme_bw() + theme(legend.position = legend_position)
}
