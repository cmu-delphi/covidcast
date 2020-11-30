#' Plot calibration curves
#'
#' @param scorecard Single score card.
#' @param type One of "wedgeplot" or "traditional".
#' @param alpha Deprecated parameter to be removed soon.
#' @param legend_position Legend position, the default being "bottom".
#'
#' @export
plot_calibration <- function(scorecard,
                             type = c("wedgeplot", "traditional"),
                             grp_vars = c("forecaster", "forecast_date", "ahead"),
                             avg_vars = c("location"),
                             legend_position = "bottom") {
  type <- match.arg(type)
  if (type == "wedgeplot") {
    calib <- compute_calibration(scorecard, grp_vars, avg_vars) %>%
      pivot_longer(contains("prop"),
                   names_to = "coverage_type",
                   values_to = "proportion") %>%
      mutate(emph = ifelse(
        (.data$coverage_type == "prop_above" & .data$nominal_quantile < 0.5) |
          (.data$coverage_type == "prop_below" & .data$nominal_quantile >= 0.5), 
        0.5, 1)) %>%
      mutate(coverage_type = recode(.data$coverage_type,
                                    prop_above = "Proportion above",
                                    prop_below = "Proportion below"))
    g <- ggplot(aes(x = .data$nominal_prob,
                    y = .data$proportion,
                    colour = .data$coverage_type)) +
      geom_line(aes(alpha = .data$emph, size = .data$emph)) +
      geom_point(aes(alpha = .data$emph, size = .data$emph)) +
      geom_abline(intercept = 0, slope = 1) +
      geom_abline(intercept = 1, slope = -1) +
      labs(x = "Nominal quantile level",
           y = "Proportion",
           title = sprintf("%s (ahead = %s): Proportion above/below", name, ahead)) +
      scale_colour_discrete(name = "") +
      scale_alpha_continuous(range = c(0.5, 1)) +
      scale_size_continuous(range = c(0.5, 1)) +
      guides(alpha = FALSE, size = FALSE)
  } else if (type == "traditional") {
    calib <- compute_actual_vs_nominal_prob(scorecard, grp_vars, avg_vars)
    g <- calib %>%
      ggplot(aes(x = .data$nominal_prob, y = .data$prop_below)) +
      geom_line(color = "red") +
      geom_point(color = "red") +
      geom_abline(slope = 1, intercept = 0) +
      labs(x = "Quantile level",
           y = "Proportion",
           title = sprintf("%s (ahead %s): Calibration", name, ahead))
  }
  g +
    facet_wrap(~ forecast_date) +
    xlim(0, 1) +
    ylim(0, 1) +
    theme_bw() + theme(legend.position = legend_position)
}

#' Plot interval coverage
#'
#' @param scorecards List of different score cards, all on the same forecasting
#'   task (i.e., same ahead, etc.).
#' @param type One of "all" or "none", indicating whether to show coverage
#'   across all nominal levels (in which case averaging is performed across
#'   forecast dates and locations) or whether to show it for one specific alpha
#'   value.
#' @param grp_vars variables over which to compare coverage. The first 
#'   determines the color of the lines while the rest will be faceted over
#' @param avg_vars variables over which we average to determine the proportion
#'   of coverage. If `type = "one"`, 
#' @param alpha If `type = "one"`, then 1-alpha is the nominal interval coverage
#'   shown.
#' @param legend_position Legend position, the default being "bottom".
#' 
#' @export 
plot_coverage <- function(scorecards, type = c("all", "one"), 
                          grp_vars = c("forecaster", "forecast_date", "ahead"),
                          avg_vars = c("location"),
                          alpha = 0.2, 
                          legend_position = "bottom") {
  type <- match.arg(type)
  # make sure scorecards are comparable:
  unique_attr(scorecards, "ahead")
  unique_attr(scorecards, "as_of")
  unique_attr(scorecards, "geo_type")
  unique_attr(scorecards, "incidence_period")
  unique_attr(scorecards, "backfill_buffer")
  unique_attr(scorecards, "response")
  scorecards <- intersect_locations(scorecards)
  cover <- scorecards %>%
    set_names(all_attr(scorecards, "name_of_forecaster")) %>%
    map_dfr(compute_coverage, .id = "forecaster")
  if (type == "all") {
    cover %>%
      ggplot(aes(x = .data$nominal_coverage_prob,
                 y = .data$prop_covered,
                 color = .data$forecaster)) +
      geom_line() +
      geom_abline(slope = 1, intercept = 0) +
      facet_wrap(~ .data$forecast_date) +
      xlim(0, 1) +
      ylim(0, 1) +
      labs(x = "Nominal coverage", y = "Empirical coverage") +
      theme_bw() + theme(legend.position = legend_position)
  } else {
    cover %>%
      filter(.data$nominal_coverage_prob == 1 - alpha) %>%
      group_by(.data$forecast_date, .data$forecaster) %>%
      summarize(prop_covered = mean(.data$prop_covered, na.rm = TRUE)) %>%
      ggplot(aes(x = .data$forecast_date,
                 y = .data$prop_covered,
                 color = .data$forecaster)) +
      geom_point() + geom_line() +
      geom_hline(yintercept = 1 - alpha, lty = 2) +
      labs(x = "Forecast date", y = "Empirical coverage") +
      theme_bw() + theme(legend.position = legend_position)
  }
}
