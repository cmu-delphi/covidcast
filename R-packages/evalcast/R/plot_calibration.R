#' @export
plot_calibration <- function(scorecard,
                             type = c("wedgeplot", "traditional"),
                             alpha = 0.1) {
  name <- attr(scorecard, "name_of_forecaster")
  ahead <- attr(scorecard, "ahead")
  type <- match.arg(type)
  if (type == "wedgeplot") {
    g <- compute_calibration(scorecard) %>%
      pivot_longer(contains("prop"),
                   names_to = "coverage_type",
                   values_to = "proportion") %>%
      filter(coverage_type != "prop_covered") %>%
      mutate(emph = ifelse((coverage_type == "prop_above" & nominal_quantile < 0.5) |
                              (coverage_type == "prop_below" & nominal_quantile >= 0.5), 0.5, 1)) %>%
      ggplot(aes(x = nominal_quantile,
                 y = proportion,
                 colour = coverage_type)) +
      geom_line(aes(alpha = emph, size = emph)) +
      geom_point(aes(alpha = emph, size = emph)) +
      geom_abline(intercept = 0, slope = 1) +
      geom_abline(intercept = 1, slope = -1) +
      labs(x = "Nominal quantile level",
           y = "Proportion",
           title = sprintf(
             "%s (ahead %s) - How often is actual above/below quantile?",
             name,
             ahead)) +
      geom_vline(xintercept = c(alpha, 1 - alpha), lty = 2) +
      scale_colour_discrete(name = "") +
      scale_alpha_continuous(range = c(0.5, 1)) +
      scale_size_continuous(range = c(0.5, 1)) +
      guides(alpha = FALSE, size = FALSE)
  } else if (type == "traditional") {
    calib <- compute_actual_vs_nominal_prob(scorecard)
    g <- calib %>%
      ggplot(aes(x = nominal_prob, y = prop_below)) +
      geom_line(color = "red") +
      geom_point(color = "red") +
      geom_abline(slope = 1, intercept = 0) +
      labs(x = "Nominal quantile level",
           y = "Proportion",
           title = sprintf(
             "%s (ahead %s) - Calibration",
             name,
             ahead))
  }
  g +
    facet_wrap(~ forecast_date) +
    geom_vline(xintercept = c(alpha, 1 - alpha), lty = 2) +
    xlim(0, 1) +
    ylim(0, 1) +
    theme_bw()
}

#' Plot the interval coverage
#'
#' @param scorecards a list of different forecasters scorecards, all on the
#' same forecasting task (i.e., same ahead, etc.)
#' @param alpha location of vertical line if type = "all" is 1-alpha; if
#' type="one" then 1-alpha is the nominal coverage probability shown.
#' @param type whether to show coverage across all nominal levels
#' (in which case averaging is performed across forecast dates and locations)
#' or whether to show it for one specific alpha value.
#' @export
plot_coverage <- function(scorecards, alpha = 0.2, type = c("all", "one")) {
  type <- match.arg(type)
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
      ggplot(aes(x = nominal_coverage_prob,
                 y = prop_covered,
                 color = forecaster)) +
      geom_line() +
      geom_abline(slope = 1, intercept = 0) +
      geom_vline(xintercept = 1 - alpha, lty = 2) +
      facet_wrap(~ forecast_date) +
      theme_bw() +
      xlim(0, 1) +
      ylim(0, 1) +
      labs(x = "Nominal coverage", y = "Coverage proportion")
  } else {
    stop("not yet implemented")
    # plot of prop_covered vs. forecast_date.  Each forecaster is a line
    # shows coverage only for 1 - alpha interval
  }
}
