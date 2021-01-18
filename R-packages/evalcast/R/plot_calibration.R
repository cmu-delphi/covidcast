#' Plot calibration curves
#'
#' @param scorecard Single score card.
#' @param type One of "wedgeplot" or "traditional".
#' @param grp_vars variables over which to compare calibration. These
#'   determines the color of the lines and faceting depending on `type`
#' @param avg_vars variables over which we average to determine the calibration.
#' @param legend_position Legend position, the default being "bottom".
#'
#' @export
plot_calibration <- function(scorecard,
                             type = c("wedgeplot", "traditional"),
                             grp_vars = c("forecaster", "forecast_date", "ahead"),
                             avg_vars = c("geo_value"),
                             legend_position = "bottom") {
  
  type <- match.arg(type)
  grp_tbl <- scorecard %>% 
    summarise(across(all_of(grp_vars), ~n_distinct(.x))) %>%
    pivot_longer(everything()) %>%
    filter(.data$value > 1) 
  if (nrow(grp_tbl) > 0) {
    grps <- grp_tbl %>% 
      arrange(desc(.data$value)) %>%
      select(.data$name) %>%
      pull()
  }
    
  if (type == "wedgeplot") {
    assert_that(nrow(grp_tbl) < 3,
                msg = paste("For wedgeplots, it's challenging to see results",
                            "with more than two groupings. Either filter your",
                            "scorecard, use the traditional version, or try",
                            "writing your own version."))
    calib <- compute_calibration(scorecard, grp_vars, avg_vars) %>%
      setup_wedgeplot()

    g <- calib %>%
      ggplot(aes(x = .data$nominal_prob,
                 y = .data$proportion,
                 colour = .data$coverage_type)) +
      geom_line(aes(alpha = .data$emph, size = .data$emph)) +
      scale_color_manual(values = c("blue","orange"), name="") +
      geom_point(aes(alpha = .data$emph, size = .data$emph)) +
      geom_segment(aes(x = 0, y = 0, xend = 0.5, yend = 0.5), 
                   size = 0.25, color = "black") +
      geom_segment(aes(x = 0.5, y = 0.5, xend = 1, yend = 0), 
                   size = 0.25, color = "black") +
      labs(x = "Nominal quantile level",
           y = "Proportion",
           title = sprintf("%s (ahead = %s): Proportion above/below", 
                           .data$name, .data$ahead)) +
      scale_alpha_continuous(range = c(0.5, 1)) +
      scale_size_continuous(range = c(0.5, 1)) +
      guides(alpha = FALSE, size = FALSE)
    if (nrow(grp_tbl) == 2L) {
      g <- g + facet_grid(stats::as.formula(paste(grps, collapse = "~")))
    }
    if (nrow(grp_tbl) == 1L) {
      g <- g + facet_wrap(stats::as.formula(paste0("~", grps)))
    }
    
  } else if (type == "traditional") {
    assert_that(nrow(grp_tbl) < 4,
                msg = paste("For traditional calibration plots, it's",
                            "challenging to see results",
                            "with more than three groupings. Either filter",
                            "your scorecard or try writing your own version."))
    calib <- compute_calibration(scorecard, grp_vars, avg_vars)
    g <- calib %>%
      ggplot(aes(x = .data$nominal_prob, y = .data$prop_below)) +
      geom_abline(slope = 1, intercept = 0) +
      labs(x = "Quantile level",
           y = "Proportion",
           title = sprintf("%s (ahead %s): Calibration", 
                           .data$name, .data$ahead))
    if (nrow(grp_tbl) == 3L) {
      g <- g + geom_line(aes(color = !!sym(grps[1]))) +
        scale_color_viridis_d() +
        facet_grid(stats::as.formula(paste(grps[2:3], collapse = "~")))
    }
    if (nrow(grp_tbl) == 2L) {
      g <- g + geom_line(aes(color = !!sym(grps[1]))) +
        scale_color_viridis_d() +
        facet_wrap(stats::as.formula(paste0("~", grps[2])))
    }
    if (nrow(grp_tbl) == 1L) {
      g <- g + geom_line(color="orange") + geom_point() + 
        facet_wrap(stats::as.formula(paste0("~", grps)))
    }
  }
  g + xlim(0, 1) + ylim(0, 1) + theme_bw() + 
    theme(legend.position = legend_position)
}

#' Preprocessing for wedge calibration plot
#'
#' @param calib the output from [compute_calibration()]
#'
#' @return a data frame
#' @export
setup_wedgeplot <- function(calib) {
  calib %>%
    pivot_longer(contains("prop"), 
                 names_to = "coverage_type", 
                 values_to = "proportion") %>%
    mutate(emph = ifelse(
      (.data$coverage_type == "prop_above" & .data$nominal_prob < 0.5) |
        (.data$coverage_type == "prop_below" & .data$nominal_prob >= 0.5), 
      0.5, 1)) %>%
    mutate(coverage_type = recode(.data$coverage_type,
                                  prop_above = "Proportion above",
                                  prop_below = "Proportion below"))
}


#' Plot interval coverage
#'
#' @param scorecards List of different score cards, all on the same forecasting
#'   task (i.e., same ahead, etc.).
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
plot_coverage <- function(scorecards, 
                          type = c("all", "one"), 
                          grp_vars = c("forecaster", "forecast_date", "ahead"),
                          avg_vars = c("geo_value"),
                          coverage = 0.8, 
                          legend_position = "bottom") {
  type <- match.arg(type)
  # make sure scorecards are comparable:
  grps <- grp_processing_for_facets(scorecards, grp_vars)
  ngrps <- length(grps)
  
  cover <- compute_coverage(scorecards, grp_vars, avg_vars)
  if (type == "all") {
   
    assert_that(ngrps < 4,
                msg = paste("For `all` coverage plots, it's",
                            "challenging to see results",
                            "with more than three groupings. Either filter",
                            "your scorecard or try writing your own version."))
    
    g <- cover %>%
      ggplot(aes(x = .data$nominal_prob, y = .data$prop_covered)) +
      geom_abline(slope = 1, intercept = 0) +
      xlim(0, 1) +
      ylim(0, 1) +
      labs(x = "Nominal coverage", y = "Empirical coverage")
    
    
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
      g <- g + geom_line(aes(color = !!sym(grps)))
    }
    if (ngrps == 0L) {
      g <- g + geom_line(color = "orange") + geom_point(color="orange")
    }
  } else {
    assert_that(ngrps < 5,
                msg = paste("For `one` coverage plots, it's",
                            "challenging to see results",
                            "with more than four groupings. Either filter",
                            "your scorecard or try writing your own version."))
    g <- cover %>%
      filter(.data$nominal_prob == coverage) %>%
      group_by(across(all_of(grp_vars))) %>%
      summarize(prop_covered = mean(.data$prop_covered, na.rm = TRUE)) %>%
      ggplot(aes(x = !! sym(grps[1]),
                 y = .data$prop_covered)) +
      geom_hline(yintercept = 1 - alpha, lty = 2) +
      labs(x = grps[1], y = "Empirical coverage")
    if (ngrps > 1L) {
      g <- g + 
        geom_line(aes(color = !!sym(grps[2]))) +
        geom_point(aes(color = !!sym(grps[2]))) +
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
