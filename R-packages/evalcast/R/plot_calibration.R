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
  
    
  if (type == "wedgeplot") {
    grps <- grp_processing_for_facets(scorecard, grp_vars, 3, "wedgeplots")
    ngrps <- length(grps)
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
    if (ngrps == 2L) {
      g <- g + facet_grid(stats::as.formula(paste(grps, collapse = "~")))
    }
    if (ngrps == 1L) {
      g <- g + facet_wrap(stats::as.formula(paste0("~", grps)))
    }
    
  } else if (type == "traditional") {
    grps <- grp_processing_for_facets(scorecard, grp_vars, 4, 
                                      "traditional calibration plots")
    ngrps <- length(grps)
    calib <- compute_calibration(scorecard, grp_vars, avg_vars)
    g <- calib %>%
      ggplot(aes(x = .data$nominal_prob, y = .data$prop_below)) +
      geom_abline(slope = 1, intercept = 0) +
      labs(x = "Quantile level",
           y = "Proportion",
           title = sprintf("%s (ahead %s): Calibration", 
                           .data$name, .data$ahead))
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
