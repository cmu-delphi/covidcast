#' Plot calibration curves
#'
#' @param predictions_cards Either one predictions_card or several joined
#'   together using bind_rows().
#' @param facet_rows A variable name to facet data over. Creates a
#'   separate row of plots for each value of specified variable. Can be used
#'   with `facet_cols` to create a grid of plots.  Should be passed to 
#'   plot_calibration when customized.
#' @param facet_cols Same as `facet_rows`, but with columns.
#' @param legend_position Legend position, the default being "bottom".
#' @param grp_vars variables over which to compare calibration. These
#'   determines the color of the lines and faceting depending on `type`
#' @param avg_vars variables over which we average to determine the calibration.
#' @template geo_type-template
#' @param type One of "wedgeplot" or "traditional"
#' @param backfill_buffer How many days until response is deemed trustworthy
#'   enough to be taken as correct?.
#' @export
plot_calibration <- function(predictions_cards,
                             facet_rows = NULL,
                             facet_cols = NULL,
                             legend_position = "bottom",
                             grp_vars = c("forecaster", "forecast_date", "ahead"),
                             avg_vars = c("geo_value"),
                             geo_type = c("county", "hrr", "msa", "dma", "state",
                                          "hhs", "nation"),
                             type = c("wedgeplot", "traditional"),
                             backfill_buffer = 10) {
  
  geo_type <- match.arg(geo_type)
  type <- match.arg(type)

  calib <- compute_calibration(predictions_cards,
                               geo_type,
                               backfill_buffer,
                               grp_vars,
                               avg_vars)
  
  if (type == "wedgeplot") {
    if (is.null(facet_rows) & is.null(facet_cols)) {
      g <- format_wedgeplot(calib, grp_vars)
    } else {
      g <- format_wedgeplot(calib, grp_vars, facet_rows, facet_cols)
    }
  } else if (type == "traditional") {
    if (is.null(facet_rows) & is.null(facet_cols)) {
      g <- format_traditional_calib_plot(calib, grp_vars)
    } else {
      g <- format_traditional_calib_plot(
        calib, grp_vars, facet_rows, facet_cols)
    }
  }
  
  return(
    g + xlim(0, 1) + ylim(0, 1) + theme_bw() + 
    theme(legend.position = legend_position))
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


#' ggplot formatting for wedge calibration plot.
#'
#' @param calib the output from [compute_calibration()]
#' @param grp_vars variables over which to compare calibration. These
#'   determines the color of the lines and faceting depending on `type`
#' @param facet_rows A variable name to facet data over. Creates a
#'   separate row of plots for each value of specified variable. Can be used
#'   with `facet_cols` to create a grid of plots.  Should be passed to 
#'   plot_calibration when customized.
#' @param facet_cols Same as `facet_rows`, but with columns.
#'
#' @return ggplot object
format_wedgeplot <- function(calib, 
                             grp_vars,
                             facet_rows = "forecast_date",
                             facet_cols = "forecaster"){
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
  g <- calib %>%
    setup_wedgeplot() %>%
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
    guides(alpha = FALSE, size = FALSE) +
    facet_layer
  
  return(g)
}

#' ggplot formatting for traditional calibration plot.
#'
#' @param calib the output from [compute_calibration()]
#' @param grp_vars variables over which to compare calibration. These
#'   determines the color of the lines and faceting depending on `type`
#' @param facet_rows A variable name to facet data over. Creates a
#'   separate row of plots for each value of specified variable. Can be used
#'   with `facet_cols` to create a grid of plots.  Should be passed to 
#'   plot_calibration when customized.
#' @param facet_cols Same as `facet_rows`, but with columns.
#'
#' @return ggplot object
format_traditional_calib_plot <- function(calib, 
                                          grp_vars,
                                          facet_rows = "ahead",
                                          facet_cols = "forecaster"){
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
  
  g <- calib %>%
    mutate(color = as.factor(Interaction(!!!syms(color_vars)))) %>%
    ggplot(aes(x = .data$nominal_prob, y = .data$prop_below)) +
    geom_abline(slope = 1, intercept = 0) +
    labs(x = "Quantile level",
         y = "Proportion",
         title = sprintf("%s (ahead %s): Calibration", 
                         .data$name, .data$ahead)) +
    geom_line(aes(color = .data$color, group = .data$color)) +
    scale_color_viridis_d() +
    facet_layer 
  
  return(g)
}
