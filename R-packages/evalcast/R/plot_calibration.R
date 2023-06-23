#' Plot calibration curves
#'
#' @param predictions_cards Either one predictions_card or several joined
#'   together using bind_rows().
#' @param type One of "wedgeplot" or "traditional"
#' @param facet_rows A variable name to facet data over. Creates a
#'   separate row of plots for each value of specified variable. Can be used
#'   with `facet_cols` to create a grid of plots.  Should be passed to 
#'   plot_calibration when customized.
#' @param facet_cols Same as `facet_rows`, but with columns.
#' @param grp_vars variables over which to compare calibration. These
#'   determines the color of the lines and faceting depending on `type`
#' @param avg_vars variables over which we average to determine the calibration.
#' @template geo_type-template
#' @param backfill_buffer How many days until response is deemed trustworthy
#'   enough to be taken as correct?.
#' @export
plot_calibration <- function(predictions_cards,
                             type = c("wedgeplot", "traditional"),
                             facet_rows = NULL,
                             facet_cols = NULL,
                             grp_vars = c("forecaster", "forecast_date", "ahead"),
                             avg_vars = c("geo_value"),
                             geo_type = c("county", "hrr", "msa", "dma", "state",
                                          "hhs", "nation"),
                             backfill_buffer = 0) {
  
  geo_type <- match.arg(geo_type)
  type <- match.arg(type)

  calib <- compute_calibration(predictions_cards,
                               geo_type, 
                               backfill_buffer,
                               grp_vars,
                               avg_vars)
  
  g <- switch (type,
    wedgeplot = format_wedgeplot(calib, grp_vars, facet_rows, facet_cols),
    traditional = format_traditional_calib_plot(calib, grp_vars, 
                                                facet_rows, facet_cols),
    stop("illegal calibration plot type selected.")
  )
  
  return(g + xlim(0, 1) + ylim(0, 1) + theme_bw())
}

#' Preprocessing for wedge calibration plot
#'
#' @param calib the output from [compute_calibration()]
#'
#' @importFrom tidyr pivot_longer
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
                             facet_rows,
                             facet_cols){
  
  test_legal_faceting(facet_rows, facet_cols, grp_vars)
  
  non_faceted_grps <- setdiff(grp_vars, c(facet_rows, facet_cols))
  
  assert_that(length(non_faceted_grps) == 0,
              msg = paste("All grp_vars need to be faceted for wedgeplots:",
                          non_faceted_grps))
  
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
         y = "Proportion") +
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
                                          facet_rows,
                                          facet_cols){
  
  test_legal_faceting(facet_rows, facet_cols, grp_vars)
  
  facet_layer <- facet_grid(rows = vars(!!!syms(facet_rows)),
                            cols = vars(!!!syms(facet_cols)))
  
  color_vars <- setdiff(grp_vars, c(facet_rows, facet_cols))
  
  g <- calib %>%
    mutate(color = as.factor(Interaction(!!!syms(color_vars)))) %>%
    ggplot(aes(x = .data$nominal_prob, y = .data$prop_below)) +
    geom_abline(slope = 1, intercept = 0) +
    labs(x = "Quantile level",
         y = "Proportion") +
    geom_line(aes(color = .data$color, group = .data$color)) +
    scale_color_viridis_d() +
    facet_layer 
  
  return(g)
}


#' Compute calibration of a quantile forecaster
#'
#' @param predictions_cards tibble containing at least columns actual, quantile, 
#'   value and any grouping or averaging variables named in the next arguments
#' @template geo_type-template
#' @param backfill_buffer How many days until response is deemed trustworthy
#'   enough to be taken as correct?
#' @param grp_vars character vector of named columns in the score_card at which
#'   average performance will be returned
#' @param avg_vars character vector of named columns in the score_card over which
#'   averaging performance will be computed
#'   
#' @details Note that no checks are performed to ensure that averaging variables
#'   all contain the same predicted quantiles. avg_vars and grp_vars must
#'   have an empty intersection.
#'
#' @return A tibble containing grp_vars, nominal_prob (the claimed interval
#'   coverage), prop_below (the proportion of actual values falling below
#'   the forecast quantile) and prop_above (the proportion of
#'   actual values falling above the forecast quantile). All 
#'   proportions are calculated for each available probability at each
#'   combination of grouping variables by averaging over any variables listed
#'   in avg_vars.
#' @export
#'
compute_calibration <- function(
  predictions_cards,
  geo_type,
  backfill_buffer = 10,
  grp_vars = c("forecaster", "forecast_date", "ahead"),
  avg_vars = c("geo_value")) {
  
  score_card <- left_join(predictions_cards,
                          get_covidcast_data(predictions_cards,
                                             backfill_buffer,
                                             geo_type=geo_type),
                          by = c("geo_value",
                                 "forecast_date",
                                 "ahead"))
  
  assert_that(all(c(grp_vars, avg_vars, "quantile", "value", "actual") %in%
                    names(score_card)),
              msg = paste("In compute_actual_vs_nominal_prob:",
                          "grp_vars must be present in the score_card.",
                          "See details."))
  assert_that(is_empty(intersect(grp_vars, avg_vars)),
              msg = paste("In compute_actual_vs_nominal_prob:",
                          "grp_vars and avg_vars must have empty intersection.",
                          "See details."))
  score_card <- intersect_averagers(score_card, grp_vars, avg_vars)
  score_card <- filter(score_card, !is.na(.data$quantile)) %>%
    select(all_of(grp_vars), all_of(avg_vars),
           .data$actual, .data$quantile, .data$value)
  
  score_card <- averaging_checks(score_card, grp_vars, avg_vars)
  
  score_card <- score_card %>% 
    mutate(below = .data$actual < .data$value,
           above = .data$actual > .data$value) %>%
    group_by(across(all_of(grp_vars)), .data$quantile) %>%
    summarise(prop_below = mean(.data$below, na.rm = TRUE),
              prop_above = mean(.data$above, na.rm = TRUE)) %>%
    rename(nominal_prob = .data$quantile)
  return(score_card)
}

#' @inherit compute_calibration
#' @export
compute_actual_vs_nominal_prob <- compute_calibration
