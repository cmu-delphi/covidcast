
#' Plot interval coverage
#'
#' @param predictions_cards tibble of predictions
#'   that are all for the same prediction task, meaning they are for the same
#'   response, incidence period,and geo type. Forecasts may be for a
#'   different forecast date or forecaster.
#'   A predictions card may be created by the function
#'   [get_predictions()], downloaded with [get_covidhub_predictions()] or
#'   possibly created manually.
#' @param type One of "all" or "one", indicating whether to show coverage
#'   across all nominal levels (in which case averaging is performed across
#'   `avg_vars`) or whether to show it for one specific alpha
#'   value.
#' @param coverage If `type = "one"`, then coverage is the nominal interval 
#'   coverage shown.
#' @param facet_rows A variable name to facet data over. Creates a
#'   separate row of plots for each value of specified variable. Can be used
#'   with `facet_cols` to create a grid of plots.
#' @param facet_cols Same as `facet_rows`, but with columns.
#' @param grp_vars variables over which to compare coverage
#' @param avg_vars variables over which we average to determine the proportion
#'   of coverage.
#' @template geo_type-template
#' @param backfill_buffer Howf many days until response is deemed trustworthy
#'   enough to be taken as correct?
#' 
#' @export 
plot_coverage <- function(predictions_cards,
                          type = c("all", "one"),                          
                          coverage = 0.8,
                          facet_rows = c("forecaster"),
                          facet_cols = c("forecast_date"),
                          grp_vars = c("forecaster", "forecast_date", "ahead"),
                          avg_vars = c("geo_value"),
                          geo_type = c("county", "hrr", "msa", "dma", "state",
                                       "hhs", "nation"),
                          backfill_buffer = 0) {
  geo_type <- match.arg(geo_type)
  type <- match.arg(type)
  
  test_legal_faceting(facet_rows, facet_cols, grp_vars)
  
  # make sure scorecards are comparable:
  grps <- grp_processing_for_facets(predictions_cards, grp_vars)
  
  cover <- compute_coverage(predictions_cards,
                            geo_type, grp_vars, avg_vars, backfill_buffer)

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
      theme_bw()
  )
}


#' Compute observed coverage from a quantile forecaster
#'
#' @template predictions_cards-template
#' @param grp_vars character vector of named columns in the score_card at which
#'   average performance will be returned
#' @param avg_vars character vector of named columns in the score_card over which
#'   averaging performance will be computed
#'   
#' @details Checks __are__ performed to ensure that averaging variables
#'   all contain the same confidence intervals though these may vary over
#'   grouping variables. 
#'   avg_vars and grp_vars must
#'   have an empty intersection.
#'
#' @return A tibble containing grp_vars, nominal_prob (the claimed interval
#'   coverage), prop_below (the proportion of actual values falling below
#'   the lower end of the confidence interval), prop_above (the proportion of
#'   actual values falling above the upper end of the confidence interval),
#'   and prop_covered (the proportion falling inside the interval). All 
#'   proportions are calculated for each available symmetric interval at each
#'   combination of grouping variables by averaging over any variables listed
#'   in avg_vars.
#' @export
#'
compute_coverage <- function(
  predictions_cards,
  geo_type = c("county", "hrr", "msa", "dma", "state",
                 "hhs", "nation"),
  grp_vars = c("forecaster", "forecast_date", "ahead"),
  avg_vars = c("geo_value"),
  backfill_buffer = 0) {

  geo_type <- match.arg(geo_type)
  
  assert_that(all(c(grp_vars, avg_vars, "quantile", "value") %in%
                    names(predictions_cards)),
              msg = paste("In compute_coverage:",
                          "grp_vars and avg_vars must be present in the predictions_card.",
                          "See details."))
  assert_that(is_empty(intersect(grp_vars, avg_vars)),
              msg = paste("In compute_coverage:",
                          "grp_vars and avg_vars must have empty intersection.",
                          "See details."))
  
  score_card <- left_join(predictions_cards,
                          get_covidcast_data(predictions_cards,
                                             backfill_buffer,
                                             geo_type),
                          by = c("geo_value",
                                 "forecast_date",
                                 "ahead"))
  
  score_card <- intersect_averagers(score_card, grp_vars, avg_vars)
  score_card <- score_card %>% 
    filter(!is.na(.data$quantile)) %>%
    select(all_of(grp_vars), all_of(avg_vars),
           .data$quantile, .data$value, .data$actual) 
  
  score_card <- averaging_checks(score_card, grp_vars, avg_vars)
  
  score_card <- score_card %>% 
    group_by(across(all_of(c(grp_vars,avg_vars)))) %>%
    mutate(rev_value = rev(.data$value)) %>%
    filter(.data$quantile < 0.5) %>%
    mutate(is_below = .data$actual < .data$value,
           is_above = .data$actual > .data$rev_value,
           is_covered = !.data$is_below & !.data$is_above,
           nominal_prob = 1 - 2 * .data$quantile) %>%
    ungroup() %>%
    select(all_of(grp_vars), .data$is_above, .data$is_below, .data$is_covered,
           .data$nominal_prob) %>%
    group_by(across(all_of(grp_vars)), .data$nominal_prob) %>%
    summarise(prop_below = mean(.data$is_below, na.rm = TRUE),
              prop_above = mean(.data$is_above, na.rm = TRUE),
              prop_covered = mean(.data$is_covered, na.rm = TRUE))
  return(score_card)
}
