#' A canonical function for creating plots within evalcast
#'
#' @param df dataframe containing the data to plot
#' @param facet_rows A variable name to facet data over. Creates a
#'   separate row of plots for each value of specified variable. Can be used
#'   with `facet_cols` to create a grid of plots.
#' @param facet_cols Same as `facet_rows`, but with columns.
#' @param legend_position where to position the legend ("bottom" by default)
#' @param grp_vars The set of variable names in `df` which to aggregate over.
#'   "forecaster" by default.
#' @param x Name of the variable in `df` to be used for the x-axis
#' @param y name of the variable in `df` to be used for the y-axis
#' @param aggr function to use to aggregate data over `grp_vars`
#' @param dots when set to TRUE, a dot layer is included in the plot
#' @param lines when set to TRUE, a line layer is included in the plot
#' @param base_forecaster If set, scales the y-value for all forecasters by
#'   the corresponding values of the `base_forecaster`
#' @param scale_before_aggr If TRUE, scales results by the
#'   `base_forecaster` before aggregating by the `aggr` function. FALSE by
#'   default.
#' @param title title of the plot, if set
#' @param subtitle subtitle of the plot, if set
#' @param xlab x-axis label, if set
#' @param ylab y-axis label, if set
#' @param legend_title title of the legend, if set
#' 
#' @export
plot_canonical <- function(df, x, y,
                           facet_rows = NULL,
                           facet_cols = NULL,
                           legend_position = "bottom", 
                           grp_vars = "forecaster",
                           aggr = mean, dots = TRUE, lines = TRUE,
                           base_forecaster = NULL,
                           scale_before_aggr = FALSE,
                           title = waiver(), subtitle = waiver(),
                           xlab = waiver(), ylab  = waiver(),
                           legend_title = NULL) {
  assert_that("data.frame" %in% class(df),
              msg = "df must be of class data.frame")
  assert_that(dots | lines,
              msg = "Either dots or lines must be TRUE")
  if (!is.null(facet_rows)) {
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
  assert_that(!(scale_before_aggr & is.null(base_forecaster)),
              msg = paste0("Cannot scale before aggregation without a",
                           " base_forecaster to scale by"))

  # Scale before aggregation, if we need to
  if (!is.null(base_forecaster) && scale_before_aggr) {
    df <- scale_by_forecaster(df, y, base_forecaster)
  }

  # Aggregate
  df <- df %>%
    group_by(!!!syms(grp_vars), !!sym(x)) %>%
    drop_na(!!sym(x), !!sym(y)) %>%
    summarize(!!y := aggr(!!sym(y)))

  # Scale after aggregation, if we need to
  if (!is.null(base_forecaster) && !scale_before_aggr) {
    df <- scale_by_forecaster(df, y, base_forecaster, id_cols = c(x, group_vars))
  }

  # Set up plotting layers
  dots_layer <- NULL
  line_layer <- NULL
  color_vars <- setdiff(grp_vars, c(facet_rows, facet_cols))
  df <- df %>% mutate(color = Interaction(!!!syms(color_vars)))
  if (dots) { 
    dots_layer <- geom_point(aes(color = .data$color, group = .data$color))
  }
  if (lines) {
    line_layer <- geom_line(aes(color = .data$color, group = .data$color))
  }
  facet_layer <- facet_grid(rows = vars(!!!syms(facet_rows)),
                            cols = vars(!!!syms(facet_cols)))
  label_layer <- labs(title = title, subtitle = subtitle,
                      x = xlab, y = ylab, color = legend_title)
  theme_layer <- theme(legend.position = legend_position)

  # Plot and return
  ggplot(df, aes(x = !!sym(x), y = !!sym(y))) +
    line_layer + dots_layer + facet_layer + label_layer + theme_layer
}

# Helpful wrapper on interaction() for our canonical plotting function
Interaction <- function(...) {
  params <- list(...)
  if (length(params) == 0) return(NULL)
  else if (length(params) == 1) return(as.factor(params[[1]]))
  else return(interaction(...))
}
