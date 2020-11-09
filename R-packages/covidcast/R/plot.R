#' Plot `covidcast_signal` object
#'
#' Several plot types are provided, including choropleth plots (maps), bubble
#' plots, and time series plots showing the change of signals over time, for a 
#' data frame returned by `covidcast_signal()`. (Only the latest issue from the
#' data frame is used for plotting.) See the [plotting
#' vignette](https://cmu-delphi.github.io/covidcast/covidcastR/articles/plotting-signals.html)
#' for examples.
#'
#' @param x The `covidcast_signal` object to map or plot. If the object contains
#'   multiple issues of the same observation, only the most recent issue is
#'   mapped or plotted. 
#' @param plot_type One of "choro", "bubble", "line" indicating whether to plot
#'   a choropleth map, bubble map, or line (time series) graph, respectively.
#'   The default is "choro".
#' @param time_value Date object (or string in the form "YYYY-MM-DD")
#'   specifying the day to map, for choropleth and bubble maps. If `NULL`, the
#'   default, then the last date in `x` is used for the maps. Time series plots
#'   always include all available time values in `x`.
#' @param include Vector of state abbreviations (case insensitive, so "pa" and
#'   "PA" both denote Pennsylvania) indicating which states to include in the
#'   choropleth and bubble maps. Default is `c()`, which is interpreted to mean
#'   all states.
#' @param range Vector of two values: min and max, in this order, to use when
#'   defining the color scale for choropleth maps and the size scale for bubble
#'   maps, or the range of the y-axis for the time series plot. If `NULL`, the
#'   default, then for the maps, the min and max are set to be the mean +/- 3
#'   standard deviations, where this mean and standard deviation are as provided
#'   in the metadata for the given data source and signal; and for the time
#'   series plot, they are set to be the observed min and max of the values over  
#'   the given time period.
#' @param choro_col Vector of colors, as specified in hex code, to use for the
#'   choropleth color scale. Can be arbitrary in length. Default is similar to
#'   that from covidcast.cmu.edu.
#' @param alpha Number between 0 and 1, indicating the transparency level to be
#'   used in the maps. For choropleth maps, this determines the transparency
#'   level for the mega counties. For bubble maps, this determines the
#'   transparency level for the bubbles. Default is 0.5.
#' @param bubble_col Bubble color for the bubble map. Default is "purple".
#' @param num_bins Number of bins for determining the bubble sizes for the
#'   bubble map (here and throughout, to be precise, by bubble size we mean
#'   bubble area). Default is 8. These bins are evenly-spaced in between the min
#'   and max as specified through the `range` parameter. Each bin is assigned
#'   the same bubble size. Also, values of zero special: it has its own separate
#'   (small) bin, and values mapped to the zero bin are not drawn.
#' @param title Title for the plot. If `NULL`, the default, then a simple title
#'   is used based on the given data source, signal, and time values.
#' @param choro_params,bubble_params,line_params Additional parameter lists for
#'   the different plot types, for further customization. See details below.
#' @param ... Additional arguments, for compatibility with `plot()`. Currently
#'   unused.
#'
#' @details The following named arguments are supported through the lists 
#'   `choro_params`, `bubble_params`, and `line_params`.
#'
#' For both choropleth and bubble maps:
#' \describe{
#' \item{`subtitle`}{Subtitle for the map.}
#' \item{`missing_col`}{Color assigned to missing or NA geo locations.}
#' \item{`border_col`}{Border color for geo locations.}
#' \item{`border_size`}{Border size for geo locations.}
#' \item{`legend_position`}{Position for legend; use "none" to hide legend.} 
#' \item{`legend_height`, `legend_width`}{Height and width of the legend.} 
#' \item{`breaks`}{Breaks for a custom (discrete) color or size scale.  Note
#'   that we must set `breaks` to be a vector of the same length as `choro_col`
#'   for choropleth maps. This works as follows: we assign the `i`th color for
#'   choropleth maps, or the `i`th size for bubble maps, if and only if the
#'   given value satisfies `breaks[i] <= value < breaks[i+1]`, where we take by 
#'   convention `breaks[0] = -Inf` and `breaks[N+1] = Inf` for `N =
#'   length(breaks)`.}   
#' \item{`legend_digits`}{Number of decimal places to show for the legend
#'   labels.}  
#' }
#'
#' For choropleth maps only:
#' \describe{
#' \item{`legend_n`}{Number of values to label on the legend color bar. Ignored
#'   for discrete color scales (when `breaks` is set manually).}
#' }
#'
#' For bubble maps only:
#' \describe{
#' \item{`remove_zero`}{Should zeros be excluded from the size scale (hence
#'   effectively drawn as bubbles of zero size)?}
#' \item{`min_size`, `max_size`}{Min size for the size scale.}
#' }
#'
#' For line graphs:
#' \describe{
#' \item{`xlab`, `ylab`}{Labels for the x-axis and y-axis.}
#' \item{`stderr_bands`}{Should standard error bands be drawn?}
#' \item{`stderr_alpha`}{Transparency level for the standard error bands.}
#' }
#'
#' @method plot covidcast_signal
#' @importFrom stats sd
#' @importFrom rlang warn
#' @export
plot.covidcast_signal = function(x, plot_type = c("choro", "bubble", "line"),
                                 time_value = NULL, include = c(),
                                 range = NULL,
                                 choro_col = c("#FFFFCC", "#FD893C", "#800026"),
                                 alpha = 0.5, bubble_col = "purple", num_bins = 8,
                                 title = NULL, choro_params = list(),
                                 bubble_params = list(), line_params = list(),
                                 ...) {
  plot_type = match.arg(plot_type)
  x = latest_issue(x)

  # For the maps, set range, if we need to (to mean +/- 3 standard deviations,
  # from metadata) 
  if (is.null(range) && (plot_type == "choro" || plot_type == "bubble")) {
    if (is.null(attributes(x)$metadata)) { 
      warn(paste("Metadata for signal mean and standard deviation not",
                 "available; defaulting to observed mean and standard",
                 "deviation to set plot range."),
           class = "covidcast_plot_meta_not_found")
      mean_value = mean(x$value)
      stdev_value = sd(x$value)
    } else {
      mean_value = attributes(x)$metadata$mean_value
      stdev_value = attributes(x)$metadata$stdev_value
    }
    range = c(mean_value - 3 * stdev_value, mean_value + 3 * stdev_value)
    range = pmax(0, range)
    # TODO: figure out for which signals we need to clip the top of the range.
    # For example, for percentages, we need to clip it at 100
  }

  # For the maps, take the most recent time value if more than one is passed,
  # and check that the include arguments indeed contains state names
  if (plot_type == "choro" || plot_type == "bubble") {
    if (!is.null(include)) {
      include = toupper(include)
      no_match = which(!(include %in% c(state.abb, "DC")))

      if (length(no_match) > 0) {
        warn("'include' must only contain US state abbreviations or 'DC'.",
             not_match = include[no_match], class = "plot_include_no_match")
        include = include[-no_match]
      }
    }
  }

  # Choropleth map
  if (plot_type == "choro") {
    plot_choro(x, time_value = time_value, include = include, range = range,
               col = choro_col, alpha = alpha, title = title,
               params = choro_params)
  }


  # Bubble map
  else if (plot_type == "bubble") {
    plot_bubble(x, time_value = time_value, include = include, range = range,
                col = bubble_col, alpha = alpha, num_bins = num_bins,
                title = title, params = bubble_params)
  }

  # Line (time series) plot
  else {
    plot_line(x, range = range, title = title, params = line_params)
  }
}

# Plot a choropleth map of a covidcast_signal object.

#' @importFrom stats approx
plot_choro = function(x, time_value = NULL, include = c(), range,
                      col = c("#FFFFCC", "#FD893C", "#800026"),
                      alpha = 0.5, title = NULL, params = list()) {
  # Check that we're looking at either counties or states
  if (!(attributes(x)$metadata$geo_type == "county" ||
                     attributes(x)$metadata$geo_type == "state")) {
    stop("Only 'county' and 'state' are supported for choropleth maps.")
  }

  # Set the time value, if we need to (last observed time value)
  if (is.null(time_value)) time_value = max(x$time_value)

  # Set a title, if we need to (simple combo of data source, signal, time value)
  if (is.null(title)) title = paste0(unique(x$data_source), ": ",
                                     unique(x$signal), ", ", time_value)

  # Set a subtitle, if there are specific states we're viewing
  subtitle = params$subtitle
  if (length(include) != 0 && is.null(subtitle)) {
    subtitle = paste("Viewing", paste(include, collapse=", "))
  }

  # Set other map parameters, if we need to
  missing_col = params$missing_col
  border_col = params$border_col
  border_size = params$border_size
  legend_height = params$legend_height
  legend_width = params$legend_width
  legend_digits = params$legend_digits
  if (is.null(missing_col)) missing_col = "gray"
  if (is.null(border_col)) border_col = "white"
  if (is.null(border_size)) border_size = 0.1
  if (is.null(legend_height)) legend_height = 0.5
  if (is.null(legend_width)) legend_width = 15
  if (is.null(legend_digits)) legend_digits = 2

  # Create a continuous color function, if we need to
  breaks = params$breaks
  if (is.null(breaks)) {
    ramp_fun = grDevices::colorRamp(col)
    col_fun = function(val, alpha = 1) {
      val = pmin(pmax(val, range[1]), range[2])
      val = (val - range[1]) / (range[2] - range[1])
      rgb_mat = ramp_fun(val)
      not_na = rowSums(is.na(rgb_mat)) == 0
      col_out = rep(missing_col, length(val))
      col_out[not_na] = grDevices::rgb(rgb_mat[not_na,], alpha = alpha*255, max = 255)
      return(col_out)
    }
  }

  # Create a discrete color function, if we need to
  else {
    if (length(breaks) != length(col)) {
      stop("`breaks` must have length equal to the number of colors.")
    }
    col_fun = function(val, alpha = 1) {
      alpha_str = substr(grDevices::rgb(0, 0, 0, alpha = alpha), 8, 9)
      not_na = !is.na(val)
      col_out = rep(missing_col, length(val))
      col_out[not_na] = col[1]
      for (i in 1:length(breaks)) col_out[val >= breaks[i]] = col[i]
      col_out[not_na] = paste0(col_out[not_na], alpha_str)
      return(col_out)
    }
  }

  # Set some basic layers
  element_text = ggplot2::element_text
  margin = ggplot2::margin
  title_layer = ggplot2::labs(title = title, subtitle = subtitle)
  theme_layer = ggplot2::theme_void() +
    ggplot2::theme(plot.title = element_text(hjust = 0.5, size = 12),
                   plot.subtitle = element_text(hjust = 0.5, size = 10,
                                                margin = margin(t = 5)),
                   legend.position = "bottom")

  # Grab the values
  given_time_value = time_value
  df = x %>%
    dplyr::filter(time_value == given_time_value) %>%
    dplyr::select(val = "value", geo = "geo_value")
  val = df$val
  geo = df$geo
  names(val) = geo

  # Create the choropleth colors for counties
  if (attributes(x)$metadata$geo_type == "county") {
    map_df = usmap::us_map("county", include = include)
    map_geo = map_df$fips
    map_col = rep(missing_col, length(map_geo))

    # First set the colors for mega counties
    mega_cty = geo[which(substr(geo, 3, 5) == "000")]
    map_mega = map_geo[substr(map_geo, 1, 2) %in% substr(mega_cty, 1, 2)]
    map_col[substr(map_geo, 1, 2) %in% substr(mega_cty, 1, 2)] =
      col_fun(val[paste0(substr(map_mega, 1, 2), "000")], alpha = alpha)

    # Now overwrite the colors for observed counties
    obs_cty = geo[substr(geo, 3, 5) != "000"]
    map_obs = map_geo[map_geo %in% obs_cty]
    map_col[map_geo %in% obs_cty] = col_fun(val[map_obs])

    # TODO: implement megacounties "properly"? For this we should first draw the
    # states (not counties) in transparent colors, then layer over the observed
    # counties. Hence, eventually, two calls to usmap::us_map() and two polygon
    # layers?
  }

  # Create the choropleth colors for states
  else if (attributes(x)$metadata$geo_type == "state") {
    map_df = usmap::us_map("state", include = include)
    map_geo = tolower(map_df$abbr)
    map_col = rep(missing_col, length(map_geo))

    # Overwrite the colors for observed states
    map_obs = map_geo[map_geo %in% geo]
    map_col[map_geo %in% geo] = col_fun(val[map_obs])
  }

  # Create the polygon layer
  aes = ggplot2::aes
  geom_args = list()
  geom_args$color = border_col
  geom_args$size = border_size
  geom_args$fill = map_col
  geom_args$mapping = aes(x = x, y = y, group = group)
  geom_args$data = map_df
  polygon_layer = do.call(ggplot2::geom_polygon, geom_args)
  
  # For continuous color scale, create a legend layer
  if (is.null(breaks)) {
    # Create legend breaks and legend labels, if we need to
    n = params$legend_n
    if (is.null(n)) n = 8
    legend_breaks = seq(range[1], range[2], len = n)
    legend_labels = round(legend_breaks, legend_digits)

    # Create a dense set of breaks, for the color gradient (especially important
    # if many colors were passed)
    d = approx(x = 0:(n-1) / (n-1), y = legend_breaks, xout = 0:999 / 999)$y

    # Now the legend layer (hidden + scale)
    hidden_df = data.frame(x = rep(Inf, n), z = legend_breaks)
    hidden_layer = ggplot2::geom_point(aes(x = x, y = x, color = z),
                                       data = hidden_df, alpha = 0)
    guide = ggplot2::guide_colorbar(title = NULL, horizontal = TRUE,
                                    barheight = legend_height,
                                    barwidth = legend_width)
    scale_layer = ggplot2::scale_color_gradientn(colors = col_fun(d),
                                                 limits = range(d),
                                                 breaks = legend_breaks,
                                                 labels = legend_labels,
                                                 guide = guide)
  }

  # For discrete color scale, create a legend layer
  else {
    # Create legend breaks and legend labels
    n = length(breaks)
    legend_breaks = breaks
    legend_labels = round(legend_breaks, legend_digits)

    # Now the legend layer (hidden + scale)
    hidden_df = data.frame(x = rep(Inf, n), z = as.factor(legend_breaks))
    hidden_layer = ggplot2::geom_polygon(aes(x = x, y = x, fill = z),
                                         data = hidden_df, alpha = 0)
    guide = ggplot2::guide_legend(title = NULL, horizontal = TRUE, nrow = 1,
                                  keyheight = legend_height,
                                  keywidth = legend_width / n,
                                  label.position = "bottom", label.hjust = 0,
                                  override.aes = list(alpha = 1))
    scale_layer = ggplot2::scale_fill_manual(values = col,
                                             breaks = legend_breaks,
                                             labels = legend_labels,
                                             guide = guide)
  }

  # Put it all together and return
  return(ggplot2::ggplot() + polygon_layer + ggplot2::coord_equal() +
         title_layer + hidden_layer + scale_layer + theme_layer)
}

# Plot a bubble map of a covidcast_signal object.

plot_bubble = function(x, time_value = NULL, include = c(), range = NULL,
                       col = "purple", alpha = 0.5, num_bins = 8,
                       title = NULL, params = list()) {
  # Check that we're looking at either counties or states
  if (!(attributes(x)$metadata$geo_type == "county" ||
                     attributes(x)$metadata$geo_type == "state")) {
    stop("Only 'county' and 'state' are supported for bubble maps.")
  }

  # Set the time value, if we need to (last observed time value)
  if (is.null(time_value)) time_value = max(x$time_value)

  # Set a title, if we need to (simple combo of data source, signal, time value)
  if (is.null(title)) title = paste0(unique(x$data_source), ": ",
                                     unique(x$signal), ", ", time_value)

  # Set a subtitle, if there are specific states we're viewing
  subtitle = params$subtitle
  if (length(include) != 0 && is.null(subtitle)) {
    subtitle = paste("Viewing", paste(include, collapse=", "))
  }

  # Set other map parameters, if we need to
  missing_col = params$missing_col
  border_col = params$border_col
  border_size = params$border_size
  legend_height = params$legend_height
  legend_width = params$legend_width
  legend_digits = params$legend_digits
  legend_pos = params$legend_position
  if (is.null(missing_col)) missing_col = "gray"
  if (is.null(border_col)) border_col = "darkgray"
  if (is.null(border_size)) border_size = 0.1
  if (is.null(legend_height)) legend_height = 0.5
  if (is.null(legend_width)) legend_width = 15
  if (is.null(legend_digits)) legend_digits = 2
  if (is.null(legend_pos)) legend_pos = "bottom"

  # Create breaks, if we need to
  breaks = params$breaks
  if (!is.null(breaks)) num_bins = length(breaks)
  else {
    # Set a lower bound if range[1] == 0 and we're removing zeros
    lower_bd = range[1]
    if (!isFALSE(params$remove_zero) && range[1] == 0) {
      lower_bd = min(0.1, range[2] / num_bins)
    }
    breaks = seq(lower_bd, range[2], length = num_bins)
  }

  # Max and min bubble sizes
  min_size = params$min_size
  max_size = params$max_size
  if (is.null(min_size)) {
    min_size = ifelse(attributes(x)$metadata$geo_type == "county", 0.1, 1)
  }
  if (is.null(max_size)) {
    max_size = ifelse(attributes(x)$metadata$geo_type == "county", 4, 12)
  }

  # Bubble sizes. Important note the way we set sizes later, via
  # scale_size_manual(), this actually sets the *radius* not the *area*, so we
  # need to define these sizes to be evenly-spaced on the squared scale
  sizes = sqrt(seq(min_size^2, max_size^2, length = num_bins))

  # Create discretization function
  dis_fun = function(val) {
    val_out = rep(NA, length(val))
    for (i in 1:length(breaks)) val_out[val >= breaks[i]] = breaks[i]
    return(val_out)
  }

  # Set some basic layers
  element_text = ggplot2::element_text
  margin = ggplot2::margin
  title_layer = ggplot2::labs(title = title, subtitle = subtitle)
  theme_layer = ggplot2::theme_void() +
    ggplot2::theme(plot.title = element_text(hjust = 0.5, size = 12),
                   plot.subtitle = element_text(hjust = 0.5, size = 10,
                                                margin = margin(t = 5)),
                   legend.position = legend_pos)

  # Grab the values
  given_time_value = time_value
  df = x %>%
    dplyr::filter(time_value == given_time_value) %>%
    dplyr::select(val = value, geo = geo_value)
  val = df$val
  geo = df$geo
  names(val) = geo

  # Grap the map data frame for counties
  if (attributes(x)$metadata$geo_type == "county") {
    map_df = usmap::us_map("county", include = include)
    map_geo = map_df$fips
  }

  # Grap the map data frame for states
  else if (attributes(x)$metadata$geo_type == "state") {
    map_df = usmap::us_map("state", include = include)
    map_geo = tolower(map_df$abbr)
  }

  # Determine which locations are missing
  map_mis = rep(1, length(map_geo))
  map_mis[map_geo %in% geo] = 0

  # Warn if there's any missing locations
  if (sum(map_mis == 1) > 0) {
    warning("Bubble maps can be hard to read when there is missing data;",
            "the locations without data are filled in gray.")
  }

  # Create the polygon layer
  aes = ggplot2::aes
  geom_args = list()
  geom_args$color = border_col
  geom_args$size = border_size
  geom_args$fill = c("white", missing_col)[map_mis + 1]
  geom_args$mapping = aes(x = x, y = y, group = group)
  geom_args$data = map_df
  polygon_layer = do.call(ggplot2::geom_polygon, geom_args)

  # Retrieve coordinates for mapping
  # Reading from usmap files to ensure consistency with borders
  if (attributes(x)$metadata$geo_type == "county") {
    centroids = covidcast::county_geo[covidcast::county_geo$fips %in% map_geo, ]
    cur_geo = centroids$fips
    cur_val = rep(NA, length(cur_geo))
  }
  else if (attributes(x)$metadata$geo_type == "state") {
    centroids = covidcast::state_geo
    centroids$abbr = tolower(centroids$abbr)
    centroids = centroids[centroids$abbr %in% map_geo, ]
    cur_geo = centroids$abbr
    cur_val = rep(NA, length(cur_geo))
  }

  # Overwrite the values for observed locations
  cur_obs = cur_geo[cur_geo %in% geo]
  cur_val[cur_geo %in% geo] = dis_fun(val[cur_obs])

  # Important: make into a factor and set the levels (for the legend)
  cur_val = factor(cur_val, levels = breaks)

  # Explicitly drop zeros (and from levels) unless we're asked not to
  if (!isFALSE(params$remove_zero)) {
    cur_val[cur_val == 0] = NA
    levels(cur_val)[levels(cur_val) == 0] = NA
  }

  # Create the bubble layer
  bubble_df = data.frame(lat = centroids$x, lon = centroids$y, val = cur_val)
  bubble_layer = ggplot2::geom_point(aes(x = lat, y = lon, size = val),
                                     data = bubble_df, color = col,
                                     alpha = alpha, na.rm = TRUE)

  # Create the scale layer
  labels = round(breaks, legend_digits)
  guide = ggplot2::guide_legend(title = NULL, horizontal = TRUE, nrow = 1)
  scale_layer = ggplot2::scale_size_manual(values = sizes, breaks = breaks,
                                           labels = labels, drop = FALSE,
                                           guide = guide)

  # Put it all together and return
  return(ggplot2::ggplot() + polygon_layer + ggplot2::coord_equal() +
         title_layer + bubble_layer + scale_layer + theme_layer)
}

# Plot a line (time series) graph of a covidcast_signal object.
plot_line = function(x, range = NULL, title = NULL, params = list()) {
  # Set a title, if we need to (simple combo of data source, signal)
  if (is.null(title)) title = paste0(unique(x$data_source), ": ",
                                     unique(x$signal))

  # Set other plot parameters, if we need to
  xlab = params$xlab
  ylab = params$ylab
  stderr_bands = params$stderr_bands
  stderr_alpha = params$stderr_alpha
  if (is.null(xlab)) xlab = "Date"
  if (is.null(ylab)) ylab = "Value"
  if (is.null(stderr_bands)) stderr_bands = FALSE
  if (is.null(stderr_alpha)) stderr_alpha = 0.5

  # Grab the values
  df = x %>% dplyr::select(value, time_value, geo_value, stderr)

  # Set the range, if we need to
  if (is.null(range)) range = base::range(df$value, na.rm = TRUE)

  # Create label and theme layers
  label_layer = ggplot2::labs(title = title, x = xlab, y = ylab)
  theme_layer = ggplot2::theme_bw() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
    ggplot2::theme(legend.position = "bottom",
                   legend.title = ggplot2::element_blank())

  # Create lim, line, and ribbon layers
  aes = ggplot2::aes
  lim_layer = ggplot2::coord_cartesian(ylim = range)
  line_layer = ggplot2::geom_line(aes(y = value, color = geo_value,
                                      group = geo_value))
  ribbon_layer = NULL
  if (stderr_bands) {
    ribbon_layer = ggplot2::geom_ribbon(aes(ymin = value - stderr,
                                            ymax = value + stderr,
                                            fill = geo_value),
                                        alpha = stderr_alpha)
  }

  # Put it all together and return
  return(ggplot2::ggplot(aes(x = time_value), data = df) +
         line_layer + ribbon_layer + lim_layer + label_layer + theme_layer)
}
