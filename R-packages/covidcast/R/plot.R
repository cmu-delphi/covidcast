# Plot a choropleth map of a covidcast_signal object.

plot_choro = function(x, time_value = NULL, include = c(), range = NULL,
                      col = c("#FFFFCC", "#FD893C", "#800026"), 
                      alpha = 0.5, direction = FALSE,
                      dir_col = c("#BADEE8", "#F2DF91", "#CE0A05"),
                      title = NULL, params = list()) { 
  # Check that we're looking at either counties or states 
  if (!(attributes(x)$geo_type == "county" ||
                     attributes(x)$geo_type == "state")) {
    stop("Currently only 'county' and 'state' are supported for maps.")
  }
  
  # Set time_value, if we need to (to most recent observed time_value)
  if (is.null(time_value)) time_value = max(x$time_value)
  else time_value = max(time_value)

  # Check the include arguments indeed contains state names, and if not, drop
  # the offenders
  if (!is.null(include)) {
    include = toupper(include)
    no_match = which(!(include %in% c(state.abb, "DC")))
    if (length(no_match) > 0) {
      warning("'include' must only contain US state abbreviations or 'DC'.")
      include = include[-no_match]
    }
  }

  # Set range, if we need to (to mean +/- 3 standard deviations, from metadata)
  if (is.null(range)) {
    range = c(attributes(x)$mean_value - 3 * attributes(x)$stdev_value,
              attributes(x)$mean_value + 3 * attributes(x)$stdev_value)
    range = pmax(0, range)
    # TODO: figure out for which signals we need to clip the top of the range.
    # For example, for percentages, we need to clip it at 100
  }

  # Set title, if we need to (simple combo of data_source, signal, time_value)
  if (is.null(title)) title = paste0(attributes(x)$data_source, ": ",
                                   attributes(x)$signal, ", ", time_value)

  # Set a subtitle, if there are specific states we're viewing
  subtitle = params$subtitle
  if (!is.null(include) && is.null(subtitle)) {
    subtitle = paste("Viewing", paste(include, collapse=", "))
  }
  
  # Set other map parameters, if we need to
  missing_col = params$missing_col
  border_col = params$border_col
  size = params$size
  if (is.null(missing_col)) missing_col = "gray"
  if (is.null(border_col)) border_col = "white"
  if (is.null(size)) size = 0.1

  # For intensity, create a color function, if we need to
  col_fun = params$col_fun
  if (!direction && is.null(col_fun)) {
    ramp_fun = colorRamp(col)
    col_fun = function(val, alpha = 1) {
      val = pmin(pmax(val, range[1]), range[2])
      val = (val - range[1]) / (range[2] - range[1])
      rgb_mat = ramp_fun(val)
      not_na = rowSums(is.na(rgb_mat)) == 0
      col_vec = rep(NA, length(val))
      col_vec[not_na] = rgb(rgb_mat[not_na,], alpha = alpha*255, max = 255)
      return(col_vec)
    }
  }

  # For intensity, adapt the given function for transparency, if we need to
  else if (!direction && !is.null(col_fun)) {
    col_fun_old = col_fun
    col_fun = function(val, alpha = 1) {
      alpha_str = substr(rgb(0, 0, 0, alpha = alpha), 8, 9)
      not_na = !is.na(val)
      col_vec = rep(NA, length(val))
      col_vec[not_na] = paste0(col_fun_old(val[not_na]), alpha_str)
      return(col_vec)
    }
  }

  # For direction, create a color function
  else if (direction) {
    if (length(dir_col) != 3) {
      stop("'dir_col' must have length 3.")
    }
    col_fun = function(val, alpha = 1) {
      alpha_str = substr(rgb(0, 0, 0, alpha = alpha), 8, 9)
      not_na = !is.na(val)
      col_vec = rep(NA, length(val))
      col_vec[not_na] = paste0(dir_col[val[not_na] + 2], alpha_str)
      return(col_vec)
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
    dplyr::select(value, direction, geo_value)
  
  if (!direction) val = df$value
  else val = df$direction
  
  geo = df$geo_value
  names(val) = geo
  
  # Create the choropleth colors for counties 
  if (attributes(x)$geo_type == "county") {
    map_df = usmap::us_map("county", include = include)
    geo_all = map_df$fips
    col_all = rep(missing_col, length(geo_all))

    # First set the colors for mega counties
    mega_cty = geo[which(substr(geo, 3, 5) == "000")]
    mega_all = geo_all[substr(geo_all, 1, 2) %in% substr(mega_cty, 1, 2)]
    col_all[substr(geo_all, 1, 2) %in% substr(mega_cty, 1, 2)] =
      col_fun(val[paste0(substr(mega_all, 1, 2), "000")], alpha = alpha)
    
    # Now overwrite the colors for observed counties
    obs_cty = geo[substr(geo, 3, 5) != "000"]
    obs_all = geo_all[geo_all %in% obs_cty]
    col_all[geo_all %in% obs_cty] = col_fun(val[obs_all])
    
    # TODO: implement megacounties "properly"? For this we should first draw the
    # states (not counties) in transparent colors, then layer over the observed
    # counties. Hence, eventually, two calls to usmap::us_map() and two polygon
    # layers?
  }

  # Create the choropleth colors for states
  else if (attributes(x)$geo_type == "state") {
    map_df = usmap::us_map("state", include = include)
    geo_all = tolower(map_df$abbr)
    col_all = rep(missing_col, length(geo_all))
    
    # Overwrite the colors for observed states
    obs_all = geo_all[geo_all %in% geo]
    col_all[geo_all %in% geo] = col_fun(val[geo_all])
  }
  
  # Create the polygon layer
  geom_args = list()
  geom_args$color = border_col
  geom_args$size = size
  geom_args$fill = col_all
  geom_args$mapping = ggplot2::aes(x = map_df$x, y = map_df$y,
                                   group = map_df$group)
  polygon_layer = do.call(ggplot2::geom_polygon, geom_args)
  
  # For intensity, create a legend layer
  if (!direction) {
    # Set the legend values, if we need to
    legend_values = params$legend_values
    if (is.null(legend_values)) legend_values = seq(range[1], range[2], len = 8)
    
    # Set the legend labels, if we need to
    legend_labels = params$legend_labels
    if (is.null(legend_labels)) legend_labels = sprintf("%0.2f", legend_values)

    # Create a dense set of values, for the color gradient (especially important
    # if the custom col_fun is not a linear color gradient)
    N = length(legend_values)
    d = approx(x = 0:(N-1) / (N-1), y = legend_values, xout = 0:299 / 299)$y
    
    # Define visual breaks (evenly-spaced across the range)
    legend_breaks = seq(min(legend_values), max(legend_values), length = N)

    # Now the legend layer
    legend_df = data.frame(x = rep(Inf, N), z = legend_values)
    hidden_layer = ggplot2::geom_point(ggplot2::aes(x = x, y = x, color = z),
                                       data = legend_df, alpha = 0)
    guide = ggplot2::guide_colorbar(title = NULL, horizontal = TRUE, 
                                    barheight = 0.5, barwidth = 15,
                                    draw.ulim = FALSE, draw.llim = FALSE)
    legend_layer = ggplot2::scale_color_gradientn(colors = col_fun(d),
                                                  limits = range(d),
                                                  breaks = legend_breaks,
                                                  labels = legend_labels,
                                                  guide = guide)
  }

  # For direction, create a legend layer
  else {
    # Set the legend labels, if we need to
    legend_labels = params$legend_labels
    if (is.null(legend_labels)) legend_labels = c("Decreasing", "Steady",
                                                  "Increasing")

    # Now the legend layer
    legend_df = data.frame(x = rep(Inf, 3), z = as.factor(-1:1))
    hidden_layer = ggplot2::geom_point(ggplot2::aes(x = x, y = x, color = z),
                                       data = legend_df, alpha = 0)
    guide = ggplot2::guide_legend(title = NULL, horizontal = TRUE,
                                  override.aes = list(alpha = 1, size = 2))
    legend_layer = ggplot2::scale_color_manual(values = dir_col,
                                               breaks = -1:1,
                                               labels = legend_labels,
                                               guide = guide)
  }

  # Put it all together
  return(ggplot2::ggplot(data = map_df) + polygon_layer +
         ggplot2::coord_equal() + title_layer + hidden_layer +
         legend_layer + theme_layer)
}

# Plot a bubble map of a covidcast_signal object.

plot_bubble = function(x, time_value = NULL, include = c(), range = NULL,
                       col = "purple", alpha = 0.5, direction = FALSE,
                       dir_col = c("#BADEE8", "#F2DF91", "#CE0A05"),
                       title = NULL, params = list()) {
}

# Plot a line (time series) graph of a covidcast_signal object.

plot_line = function(x, time_values = NULL, geo_values = NULL, range = NULL,
                     col = 1:6, lty = 1:5, direction = FALSE,
                     dir_col = c("#BADEE8", "#F2DF91", "#CE0A05"),
                     title = NULL, params = list()) {
}
