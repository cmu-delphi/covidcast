# Plot a choropleth map of a covidcast_signal object.

#' @importFrom stats approx
plot_choro = function(x, time_value = NULL, include = c(), range,
                      col = c("#FFFFCC", "#FD893C", "#800026"),
                      alpha = 0.5, direction = FALSE,
                      dir_col = c("#6F9CC6", "#E4E4E4", "#C56B59"),
                      title = NULL, params = list()) {
  # Check that we're looking at either counties or states
  if (!(attributes(x)$geo_type == "county" ||
                     attributes(x)$geo_type == "state")) {
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
  if (is.null(missing_col)) missing_col = "gray"
  if (is.null(border_col)) border_col = "white"
  if (is.null(border_size)) border_size = 0.1
  if (is.null(legend_height)) legend_height = 0.5
  if (is.null(legend_width)) legend_width = 15

  # For intensity, create a continuous color function, if we need to
  breaks = params$breaks
  if (!direction && is.null(breaks)) {
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

  # For intensity, create a discrete color function, if we need to
  else if (!direction && !is.null(breaks)) {
    if (length(breaks) != length(col)) {
      stop("'breaks' must have length equal to the number of colors.")
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

  # For direction, create a discrete color function
  else {
    if (length(dir_col) != 3) {
      stop("'dir_col' must have length 3.")
    }
    col_fun = function(val, alpha = 1) {
      alpha_str = substr(grDevices::rgb(0, 0, 0, alpha = alpha), 8, 9)
      not_na = !is.na(val)
      col_out = rep(NA, length(val))
      col_out[not_na] = paste0(dir_col[val[not_na] + 2], alpha_str)
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
    dplyr::select(val = ifelse(direction, "direction", "value"),
                  geo = "geo_value")
  val = df$val
  geo = df$geo
  names(val) = geo

  # Create the choropleth colors for counties
  if (attributes(x)$geo_type == "county") {
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
  else if (attributes(x)$geo_type == "state") {
    map_df = usmap::us_map("state", include = include)
    map_geo = tolower(map_df$abbr)
    map_col = rep(missing_col, length(map_geo))

    # Overwrite the colors for observed states
    map_obs = map_geo[map_geo %in% geo]
    map_col[map_geo %in% geo] = col_fun(val[map_obs])
  }

  # Create the polygon layer
  geom_args = list()
  geom_args$color = border_col
  geom_args$size = border_size
  geom_args$fill = map_col
  geom_args$mapping = ggplot2::aes(x = map_df$x, y = map_df$y,
                                   group = map_df$group)
  polygon_layer = do.call(ggplot2::geom_polygon, geom_args)

  # For intensity and continuous color scale, create a legend layer
  if (!direction && is.null(breaks)) {
    # Create legend breaks and legend labels
    n = params$legend_n
    if (is.null(n)) n = 8
    legend_breaks = seq(range[1], range[2], length = n)
    legend_labels = round(legend_breaks, 2)

    # Create a dense set of breaks, for the color gradient (especially important
    # if many colors were passed)
    d = approx(x = 0:(n-1) / (n-1), y = legend_breaks, xout = 0:999 / 999)$y

    # Now the legend layer (hidden + scale)
    hidden_df = data.frame(x = rep(Inf, n), z = legend_breaks)
    hidden_layer = ggplot2::geom_point(ggplot2::aes(x = x, y = x, color = z),
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

  # For intensity and discrete color scale, create a legend layer
  else if (!direction && !is.null(breaks)) {
    # Create legend breaks and legend labels
    n = length(breaks)
    legend_breaks = breaks
    legend_labels = round(legend_breaks, 2)

    # Now the legend layer (hidden + scale)
    hidden_df = data.frame(x = rep(Inf, n), z = as.factor(legend_breaks))
    hidden_layer = ggplot2::geom_polygon(ggplot2::aes(x = x, y = x, fill = z),
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

  # For direction, create a legend layer
  else {
    # Create legend breaks and legend labels
    legend_breaks = -1:1
    legend_labels = c("Decreasing", "Steady", "Increasing")

    # Now the legend layer (hidden + scale)
    hidden_df = data.frame(x = rep(Inf, 3), z = as.factor(legend_breaks))
    hidden_layer = ggplot2::geom_polygon(ggplot2::aes(x = x, y = x, fill = z),
                                         data = hidden_df, alpha = 1)
    guide = ggplot2::guide_legend(title = NULL, horizontal = TRUE, nrow = 1,
                                  keyheight = legend_height,
                                  keywidth = legend_width / 3,
                                  label.position = "bottom",
                                  override.aes = list(alpha = 1))
    scale_layer = ggplot2::scale_fill_manual(values = dir_col,
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
  if (!(attributes(x)$geo_type == "county" ||
                     attributes(x)$geo_type == "state")) {
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
  if (is.null(missing_col)) missing_col = "gray"
  if (is.null(border_col)) border_col = "darkgray"
  if (is.null(border_size)) border_size = 0.1
  if (is.null(legend_height)) legend_height = 0.5
  if (is.null(legend_width)) legend_width = 15

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

  # Create bubble sizes
  min_size = params$min_size
  max_size = params$max_size
  if (is.null(min_size)) min_size = ifelse(attributes(x)$geo_type == "county",
                                           0.1, 1)
  if (is.null(max_size)) max_size = ifelse(attributes(x)$geo_type == "county",
                                           4, 12)
  sizes = seq(min_size, max_size, length = num_bins)

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
                   legend.position = "bottom")

  # Grab the values
  given_time_value = time_value
  df = x %>%
    dplyr::filter(time_value == given_time_value) %>%
    dplyr::select(val = value, geo = geo_value)
  val = df$val
  geo = df$geo
  names(val) = geo

  # Grap the map data frame for counties
  if (attributes(x)$geo_type == "county") {
    map_df = usmap::us_map("county", include = include)
    map_geo = map_df$fips
  }

  # Grap the map data frame for states
  else if (attributes(x)$geo_type == "state") {
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
  geom_args = list()
  geom_args$color = border_col
  geom_args$size = border_size
  geom_args$fill = c("white", missing_col)[map_mis + 1]
  geom_args$mapping = ggplot2::aes(x = map_df$x, y = map_df$y,
                                   group = map_df$group)
  polygon_layer = do.call(ggplot2::geom_polygon, geom_args)

  # Set the lats and lons for counties
  if (attributes(x)$geo_type == "county") {
    g = county_geo[county_geo$FIPS %in% map_geo, ]
    cur_geo = g$FIPS
    cur_lon = g$LON
    cur_lat = g$LAT
    cur_val = rep(NA, length(cur_geo))
  }

  # Set the lats and lons for states
  else if (attributes(x)$geo_type == "state") {
    state_geo$STATE = tolower(state_geo$STATE)
    g = state_geo[state_geo$STATE %in% map_geo, ]
    cur_geo = g$STATE
    cur_lon = g$LON
    cur_lat = g$LAT
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
  bubble_df = data.frame(lon = cur_lon, lat = cur_lat, val = cur_val)
  bubble_trans = usmap::usmap_transform(bubble_df)
  bubble_layer = ggplot2::geom_point(ggplot2::aes(x = lon.1, y = lat.1,
                                                  size = val),
                                     data = bubble_trans, color = col,
                                     alpha = alpha, na.rm = TRUE)

  # Create the scale layer
  labels = round(breaks, 2)
  guide = ggplot2::guide_legend(title = NULL, horizontal = TRUE, nrow = 1)
  scale_layer = ggplot2::scale_size_manual(values = sizes, breaks = breaks,
                                           labels = labels, drop = FALSE,
                                           guide = guide)

  # Put it all together and return
  return(ggplot2::ggplot() + polygon_layer + ggplot2::coord_equal() +
         title_layer + bubble_layer + scale_layer + theme_layer)
}

# Plot a line (time series) graph of a covidcast_signal object.
plot_line = function(x, range = NULL, col = 1:6, line_type = rep(1:6, each = length(col)),
                     title = NULL, params = list()) {
  # Set a title, if we need to (simple combo of data source, signal)
  if (is.null(title)) title = paste0(unique(x$data_source), ": ",
                                     unique(x$signal))

  # Set other map parameters, if we need to
  xlab = params$xlab
  ylab = params$ylab
  if (is.null(xlab)) xlab = "Date"
  if (is.null(ylab)) ylab = "Value"

  # Grab the values
  df = x %>% dplyr::select(value, time_value, geo_value)

  # Expand the range, if we need to
  range = base::range(c(range, df$value))
  
  # Create label and theme layers
  label_layer = ggplot2::labs(title = title, x = xlab, y = ylab)
  theme_layer = ggplot2::theme_bw() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
    ggplot2::theme(legend.position = "bottom",
                   legend.title = ggplot2::element_blank())

  # Create line and lim layers
  line_layer = ggplot2::geom_line(ggplot2::aes(x = time_value, y = value,
                                               color = geo_value,
                                               group = geo_value), data = df) 
  lim_layer = ggplot2::lims(y = c(range[1], range[2]))
  
  # TODO: show standard error bands?

  # Put it all together and return
  return(ggplot2::ggplot() + line_layer + lim_layer + label_layer + theme_layer)
}

# TODO: plot functions for covidcast_signals objects (note the plural form)?
