# Plot a choropleth map of a covidcast_signal object.

#' @importFrom stats approx
plot_choro = function(x, time_value = NULL, include = c(), range,
                      col = c("#FFFFCC", "#FD893C", "#800026"),
                      alpha = 0.5, direction = FALSE,
                      dir_col = c("#6F9CC6", "#E4E4E4", "#C56B59"),
                      title = NULL, params = list()) {
  # Check that we're looking at either counties or states
  if (!(attributes(x)$geo_type == "county" ||
                     attributes(x)$geo_type == "state" ||
                     attributes(x)$geo_type == "hrr" ||
                     attributes(x)$geo_type == "msa")) {
    stop("Only 'county', 'state', 'hrr' and 'msa' are supported 
         for choropleth maps.")
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

  # For direction, create a discrete color function
  else {
    if (length(dir_col) != 3) {
      stop("`dir_col` must have length 3.")
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

  # Make background layer for MSA and HRR maps which are incomplete
  if ((attributes(x)$geo_type == "msa") |
      (attributes(x)$geo_type == "hrr")) {
    map_df = sf::st_read('../data/shapefiles/state/cb_2019_us_state_5m.shp')
    background_crs = sf::st_crs(map_df)
    map_df$STATEFP <- as.character(map_df$STATEFP)
    map_df$is_alaska = map_df$STATEFP == '02'
    map_df$is_hawaii = map_df$STATEFP == '15'
    map_df$is_pr = map_df$STATEFP == '72'
    map_df$STATEFP <- as.numeric(map_df$STATEFP)
    map_df$is_state = map_df$STATEFP < 57
    map_df$color = missing_col

    pr_df = map_df %>% filter(.$is_pr)
    hawaii_df = map_df %>% filter(.$is_hawaii)
    alaska_df = map_df %>% filter(.$is_alaska)

    main_df = map_df %>% filter(!map_df$is_alaska)
    main_df = main_df %>% filter(!main_df$is_hawaii)
    main_df = main_df %>% filter(main_df$is_state)

    main_df = sf::st_transform(main_df, 102003)
    hawaii_df = sf::st_transform(hawaii_df, 102007)
    alaska_df = sf::st_transform(alaska_df, 102006)
    pr_df = sf::st_transform(pr_df, 102003)

    hawaii_shift = sf::st_geometry(hawaii_df) + c(-1e+6, -2e+6)
    hawaii_df = sf::st_set_geometry(hawaii_df, hawaii_shift)
    hawaii_df = sf::st_set_crs(hawaii_df, 102003)

    alaska_centroid = sf::st_centroid(sf::st_geometry(alaska_df))
    print('hi')
    alaska_scale = (sf::st_geometry(alaska_df) - alaska_centroid) * 0.35 + alaska_centroid
    alaska_df = sf::st_set_geometry(alaska_df, alaska_scale)
    alaska_shift = sf::st_geometry(alaska_df) + c(-2e+6, -2.6e+6)
    alaska_df = sf::st_set_geometry(alaska_df, alaska_shift)
    alaska_df = sf::st_set_crs(alaska_df, 102003)

    print('hi')

    pr_shift = sf::st_geometry(pr_df) + c(-0.9e+6, 1e+6)
    pr_df = sf::st_set_geometry(pr_df, pr_shift)
    r = -16 * pi / 180
    rotation = matrix(c(cos(r), sin(r), -sin(r), cos(r)), nrow = 2, ncol = 2)
    pr_centroid = sf::st_centroid(sf::st_geometry(pr_df))
    pr_rotate = (sf::st_geometry(pr_df) - pr_centroid) * rotation + pr_centroid
    pr_df = sf::st_set_geometry(pr_df, pr_rotate)
    pr_df = sf::st_set_crs(pr_df, 102003)

    main_col = main_df$color
    hawaii_col = hawaii_df$color
    alaska_col = alaska_df$color
    pr_col = pr_df$color

    aes = ggplot2::aes
    geom_args = list()
    geom_args$color = border_col
    geom_args$size = border_size
    geom_args$mapping = aes(geometry=geometry)

    geom_args$fill = main_col
    geom_args$data = main_df
    back_main_layer = do.call(ggplot2::geom_sf, geom_args)
    geom_args$fill = pr_col
    geom_args$data = pr_df
    back_pr_layer = do.call(ggplot2::geom_sf, geom_args)
    geom_args$fill = hawaii_col
    geom_args$data = hawaii_df
    back_hawaii_layer = do.call(ggplot2::geom_sf, geom_args)
    geom_args$fill = alaska_col
    geom_args$data = alaska_df
    back_alaska_layer = do.call(ggplot2::geom_sf, geom_args)
    }

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

  else if (attributes(x)$geo_type == "msa") {
    map_df = sf::st_read('../data/shapefiles/msa/cb_2019_us_cbsa_5m.shp')
    map_df = map_df %>% filter(map_df$LSAD == 'M1') # only get metro and not micropolitan areas
    if (length(include) > 0) {
      map_df = map_df %>% filter(map_df$GEOID %in% include)
    }
    map_df$NAME <- as.character(map_df$NAME)
    map_df$is_alaska = substr(map_df$NAME, nchar(map_df$NAME) - 1, nchar(map_df$NAME)) == 'AK'
    map_df$is_hawaii = substr(map_df$NAME, nchar(map_df$NAME) - 1, nchar(map_df$NAME)) == 'HI'
    map_df$is_pr = substr(map_df$NAME, nchar(map_df$NAME) - 1, nchar(map_df$NAME)) == 'PR'
    map_df$color = ifelse(map_df$GEOID %in% geo,
                          col_fun(val[map_df$GEOID]),
                          missing_col)

    pr_df = map_df %>% filter(map_df$is_pr)
    hawaii_df = map_df %>% filter(map_df$is_hawaii)
    alaska_df = map_df %>% filter(map_df$is_alaska)
    main_df = map_df %>% filter(!map_df$is_alaska)
    main_df = main_df %>% filter(!main_df$is_hawaii)
    main_df = main_df %>% filter(!main_df$is_pr)

    main_df = sf::st_transform(main_df, 102003)
    hawaii_df = sf::st_transform(hawaii_df, 102007)
    alaska_df = sf::st_transform(alaska_df, 102006)
    pr_df = sf::st_transform(pr_df, 102003)

    hawaii_shift = sf::st_geometry(hawaii_df) + c(-1e+6, -2e+6)
    hawaii_df = sf::st_set_geometry(hawaii_df, hawaii_shift)
    hawaii_df = sf::st_set_crs(hawaii_df, 102003)

    # Note centroid is centroid for entire state (defined for background)
    alaska_scale = (sf::st_geometry(alaska_df) - alaska_centroid) * 0.35 + alaska_centroid
    alaska_df = sf::st_set_geometry(alaska_df, alaska_scale)
    alaska_shift = sf::st_geometry(alaska_df) + c(-2e+6, -2.6e+6)
    alaska_df = sf::st_set_geometry(alaska_df, alaska_shift)
    alaska_df = sf::st_set_crs(alaska_df, 102003)

    pr_shift = sf::st_geometry(pr_df) + c(-0.9e+6, 1e+6)
    pr_df = sf::st_set_geometry(pr_df, pr_shift)
    r = -16 * pi / 180
    rotation = matrix(c(cos(r), sin(r), -sin(r), cos(r)), nrow = 2, ncol = 2)
    # Note centroid is same as for entire territory (defined for background)
    pr_rotate = (sf::st_geometry(pr_df) - pr_centroid) * rotation + pr_centroid
    pr_df = sf::st_set_geometry(pr_df, pr_rotate)
    pr_df = sf::st_set_crs(pr_df, 102003)

    main_col = main_df$color
    hawaii_col = hawaii_df$color
    alaska_col = alaska_df$color
    pr_geo = pr_df$color
  }

  else if (attributes(x)$geo_type == "hrr") {
    map_df = sf::st_read('../data/shapefiles/hrr/geo_export_ad86cff5-e5ed-432e-9ec2-2ce8732099ee.shp')
    if (length(include) > 0) {
      map_df = map_df %>% filter(map_df$hrr_num %in% include)
    }
    map_df = sf::st_transform(map_df, background_crs)
    hrr_shift = sf::st_geometry(map_df) + c(0, -0.185)
    map_df = sf::st_set_geometry(map_df, hrr_shift)
    map_df = sf::st_set_crs(map_df, background_crs)
    map_df$hrr_name <- as.character(map_df$hrr_name)
    map_df$is_alaska = substr(map_df$hrr_name, 1, 2) == 'AK'
    map_df$is_hawaii = substr(map_df$hrr_name, 1, 2) == 'HI'
    map_df$is_pr = substr(map_df$hrr_name, 1, 2) == 'PR'
    map_df$color = ifelse(map_df$hrr_num %in% geo,
                          col_fun(val[map_df$hrr_num]),
                          missing_col)

    pr_df = map_df %>% filter(map_df$is_pr)
    hawaii_df = map_df %>% filter(map_df$is_hawaii)
    alaska_df = map_df %>% filter(map_df$is_alaska)

    main_df = map_df %>% filter(!map_df$is_alaska)
    main_df = main_df %>% filter(!main_df$is_hawaii)
    main_df = main_df %>% filter(!main_df$is_pr)

    main_df = sf::st_transform(main_df, 102003)
    hawaii_df = sf::st_transform(hawaii_df, 102007)
    alaska_df = sf::st_transform(alaska_df, 102006)
    pr_df = sf::st_transform(pr_df, 102003)

    hawaii_shift = sf::st_geometry(hawaii_df) + c(-1e+6, -2e+6)
    hawaii_df = sf::st_set_geometry(hawaii_df, hawaii_shift)
    hawaii_df = sf::st_set_crs(hawaii_df, 102003)

    # Note centroid is centroid for entire state (defined for background)
    alaska_scale = (sf::st_geometry(alaska_df) - alaska_centroid) * 0.35 + alaska_centroid
    alaska_df = sf::st_set_geometry(alaska_df, alaska_scale)
    alaska_shift = sf::st_geometry(alaska_df) + c(-2e+6, -2.6e+6)
    alaska_df = sf::st_set_geometry(alaska_df, alaska_shift)
    alaska_df = sf::st_set_crs(alaska_df, 102003)

    pr_shift = sf::st_geometry(pr_df) + c(-0.9e+6, 1e+6)
    pr_df = sf::st_set_geometry(pr_df, pr_shift)
    r = -16 * pi / 180
    rotation = matrix(c(cos(r), sin(r), -sin(r), cos(r)), nrow = 2, ncol = 2)
    # Note centroid is same as for entire territory (defined for background)
    pr_rotate = (sf::st_geometry(pr_df) - pr_centroid) * rotation + pr_centroid
    pr_df = sf::st_set_geometry(pr_df, pr_rotate)
    pr_df = sf::st_set_crs(pr_df, 102003)

    main_col = main_df$color
    hawaii_col = hawaii_df$color
    alaska_col = alaska_df$color
    pr_col = pr_df$color
  }

  # Create the polygon layer 
  if (attributes(x)$geo_type == "county" || 
      attributes(x)$geo_type == "state") {
    aes = ggplot2::aes
    geom_args = list()
    geom_args$color = border_col
    geom_args$size = border_size
    geom_args$mapping = aes(x = x, y = y, group = group)
    geom_args$fill = map_col
    geom_args$data = map_df
    polygon_layer = do.call(ggplot2::geom_polygon, geom_args)
    coord_layer = ggplot2::coord_equal()
    }
  else if (attributes(x)$geo_type == "msa" || 
          attributes(x)$geo_type == "hrr") {
    aes = ggplot2::aes
    geom_args = list()
    geom_args$color = border_col
    geom_args$size = border_size
    geom_args$mapping = aes(geometry=geometry)
    coord_args = list()

    geom_args$fill = main_col
    geom_args$data = main_df
    main_layer = do.call(ggplot2::geom_sf, geom_args)
    geom_args$fill = pr_col
    geom_args$data = pr_df
    pr_layer = do.call(ggplot2::geom_sf, geom_args)
    geom_args$fill = hawaii_col
    geom_args$data = hawaii_df
    hawaii_layer = do.call(ggplot2::geom_sf, geom_args)
    geom_args$fill = alaska_col
    geom_args$data = alaska_df
    alaska_layer = do.call(ggplot2::geom_sf, geom_args)
    coord_layer = do.call(ggplot2::coord_sf, coord_args)
    }

  # For intensity and continuous color scale, create a legend layer
  if (!direction && is.null(breaks)) {
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

  # For intensity and discrete color scale, create a legend layer
  else if (!direction && !is.null(breaks)) {
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

  # For direction, create a legend layer
  else {
    # Create legend breaks and legend labels
    n = 3
    legend_breaks = -1:1
    legend_labels = c("Decreasing", "Steady", "Increasing")

    # Now the legend layer (hidden + scale)
    hidden_df = data.frame(x = rep(Inf, n), z = as.factor(legend_breaks))
    hidden_layer = ggplot2::geom_polygon(aes(x = x, y = x, fill = z),
                                         data = hidden_df, alpha = 1)
    guide = ggplot2::guide_legend(title = NULL, horizontal = TRUE, nrow = 1,
                                  keyheight = legend_height,
                                  keywidth = legend_width / n,
                                  label.position = "bottom",
                                  override.aes = list(alpha = 1))
    scale_layer = ggplot2::scale_fill_manual(values = dir_col,
                                             breaks = legend_breaks,
                                             labels = legend_labels,
                                             guide = guide)
  }
  # Put it all together and return
  if ((attributes(x)$geo_type == "msa") |
      (attributes(x)$geo_type == "hrr")) {
    return(ggplot2::ggplot() + 
          back_main_layer + back_pr_layer + back_hawaii_layer + back_alaska_layer + 
          main_layer + pr_layer + alaska_layer + hawaii_layer + coord_layer +
          title_layer + hidden_layer + scale_layer + theme_layer)
      }
  else {
    return(ggplot2::ggplot() + polygon_layer + coord_layer +
          title_layer + hidden_layer + scale_layer + theme_layer)
  }
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
  if (is.null(min_size)) min_size = ifelse(attributes(x)$geo_type == "county",
                                           0.1, 1)
  if (is.null(max_size)) max_size = ifelse(attributes(x)$geo_type == "county",
                                           4, 12)

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
  if (attributes(x)$geo_type == "county") {
    centroids = covidcast::county_geo[covidcast::county_geo$fips %in% map_geo, ]
    cur_geo = centroids$fips
    cur_val = rep(NA, length(cur_geo))
  }
  else if (attributes(x)$geo_type == "state") {
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

# TODO: plot functions for covidcast_signals objects (note the plural form)?
