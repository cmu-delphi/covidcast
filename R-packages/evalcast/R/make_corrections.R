create_corrections_db <- function(...,
                                  correction_mutation = list(corrected = value),
                                  write_db = FALSE,
                                  db_object_name = NULL) {
  # need "corrected" as one name, all list elements need to be named
  # check that no other weird stuff in the list
  args <- list(...)
  orig_data <- download_signal(args)
  orig_data <- covidcast:::latest_issue(orig_data) # as_of?
  covidcast_schema <- tibble::as_tibble(orig_data[FALSE,])

  # we want to pass a named list of new columns to dplyr::mutate()
  # see https://community.rstudio.com/t/passing-named-list-to-mutate-and-probably-other-dplyr-verbs/2553/6
  new_cols <- rlang::enexpr(correction_mutation)
  list_of_new_cols <- rlang::lang_args(new_cols)
  wide_data <- dplyr::mutate(orig_data, !!! list_of_new_cols)

  corrected_data <- dplyr::select(
    wide_data,
    dplyr::matches(c(names(covidcast_schema),"corrected")))

  if(!is.null(db_object_name) && write_db){
    update_corrections(write_db, db_object_name, corrected_data)
    msg <- stringr::str_glue(
      "Writing corrected {signal} from {source} as {object} to corrections database.",
      signal = args$signal,
      source = args$data_source,
      object = db_object_name,
      .sep = " "
    )
  }
  class(wide_data) <- c("covidcast_corrected", class(orig_data))
  invisible(wide_data)
}


plot.covidcast_corrected <- function(x,
                                     range=NULL,
                                     title=NULL,
                                     facet_by="location",
                                     line_params=list(
                                       scales="free_y",
                                       ncol=5)) {
  ## taken from covidcast::plot_line() commit a4bfc26

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
  df = x %>%
    dplyr::select(value, corrected, time_value, location) %>%
    pivot_longer(value:corrected)


  # Create label and theme layers
  label_layer = ggplot2::labs(title = title, x = xlab, y = ylab)
  theme_layer = ggplot2::theme_bw() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
    ggplot2::theme(legend.position = "bottom",
                   legend.title = ggplot2::element_blank())

  # Create lim, line, and ribbon layers
  aes = ggplot2::aes
  lim_layer = ggplot2::coord_cartesian(ylim = range)
  line_layer = ggplot2::geom_line(aes(y = value, color = name))

  ribbon_layer = NULL
  if (stderr_bands) {
    ribbon_layer = ggplot2::geom_ribbon(
      aes(ymin = value - stderr, ymax = value + stderr),
      alpha = stderr_alpha)
  }

  facet_layer <- NULL
  scales <- ifelse(is.null(line_params$scales), "fixed", line_params$scales)
  facet_labels <- ifelse(
    attributes(x)$geo_type == county,
    county_label_lookup,
    "label_value")

  if(!is.null(facet_by)){
    facet_layer <- ggplot2::facet_wrap(
      facet_by, scales=scales, labeller=labeller(location=facet_labels))
  }

  # Put it all together and return
  ggplot2::ggplot(aes(x = time_value), data = df) + line_layer +
    ribbon_layer + facet_layer + label_layer + theme_layer
}
