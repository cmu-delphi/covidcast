#' Create or overwrite a corrections database
#'
#' This function pulls data from the covidcast api, creates new variables as
#' necessary, writes a file to a database, and optionally returns the modified
#' dataframe.
#'
#'
#' @param data_source String identifying the data source to query. See the
#'   [signal
#'   documentation](https://cmu-delphi.github.io/delphi-epidata/api/covidcast_signals.html)
#'   for a list of available data sources.
#' @param signal String identifying the signal from that source to query. Again,
#'   see the [signal
#'   documentation](https://cmu-delphi.github.io/delphi-epidata/api/covidcast_signals.html)
#'   for a list of available signals.
#' @template geo_type-template
#' @template geo_value-template
#' @param subsetter a function that receives the data frame downloaded from covidcast and
#'   returns a subset of its rows
#' @param corrector a function that receives the subset and adds desired columns. At least
#'   one of these columns should be names "corrected" with the replacement values desired.
#'   If a logical column is named "flag", only `geo_values` containing an instance of `TRUE`
#'   will be optionally returned.
#' @param write_db a path to an SQLite database where any rows with `corrected` != `value`
#'   will be stored. This file will be created if it doesn't already exist.
#' @param db_object_name the name of the data frame in which to store the corrections
#' @param ... additional arguments passed to \link[covidcast]{covidcast_signal()}
#'
#' @return Optionally returns the wide, subset data frame created by `corrector`
#' @export
create_corrections_db <- function(data_source,
                                  signal,
                                  geo_type = c("county", "hrr", "msa", "dma", "state"),
                                  geo_values = "*",
                                  subsetter = function(x) x,
                                  corrector = function(x) x,
                                  write_db = FALSE,
                                  db_object_name = NULL,
                                  ...
                                  ) {
  # need "corrected" as one name, all elements in correction_mutation need
  # to make sense. No idea how to check these
  orig_data <- download_signal(data_source, signal, geo_type, geo_values, ...)

  # orig_data <- covidcast:::latest_issue(orig_data) # as_of?,
  # above line doesnt work since download_signal renames geo_value to location
  covidcast_schema <- tibble::as_tibble(orig_data[FALSE,])

  filtered_data <- subsetter(orig_data)
  wide_data <- corrector(orig_data)

  # check if we made a column called corrected, and didn't delete anything important
  if(!any(names(wide_data)=="corrected")) {
    wide_data$corrected = wide_data$value
    warning("You didn't create any corrections. Adding a column with the original values.")
  }

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
    message(msg)
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
  params = line_params
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
    tidyr::pivot_longer(value:corrected)


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

  # facet_layer <- NULL
  # scales <- ifelse(is.null(params$scales), "fixed", params$scales)
  # facet_labels <- ifelse(
  #   attributes(x)$geo_type == "county",
  #   county_label_lookup,
  #   "label_value")
  #
  # if(!is.null(facet_by)){
  #   facet_layer <- ggplot2::facet_wrap(
  #     facet_by, scales=scales, labeller=labeller(location=facet_labels))
  # }

  # Put it all together and return
  ggplot2::ggplot(aes(x = time_value), data = df) + line_layer +
    ribbon_layer + #facet_layer +
    label_layer + theme_layer
}
