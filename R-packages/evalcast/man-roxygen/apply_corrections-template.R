#' @param apply_corrections an optional function that applies data corrections
#'   to the signals. Input is a data frame as returned by `download_signals()`.
#'   The returned object must be the same but with additional variables.
#'   Corrected values must exist in the column `corrected`.
