#' @param apply_corrections an optional function that applies data corrections
#'   to the signals. Input is a data frame or list as returned as
#'   `df <- covidcast::download_signals()`.
#'   The returned object should be of the type expected by your forecaster. 
#'   This function will be called as `apply_corrections(df, ...)` using any
#'   named arguments passed in `...` to `get_predictions()`. 
