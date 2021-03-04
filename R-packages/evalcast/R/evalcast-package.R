#' evalcast
#'
#' A package to evaluate forecasters using the `covidcast` R package.
#'
#' @name evalcast-package
#' @docType package
#' @author Jacob Bien
#' Maintainer: Jacob Bien <jbien@usc.edu>
#' @keywords package
#' @import covidcast
#' @import dplyr
#' @import purrr
#' @import tibble
#' @import tidyr
#' @import readr
#' @import stringr
#' @import ggplot2
#' @import lubridate
#' @importFrom assertthat assert_that
#' @importFrom memoise memoise
NULL

.onLoad <- function(libname, pkgname) {
    download_signal <<- memoise::memoise(download_signal)
    download_signals <<- memoise::memoise(download_signals)
    get_covidhub_predictions <<- memoise::memoise(get_covidhub_predictions)
    get_forecaster_predictions <<- memoise::memoise(get_forecaster_predictions)
    msg <- c(
        "Calls to the `evalcast` functions",
        "  - `download_signal()`",
        "  - `download_signals()`",
        "  - `get_covidhub_predictions()`",
        "  - `get_forecast_predictions()`",
        "are memoized in memory by default. To cache to disk instead use",
        "`memoise::cache_filesystem()`. Ex:",
        "",
        " db <- memoise::cache_filesystem(\"path/to/dir/.rcache\")",
        " assign(\"_cache\", db, envir=environment(evalcast::download_signal))",
        "")
    packageStartupMessage(paste(msg, collapse = "\n"))
}
