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
NULL

.onLoad <- function(libname, pkgname) {
    download_signal <<- memoise::memoise(download_signal)
    get_covidhub_predictions <<- memoise::memoise(get_covidhub_predictions)
    msg <- c("Calls to functions `download_signal()` and `get_covidhub_predictions()` are memoized",
    "in memory by default.  To cache to disk instead use `memoise::cache_filesystem()`.  Ex:",
    "",
    "  db <- memoise::cache_filesystem(\"path/to/my/cache/dir/.rcache\")",
    "  download_signal <<- memoise::memoise(download_signal, cache = db)",
    "")
    packageStartupMessage(paste(msg, collapse = "\n"))
}