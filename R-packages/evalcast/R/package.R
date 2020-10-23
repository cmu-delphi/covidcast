#' evalcast
#'
#' A package to evaluate forecasters using the \code{covidcast} R package.
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
#' @importFrom memoise memoise
NULL

.onLoad <- function(libname, pkgname){
  download_signal <<- memoise::memoise(download_signal)
}
