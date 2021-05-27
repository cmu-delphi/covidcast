#' Wrappers to functions that remove `NA` values by default
#' @export
Min <- function(x) min(x, na.rm = TRUE)

#' @rdname Min
#' @export
Max <- function(x) max(x, na.rm = TRUE)

#' @rdname Min
#' @export
Sum <- function(x) sum(x, na.rm = TRUE)

#' @rdname Min
#' @export
Mean <- function(x) mean(x, na.rm = TRUE)

#' @rdname Min
#' @export
Median <- function(x) median(x, na.rm = TRUE)

##########

#' Returns the first or last parts of a vector, data frame / tibble, matrix, 
#' table, etc.
#' @export
#' @noRd
Start <- function(x) head(x, 1)

#' @export
#' @noRd
End <- function(x) tail(x, 1)

##########

#' Function for suppressing all types of output messages
#' @export
#' @noRd
quiet <- function(x) { 
  sink(tempfile()) 
  on.exit(sink()) 
  invisible(force(x)) 
}
