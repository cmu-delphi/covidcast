#' Wrappers to functions that remove `NA` values by default
#' @export
#' @noRd
Min <- function(x) min(x, na.rm = TRUE)

#' @rdname Min
#' @export
#' @noRd
Max <- function(x) max(x, na.rm = TRUE)

#' @rdname Min
#' @export
#' @noRd
Sum <- function(x) sum(x, na.rm = TRUE)

#' @rdname Min
#' @export
#' @noRd
Mean <- function(x) mean(x, na.rm = TRUE)

#' @rdname Min
#' @export
#' @noRd
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

#' Drop prefix in column names, i.e. 'value:0<data_source>', after pivoting 
#' to wide format
#' @export
#' @noRd
drop_prefix <- function(x, data_source){
  x %>% setNames(gsub(paste0("value\\+0:", data_source, "_"), "", names(.)))
}

#' Function for suppressing all types of output messages
#' @export
#' @noRd
quiet <- function(x) { 
  sink(tempfile()) 
  on.exit(sink()) 
  invisible(force(x)) 
}
