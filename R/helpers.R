`%hascols%` <- function(x, y) {
  all(y %in% colnames(x))
}

`%hasvals%` <- function(x, y) {
  all(x %in% y)
}

`%keepcols%` <- function(x, y) {
  x[, y, drop = FALSE]
}

#' Find proportion of a value in vector
#'
#' @param x An atomic vector.
#' @param val A value belonging to vector.
#' @param na.rm Remove \code{NA} values?
#'
#' @return proportion of \code{val} in \code{x}
#'
#' @importFrom stats complete.cases
prop_value <- function(x, val, na.rm = TRUE) {

  stopifnot(is.atomic(x))

  if (na.rm == TRUE) {
    ind <- complete.cases(x)
    x <- x[ind]
  }

  sum(x == val) / length(x)
}
