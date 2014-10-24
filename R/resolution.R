#' Compute the "resolution" of a data vector.
#'
#' The resolution is is the smallest non-zero distance between adjacent
#' values.  If there is only one unique value, then the resolution is defined
#' to be one.
#'
#' If x is an integer vector, then it is assumed to represent a discrete
#' variable, and the resolution is 1.
#'
#' @param x numeric vector
#' @param zero should a zero value be automatically included in the
#'   computation of resolution
#' @export
#' @examples
#' resolution(1:10)
#' resolution((1:10) - 0.5)
#' resolution((1:10) - 0.5, FALSE)
#' resolution(c(1,2, 10, 20, 50))
#' resolution(as.integer(c(1, 10, 20, 50)))  # Returns 1
resolution <- function(x, zero = TRUE) {
  if (is.integer(x))
    return(1)

  resolution_numeric(x, zero = zero)
}
