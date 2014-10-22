#' Efficient implementation of range.
#'
#' This is an efficient C++ implementation of range for numeric vectors:
#' it avoids S3 dispatch, and computes both min and max in a single pass
#' through the input.
#'
#' @param x A numeric vector.
#' @param finite If \code{TRUE} ignores missing values and infinities. Note
#'   that if the vector is empty, or only contains missing values,
#'   \code{frange} will return \code{c(Inf, -Inf)} because those are the
#'   identity values for \code{\link{min}} and \code{\link{max}} respectively.
#' @export
#' @examples
#' x <- runif(1e6)
#' system.time(range(x))
#' system.time(frange(x))
frange <- function(x, finite = TRUE) {
  if (!is_numeric(x)) {
    stop("x must be numeric", call. = FALSE)
  }

  restore(x, frange_(x, finite))
}
