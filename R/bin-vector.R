#' Bin vectors
#'
#' A generic and several implementations for binning vectors.
#'
#' @param x A vector to bin
#' @param weight If specified, an integer vector of the same length as \code{x}
#'   representing the number of occurances of each value in \code{x}
#' @param width The width of a bin
#' @param origin The left-most value for bins.
#' @param closed One of \code{"right"} or \code{"left"} indicating whether
#'   right or left edges of bins are included in the bin.
#' @param pad A logical indicating whether the bins should be padded to include
#'   an empty bin on each side.
#' @keywords internal
#' @export
#' @examples
#' x <- runif(1e6)
#' bin_vector(x, 0.1)
bin_vector <- function(x, width = 1, origin = 0, weight = NULL,
                      closed = c("right", "left"), pad = FALSE, na.rm = FALSE) {
  closed <- match.arg(closed)

  if (length(weight) == 0) {
    weight <- numeric()
  }

  condense_count(x, origin = origin, width = width, w = weight)
}

# TODO:
# * implement closed right/left
# * implement pad = TRUE
# * implement na.rm = TRUE
# * implement restore for restoring properties of numeric variants

# Adapt break fuzziness from base::hist - this protects from floating
# point rounding errors
adjust_breaks <- function(breaks, closed = "left") {
  closed <- match.arg(closed, c("right", "left"))

  diddle <- 1e-08 * median(diff(breaks))
  if (closed == "right") {
    # first bin gets negative diddle, all others get positive
    fuzz <- c(-diddle, rep.int(diddle, length(breaks) - 1))
  } else {
    # last bin gets positive diddle, all others get negative
    fuzz <- c(rep.int(-diddle, length(breaks) - 1), diddle)
  }
  sort(breaks) + fuzz
}

