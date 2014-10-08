#' Bin vectors
#'
#' Bin a numeric vector and count how many observations fall in each bin.
#' Supports weights so that you can re-bin pre-binned data.
#'
#' @param x A numeric vector to bin.
#'
#'   You can also bin S3 objects that are build on top of integer and
#'   double atomic vectors, as long as there is a method for
#'   \code{\link{restore}()}.
#' @param weight If specified, an integer vector of the same length as \code{x}
#'   giving weights. If weights are provided, the weights in each bin are
#'   summed, rather than just counting the number of observations.
#' @param width The width of a bin. Must be positive.
#'
#'   For S3 objects, the interpretation of width depends on the interpretation
#'   of the underlying numeric vector. For example, for dates, 1 = 1 day;
#'   for times 1 = 1 second; and for difftime, the units vary.
#' @param origin The location of the left-most bin edge. Any values smaller
#'   than the \code{origin} will be treated as if they are missing.
#' @param closed One of \code{"right"} or \code{"left"} indicating whether the
#'   bin interval is left-closed (i.e. [a, b)), or right-closed (i.e. (a, b]).
#' @param pad A logical indicating whether the bins should be padded to include
#'   an empty bin on each side. This is useful for frequency polygons which
#'   need to go back down to zero at either end of the range.
#' @keywords internal
#' @export
#' @examples
#' x <- runif(1e6)
#' bin_vector(x, 0.1)
#'
#' # Bin other types of object
#' bin_vector(Sys.time() + runif(10) * 60, 15)
#' bin_vector(Sys.Date() + sample(30, 10), 7)
#'
#' # Performance scales linearly with the size of x, and the number
#' # of bins has limited impact
#' x <- runif(1e7)
#' system.time(bin_vector(x))
#' system.time(bin_vector(x, width = 1 / 100))
#' system.time(bin_vector(x, width = 1 / 1e5))
bin_vector <- function(x, width = 1, origin = min(x, na.rm = TRUE),
                      weight = NULL, closed = c("right", "left"), pad = FALSE,
                      na.rm = FALSE) {
  stopifnot(is.atomic(x), typeof(x) %in% c("double", "integer"))
  closed <- match.arg(closed)

  if (length(weight) == 0) {
    weight <- numeric()
  }

  out <- condense_count(x, origin = origin, width = width, w = weight)
  out$x <- restore(x, out$x)

  `as.data.frame!`(out, length(out[[1]]))
  out
}

# TODO:
# * add boundaries to output
# * implement closed right/left
# * implement pad = TRUE
# * implement na.rm = TRUE

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

