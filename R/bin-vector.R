#' Bin vector into equal sized ranges.
#'
#' Bin a numeric vector and count how many observations fall in each bin.
#' Supports weights so that you can re-bin pre-binned data.
#'
#' @section Floating point:
#' If a point is less than \code{binwidth} / 10^8 from the boundary between
#' two bins, it is shifted to fall in the bin with the closest "closed" side.
#'
#' @param x A numeric vector to bin.
#'
#'   You can also bin S3 objects that are build on top of integer and
#'   double atomic vectors, as long as there is a method for
#'   \code{\link{restore}()}.
#' @param weight If specified, an integer vector of the same length as \code{x}
#'   giving weights. If weights are provided, the weights in each bin are
#'   summed, rather than just counting the number of observations.
#' @param width (Positive real). The width of a bin. For S3 objects, the
#'   interpretation of width depends on the interpretation
#'   of the underlying numeric vector. For example, for dates, 1 = 1 day;
#'   for times 1 = 1 second; and for difftime, the units vary.
#'
#'   If \code{NULL}, the \code{width} will be derived from the data,
#'   picking approximately 30 bins with nice widths.
#' @param boundary,center Set the position of the first bin by specifying
#'   the position of either a boundary or the center of a bin.
#' @param origin The location of the left-most bin edge. Any values smaller
#'   than the \code{origin} will be treated as if they are missing. If
#'   \code{NULL} will be computed from \code{center} and \code{boundary}.
#' @param closed One of \code{"right"} or \code{"left"} indicating whether the
#'   bin interval is left-closed (i.e. [a, b)), or right-closed (i.e. (a, b]).
#' @param pad A logical indicating whether the bins should be padded to include
#'   an empty bin on each side. This is useful for frequency polygons which
#'   need to go back down to zero at either end of the range.
#' @keywords internal
#' @export
#' @examples
#' x <- runif(1e6)
#' vector_bin(x)
#' vector_bin(x, width = 0.25)
#'
#' # Bin other types of object
#' vector_bin(Sys.time() + runif(10) * 60, 15)
#' vector_bin(Sys.Date() + sample(30, 10), 7)
#'
#' # Performance scales linearly with the size of x, and the number
#' # of bins has limited impact
#' x <- runif(1e7)
#' system.time(vector_bin(x, width = 0.1))
#' system.time(vector_bin(x, width = 1 / 100))
#' system.time(vector_bin(x, width = 1 / 1e5))
vector_bin <- function(x, width = NULL, origin = NULL, center = NULL,
                      boundary = NULL, weight = NULL,
                      closed = c("right", "left"), pad = FALSE) {
  stopifnot(is.atomic(x), typeof(x) %in% c("double", "integer"), !is.factor(x))
  closed <- match.arg(closed)

  if (length(weight) == 0) {
    weight <- numeric()
  }

  right_closed <- identical(closed, "right")
  params <- bin_params(frange(x), width = width, center = center,
    boundary = boundary, right_closed = right_closed)

  out <- condense_count(x,
    origin = origin %||% params$origin,
    width = params$width,
    pad = pad,
    right_closed = right_closed,
    w = weight
  )
  out$x_ <- restore(x, out$x_)
  out$xmin_ <- restore(x, out$xmin_)
  out$xmax_ <- restore(x, out$xmax_)

  `as.data.frame!`(out, length(out[[1]]))
  out
}
