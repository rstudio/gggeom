#' Compute the Douglas-Peucker distance for paths.
#'
#' Douglas-Peucker is a recursive line simplification algorithm. It starts by
#' defining a line from the first to the last point, and then finds the
#' point that is furthest from the line. It then recursively breaks up the
#' into two pieces around the furthest point, and finds the furthest point
#' from those sublines. See
#' \url{http://en.wikipedia.org/wiki/Ramer-Douglas-Peucker_algorithm} for
#' more details.
#'
#' Note that this function does not do any simplification - it just adds an
#' additional column the measures the distaince between each point and it
#' subline. Filtering on this column will perform simplification.
#'
#' @param data A data frame like object.
#' @param x_var,y_var Formulas specifying either variable names or
#'   expressions to use as x and y positions.
#' @seealso \code{\link{geometry_simplify}()} for an function that works on
#'   a geometry and does simplification given tolerance or percentage of
#'   points to keep.
#' @return A data frame with columns:
#'  \item{x_,y_}{Position}
#'  \item{distance_}{Distance between point and sub-line}
#' @export
#' @examples
#' x <- 1:10
#' y <- x * 2
#' df <- data.frame(x, y)
#'
#' # For a straight line, can remove all points except first and last
#' compute_dp_distance(df, ~x, ~y)
compute_dp_distance <- function(data, x_var, y_var) {
  UseMethod("compute_dp_distance")
}

#' @export
compute_dp_distance.data.frame <- function(data, x_var, y_var) {
  x <- eval_vector(data, x_var)
  y <- eval_vector(data, y_var)

  data.frame(
    x_ = x,
    y_ = y,
    dist_ = dp_distance(x, y)
  )
}

#' @export
compute_dp_distance.grouped_df <- function(data, x_var, y_var) {
  dplyr::do(data, compute_dp_distance(., x_var, y_var))
}
