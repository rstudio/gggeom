#' Compute the distance for paths with Visvalingam's algorithm.
#'
#' Visvalingam's algorithm is a method of simplifying lines. It starts by
#' computing the area of the triangle formed by each sequence of three points.
#' (The triangles are overlapping.) Then it removes the middle point of the
#' triangle with the smallest area, and recomputes the area of the neighbor
#' triangles that are affected by the removal of that point. This repeats until
#' all the points are removed except the two endpoints.
#'
#' See \url{http://bost.ocks.org/mike/simplify/} for more details.
#'
#' Note that this function does not do any simplification - it just adds an
#' additional column the measures the area of the triangle at each point (where
#' the point is the middle point of the triangle). Filtering on this column will
#' perform simplification.
#'
#' @param data A data frame like object.
#' @param x_var,y_var Formulas specifying either variable names or expressions
#'   to use as x and y positions.
#' @seealso \code{\link{geometry_simplify}()} for an function that works on a
#'   geometry and does simplification given tolerance or percentage of points to
#'   keep.
#' @return A data frame with columns: \item{x_,y_}{Position}
#'   \item{distance_}{Distance between point and sub-line}
#' @export
#' @examples
#' df <- data.frame(x = c(1,2,3,4,5), y = c(1,1.2,3.8,3.3,5))
#'
#' # compute_v_distance(df, ~x, ~y)
compute_v_distance <- function(data, x_var, y_var) {
  UseMethod("compute_v_distance")
}

#' @export
compute_v_distance.data.frame <- function(data, x_var, y_var) {
  x <- eval_vector(data, x_var)
  y <- eval_vector(data, y_var)

  data.frame(
    x_ = x,
    y_ = y,
    dist_ = v_distance(x, y)
  )
}

#' @export
compute_v_distance.grouped_df <- function(data, x_var, y_var) {
  dplyr::do(data, compute_v_distance(., x_var, y_var))
}
