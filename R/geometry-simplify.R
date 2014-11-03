#' Simplify a path geometry with Douglas-Peucker
#'
#' @inheritParams geometry_rotate
#' @param tol_prop,tol_dist Either specify the proportion of points to keep,
#'   or a distance threshold.
#' @seealso \code{\link{compute_dp_distance}()} to add the distance as
#'   an explicit column so you can experimenting with different thresholds
#'   more easily.
#' @export
geometry_simplify <- function(geom, tol_prop = 0.1, tol_dist = NULL) {
  if (is.null(tol_prop) + is.null(tol_dist) != 1) {
    stop("Must supply exactly one of tol_prop and tol_dist")
  }

  UseMethod("geometry_simplify")
}

#' @export
geometry_simplify.geom_path <- function(geom, tol_prop = 0.1, tol_dist = NULL) {

  dist <- compute_dp_distance(geom, ~x, ~y)$dist

  if (is.null(tol_prop)) {
    tol_dist <- quantile(dist, tol_prop)
    message("Using tol_dist of ", format(tol_dist, digits = 3))
  }

  geom[dist >= tol_dist]
}
