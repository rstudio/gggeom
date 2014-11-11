#' Warp a path or polygon with adaptive resampling
#'
#' This recursively split each line segment into pieces until the transformed
#' point is less that \code{tolerance} away from the transformed line
#' segment.
#'
#' @inheritParams geometry_flip
#' @param fun A warping function to use. Currently these are hard coded
#'   because they much be implemented in C++ for performance reasons.
#' @param tolerance Approximation errors below this threshold will be
#'   ignored.
#' @export
#' @examples
#' spiral <- data.frame(
#'   x = seq(0, 6 * pi, length = 10),
#'   y = seq(0, 1, length = 10)
#' )
#' path <- render_path(spiral, ~x, ~y)
#' path %>% plot() %>% points()
#'
#' path %>% geometry_warp("polar") %>% plot()
#' path %>% geometry_warp("polar", tolerance = 0.00001) %>% plot() %>% points()
geometry_warp <- function(geom, fun = c("polar", "identity"), tolerance = 0.001) {
  UseMethod("geometry_warp")
}

#' @export
geometry_warp.geom_path <- function(geom, fun = c("polar", "identity"),
                                    tolerance = 0.001) {

  warped <- Map(function(x, y) warp(x, y, fun, tolerance), geom$x_, geom$y_)
  geom$x_ <- pluck(warped, "x")
  geom$y_ <- pluck(warped, "y")

  geom
}