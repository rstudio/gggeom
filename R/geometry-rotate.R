#' Rotate a geometry about a pointclockwise, by a specified angle
#'
#' Rotate a geometry about a point clockwise, by a specified angle.
#'
#' @param geom A geometry data frame.
#' @param angle The angle by which to rotate, clockwise.
#' @param x,y The coordinates of the center of rotation.
#' @export
#' @examples
#' scatter_ex %>% plot()
#' # Rotate 5 degrees about the origin
#' scatter_ex %>% plot() %>%
#'   geometry_rotate(5) %>% plot(add = TRUE, col = "red")
#' # Rotate 5 degrees about the smallest point
#' scatter_ex %>% plot() %>%
#'   geometry_rotate(5, x = 9, y = 12) %>% plot(add = TRUE, col = "red")
#'
#' # More extreme rotations
#' scatter_ex %>% plot(xlim = c(-35, 35), ylim = c(-44, 44)) %>%
#'    geometry_rotate(90) %>% plot(add = TRUE, col = "red")
#' scatter_ex %>% geometry_rotate(75, x = 20, y = 20) %>% plot()
#'
#' # To rotate bars, you need to convert to polygons with pointificate
#' bar_ex %>% unique() %>% plot()
#' bar_ex %>% unique() %>% geometry_pointificate() %>%
#'   geometry_rotate(45, 6, 0.5) %>% plot()
geometry_rotate <- function(geom, angle = NULL, x = 0, y = 0) {
  if (missing(angle)) {
    stop("angle must be supplied.")
  }

  theta <- angle / 180 * pi
  m <- matrix(c(cos(theta), -sin(theta), sin(theta), cos(theta)), nrow = 2)
  geometry_transform(geom, m, x, y)
}
