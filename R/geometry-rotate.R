#' Rotate a geometry about a point, by a specified angle
#'
#' Rotate a geometry about a point, by a specified angle.
#'
#' @param geom A geometry data frame.
#' @param angle The angle by which to rotate, going clockwise.
#' @param x,y The coordinates of the center of rotation.
#' @export
#' @examples
#' scatter_ex %>% plot()
#' scatter_ex %>% geometry_rotate(90) %>% plot()
#' scatter_ex %>% geometry_rotate(75, x = 20, y = 20) %>% plot()
#'
#' bar_ex %>% plot()
#' bar_ex %>% geometry_pointificate() %>%
#'   geometry_rotate(45, 6, 0.5) %>% plot()
geometry_rotate <- function(geom, angle = NULL, x = 0, y = 0) {
  if (missing(angle)) {
    stop("angle must be supplied.")
  }

  theta <- angle / 180 * pi
  m <- matrix(c(cos(theta), -sin(theta), sin(theta), cos(theta)), nrow = 2)
  geometry_transform(geom, m, x, y)
}
