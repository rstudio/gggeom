#' Perform a linear transformation on a geometry.
#'
#' Perform a linear transformation on a geometry, using a specified point as the
#' center.
#'
#' @param geom A geometry data frame.
#' @param m A 2x2 transformation matrix.
#' @param x,y The coordinates of the center to use for the transformation.
#' @export
#' @examples
#' # Some transformation matrices
#' shear <- matrix(c(1, 0, 0.75, 1), nrow = 2)
#' reflect_y <- matrix(c(-1, 0, 0, 1), nrow = 2)
#' t <- 30 * pi / 180
#' rotate30 <- matrix(c(cos(t), -sin(t), sin(t), cos(t)), nrow = 2)
#'
#' scatter_ex %>% plot()
#' scatter_ex %>% plot() %>% geometry_transform(shear)
#' scatter_ex %>% geometry_transform(reflect_y) %>% plot()
#' scatter_ex %>% geometry_transform(rotate30) %>% plot()
#'
#' bar_ex %>% plot()
#' # Convert to geometry_polygon
#' bar_ex2 <- bar_ex %>% unique() %>% geometry_pointificate()
#' bar_ex2 %>% geometry_transform(shear) %>% plot()
#' bar_ex2 %>% geometry_transform(reflect_y) %>% plot()
#' bar_ex2 %>% geometry_transform(rotate30) %>% plot()
#'
#' # Rotate about (10, 1) instead of origin
#' bar_ex2 %>% geometry_transform(rotate30, 10, 1) %>% plot()
geometry_transform <- function(geom, m, x = 0, y = 0) {
  UseMethod("geometry_transform")
}

#' @export
geometry_transform.geom_point <- function(geom, m, x = 0, y = 0) {
  trans <- transform(geom$x_, geom$y_, m, x, y)

  geom$x_ <- trans$x
  geom$y_ <- trans$y
  geom
}
#' @export
geometry_transform.geom_text <- geometry_transform.geom_point
#' @export
geometry_transform.geom_arc <- geometry_transform.geom_point


#' @export
geometry_transform.geom_path <- function(geom, m, x = 0, y = 0) {
  trans <- Map(
    function(x1, y1) transform(x1, y1, m, x_center = x, y_center = 0),
    geom$x_,
    geom$y_
  )

  geom$x_ <- pluck(trans, "x")
  geom$y_ <- pluck(trans, "y")
  geom
}

#' @export
geometry_transform.geom_line <- function(geom, m, x = 0, y = 0) {
  warning("Transforming lines converts to paths", call. = FALSE)
  out <- geometry_transform(geom, m, x, y)
  class(out) <- setdiff(class(out), "geom_line")
  out
}

#' @export
geometry_transform.geom_ribbon <- function(geom, m, x = 0, y = 0) {
  stop("Can't transform ribbons. Perhaps you want to use geometry_pointificate() first?", call. = FALSE)
}

#' @export
geometry_transform.geom_rect <- function(geom, m, x = 0, y = 0) {
  stop("Can't transform rects. Perhaps you want to use geometry_pointificate() first?", call. = FALSE)
}

transform <- function(x, y, m, x_center, y_center) {
  trans <- cbind(x_center, y_center)[rep(1, length(x)), ]

  res <- cbind(x, y) - trans
  res <- res %*% m
  res <- res + trans

  list(x = res[, 1], y = res[, 2])
}
