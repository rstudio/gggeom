#' @export
print.geom <- function(x, ...) {
  NextMethod()
  cat("Geometry: ", class(x)[1], "\n", sep = "")
}

# Point -----------------------------------------------------------------------

#' Render point and text geometries.
#'
#' @param data A data frame.
#' @param x,y Formulas specifying x and y positions.
#' @export
#' @examples
#' render_point(mtcars, ~mpg, ~wt)
#' render_text(mtcars, ~mpg, ~wt)
render_point <- function(data, x, y) {
  data$x_ <- eval_vector(data, x)
  data$y_ <- eval_vector(data, y)

  class(data) <- c("geom_point", "geom", "tbl_df", "data.frame")
  data
}

#' @export
plot.geom_point <- function(x, y, pch = 20, ..., add = FALSE) {
  if (!add) plot_init(x$x_, x$y_, ...)

  points(x$x_, x$y_, pch = pch, ...)
  invisible(x)
}

#' @export
#' @rdname render_point
render_text <- function(data, x, y) {
  data$x_ <- eval_vector(data, x)
  data$y_ <- eval_vector(data, y)

  class(data) <- c("geom_text", "geom", "tbl_df", "data.frame")
  data
}

#' @export
plot.geom_text <- function(x, y, labels = 1:nrow(x), ..., add = FALSE) {
  if (!add) plot_init(x$x_, x$y_, ...)

  text(x$x_, x$y_, labels = labels, ...)
  invisible(x)
}

# Path -------------------------------------------------------------------------

#' Render line, path and polygon geometries.
#'
#' A polygon is a closed path. A line is path where x values are ordered.
#'
#' @param data A data frame.
#' @param x,y Formulas specifying x and y positions.
#' @export
#' @examples
#' # For paths and polygons, the x_ and y_ variables are lists of vectors
#' # See ?coord for more details
#' theta <- seq(0, 6*pi, length = 200)
#' r <- seq(1, 0, length = 200)
#' df <- data.frame(x = r * sin(theta), y = r * cos(theta))
#' spiral <- df %>% render_path(~x, ~y)
#'
#' spiral
#' str(spiral)
#' spiral %>% plot()
#'
#' # Rendering a spiral as a line doesn't work so well
#' df %>% render_line(~x, ~y) %>% plot()
#'
#' nz
#' nz %>% plot()
render_path <- function(data, x, y) {
  poly <- render_polygon(data, x, y)

  class(poly) <- c("geom_path", "geom", "tbl_df", "data.frame")
  poly
}

#' @export
plot.geom_path <- function(x, y, col = "grey10", ..., add = FALSE) {
  if (!add) plot_init(x$x_, x$y_, ...)

  lines(ungroupNA(x$x_), ungroupNA(x$y_), col = col, ...)
  invisible(x)
}

#' @export
points.geom_path <- function(x, y, pch = 20, ...) {
  points(ungroupNA(x$x_), ungroupNA(x$y_), pch = pch, ...)
  invisible(x)
}

#' @rdname render_path
#' @export
render_line <- function(data, x, y) {
  poly <- render_polygon(data, x, y)

  order_x <- function(x, y) {
    ord <- order(x)
    list(x = x[ord], y = y[ord])
  }
  ordered <- Map(order_x, poly$x_, poly$y)
  poly$x_ <- pluck(ordered, "x")
  poly$y_ <- pluck(ordered, "y")

  class(poly) <- c("geom_path", "geom_line", "geom", "tbl_df", "data.frame")
  poly
}

# Polygon ----------------------------------------------------------------------

#' @export
#' @rdname render_path
render_polygon <- function(data, x, y) UseMethod("render_polygon")

#' @export
render_polygon.data.frame <- function(data, x, y) {
  out <- list(
    x_ = coords(list(eval_vector(data, x))),
    y_ = coords(list(eval_vector(data, y)))
  )
  `as.data.frame!`(out, 1L)
  class(out) <- c("geom_polygon", "geom", "tbl_df", "data.frame")
  out
}

#' @export
render_polygon.grouped_df <- function(data, x, y) {

  data <- data %>%
    dplyr::do(render_polygon(., x, y))

  class(data) <- c("geom_polygon", "geom", "tbl_df", "data.frame")
  data
}

#' @export
plot.geom_polygon <- function(x, y, col = "#7F7F7F7F", ..., add = FALSE) {
  if (!add) plot_init(x$x_, x$y_, ...)

  polygon(ungroupNA(x$x_), ungroupNA(x$y_), col = col, ...)
  invisible(x)
}

#' @export
points.geom_polygon <- points.geom_path


# Segment ----------------------------------------------------------------------

#' Render a line segment
#'
#' A line segment is a single straight line.
#'
#' @inheritParams render_point
#' @param x1,y1,x2,y2 Locations of start and end points. sides.
#' @export
#' @examples
#' df <- expand.grid(x = 1:2, y = 1:2)
#' a <- render_rect(df, ~x - 0.5, ~y - 0.5, ~x + 0.5, ~y + 0.5)
#' b <- render_segment(df, ~x - 0.5, ~y - 0.5, ~x + 0.5, ~y + 0.5)
#'
#' plot(a)
#' plot(b, add = TRUE, col = "red", lwd = 2)
render_segment <- function(data, x1, y1, x2, y2) {
  data$x1_ <- eval_vector(data, x1)
  data$x2_ <- eval_vector(data, x2)
  data$y1_ <- eval_vector(data, y2)
  data$y2_ <- eval_vector(data, y1)

  class(data) <- c("geom_segment", "geom", "tbl_df", "data.frame")
  data
}

#' @export
plot.geom_segment <- function(x, y, col = "grey10", ..., add = FALSE) {
  if (!add) plot_init(c(x$x1_, x$x2_), c(x$y1_, x$y2_), ...)
  segments(x$x1_, x$y1_, x$x2_, x$y2_, col = col, ...)
  invisible(x)
}

# Rect -------------------------------------------------------------------------

#' Render a rect.
#'
#' A rect is defined by the coordinates of its sides. Bars and tiles are
#' convenient parameterisations based on the length of the sides.
#'
#' @inheritParams render_point
#' @param x1,y1,x2,y2 Describe a rectangle by the locations of its sides.
#' @param x,y,width,height Describe a rectangle by location and dimension.
#' @param halign,valign Horizontal and vertical aligned. Defaults to 0.5,
#'   centered.
#' @export
#' @examples
#' # Two equivalent specifications
#' render_rect(mtcars, ~cyl - 0.5, ~gear - 0.5, ~cyl + 0.5, ~gear + 0.5)
#' render_tile(mtcars, ~cyl, ~gear, 1, 1)
#'
#' mtcars %>%
#'   compute_count(~cyl) %>%
#'   render_bar(~x_, ~count_, width = 1)
#' .Last.value %>% plot()
render_rect <- function(data, x1, y1, x2, y2) {
  data$x1_ <- eval_vector(data, x1)
  data$x2_ <- eval_vector(data, x2)
  data$y1_ <- eval_vector(data, y2)
  data$y2_ <- eval_vector(data, y1)

  class(data) <- c("geom_rect", "geom", "tbl_df", "data.frame")
  data
}

#' @export
plot.geom_rect <- function(x, y, col = "#7F7F7F7F", ..., add = FALSE) {
  if (!add) plot_init(c(x$x1_, x$x2_), c(x$y1_, x$y2_), ...)

  rect(x$x1_, x$y1_, x$x2_, x$y2_, col = col, ...)
  invisible(x)
}

#' @export
#' @rdname render_rect
render_bar <- function(data, x, y, width = resolution(x) * 0.9, halign = 0.5) {
  x <- eval_vector(data, x)
  y <- eval_vector(data, y)
  width <- eval_vector(data, width)

  data$x1_ <- x - width * halign
  data$x2_ <- x + width * (1 - halign)
  data$y1_ <- 0
  data$y2_ <- y

  class(data) <- c("geom_rect", "geom", "tbl_df", "data.frame")
  data
}

#' @export
#' @rdname render_rect
render_tile <- function(data, x, y, width = resolution(x),
                        height = resolution(y), halign = 0.5, valign = 0.5) {
  x <- eval_vector(data, x)
  y <- eval_vector(data, y)
  width <- eval_vector(data, width)
  height <- eval_vector(data, height)

  data$x1_ <- x - width * halign
  data$x2_ <- x + width * (1 - halign)
  data$y1_ <- y - height * valign
  data$y2_ <- y + height * (1 - valign)

  class(data) <- c("geom_rect", "geom", class(data))
  data
}

# Ribbon -----------------------------------------------------------------------

#' Render a ribbon.
#'
#' @inheritParams render_point
#' @param x,y1,y2 x location and y interval.
#' @export
#' @examples
#' x <- 1:10
#' y <- runif(10, 0, 2)
#' df <- data.frame(x = x, y1 = x * 2 - y, y2 = x * 2 + y)
#' render_ribbon(df, ~x, ~y1, ~y2)
#' .Last.value %>% plot()
render_ribbon <- function(data, x, y1, y2) UseMethod("render_ribbon")

#' @export
render_ribbon.data.frame <- function(data, x, y1, y2) {
  out <- data[1, , drop = FALSE]
  out$x_ <- list(eval_vector(data, x))
  out$y1_ <- list(eval_vector(data, y1))
  out$y2_ <- list(eval_vector(data, y2))

  class(out) <- c("geom_ribbon", "geom", "tbl_df", "data.frame")
  out
}

#' @export
render_ribbon.grouped_df <- function(data, x, y1, y2) {

  data <- data %>%
    dplyr::do(render_ribbon(., x, y1, y2))

  class(data) <- c("geom_ribbon", "geom", "tbl_df", "data.frame")
  data
}

#' @export
#' @rdname render_ribbon
render_area <- function(data, x, y2) {
  render_ribbon(data, x, 0, y2)
}

#' @export
plot.geom_ribbon <- function(x, y, col = "#7F7F7F7F", ..., add = FALSE) {
  if (!add) plot_init(x$x_, c(x$y1_, x$y2_), ...)

  x <- geometry_pointificate(x)
  plot(x, col = col, add = TRUE, ...)

  invisible(x)
}

# Arc --------------------------------------------------------------------------

#' Render an arc
#'
#' @inheritParams render_point
#' @param x,y Location of arc
#' @param r1,r2 Extent of radius
#' @param theta1,theta2 Extent of angle (in radians).
#' @export
#' @examples
#' render_arc(mtcars, ~vs, ~am, 0, 0.1, 0, ~mpg / max(mpg) * 2 * pi)
render_arc <- function(data, x, y, r1, r2, theta1, theta2) {
  data$x_ <- eval_vector(data, x)
  data$y_ <- eval_vector(data, y)
  data$r1_ <- eval_vector(data, r1)
  data$r2_ <- eval_vector(data, r2)
  data$theta1_ <- eval_vector(data, theta1)
  data$theta2_ <- eval_vector(data, theta2)

  class(data) <- c("geom_arc", "geom", "tbl_df", "data.frame")
  data
}

#' @export
plot.geom_arc <- function(x, y, ..., col = "#7F7F7F7F", add = FALSE) {
  x$id_ <- 1:nrow(x)
  polys <- x %>%
    dplyr::group_by_(~ id_) %>%
    dplyr::do(
      make_arc(.$x_, .$y_, c(.$r1_, .$r2_), c(.$theta1_, .$theta2_))
    )

  if (!add) plot_init(polys$x_, polys$y_, ...)
  dplyr::do(polys, `_` = polygon(.$x, .$y, col = col, ...))
  invisible(x)
}

make_arc <- function(x, y, r, theta) {
  inner_theta <- seq(theta[1], theta[2], length = 1 + (r[1] * 2 * pi) / 0.01)
  inner_x <- x + r[1] * cos(inner_theta)
  inner_y <- y + r[1] * sin(inner_theta)

  outer_theta <- seq(theta[1], theta[2], length = 1 + (r[2] * 2 * pi) / 0.01)
  outer_x <- x + r[2] * cos(rev(outer_theta))
  outer_y <- y + r[2] * sin(rev(outer_theta))

  data.frame(x_ = c(inner_x, outer_x), y_ = c(inner_y, outer_y))
}
