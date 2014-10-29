# Point -----------------------------------------------------------------------

render_point <- function(data, x, y) {
  data$x_ <- eval_vector(data, x)
  data$y_ <- eval_vector(data, y)

  class(data) <- c("geom_point", "geom", data)
  data
}

# Polygon ----------------------------------------------------------------------

render_polygon <- function(data, x, y) {
  data$x_ <- eval_vector(data, x)
  data$y_ <- eval_vector(data, y)

  class(data) <- c("geom_polygon", "geom", data)
  data
}

# Path -------------------------------------------------------------------------

render_path <- function(data, x, y) {
  data$x_ <- eval_vector(data, x)
  data$y_ <- eval_vector(data, y)

  class(data) <- c("geom_path", "geom", data)
  data
}

# Rect -------------------------------------------------------------------------

render_rect <- function(data, x1, y1, x2, y2) {
  data$x1_ <- eval_vector(data, x1)
  data$x2_ <- eval_vector(data, x2)
  data$y1_ <- eval_vector(data, y2)
  data$y2_ <- eval_vector(data, y1)

  class(data) <- c("geom_rect", "geom", data)
  data
}

render_bar <- function(data, x, y, width) {
  x <- eval_vector(data, x)
  y <- eval_vector(data, y)
  width <- eval_vector(data, width)

  data$x1_ <- x - width / 2
  data$x2_ <- x + width / 2
  data$y1 <- 0
  data$y2 <- y

  class(data) <- c("geom_rect", "geom", data)
  data
}

render_tile <- function(data, x, y, width, height) {
  x <- eval_vector(data, x)
  y <- eval_vector(data, y)
  width <- eval_vector(data, width)
  height <- eval_vector(data, height)


  data$x1_ <- x - width / 2
  data$x2_ <- x + width / 2
  data$y1 <- y - height / 2
  data$y2 <- y + height / 2

  class(data) <- c("geom_rect", "geom", data)
  data
}

# Ribbon -----------------------------------------------------------------------

render_ribbon <- function(data, x, y1, y2) {
  data$x_ <- eval_vector(data, x)
  data$y1_ <- eval_vector(data, y2)
  data$y2_ <- eval_vector(data, y1)

  class(data) <- c("geom_ribbon", "geom", data)
  data
}

render_area <- function(data, x) {
  render_ribbon(data, x, 0, y)
}

