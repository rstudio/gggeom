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
#' scatter_ex %>% geometry_transform(shear) %>% plot()
#' scatter_ex %>% geometry_transform(reflect_y) %>% plot()
#' scatter_ex %>% geometry_transform(rotate30) %>% plot()
#'
#' bar_ex %>% plot()
#' # Convert to geometry_polygon
#' bar_ex2 <- bar_ex %>% geometry_pointificate()
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
geometry_transform.geom <- function(geom, m, x = 0, y = 0) {
  matrix_transform(geom, "x_", "y_", m, x, y)
}

#' @export
geometry_transform.geom_ribbon <- function(geom, m, x = 0, y = 0) {
  stop("Can't transform ribbons", call. = FALSE)
}

#' @export
geometry_transform.geom_rect <- function(geom, m, x = 0, y = 0) {
  stop("Can't transform ribbons rects", call. = FALSE)
}

# Using x and y as the center, multiply matrix m with df[c(a, b), ]
matrix_transform <- function(df, a, b, m, x = 0, y = 0) {
  nms <- names(df)

  pos <- match(c(a, b), nms)
  if (any(is.na(pos))) {
    stop("Couldn't find column ", c(a, b)[is.na(pos)], call. = FALSE)
  }

  # Convert to data frame (needed because df[pos] won't work with grouped_df)
  res <- as.data.frame(df)
  # Transpose and translate the data before the matrix multiplication,
  # then un-translate.
  res <- t(res[pos]) - c(x, y)
  res <- m %*% res
  res <- res + c(x, y)

  # Extract the rows of the matrix, and store as columns in df. This is so we
  # preserve classes on df.
  df[[pos[1]]] <- res[1, ]
  df[[pos[2]]] <- res[2, ]
  df
}
