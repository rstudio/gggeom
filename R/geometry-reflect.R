#' Reflect positions around an axis.
#'
#' @inheritParams geometry_flip
#' @param dir Direction in which to reflect. One of "x" and "y". (Specifying "x"
#'   will flip the x values about the y axis.
#' @export
#' @examples
#' scatter_ex %>% plot()
#' scatter_ex %>% geometry_reflect() %>% plot()
#' scatter_ex %>% geometry_reflect("y") %>% plot()
#'
#' histogram_ex %>% plot()
#' histogram_ex %>% geometry_reflect() %>% plot()
#' histogram_ex %>% geometry_reflect("y") %>% plot()
#' histogram_ex %>% geometry_reflect("y") %>% geometry_flip() %>% plot()
geometry_reflect <- function(geom, dir = c("x", "y")) {
  UseMethod("geometry_reflect")
}

#' @export
geometry_reflect.geom <- function(geom, dir = c("x", "y")) {
  dir <- match.arg(dir)

  if (dir == "x") {
    geom$x_ <- -geom$x_
  } else {
    geom$y_ <- -geom$y_
  }

  geom
}

#' @export
geometry_reflect.geom_ribbon <- function(geom, dir = c("x", "y")) {
  dir <- match.arg(dir)

  if (dir == "x") {
    geom$x1_ <- -geom$x1_
    geom$x2_ <- -geom$x2_
    geom <- switch_cols(geom, "x1_", "x2")
  } else {
    geom$y_ <- -geom$y
  }

  geom
}

#' @export
geometry_reflect.geom_rect <- function(geom, dir = c("x", "y")) {
  dir <- match.arg(dir)

  if (dir == "x") {
    geom$x1_ <- -geom$x1_
    geom$x2_ <- -geom$x2_
    geom <- switch_cols(geom, "x1_", "x2_")
  } else {
    geom$y1_ <- -geom$y1_
    geom$y2_ <- -geom$y2_
    geom <- switch_cols(geom, "y1_", "y2_")
  }

  geom
}

#' @export
geometry_reflect.geom_segment <- function(geom, dir = c("x", "y")) {
  geometry_reflect.geom_rect(geom, dir)
}

