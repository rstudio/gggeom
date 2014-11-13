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
#'
#' nz %>% plot()
#' nz %>% geometry_reflect() %>% plot()
#' nz %>% geometry_reflect("y") %>% plot()
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
geometry_reflect.geom_path <- function(geom, dir = c("x", "y")) {
  dir <- match.arg(dir)

  if (dir == "x") {
    geom$x_ <- lapply(geom$x_, `-`)
  } else {
    geom$y_ <- lapply(geom$y_, `-`)
  }

  geom
}

#' @export
geometry_reflect.geom_ribbon <- function(geom, dir = c("x", "y")) {
  dir <- match.arg(dir)

  if (dir == "x") {
    geom$y1_ <- -geom$y1_
    geom$y2_ <- -geom$y2_
    geom <- switch_cols(geom, "y1_", "y2_")
  } else {
    geom$x_ <- -rev(geom$x_)
    geom$y1_ <- rev(geom$y1_)
    geom$y2_ <- rev(geom$y2_)
  }

  geom
}

#' @export
geometry_reflect.geom_line <- function(geom, dir = c("x", "y")) {
  dir <- match.arg(dir)

  if (dir == "x") {
    geom$y_ <- -geom$y_
  } else {
    geom$x_ <- -rev(geom$x_)
    geom$y_ <- rev(geom$y_)
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
