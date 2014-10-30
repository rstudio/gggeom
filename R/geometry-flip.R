#' Flip positions around an axis.
#'
#' @inheritParams geometry_rotate
#' @param dir Direction in which to flip. One of "x" and "y".
#' @export
#' @examples
#' scatter <- render_point(mtcars, ~mpg, ~wt)
#' scatter %>% plot()
#' scatter %>% geometry_flip() %>% plot()
#' scatter %>% geometry_flip("y") %>% plot()
#'
#' bar <- mtcars %>%
#'   compute_count(~cyl) %>%
#'   render_bar(~x_, ~count_, width = 1)
#' bar %>% plot()
#' bar %>% geometry_flip() %>% plot()
#' bar %>% geometry_flip("y") %>% plot()
#' bar %>% geometry_flip("y") %>% geometry_rotate() %>% plot()
geometry_flip <- function(geom, dir = c("x", "y")) {
  UseMethod("geometry_flip")
}

#' @export
geometry_flip.geom <- function(geom, dir = c("x", "y")) {
  dir <- match.arg(dir)

  if (dir == "x") {
    geom$x_ <- -geom$x_
  } else {
    geom$y_ <- -geom$y
  }

  geom
}

#' @export
geometry_flip.geom_ribbon <- function(geom, dir = c("x", "y")) {
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
geometry_flip.geom_rect <- function(geom, dir = c("x", "y")) {
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

