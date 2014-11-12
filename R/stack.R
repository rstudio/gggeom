#' Stack objects on top of one another.
#'
#' Rects are always stacked upwards from the x-axis, ignoring non-zero
#' \code{y1_}.
#'
#' @inheritParams geometry_flip
#' @param dir Direction in which to stack. "x" or "y" for rects,
#'   only "y" for smooths, "r" or "theta" for arcs.
#' @export
#' @examples
#' bar_ex %>% plot()
#' bar_ex %>% geometry_stack() %>% plot()
#'
#' bar_ex %>% geometry_flip() %>% plot()
#' bar_ex %>% geometry_flip() %>% geometry_stack("x") %>% plot()
#'
#' # Overlapping bars are stacked on top of each other
#' df <- data.frame(x = 1:3, y = 1:3)
#' df %>% render_bar(~x, ~y, 2) %>% plot()
#' df %>% render_bar(~x, ~y, 2) %>% geometry_stack() %>% plot()
#'
#' # Stacking ribbons
#' theta <- seq(0, 2 * pi, length = 50)
#' df <- data.frame(theta)
#' waves <- rbind(
#'   df %>% render_area(~theta, ~abs(sin(theta))),
#'   df %>% render_area(~theta, ~abs(cos(theta)))
#' )
#' waves %>% plot(col = c("red", "black"))
#' waves %>% geometry_stack() %>% plot(col = c("red", "black"))
#' df %>% render_area(~theta, ~abs(sin(theta)) + abs(cos(theta))) %>% plot()
#'
#' # You can also stack arcs, in either r or theta direction
#' pies <- render_arc(mtcars, ~vs, ~am, 0, 0.1, 0, ~mpg / max(mpg) * 2 / pi)
#' pies %>% plot()
#' pies %>% geometry_stack() %>% plot()
#' pies %>% geometry_stack("r") %>% plot()
#'
#' disks <- render_arc(mtcars, ~vs, ~am, 0, 0.05, 0, 2 * pi)
#' disks %>% geometry_stack("r") %>% plot()
geometry_stack <- function(geom, dir = c("y", "x")) {
  UseMethod("geometry_stack")
}

#' @export
geometry_stack.geom <- function(geom, dir) {
  warning("Stacking ", class(geom)[1], " isn't well defined", call. = FALSE)
  geom
}

#' @export
geometry_stack.geom_rect <- function(geom, dir = c("y", "x")) {
  dir <- match.arg(dir)

  if (dir == "x") {
    stacked <- stack_rects(geom$y1_, geom$y2_, geom$x1_, geom$x2_)
    geom$x1_ <- stacked$y1_
    geom$x2_ <- stacked$y2_
  } else {
    stacked <- stack_rects(geom$x1_, geom$x2_, geom$y1_, geom$y2_)
    geom$y1_ <- stacked$y1_
    geom$y2_ <- stacked$y2_
  }

  geom
}

#' @export
geometry_stack.geom_ribbon <- function(geom, dir) {
  stacked <- stack_ribbons(geom$x_, geom$y1_, geom$y2_)
  geom$y1_ <- coords(stacked$y1_)
  geom$y2_ <- coords(stacked$y2_)
  geom
}

#' @export
geometry_stack.geom_arc <- function(geom, dir = c("theta", "r")) {
  dir <- match.arg(dir)
  old_groups <- dplyr::groups(geom)

  geom <- dplyr::group_by_(geom, .dots = c("x_", "y_"), add = TRUE)

  if (dir == "theta") {
    geom <- stack_df(geom, "r1_", "r2_", "theta1_", "theta2_")
  } else {
    geom <- stack_df(geom, "theta1_", "theta2_", "r1_", "r2_")
  }

  # Restore old grouping
  if (!is.null(old_groups)) {
    geom <- dplyr::group_by_(geom, .dots = old_groups)
  } else {
    geom <- dplyr::ungroup(geom)
  }

  class(geom) <- c("geom_arc", "geom", "data.frame")
  geom

}

stack_df <- function(data, x1, x2, y1, y2) {
  stacked <- data %>%
    dplyr::do({
      out <- stack_rects(.[[x1]], .[[x2]], .[[y1]], .[[y2]])
      data <- .
      data[[y1]] <- out$y1_
      data[[y2]] <- out$y2_
      data
    })

  class(stacked) <- class(data)
  stacked
}
