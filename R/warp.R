#' Warp a path or polygon with adaptive resampling
#'
#' This recursively split each line segment into pieces until the transformed
#' point is less that \code{tolerance} away from the transformed line
#' segment.
#'
#' @inheritParams geometry_flip
#' @param fun A warping function to use. Currently these are hard coded
#'   because they much be implemented in C++ for performance reasons.
#' @param tolerance Approximation errors below this threshold will be
#'   ignored. If \code{NULL}, attempts to guess a reasonable value.
#' @export
#' @examples
#' spiral <- data.frame(
#'   x = seq(0, 6 * pi, length = 10),
#'   y = seq(0, 1, length = 10)
#' )
#' path <- render_path(spiral, ~x, ~y)
#' path %>% plot() %>% points()
#'
#' path %>% geometry_warp("polar") %>% plot()
#' path %>% geometry_warp("polar", tolerance = 0.1) %>% plot() %>% points()
#'
#' # Crazy example
#' expand.grid(x = seq(0, pi, length = 4), y = 0:3) %>%
#'   render_tile(~x, ~y, halign = 0, valign = 0) %>%
#'   geometry_pointificate() %>%
#'   geometry_rotate(15) %>%
#'   geometry_warp("polar") %>%
#'   plot()
geometry_warp <- function(geom, fun = c("polar", "identity"), tolerance = NULL) {
  UseMethod("geometry_warp")
}

#' @export
geometry_warp.geom_path <- function(geom, fun = c("polar", "identity"),
                                    tolerance = NULL) {
  tolerance <- tolerance %||% guess_tolerance(geom$x_, geom$y_)

  warped <- Map(function(x, y) warp(x, y, fun, tolerance ^ 2), geom$x_, geom$y_)
  geom$x_ <- coords(pluck(warped, "x"))
  geom$y_ <- coords(pluck(warped, "y"))

  geom
}

#' @export
geometry_warp.geom_polygon <- function(geom, fun = c("polar", "identity"),
                                       tolerance = NULL) {

  tolerance <- tolerance %||% guess_tolerance(geom$x_, geom$y_)

  warped <- Map(function(x, y) warp(x, y, fun, tolerance ^ 2, closed = TRUE),
    geom$x_, geom$y_)
  geom$x_ <- coords(pluck(warped, "x"))
  geom$y_ <- coords(pluck(warped, "y"))

  geom
}

guess_tolerance <- function(x, y) {
  # Need to be doing this on transformed range, not original!
  x_rng <- diff(range(x, na.rm = TRUE))
  y_rng <- diff(range(y, na.rm = TRUE))

  guess <- min(x_rng, y_rng) / 1e3
  message("Using tolerance of ", format(guess, digits = 3))
  guess
}
