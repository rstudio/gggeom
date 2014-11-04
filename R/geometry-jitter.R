#' Jitter geometries to avoid overplotting.
#'
#' Jitter adds random uniform offsets to avoid overplotting.
#'
#' Jittering differs a little depending on the underlying geometry:
#' \describe{
#'   \item{point,path,polygon,text}{Jitters both x and y.}
#'   \item{ribbon}{Jitters y1 & y2 by same amount}
#'   \item{rect}{Jitters x1 & x2, and y1 & y2 by same amount - the
#'     height and width stay the same.}
#' }
#' @inheritParams geometry_rotate
#' @param x,y amount to jitter in x and y directions. In most cases, defaults
#'   to 40\% of the resolution of the data.
#' @export
#' @examples
#' scatter_ex %>%
#'  plot() %>%
#'  geometry_jitter() %>%
#'  plot(add = TRUE, col = "red")
#'
#' # Can override amount of jitter
#' scatter_ex %>%
#'  plot() %>%
#'  geometry_jitter(0.2, 0.2) %>%
#'  plot(add = TRUE, col = "red")
geometry_jitter <- function(geom, x = NULL, y = NULL) {
  UseMethod("geometry_jitter")
}

#' @export
geometry_jitter.geom <- function(geom,
                                 x = resolution(geom$x_) * 0.4,
                                 y = resolution(geom$y_) * 0.4) {
  geom$x_ <- geom$x_ + jitter(geom, x)
  geom$y_ <- geom$y_ + jitter(geom, y)
  geom
}

#' @export
geometry_jitter.geom_ribbon <- function(geom, x = 0, y = resolution(geom$y1_) * 0.4) {
  geom$x_ <- geom$x_ + jitter(geom, x)

  y_jitter <- jitter(geom, y)
  geom$y1_ <- geom$y1_ + y_jitter
  geom$y2_ <- geom$y2_ + y_jitter

  geom
}

#' @export
geometry_jitter.geom_rect <- function(geom,
                                      x = resolution(geom$x1_) * 0.4,
                                      y = resolution(geom$y1_) * 0.4) {
  x_jitter <- jitter(geom, x)
  geom$x1_ <- geom$x1_ + x_jitter
  geom$x2_ <- geom$x2_ + x_jitter

  y_jitter <- jitter(geom, y)
  geom$y1_ <- geom$y1_ + y_jitter
  geom$y2_ <- geom$y2_ + y_jitter

  geom
}

#' @export
geometry_jitter.geom_segment <- function(geom,
                                         x = resolution(geom$x1_) * 0.4,
                                         y = resolution(geom$y1_) * 0.4) {

  geometry_jitter.geom_rect(geom, x = x, y = y)
}


jitter <- function(geom, amount) {
  if (amount == 0) return(0)
  runif(nrow(geom), -amount, amount)
}
