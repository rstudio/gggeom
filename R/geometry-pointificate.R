#' Convert complex geometries in to points, paths and polygons.
#'
#' @inheritParams geometry_rotate
#' @param ... Additional arguments passed on to methods.
#'   \itemize{
#'    \item{\code{geometry_polygon}: use \code{close = TRUE} to "close" the
#'      polygon by putting the first point at the end.}
#'   }
#' @export
#' @examples
#' x <- seq(0, 4 * pi, length = 100)
#' df <- data.frame(x = x, y = sin(x))
#' rib <- render_ribbon(df, ~x, ~y - 1, ~ y + 1)
#' rib %>% plot()
#' rib %>% geometry_pointificate() %>% plot()
#' rib %>% geometry_pointificate() %>% geometry_rotate() %>% plot()
#'
#' df <- expand.grid(x = 1:3, y = 1:3)
#' df$z <- runif(9, pi, 2 * pi)
#' arc <- df %>% render_arc(~x, ~y, 0, 0.35, 0, ~z)
#' arc %>% plot()
#' arc %>% geometry_pointificate() %>% plot()
#'
#' histogram_ex %>% plot()
#' histogram_ex %>% geometry_pointificate() %>%	plot()
geometry_pointificate <- function(geom, ...) {
  UseMethod("geometry_pointificate")
}

#' @export
geometry_pointificate.geom_point <- function(geom, ...) {
  geom
}

#' @export
geometry_pointificate.geom_text <- function(geom, ...) {
  geom
}

#' @export
geometry_pointificate.geom_path <- function(geom, ...) {
  geom
}

#' @export
geometry_pointificate.geom_polygon <- function(geom, ..., close = FALSE) {
  if (!close)  {
    geom
  } else {
    dplyr::slice(geom, c(1:n(), 1))
  }
}

#' @export
geometry_pointificate.geom_ribbon <- function(geom, ...) {
  pointificate <- function(df) {
    x <- c(df$x_, rev(df$x_))
    y <- c(df$y1_, rev(df$y2_))

    df$x_ <- NULL
    df$y1_ <- NULL
    df$y2_ <- NULL

    df <- df[c(1:nrow(df), nrow(df):1), , drop = FALSE]

    df$x_ <- x
    df$y_ <- y

    df
  }

  out <- dplyr::do(geom, pointificate(.))
  class(out) <- c("geom_polygon", "geom", class(out))
  out
}

#' @export
geometry_pointificate.geom_arc <- function(geom, ...) {
  pointificate <- function(df) {
    arc <- make_arc(df$x_, df$y_, c(df$r1_, df$r2_), c(df$theta1_, df$theta2_))

    df$x_ <- NULL
    df$y_ <- NULL
    df$r1_ <- NULL
    df$r2_ <- NULL
    df$theta1_ <- NULL
    df$theta2_ <- NULL

    `as.data.frame!`(df, 1)
    df <- df[rep(1, nrow(arc)), , drop = FALSE]

    df$x_ <- arc$x_
    df$y_ <- arc$y_

    df
  }

  geom$id_ <- 1:nrow(geom)
  out <- geom %>%
    dplyr::group_by(id_) %>%
    dplyr::do(pointificate(.))
  class(out) <- c("geom_polygon", "geom", class(out))
  out
}


#' @export
geometry_pointificate.geom_rect <- function(geom, ...) {
  pointificate <- function(df) {
    x <- c(df$x1_, df$x2_, df$x2_, df$x1_)
    y <- c(df$y1_, df$y1_, df$y2_, df$y2_)

    df$x1_ <- NULL
    df$x2_ <- NULL
    df$y1_ <- NULL
    df$y2_ <- NULL

    df <- df[rep(1:nrow(df), each = 4), , drop = FALSE]

    df$x_ <- x
    df$y_ <- y

    df
  }

  geom$id_ <- 1:nrow(geom)
  out <- geom %>%
    dplyr::group_by(id_) %>%
    dplyr::do(pointificate(.))
  class(out) <- c("geom_polygon", "geom", class(out))
  out
}
