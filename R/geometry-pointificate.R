#' Convert complex geometries in to points, paths and polygons.
#'
#' @inheritParams geometry_flip
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
#' rib %>% geometry_pointificate() %>% geometry_flip() %>% plot()
#'
#' df <- expand.grid(x = 1:3, y = 1:3)
#' df$z <- runif(9, pi, 2 * pi)
#' arc <- df %>% render_arc(~x, ~y, 0, 0.35, 0, ~z)
#' arc %>% plot()
#' arc %>% geometry_pointificate() %>% plot()
#'
#' histogram_ex %>% plot()
#' histogram_ex %>% geometry_pointificate() %>%	plot()
#'
#' nz %>% plot()
#' nz %>% geometry_pointificate(close = TRUE) %>% plot()
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
    close <- function(x) x[c(1:length(x), 1)]

    geom$x_ <- lapply(geom$x_, close)
    geom$y_ <- lapply(geom$y_, close)
    geom
  }
}

#' @export
geometry_pointificate.geom_ribbon <- function(geom, ...) {
  geom$x_ <- row_apply(geom, function(df) c(df$x_, rev(df$x_)))
  geom$y_ <- row_apply(geom, function(df) c(df$y1_, rev(df$y2_)))
  geom$y1_ <- NULL
  geom$y2_ <- NULL

  class(geom) <- c("geom_polygon", "geom", "data.frame")
  geom
}

#' @export
geometry_pointificate.geom_arc <- function(geom, ...) {

  arcs <- row_apply(geom, function(df) {
    make_arc(df$x_, df$y_, c(df$r1_, df$r2_), c(df$theta1_, df$theta2_))
  })

  geom$x_ <- lapply(arcs, `[[`, "x_")
  geom$y_ <- lapply(arcs, `[[`, "y_")

  geom$r1_ <- NULL
  geom$r2_ <- NULL
  geom$theta1_ <- NULL
  geom$theta2_ <- NULL

  class(geom) <- c("geom_polygon", "geom", "data.frame")
  geom
}


#' @export
geometry_pointificate.geom_rect <- function(geom, ...) {

  geom$x_ <- row_apply(geom, function(df) c(df$x1_, df$x2_, df$x2_, df$x1_))
  geom$y_ <- row_apply(geom, function(df) c(df$y1_, df$y1_, df$y2_, df$y2_))

  geom$x1_ <- NULL
  geom$x2_ <- NULL
  geom$y1_ <- NULL
  geom$y2_ <- NULL

  class(geom) <- c("geom_polygon", "geom", "data.frame")
  geom
}
