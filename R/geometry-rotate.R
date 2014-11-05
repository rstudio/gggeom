#' Rotate.
#'
#' Rotates 90 degrees by switching x and y positions.
#'
#' @param geom A geometry data frame.
#' @export
#' @examples
#' scatter_ex %>% plot()
#' scatter_ex %>% geometry_rotate() %>% plot()
#'
#' histogram_ex %>% plot()
#' histogram_ex %>% geometry_rotate() %>% plot()
geometry_rotate <- function(geom) UseMethod("geometry_rotate")

#' @export
geometry_rotate.geom <- function(geom) {
  switch_cols(geom, "x_", "y_")
}

#' @export
geometry_rotate.geom_ribbon <- function(geom) {
  stop("Can't rotate ribbons", call. = FALSE)
}

#' @export
geometry_rotate.geom_rect <- function(geom) {
  geom <- switch_cols(geom, "x1_", "y1_")
  geom <- switch_cols(geom, "x2_", "y2_")
  geom
}

#' @export
geometry_rotate.geom_segment <- function(geom) {
  geometry_rotate.geom_rect(geom)
}

switch_cols <- function(df, a, b) {
  nms <- names(df)

  pos <- match(c(a, b), nms)
  if (any(is.na(pos))) {
    stop("Couldn't find column ", c(a, b)[is.na(pos)], call. = FALSE)
  }
  nms[pos] <- nms[rev(pos)]

  names(df) <- nms
  df
}

