skyline <- function(...) {
  mat <- do.call(rbind, list(...))

  sk <- buildSkyline(mat[, 1], mat[, 2], mat[, 3])

  `as.data.frame!`(sk, length(sk[[1]]))
  class(sk) <- c("geom_skyline", "data.frame")
  sk
}

#' @export
plot.geom_skyline <- function(x, y, ...) {
  plot_init(x$x, x$h)
  lines(x$x, x$h, type = "s", lwd = 2)
  points(x$x, x$h, pch = 20, col = "red", cex = 2)

  invisible()
}
