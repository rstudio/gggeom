#' Smooth vectors.
#'
#' @export
#' @examples
#' x <- runif(1e4, 0, 4 * pi)
#' y <- sin(x) + runif(1e4, -0.5, 0.5)
#' plot(x, y)
#' smu <- vector_smooth(x, y, span = 0.25)
#' lines(smu$x, smu$y, type = "l", col = "red", lwd = 2)
#' x_grid <- seq(0, 4 * pi, length = 100)
#' lines(x_grid, sin(x_grid), type = "l", col = "blue", lwd = 2)
#' lines(x_grid, predict(loess(y ~ x), data.frame(x = x_grid)), col = "green", lwd = 2)
vector_smooth <- function(x, z, span = 0.1, n_bin = 1000, n_smooth = 100,
                          weight = NULL) {
  # Need to remove missing values

  # Bin into n bins
  range <- frange(x)
  width <- (range[2] - range[1]) / n_bin

  if (length(weight) == 0) {
    weight <- numeric()
  }

  binned <- condense_moments(x, origin = range[1] - 1e-8 * width, width = width,
    pad = FALSE, right_closed = TRUE, z = z, w = weight, moments = 2)
  `as.data.frame!`(binned, length(binned[[1]]))

  # Smooth, weighted by standard error of means
  se <- binned$sd_ / sqrt(binned$count_)
  se[is.na(se)] <- Inf
  h <- (range[2] - range[1]) * span

  x_out <- seq(range[1], range[2], length = n_smooth)
  out <- smooth_robust(binned$x_, binned$mean_, w = 1 / se, x_out,
    h = h)

  data.frame(
    x = restore(x, x_out),
    y = out
  )
}
