#' Render 3d observations as contours.
#'
#' @inheritParams render_point
#' @param x,y,z 3d location of each point
#' @param nbreaks,breaks Either the number of breaks between the smallest and
#'   largest z values, or the positions of the breaks.
#' @return A path geometry.
#' @export
#' @examples
#' n <- 50
#' waves <- expand.grid(
#'    x = seq(-pi, pi, length = n),
#'    y = seq(-pi, pi, length = n)
#' )
#' r <- sqrt(waves$x ^ 2 + waves$y ^ 2)
#' waves$ripple <- cos(r ^ 2) * exp(-r / 6)
#' waves$hill <- exp(1 - r)
#'
#' waves %>% render_contour(~x, ~y, ~hill) %>% plot()
#' waves %>% render_contour(~x, ~y, ~ripple) %>% plot()
#'
#' # Show only where function crosses 0
#' waves %>% render_contour(~x, ~y, ~ripple, breaks = 0) %>% plot()
render_contour <- function(data, x, y, z, nbreaks = 10, breaks = NULL) {
  x <- eval_vector(data, x)
  y <- eval_vector(data, y)
  z <- eval_vector(data, z)
  contour_lines(x, y, z, nbreaks = nbreaks, breaks = breaks)
}

contour_lines <- function(x, y, z, nbreaks = 10, breaks = NULL) {
  if (is.null(breaks)) {
    z_rng <- range(z, na.rm = TRUE)
    breaks <- seq(z_rng[1], z_rng[2], length = nbreaks)
  }

  # Convert x, y, z vectors into matrix of unique values
  x0 <- sort(unique(x))
  x_ind <- match(x, x0)
  y0 <- sort(unique(y))
  y_ind <- match(y, y0)

  z_grid <- matrix(NA_real_, length(x0), length(y0))
  z_grid[cbind(x_ind, y_ind)] <- z

  cl <- contourLines(x = x0, y = y0, z = z_grid, levels = breaks)

  if (length(cl) == 0) {
    stop("Failed to generate contours", call. = FALSE)
  }

  out <- list(
    x_ = coords(pluck(cl, "x")),
    y_ = coords(pluck(cl, "y")),
    z_ = pluck(cl, "level", numeric(1))
  )
  `as.data.frame!`(out, length(cl))
  class(out) <- c("geom_path", "geom", "data.frame")
  out
}
