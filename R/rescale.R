#' Scale stacked values.
#'
#' @inheritParams geometry_flip
#' @inheritParams geometry_stack
#' @param max Maximum value to scale to. Defaults to 1, except when scaling
#'   "theta", where it defaults to 2 * pi.
#' @export
#' @examples
#' bar_ex %>% geometry_stack() %>% plot()
#' bar_ex %>% geometry_stack() %>% geometry_scale() %>% plot()
#'
#' pies <- render_arc(mtcars, ~vs, ~am, 0, 0.1, 0, ~mpg / max(mpg) * 2 / pi)
#' pies %>% geometry_stack() %>% plot()
#' pies %>% geometry_stack() %>% geometry_scale() %>% plot()
#'
#' disks <- render_arc(mtcars, ~vs, ~am, 0, 0.05, 0, 2 * pi)
#' disks %>% geometry_stack("r") %>% geometry_scale("r", 0.45) %>% plot()
geometry_scale <- function(geom, dir = c("y", "x"), max = 1) {
  UseMethod("geometry_scale")
}

#' @export
geometry_scale.geom_rect <- function(geom, dir = c("y", "x"), max = 1) {
  dir <- match.arg(dir)

  if (dir == "x") {
    scale(geom, "x", "y1_", max_value = max)
  } else {
    scale(geom, "y", "x1_", max_value = max)
  }
}

#' @export
geometry_scale.geom_ribbon <- function(geom, dir, max = 1) {
  geometry_scale.geom_rect(geom, "y", max = max)
}

#' @export
geometry_scale.geom_arc <- function(geom, dir = c("theta", "r"), max = NULL) {
  dir <- match.arg(dir)

  if (dir == "theta") {
    scale(geom, "theta", c("x_", "y_"), max %||% 2 * pi)
  } else {
    scale(geom, "r", c("x_", "y_"), max %||% 1)
  }
}

scale <- function(data, scale_var, group_vars, max_value = 1) {
  lower <- paste0(scale_var, "1_")
  upper <- paste0(scale_var, "2_")
  vals <- list(x1 = as.name(lower), x2 = as.name(upper), y = max_value)

  # This is slightly inefficient because we're calculating the abs_max twice
  new_vars <- list(
    lazyeval::interp(~ x1 / abs_max_(x2) * y, .values = vals),
    lazyeval::interp(~ x2 / abs_max_(x2) * y, .values = vals)
  )
  names(new_vars) <- c(lower, upper)

  scaled <- data %>%
    dplyr::group_by_(.dots = group_vars, add = TRUE) %>%
    dplyr::mutate_(.dots = new_vars)

  # Restore old grouping
  old_groups <- dplyr::groups(data)
  if (!is.null(old_groups)) {
    scaled <- dplyr::group_by_(.dots = old_groups)
  }

  class(scaled) <- class(data)
  scaled
}
