#' Stack objects on top of one another.
#'
#' @inheritParams geometry_rotate
#' @param dir Direction in which to stack. "x" or "y" for rects,
#'   only "y" for smooths, "r" or "theta" for arcs.
#' @export
#' @examples
#' bar_ex %>% plot()
#' bar_ex %>% geometry_stack() %>% plot()
#'
#' bar_ex %>% geometry_rotate() %>% plot()
#' bar_ex %>% geometry_rotate() %>% geometry_stack("x") %>% plot()
#'
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
    stack(geom, "x", "y1_")
  } else {
    stack(geom, "y", "x1_")
  }
}

#' @export
geometry_stack.geom_ribbon <- function(geom, dir) {
  geometry_stack.geom_rect(geom, "y")
}

#' @export
geometry_stack.geom_arc <- function(geom, dir = c("theta", "r")) {
  dir <- match.arg(dir)

  if (dir == "theta") {
    stack(geom, "theta", c("x_", "y_"))
  } else {
    stack(geom, "r", c("x_", "y_"))
  }
}


stack <- function(data, stack_var, group_vars) {
  lower_stack <- paste0(stack_var, "1_")
  upper_stack <- paste0(stack_var, "2_")

  if (any(data[[lower_stack]] != 0)) {
    warning("Stacking rects with non-zero ", lower_stack, " is not well defined",
      call. = FALSE)
  }

  new_vars <- list(
    lazyeval::interp(~(cumsum(x)), x = as.name(upper_stack)),
    lazyeval::interp(~(stats::lag(x, default = 0)), x = as.name(upper_stack))
  )
  names(new_vars) <- c(upper_stack, lower_stack)

  stacked <- data %>%
    dplyr::group_by_(.dots = group_vars, add = TRUE) %>%
    dplyr::mutate_(.dots = new_vars)

  # Restore old grouping
  old_groups <- dplyr::groups(data)
  if (!is.null(old_groups)) {
    stacked <- dplyr::group_by_(.dots = old_groups)
  }

  class(stacked) <- class(data)
  stacked
}
