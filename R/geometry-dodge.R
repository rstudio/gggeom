#' Dodge objects on next to one another.
#'
#' Currently only implemented for rects.
#'
#' @inheritParams geometry_rotate
#' @export
#' @examples
#' bar_ex %>% plot()
#' bar_ex %>% geometry_stack() %>% plot()
#' bar_ex %>% geometry_dodge() %>% plot()
geometry_dodge <- function(geom) {
  UseMethod("geometry_dodge")
}

#' @export
geometry_dodge.geom_rect <- function(geom) {
  dodged <- geom %>%
    dplyr::group_by_(~x1_, ~x2_, add = TRUE) %>%
    dplyr::do({
      n <- nrow(.) + 1
      breaks <- seq(.$x1_[1], .$x2_[1], length = n)

      data <- .
      data$x1_ <- breaks[-n]
      data$x2_ <- breaks[-1]
      data
    })

  # Restore old grouping
  old_groups <- dplyr::groups(geom)
  if (!is.null(old_groups)) {
    dodged <- dplyr::group_by_(dodged, .dots = old_groups)
  } else {
    dodged <- dplyr::ungroup(dodged)
  }

  class(dodged) <- class(geom)
  dodged
}

dodge <- function(geom, width) {
  n <- length(unique(df$group))
  if (n == 1) return(df)

  if (!all(c("xmin", "xmax") %in% names(df))) {
    df$xmin <- df$x
    df$xmax <- df$x
  }

  d_width <- max(df$xmax - df$xmin)
  diff <- width - d_width

  # Have a new group index from 1 to number of groups.
  # This might be needed if the group numbers in this set don't include all of 1:n
  groupidx <- match(df$group, sort(unique(df$group)))

  # Find the center for each group, then use that to calculate xmin and xmax
  df$x <- df$x + width * ((groupidx - 0.5) / n - .5)
  df$xmin <- df$x - d_width / n / 2
  df$xmax <- df$x + d_width / n / 2

  df
}
