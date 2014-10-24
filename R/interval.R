#' Convert points into intervals.
#'
#' This compute function is often used in conjunction with
#' \code{\link{compute_count}}, when used on data with a continuous x variable.
#' By default, the computed width will be equal to the resolution of the data,
#' or, in other words the smallest difference between two values in the data.
#'
#' @param data Dataset-like object. Built-in methods for data frames,
#'   grouped data frames and ggvis visualisations.
#' @param dir Direction, i.e. \code{"x"} or \code{"y"}.
#' @param width Width of intervals. If \code{NULL} (the default), the
#'   width is set to the resolution of the data.
#' @param align Where does the existing point fall on the new interval?
#'   0 = left edge, 0.5 = center, 1 = right edge.
#' @seealso \code{\link{compute_bin}} For counting cases within ranges of
#'   a continuous variable.
#' @seealso \code{\link{compute_count}} For counting cases at specific values
#'   of a variable.
#' @export
#' @return The original data frame, with additional columns:
#'  \item{'dir'min_}{left boundary of bin}
#'  \item{'dir'max_}{right boundary of bin}
#'
#' @examples
#' mtcars %>% compute_count(~cyl) %>% compute_interval(width = 2)
#'
#' # cf. binned data
#' mtcars %>% compute_bin(~cyl, width = 2)
compute_interval <- function(data, dir = c("x", "y"), width = NULL, align = 0.5) {
  UseMethod("compute_interval")
}

#' @export
compute_interval.data.frame <- function(data, dir = c("x", "y"), width = NULL,
                                        align = 0.5) {
  dir <- match.arg(dir)

  val <- data[[paste0(dir, "_")]]
  width <- width %||% resolution(val)

  data[[paste0(dir, "min_")]] <- val - width * align
  data[[paste0(dir, "max_")]] <- val + width * (1 - align)

  data
}
