#' Bin data along a continuous variable
#'
#' @param data Dataset-like object to bin. Built-in methods for data frames,
#'   grouped data frames and ggvis visualisations.
#' @param x_var,w_var Names of x and weight variables. The x variable must be
#'   continuous.
#' @inheritParams compute_bin_vec
# @seealso \code{\link{compute_count}} For counting cases at specific locations
#   of a continuous variable. This is useful when the variable is continuous
#   but the data is granular.
#' @return A data frame with columns:
#'  \item{count_}{the number of points}
#'  \item{x_}{mid-point of bin}
#'  \item{xmin_}{left boundary of bin}
#'  \item{xmax_}{right boundary of bin}
#'  \item{width_}{width of bin}
#' @export
#' @examples
#' mtcars %>% compute_bin(~mpg)
#' mtcars %>% compute_bin(~mpg, width = 10)
#' if (requireNamespace("dplyr", quietly = TRUE)) {
#'   mtcars %>% dplyr::group_by(cyl) %>% compute_bin(~mpg, width = 10)
#' }
#'
#' # Missing values get own bin
#' mtcars2 <- mtcars
#' mtcars2$mpg[sample(32, 5)] <- NA
#' mtcars2 %>% compute_bin(~mpg, width = 10)
compute_bin <- function(data, x_var, w_var = NULL, width = NULL,
                        center = NULL, boundary = NULL,
                        closed = c("right", "left"), pad = FALSE) {
  closed <- match.arg(closed)

  UseMethod("compute_bin")
}

#' @export
compute_bin.data.frame <- function(data, x_var, w_var = NULL, width = NULL,
                                   center = NULL, boundary = NULL,
                                   closed = c("right", "left"), pad = FALSE) {
  x_val <- eval_vector(data, x_var)
  w_val <- eval_vector(data, w_var)

  compute_bin_vec(
    x_val,
    w = w_val,
    width = width,
    center = center,
    boundary = boundary,
    closed = closed,
    pad = pad
  )
}

#' @export
compute_bin.grouped_df <- function(data, x_var, w_var = NULL, width = NULL,
                                   center = NULL, boundary = NULL,
                                   closed = c("right", "left"), pad = FALSE) {

  # We want to use the same boundary and width across groups, so calculate
  # bin params here.
  x_val <- eval_vector(data, x_var)
  params <- param_bin(frange(x_val), width = width, center = center,
    boundary = boundary, right_closed = identical(closed, "right"))

  dplyr::do(data, compute_bin(.,
    x_var = x_var,
    w_var = w_var,
    width = params$width,
    boundary = params$origin,
    closed = params$closed,
    pad = pad
  ))
}

globalVariables(".")
