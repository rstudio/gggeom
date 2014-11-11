#' A simple class for managing lists of coordinates.
#'
#' gggeom assumes that every geometry can be represented with a single row
#' of a data frame. This poses a challenge for path and polygon geometries
#' which can be of arbitrary length. To solve this problem, gggeom uses a
#' list-column, where each component of the list is of the same type.
#'
#' @export
#' @param x A list to label as containing coordinates.
#' @keywords internal
#' @examples
#' make_spiral <- function(turns, r, n = 200) {
#'   t_grid <- seq(0, turns * 2 * pi, length = n)
#'   r_grid <- seq(0, r, length = n)
#'   df <- data.frame(x = r_grid * sin(t_grid), y = r_grid * cos(t_grid))
#'   render_path(df, ~x, ~y)
#' }
#' s1 <- make_spiral(6, 1)
#' s2 <- make_spiral(-4, 10)
#'
#' s1
#' str(s1)
#' s1$x_
#'
#' spirals <- rbind(s1, s2)
#' range(spirals$x_)
#' spirals %>% plot(col = c("red", "black"))
coords <- function(x) {
  stopifnot(is.list(x))
  class(x) <- "coords"
  x
}

#' @export
range.coords <- function(x, ...) {
  range(unlist(x), ...)
}

#' @export
`[.coords` <- function(x, ...) {
  structure(NextMethod(), class = "coords")
}

#' @export
format.coords <- function(x, ...) {
  vapply(x, obj_type, character(1))
}

#' @export
print.coords <- function(x, ...) {
  print(format(x, ...), quote = FALSE)
}

obj_type <- function(x) {
  if (!is.object(x)) {
    paste0("<", type_sum(x), if (!is.array(x)) paste0("[", length(x), "]"), ">")
  } else if (!isS4(x)) {
    paste0("<S3:", paste0(class(x), collapse = ", "), ">")
  } else {
    paste0("<S4:", paste0(methods::is(x), collapse = ", "), ">")
  }
}

type_sum <- function(x) UseMethod("type_sum")
type_sum.numeric <- function(x) "dbl"
type_sum.integer <- function(x) "int"
type_sum.logical <- function(x) "lgl"
type_sum.character <- function(x) "chr"
type_sum.factor <- function(x) "fctr"
type_sum.POSIXt <- function(x) "time"
type_sum.Date <- function(x) "date"
type_sum.matrix <- function(x) {
  paste0(NextMethod(), "[", paste0(dim(x), collapse = ","), "]")
}
type_sum.array <- type_sum.matrix
type_sum.default <- function(x) unname(abbreviate(class(x)[1], 4))
