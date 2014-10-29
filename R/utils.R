notify_guess <- function(x, explanation = NULL) {
  msg <- paste0(
    "Guessing ", deparse(substitute(x)), " = ", format(x, digits = 3),
    if (!is.null(explanation)) paste0(" # ", explanation)
  )
  message(msg)
}

#' Pipe operator
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL


is_numeric <- function(x) {
  typeof(x) %in% c("double", "integer") && !is.factor(x)
}

`%||%` <- function(x, y) if (is.null(x)) y else x


eval_vector <- function(data, x) {
  if (is.atomic(x)) return(rep(x, nrow(data)))

  eval(x[[2]], data, environment(x))
}
