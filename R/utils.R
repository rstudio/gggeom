notify_guess <- function(x, explanation = NULL) {
  msg <- paste0(
    "Guessing ", deparse(substitute(x)), " = ", format(x, digits = 3),
    if (!is.null(explanation)) paste0(" # ", explanation)
  )
  message(msg)
}

# Like as.numeric, except that as.numeric(NULL) returns numeric(0), whereas
# as_numeric(NULL) returns NULL.
as_numeric <- function(x) {
  if (is.null(x)) NULL
  else as.numeric(x)
}
