compute_quantile_vec <- function(x, w = NULL, probs = seq(0, 1, 0.25)) {
  if (is.null(w)) {
    quantile(x, probs, na.rm = FALSE, names = FALSE)
  } else {
    weightedQuantile(x, w, props)
  }
}
