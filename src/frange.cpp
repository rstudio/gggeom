#include <Rcpp.h>
using namespace Rcpp;

//' Efficient implementation of range.
//'
//' This is an efficient C++ implementation of range for numeric vectors:
//' it avoids S3 dispatch, and computes both min and max in a single pass
//' through the input.
//'
//' @param x A numeric vector.
//' @param finite If \code{TRUE} ignores missing values and infinities. Note
//'   that if the vector is empty, or only contains missing values,
//'   \code{frange} will return \code{c(Inf, -Inf)} because those are the
//'   identity values for \code{\link{min}} and \code{\link{max}} respectively.
//' @export
//' @examples
//' x <- runif(1e6)
//' system.time(range(x))
//' system.time(frange(x))
// [[Rcpp::export]]
NumericVector frange(const NumericVector& x, const bool finite = true) {
  RObject cache = x.attr("range");
  if (cache.sexp_type() == REALSXP) return as<NumericVector>(cache);

  NumericVector out(2);
  out[0] = INFINITY;
  out[1] = -INFINITY;

  int n = x.length();
  for(int i = 0; i < n; ++i) {
    if (!finite && R_IsNA(x[i])) {
      out[0] = NA_REAL;
      out[1] = NA_REAL;
      return out;
    }

    // If finite, skip infinite values
    if (finite && (x[i] == INFINITY || x[i] == -INFINITY)) continue;

    if (x[i] < out[0]) out[0] = x[i];
    if (x[i] > out[1]) out[1] = x[i];
  }

  return out;
}
