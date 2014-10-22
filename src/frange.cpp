#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector frange_(const NumericVector& x, const bool finite = true) {
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
