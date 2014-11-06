#include <Rcpp.h>
using namespace Rcpp;

// Convert a list of numeric vectors into a single numeric vector where
// each original vector is followed by an NA
// [[Rcpp::export]]
NumericVector ungroupNA(ListOf<NumericVector> x) {
  int n = x.size();

  // Figure out total size needed
  int n_total = 0;
  for (int i = 0; i < n; ++i) {
    n_total += x[i].size();
  }
  n_total += n;

  NumericVector out(n_total);
  int k = 0;
  for (int i = 0; i < n; ++i) {
    NumericVector xi = x[i];
    int ni = xi.size();

    for (int j = 0; j < ni; ++j, ++k) {
      out[k] = xi[j];
    }

    out[k++] = NA_REAL;
  }

  return out;
}
