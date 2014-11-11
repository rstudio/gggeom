#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double abs_max_(const NumericVector& x, const bool finite = true) {
  double max = -INFINITY;

  int n = x.length();
  for(int i = 0; i < n; ++i) {
    double xi = x[i];
    if (!finite) {
      if (isnan(xi)) return NA_REAL;
      if (xi == INFINITY) return INFINITY;
      if (xi == -INFINITY) return INFINITY;
    }

    if (xi < 0) xi = -xi;
    if (xi > max) max = xi;
  }

  return max;
}
