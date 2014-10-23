#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List count_lgl(LogicalVector x, NumericVector w) {
  double n_t = 0, n_f = 0, n_na = 0;

  int n = x.size();
  bool has_w = w.size() != 0;

  for (int i = 0; i < n; ++i) {
    int xi = x[i];
    double wi = has_w ? w[i] : 1;

    if (xi == 1) {
      n_t += wi;
    } else if (xi == 0) {
      n_f += wi;
    } else {
      n_na += wi;
    }
  }

  return List::create(
    _["x_"] = LogicalVector::create(true, false, NA_LOGICAL),
    _["count_"] = NumericVector::create(n_t, n_f, n_na)
  );
}

