#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double resolution_numeric(NumericVector x, bool zero = true) {
  int n = x.size();
  if (n <= 1) return NAN;

  std::vector<double> y(x.begin(), x.end());
  if (zero) y.push_back(0);
  std::sort(y.begin(), y.end());

  double min = INFINITY;
  for (int i = 1; i < n; ++i) {
    double dist = y[i] - y[i - 1];
    if (dist == 0) continue;
    if (dist < min) {
      min = dist;
    }
  }

  return min;
}
