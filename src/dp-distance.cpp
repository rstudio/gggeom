#include <Rcpp.h>
#include "geometry.h"
using namespace Rcpp;

void dp_distance_rec(const NumericVector& x, const NumericVector& y,
                     int first, int last, double max, NumericVector* pOut) {
  // Rcout << first << "-" << last << "\n";
  int n = last - first + 1;
  if (n <= 2)
    return;

  // Find point furthest from line defined by first, last
  double max_dist = -INFINITY;
  int furthest = 0;
  for (int i = first + 1; i < last; ++i) {
    double dist = point_line_dist(x[i], y[i], x[first], y[first], x[last], y[last]);
    if (dist > max_dist) {
      furthest = i;
      max_dist = dist;
    }
  }

  // Ensure that distance always decreases as you recurse
  if (max_dist > max) {
    max_dist = max;
  }
  (*pOut)[furthest] = pow(max_dist, 0.5);

  // Recurse
  dp_distance_rec(x, y, first, furthest, max_dist, pOut);
  dp_distance_rec(x, y, furthest, last, max_dist, pOut);

  return;
}

// [[Rcpp::export]]
NumericVector dp_distance(const NumericVector& x, const NumericVector& y) {
  int n = x.size();
  NumericVector out(n);

  out[0] = INFINITY;
  out[n - 1] = INFINITY;
  dp_distance_rec(x, y, 0, n - 1, INFINITY, &out);

  return out;
}
