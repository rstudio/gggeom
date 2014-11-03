#include <Rcpp.h>
using namespace Rcpp;

// Squared distance between a point (x0, y0) and a line {(x1, y1), (x2, y2)}
// Adapted from http://mathworld.wolfram.com/Point-LineDistance2-Dimensional.html
inline double point_line_dist(double x0, double y0,
                       double x1, double y1,
                       double x2, double y2) {

  double x21 = x2 - x1;
  double x10 = x1 - x0;
  double y21 = y2 - y1;
  double y10 = y1 - y0;

  double num = x21 * y10 - x10 * y21;
  double den = x21 * x21 + y21 * y21;

  return (num * num) / den;
}

void dp_distance_rec(const NumericVector& x, const NumericVector& y,
                           int first, int last, NumericVector* pOut) {
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
  (*pOut)[furthest] = max_dist;

  // Recurse
  dp_distance_rec(x, y, first, furthest, pOut);
  dp_distance_rec(x, y, furthest, last, pOut);

  return;
}

// [[Rcpp::export]]
NumericVector dp_distance(const NumericVector& x, const NumericVector& y) {
  int n = x.size();
  NumericVector out(n);

  out[0] = INFINITY;
  out[n - 1] = INFINITY;
  dp_distance_rec(x, y, 0, n - 1, &out);

  return out;
}
