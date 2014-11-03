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

void compute_tolerance_rec(const std::vector<double>& x,
                           const std::vector<double>& y,
                           std::vector<double>* out,
                           int first, int last) {

  int n = last - first + 1;
  if (n == 2)
    return;

  // Rcout << first << "-" << last << "\n";
  if (n == 3) {
    int mid = first + 1; // or last - 1
    (*out)[mid] = point_line_dist(
      x[mid], y[mid],
      x[first], y[first],
      x[last], y[last]
    );
  } else if (n > 3) {
    // Find most distant point
    double max_dist = -INFINITY;
    int furthest = 0;
    for (int i = first + 1; i < last; ++i) {
      double dist = point_line_dist(x[i], y[i], x[first], y[first], x[last], y[last]);
      if (dist > max_dist) {
        furthest = i;
        max_dist = dist;
      }
    }

    (*out)[furthest] = max_dist;

    // Recurse
    compute_tolerance_rec(x, y, out, first, furthest);
    compute_tolerance_rec(x, y, out, furthest, last);
  }

  return;
}

// [[Rcpp::export]]
std::vector<double> compute_tolerance(std::vector<double> x, std::vector<double> y) {
  int n = x.size();
  std::vector<double> out(n);

  out[0] = INFINITY;
  out[n - 1] = INFINITY;
  compute_tolerance_rec(x, y, &out, 0, n - 1);

  return out;
}
