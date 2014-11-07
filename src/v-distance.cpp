#include <Rcpp.h>
using namespace Rcpp;

inline double compute_area(double x1, double y1, double x2, double y2,
                           double x3, double y3) {
  return fabs((x1 - x3) * (y2 - y1) - (x1 - x2) * (y3 - y1)) / 2;
}


//' @export
// [[Rcpp::export]]
NumericVector v_distance(const NumericVector& x, const NumericVector& y) {
  int n = x.size();

  std::vector<double> area(n);
  std::vector<int> prev(n);
  std::vector<int> next(n);

  area.front() = INFINITY;
  prev.front() = -1;
  next.front() = 1;

  area.back() = INFINITY;
  prev.back() = n - 2;
  next.back() = -1;

  // Fill in data for all the points
  for(int i = 1; i < n-1; i++) {
    area[i] = compute_area(x[i-1], y[i-1], x[i], y[i], x[i+1], y[i+1]);
    prev[i] = i - 1;
    next[i] = i + 1;
  }

  // Remove point with minimum area, and recompute neighbors' areas, repeating
  // until only endpoints remain.
  double min_area;
  int min_idx;
  for (int n_points = n; n_points > 2; n_points--) {

    // Find the point with minimum area
    min_area = INFINITY;
    int idx = 0;
    while (idx != n-1) {
      if (area[idx] < min_area) {
        min_area = area[idx];
        min_idx = idx;
      }

      idx = next[idx];
    }

    // Remove the point with min_idx
    int next_idx = next[min_idx];
    int prev_idx = prev[min_idx];

    next[prev[min_idx]] = next_idx;
    prev[next[min_idx]] = prev_idx;
    prev[min_idx] = -1;
    next[min_idx] = -1;

    // Recalculate area of neighbors (unless they're first or last)
    if (prev_idx != 0) {
      int prev_prev_idx = prev[prev_idx];
      area[prev_idx] = compute_area(x[prev_prev_idx], y[prev_prev_idx],
                                    x[prev_idx], y[prev_idx],
                                    x[next_idx], y[next_idx]);
    }

    if (next_idx != n-1) {
      int next_next_idx = next[next_idx];
      area[next_idx] = compute_area(x[prev_idx], y[prev_idx],
                                    x[next_idx], y[next_idx],
                                    x[next_next_idx], y[next_next_idx]);
    }
  }

  return NumericVector(area.begin(), area.end());
}
