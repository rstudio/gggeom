#include <Rcpp.h>
#include "heap.h"
using namespace Rcpp;

inline double compute_area(double x1, double y1, double x2, double y2,
                           double x3, double y3) {
  return fabs((x1 - x3) * (y2 - y1) - (x1 - x2) * (y3 - y1)) / 2;
}

// [[Rcpp::export]]
NumericVector vw_distance(const NumericVector& x, const NumericVector& y) {
  int n = x.size();

  Heap h(0);
  std::vector<int> prev(n);
  std::vector<int> next(n);

  // Fill in data for all the points
  h.insert(INFINITY);
  prev[0] = -1;
  next[0] = 1;

  for(int i = 1; i < (n - 1); i++) {
    double area = compute_area(x[i - 1], y[i - 1], x[i], y[i], x[i + 1], y[i + 1]);
    h.insert(area);
    prev[i] = i - 1;
    next[i] = i + 1;
  }

  h.insert(INFINITY);
  prev[n - 1] = n - 2;
  next[n - 1] = -1;

  NumericVector area(n, NAN);
  double max_area = -INFINITY;

  // Remove point with minimum area, and recompute neighbors' areas, repeating
  // until the heap is empty
  while(!h.empty()) {
    std::pair<int, double> top = h.pop();

    // Forces area to always increase so that points are added in
    // correct order
    max_area = fmax(max_area, top.second);

    int idx = top.first;
    area[idx] = max_area;

    // Update neighbouring points
    int next_idx = next[idx];
    int prev_idx = prev[idx];

    if (next_idx == -1 || prev_idx == -1)
      continue;

    next[prev[idx]] = next_idx;
    prev[next[idx]] = prev_idx;
    prev[idx] = -1;
    next[idx] = -1;

    // Recalculate area of neighbors (unless they're first or last)
    if (prev_idx != 0 && prev_idx != -1) {
      double area = compute_area(x[prev[prev_idx]], y[prev[prev_idx]],
                                 x[prev_idx], y[prev_idx],
                                 x[next_idx], y[next_idx]);
      // Rcout << "P " << prev[prev_idx] << " -- " << prev_idx << " -- " << next_idx << "\n";
      h.update(prev_idx, area);
    }

    if (next_idx != n - 1 && next_idx != -1 && prev_idx != -1) {
      double area = compute_area(x[prev_idx], y[prev_idx],
                                 x[next_idx], y[next_idx],
                                 x[next[next_idx]], next[next_idx]);
      // Rcout << "N " << prev_idx << " -- " << next_idx << " -- " << next[next_idx] << "\n";
      h.update(next_idx, area);
    }
  }

  return area;
}