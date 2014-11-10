#include <Rcpp.h>
#include "geometry.h"
using namespace Rcpp;

void warp(Point next, Point next_t, Point (f)(Point), double threshold,
          std::vector<Point>* pRaw, std::vector<Point>* pTrans) {

  if (pRaw->empty()) {
    pRaw->push_back(next);
    pTrans->push_back(next_t);
    return;
  }

  Point last = pRaw->back(), last_t = pTrans->back();
  Point mid = last.average(next), mid_t = f(mid);

  double dist = mid_t.dist_to_line(last_t, next_t);
  if (dist < threshold)
    return;

  warp(mid, mid_t, f, threshold, pRaw, pTrans);
  pRaw->push_back(mid);
  pTrans->push_back(mid_t);
  warp(next, next_t, f, threshold, pRaw, pTrans);
}

List warp(NumericVector x, NumericVector y, Point (f)(Point), double threshold = 0.01) {
  if (x.size() != y.size())
    stop("x and y must be same length");

  int n = x.size();
  std::vector<Point> raw, trans;

  for (int i = 0; i < n; ++i) {
    Point next = Point(x[i], y[i]), next_t = f(next);
    warp(next, next_t, f, threshold, &raw, &trans);
  }
  // Always include last point
  Point end = f(Point(x[n - 1], y[n - 1]));
  trans.push_back(end);

  int m = trans.size();
  NumericVector out_x(m), out_y(m);
  for (int i = 0; i < m; ++i) {
    out_x[i] = trans[i].x;
    out_y[i] = trans[i].y;
  }
  return List::create(_["x"] = out_x, _["y"] = out_y);
}

Point transform_polar(Point input) {
  return Point(input.y * sin(input.x), input.y * cos(input.x));
}
Point transform_identity(Point input) {
  return input;
}

// [[Rcpp::export]]
List warp(NumericVector x, NumericVector y, std::string f, double threshold = 0.01) {
  if (f == "polar") {
    return warp(x, y, transform_polar, threshold);
  } else if (f == "identity") {
    return warp(x, y, transform_identity, threshold);
  }

  stop("Unknown transformation type");
  return List::create();
}
