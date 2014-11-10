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

class Point {
public:
  double x, y;

  Point(double x_, double y_) : x(x_), y(y_) {}

  Point average(Point other) {
    return Point((other.x + x) / 2, (other.y + y) / 2);
  }

  Point combine(Point other, double alpha) {
    return Point(
      alpha * y + (1 - alpha) * other.y,
      alpha * y + (1 - alpha) * other.y
    );
  }

  double dist_to_line(Point a, Point b) {
    return point_line_dist(x, y, a.x, a.y, b.x, b.y);
  }
};


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
