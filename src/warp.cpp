#include <Rcpp.h>
using namespace Rcpp;


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

  Point transform(Function f) {
    ListOf<NumericVector> out = f(x, y);
    return Point(out[0][0], out[1][0]);
  }

  double dist_to_line(Point a, Point b) {
    return point_line_dist(x, y, a.x, a.y, b.x, b.y);
  }
};

void warp(Point start, Point end, Function f, double threshold,
          std::vector<double>* pOutX, std::vector<double>* pOutY) {
  Point mid = start.average(end),
        mid_t = mid.transform(f),
        start_t = start.transform(f),
        end_t = end.transform(f);


  double dist = mid_t.dist_to_line(start_t, end_t);
  if (dist < threshold)
    return;

  warp(start, mid, f, threshold, pOutX, pOutY);
  pOutX->push_back(mid_t.x);
  pOutY->push_back(mid_t.y);
  warp(mid, end, f, threshold, pOutX, pOutY);
}

// [[Rcpp::export]]
List warp(NumericVector x, NumericVector y, Function f, double threshold = 0.01) {
  if (x.size() != y.size())
    stop("x and y must be same length");

  int n = x.size();
  std::vector<double> out_x, out_y;
  out_x.reserve(n);
  out_y.reserve(n);

  for (int i = 0; i < (n - 1); ++i) {
    Point start = Point(x[i], y[i]), end = Point(x[i + 1], y[i + 1]);
    Point start_t = start.transform(f);
    out_x.push_back(start_t.x);
    out_y.push_back(start_t.y);

    warp(start, end, f, threshold, &out_x, &out_y);
  }

  Point end = Point(x[n - 1], y[n - 1]).transform(f);
  out_x.push_back(end.x);
  out_y.push_back(end.y);

  return List::create(_["x"] = out_x, _["y"] = out_y);
}

