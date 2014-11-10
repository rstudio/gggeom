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

