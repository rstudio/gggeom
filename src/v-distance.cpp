#include <Rcpp.h>
using namespace Rcpp;

struct point {
  double x, y, area;
  point* prev;
  point* next;
};

inline double compute_area(double x1, double y1, double x2, double y2,
                           double x3, double y3) {
  return fabs((x1 - x3) * (y2 - y1) - (x1 - x2) * (y3 - y1)) / 2;
}

inline double compute_area(const point& p) {
  if (p.prev == NULL || p.next == NULL)
    return INFINITY;

  return compute_area(p.x, p.y, p.prev->x, p.prev->y, p.next->x, p.next->y);
}

bool compare_points_p(point* a, point* b) {
  return a->area > b->area;
}

// Utility function for debugging:
// Loop over all points; for each one, print the previous, current, and next point.
void print_points_p(std::vector<point*>& points_p) {
  for (int i = 0; i < points_p.size(); i++) {
    point* r = points_p[i];
    std::cout << "i=" << i <<
      "prev: " << r->prev->x << "," << r->prev->y << ":" << r->prev->area << ", " <<
      "cur: "  << r->x << "," << r->y << ":" << r->area << ", " <<
      "next: " << r->next->x << "," << r->next->y << ":" << r->next->area << ", " << "\n";
  }
  std::cout << '\n';
}

// [[Rcpp::export]]
NumericVector v_distance(const NumericVector& x, const NumericVector& y) {
  int n = x.size();

  std::vector<point> points(n);

  // Fill in data for all the points
  for(int i = 0; i < n; i++) {
    points[i].x = x[i];
    points[i].y = y[i];

    if (i != 0)
      points[i].prev = &points[i-1];
    else
      points[i].prev = NULL;

    if (i != n-1)
      points[i].next = &points[i+1];
    else
      points[i].next = NULL;

    points[i].area = compute_area(points[i]);
  }

  // Create vector of point pointers, for the min-heap. We don't use the points
  // directly because the heap operations would move points around in the vector
  // and the prev/next pointers would no longer point to the correct item.
  std::vector<point*> points_p(n-2);
  for (int i = 0; i < n-2; i++) {
    points_p[i] = &points[i+1];
  }

  // print_points_p(points_p);

  // Convert to min-heap
  std::make_heap(points_p.begin(), points_p.end(), compare_points_p);

  // Remove point with min area, recompute areas of neighbors, and update
  // their neighbor pointers.
  double max_area = 0;    // Largest area removed so far
  while (points_p.size() > 0) {
    // print_points_p(points_p);

    // Remove the point from the prev/next linked list, and recompute areas of
    // neighbors.
    point* cur = points_p[0];
    point* prev = cur->prev;
    point* next = cur->next;

    prev->next = next;
    next->prev = prev;

    // Keep track of the largest area triangle so far.
    // If the area of the current point is smaller than the largest previous point (which
    // can happen due to area recomputations) just the previous max area.
    if (cur->area < max_area)
      cur->area = max_area;
    else
      max_area = cur->area;

    next->area = compute_area(*next);
    prev->area = compute_area(*prev);
    // TODO: After recomputing the areas, need to remove and reinsert them
    // in the heap so that the heap remains properly ordered.

    // Remove the current point from the heap
    std::pop_heap(points_p.begin(), points_p.end(), compare_points_p);
    points_p.pop_back();
  }

  // Pull out the areas and return them
  std::vector<double> areas(n);
  for (int i = 0; i < n; i++) {
    areas[i] = points[i].area;
  }
  return NumericVector(areas.begin(), areas.end());
}
