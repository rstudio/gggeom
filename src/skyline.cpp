#include <Rcpp.h>
using namespace Rcpp;

typedef std::map<double,double>::iterator itEdge;

class Skyline {
  std::map<double, double> edges;

public:
  void add_building(double x1, double x2, double h) {
    if (x1 >= x2) return;
    if (h == 0) return;

    // Rcout << "[" << x1 << "," << x2 << "]\n";

    // Add right edge - we do this before adding left edge because otherwise
    // it interferes with the calculation
    // The upper bound finds the first edge >= x1
    itEdge right = edges.lower_bound(x2);
    if (right == edges.end() || right == edges.begin()) {
      // Last edge always goes back down to zero
      right = edges.insert(std::make_pair(x2, 0)).first;
    } else {
      if (right->first == x2) {
        // New edge matches existing edge: don't need to do anything
      } else { // right->first > x2
        // If we're taller than the previous edge, this is where we have to
        // come back down
        itEdge prev(right); prev--;

        if (h > prev->second) {
          right = edges.insert(std::make_pair(x2, prev->second)).first;
        }
      }
    }
    // print();

    // Find or insert the edge at the left of the building.
    // The upper bound finds the first edge > x1
    itEdge left = edges.upper_bound(x1);
    if (left == edges.begin()) {
      // Left is the first element, so we need to create a new edge before it.
      // Since it's the first edge, it must have height h
      left = edges.insert(std::make_pair(x1, h)).first;
    } else {
      // Find the first edge <= x1
      left--;
      if (left->first == x1) {
        // New edge is matches existing edge, so check height
        if (h > left->second) {
          left->second = h;
        }
      } else { // left->first < x1
        // Add new edge if it's taller than the previous edge
        if (h > left->second) {
          left = edges.insert(std::make_pair(x1, h)).first;
        }
      }
    }

    // Iterate from left to right adjusting heights and removing duplicates
    double prev_height = -INFINITY;
    itEdge cur(left);
    while(cur != right) {
      // Height can never be lower than the height of this building
      if (cur->second < h) {
        cur->second = h;
      }
      // Remove it if it's the same height as the previous
      if (cur->second == prev_height) {
        itEdge old = cur;
        cur++;
        edges.erase(old);
      } else {
        prev_height = cur->second;
        cur++;
      }
    }
    // print();

  }

  void find_height(double x1, double x2) {

  }

  void print() {
    int m = edges.size();
    NumericVector out_x(m), out_h(m);

    for(itEdge it = edges.begin(); it != edges.end(); ++it) {
      Rcout << it->first << ": " << it->second << "\n";
    }

    Rcout << "\n";
  }

  List as_list() {
    int m = edges.size();
    NumericVector out_x(m), out_h(m);

    itEdge it; int i;
    for(it = edges.begin(), i = 0; it != edges.end(); ++it, ++i) {
      out_x[i] = it->first;
      out_h[i] = it->second;
    }

    return List::create(
        _["x"] = out_x,
        _["h"] = out_h
    );
  }
};

// [[Rcpp::export]]
List buildSkyline(NumericVector x1, NumericVector x2, NumericVector y) {
  int n = x1.size();

  // Sort all endpoints:
  Skyline skyline;
  for (int i = 0; i < n; ++i) {
    skyline.add_building(x1[i], x2[i], y[i]);
  }

  return skyline.as_list();
}
