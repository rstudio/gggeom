#include <Rcpp.h>

class Heap {
  std::vector<double> value;
  std::vector<int> position;

  int n;

  public:

    Heap(int n_): n(n_) {
      value = std::vector<double>(n);
      position = std::vector<int>(n);
    }

  template <class Vector>
    Heap(Vector x) {
      // Inefficient: O(n) algorithm is available
      // http://en.wikipedia.org/wiki/Binary_heap#Building_a_heap

      n = 0;
      int m = x.size();
      value.reserve(m);
      position.reserve(m);

      for (int i = 0; i < m; ++i) {
        insert(x[i]);
      }
    }

  int insert(double x) {
    value.resize(n + 1);
    position.resize(n + 1);

    value[n] = x;
    position[n] = n;
    bubble_up(n);

    return n++;
  }

  void update(int i, double x) {
    value[i] = x;

    // Either needs to go up or down
    if (needs_swap(parent(i), i)) {
      bubble_up(i);
    } else {
      sift_down(i);
    }
  }

  std::pair<int,double> pop() {
    std::pair<int, double> out = std::make_pair(position[0], value[0]);

    n--;
    value[0] = INFINITY;
    swap_el(0, n);
    sift_down(0);

    return out;
  }

  void swap_el(int a, int b) {
    std::swap(value[a], value[b]);
    std::swap(position[a], position[b]);
  }

  void sift_down(int i) {
    int l = left(i), r = right(i);
    if (needs_swap(i, l) && needs_swap(i, r)) {
      if (value[l] > value[r]) {
        swap_el(i, l);
        sift_down(l);
      } else {
        swap_el(i, r);
        sift_down(r);
      }
    } else if (needs_swap(i, l)) {
      swap_el(i, l);
      sift_down(l);
    } else if (needs_swap(i, r)) {
      swap_el(i, r);
      sift_down(r);
    }
  }

  bool needs_swap(int parent, int child) {
    if (child >= n) return false;
    return value[parent] < value[child];
  }

  bool bubble_up(int i) {
    if (i == 0) return false;

    int j = parent(i);
    if (value[i] > value[j]) {
      swap_el(i, j);
    }
    return bubble_up(j);
  }

  // Helpers for navigating around the tree
  inline int left(int i) const {
    return 2 * i + 1;
  }
  inline int right(int i) const {
    return 2 * i + 2;
  }
  inline int parent(int i) const {
    return floor((i - 1) / 2);
  }

  bool empty() const {
    return n == 0;
  }

  List asList() const {
    List out = Rcpp::List::create(
      _["value"] = Rcpp::NumericVector(value.begin(), value.end()),
      _["position"] = Rcpp::IntegerVector(position.begin(), position.end()),
      _["n"] = n
    );
    out.attr("class") = "heap";

    return out;
  }
};
