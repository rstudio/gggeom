#include <Rcpp.h>

class Heap {

public:
  std::vector<double> value;
  // Need lookup in two directions:
  //   addition order -> current position (used for updating)
  std::vector<int> position;
  //   current position -> addition order (used when popping)
  std::vector<int> original;

  int n;

    Heap(int n_): n(n_) {
      value = std::vector<double>(n, INFINITY);
      position = std::vector<int>(n);
      original = std::vector<int>(n);
      for (int i = 0; i < n; ++i) {
        position[i] = i;
        original[i] = i;
      }
    }

  template <class Vector>
    Heap(Vector x) {
      // Inefficient: O(n) algorithm is available
      // http://en.wikipedia.org/wiki/Binary_heap#Building_a_heap

      n = 0;
      int m = x.size();
      value = std::vector<double>(0);
      position = std::vector<int>(0);
      original = std::vector<int>(0);

      for (int i = 0; i < m; ++i) {
        insert(x[i]);
      }
    }

  int insert(double x) {
    value.resize(n + 1);
    position.resize(n + 1);
    original.resize(n + 1);

    value[n] = x;
    position[n] = n;
    original[n] = n;
    bubble_up(n);

    return n++;
  }

  void update(int i, double new_x) {
    double pos = position[i];

    if (pos >= n) Rcpp::stop("pos >= n");

    double old = value[pos];
    if (old == new_x) return;

    value[pos] = new_x;
    if (new_x > old) { // increase
      // Same principle as pop - move to bottom of subheap then bubble up.
      sift_down(pos);
    } else { // decrease
      bubble_up(pos);
    }

  }

  std::pair<int,double> pop() {
    std::pair<int, double> out = std::make_pair(original[0], value[0]);

    n--;
    value[0] = NAN;
    swap_el(0, n);
    sift_down(0);

    return out;
  }

  void swap_el(int a, int b) {
    std::swap(value[a], value[b]);

    int pos_a = original[a], pos_b = original[b];
    std::swap(original[a], original[b]);
    std::swap(position[pos_a], position[pos_b]);
  }

  void sift_down(int i) {
    int l = left(i), r = right(i);
    if (needs_swap(i, l) && needs_swap(i, r)) {
      if (value[l] < value[r]) {
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
    return value[child] < value[parent];
  }

  bool bubble_up(int i) {
    if (i == 0) return false;

    int j = parent(i);
    if (value[i] < value[j]) {
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

  Rcpp::List asList() const {
    Rcpp::List out = Rcpp::List::create(
      Rcpp::_["value"] = Rcpp::NumericVector(value.begin(), value.end()),
      Rcpp::_["position"] = Rcpp::IntegerVector(position.begin(), position.end()),
      Rcpp::_["n"] = n
    );
    out.attr("class") = "heap";

    return out;
  }
};


inline std::ostream& operator<<(std::ostream& os, Heap h) {
  os << "V [";
  int last = h.n - 1;
  for (int i = 0; i < h.n; ++i) {
    os << h.value[i];
    if (i != last)
      os << ", ";
  }
  os << "]\nO [";

  last = h.position.size() - 1;
  for (int i = 0; i < h.original.size(); ++i) {
    os << h.original[i];
    if (i != last)
      os << ", ";
  }
  os << "]\nP [";

  for (int i = 0; i < h.position.size(); ++i) {
    os << h.position[i];
    if (i != last)
      os << ", ";
  }
  os << "]";

  return os;
}

