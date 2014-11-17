#include <Rcpp.h>
#include "heap.h"
using namespace Rcpp;


// [[Rcpp::export]]
List make_heap(NumericVector x) {
  return Heap(x).asList();
}

// [[Rcpp::export]]
NumericVector heap_sort(NumericVector x) {
  Heap h = Heap(x);

  int n = x.size();
  NumericVector out(n);
  for (int i = 0; i < n; ++i) {
    out[i] = h.pop().second;
  }

  return out;
}

// [[Rcpp::export]]
List heap_update_sort(NumericVector x) {
  int n = x.size();
  Heap h = Heap(n);
  for (int i = 0; i < n; ++i) {
    h.update(i, x[i]);
  }

  NumericVector sort(n), order1(n);
  for (int i = 0; i < n; ++i) {
    std::pair<int, double> top = h.pop();
    order1[i] = top.first + 1;
    sort[i] = top.second;
  }

  return List::create(
      _["sort"] = sort,
      _["order1"] = order1);
}
