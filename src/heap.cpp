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
NumericVector heap_update_sort(NumericVector x) {
  int n = x.size();
  Heap h = Heap(n);
  for (int i = 0; i < n; ++i) {
    h.insert(0);
  }
  for (int i = 0; i < n; ++i) {
    h.update(i, x[i]);
  }

  NumericVector out(n);
  for (int i = 0; i < n; ++i) {
    out[i] = h.pop().second;
  }
  return out;
}
