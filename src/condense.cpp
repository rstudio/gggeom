#include <Rcpp.h>
#include "BinnedVector.hpp"
#include "condensers.hpp"
using namespace Rcpp;

template<typename Condenser>
NumericMatrix condense(const NumericVector& x, double origin, double binwidth,
              const NumericVector& z, const NumericVector& w,
              const Condenser& condenser) {

  BinnedVector group(x, binwidth, origin);
  int n_obs = group.size();
  int n_bins = group.nbins();

  bool has_w = (w.size() > 0);
  bool has_z = (z.size() > 0);

  // Push values into condensers
  std::vector<Condenser> condensers(n_bins, condenser);
  for(int i = 0; i < n_obs; ++i) {
    int bin = group.bin_i(i);
    // Rcout << "i: " << i << " bin: " << bin << "\n";
    condensers[bin].push(has_z ? z[i] : 1, has_w ? w[i] : 1);
  }

  // Compute values from condensers and determine bins
  int n_condensers = condenser.size();
  NumericMatrix out(n_bins, n_condensers + 1);

  for (int i = 0; i < n_bins; ++i) {
    out(i, 0) = group.unbin(i);

    for (int j = 0; j < n_condensers; ++j) {
      out(i, j + 1) = condensers[i].compute(j);
    }
  }

  // Name output columns
  CharacterVector out_cols(n_condensers + 1);
  out_cols[0] = "x";
  for (int j = 0; j < n_condensers; ++j) {
    out_cols[j + 1] = condenser.name(j);
  }
  out.attr("dimnames") = List::create(CharacterVector::create(), out_cols);

  return out;
}

// [[Rcpp::export]]
NumericMatrix condense_count(const NumericVector& x, double origin, double binwidth,
                    const NumericVector& z, const NumericVector& w) {
  return condense(x, origin, binwidth, z, w, SumCondenser(0));
}

// [[Rcpp::export]]
NumericMatrix condense_sum(const NumericVector& x, double origin, double binwidth,
                  const NumericVector& z, const NumericVector& w) {
  return condense(x, origin, binwidth, z, w, SumCondenser(1));
}

// [[Rcpp::export]]
NumericMatrix condense_moments(const NumericVector& x, double origin, double binwidth,
                      const NumericVector& z, const NumericVector& w,
                      int moments) {
  return condense(x, origin, binwidth, z, w, MomentCondenser(moments));
}

// [[Rcpp::export]]
NumericMatrix condense_median(const NumericVector& x, double origin, double binwidth,
                     const NumericVector& z, const NumericVector& w) {
  return condense(x, origin, binwidth, z, w, MedianCondenser());
}
