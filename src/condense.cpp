#include <Rcpp.h>
#include "BinnedVector.hpp"
#include "condensers.hpp"
using namespace Rcpp;

template<typename Condenser>
List condense(const NumericVector& x, double origin, double width,
              const NumericVector& z, const NumericVector& w,
              const Condenser& condenser) {

  BinnedVector group(x, width, origin);
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
  List out(n_condensers + 1);

  NumericVector bins(n_bins);
  for (int i = 0; i < n_bins; ++i) {
    bins[i] = group.unbin(i);
  }
  out[0] = bins;

  for (int j = 0; j < n_condensers; ++j) {
    NumericVector condensed(n_bins);
    for (int i = 0; i < n_bins; ++i) {
      condensed[i] = condensers[i].compute(j);
    }
    out[j + 1] = condensed;
  }

  // Name output columns
  CharacterVector out_cols(n_condensers + 1);
  out_cols[0] = "x";
  for (int j = 0; j < n_condensers; ++j) {
    out_cols[j + 1] = condenser.name(j);
  }
  out.attr("names") = out_cols;

  return out;
}

// [[Rcpp::export]]
List condense_count(const NumericVector& x, double origin, double width,
                    const NumericVector& w) {
  return condense(x, origin, width, NumericVector::create(), w, SumCondenser(0));
}

// [[Rcpp::export]]
List condense_sum(const NumericVector& x, double origin, double width,
                  const NumericVector& z, const NumericVector& w) {
  return condense(x, origin, width, z, w, SumCondenser(1));
}

// [[Rcpp::export]]
List condense_moments(const NumericVector& x, double origin, double width,
                      const NumericVector& z, const NumericVector& w,
                      int moments) {
  return condense(x, origin, width, z, w, MomentCondenser(moments));
}

// [[Rcpp::export]]
List condense_median(const NumericVector& x, double origin, double width,
                     const NumericVector& z, const NumericVector& w) {
  return condense(x, origin, width, z, w, MedianCondenser());
}
