#include <Rcpp.h>
#include "BinnedVector.hpp"
#include "condensers.hpp"
using namespace Rcpp;

template<typename Condenser>
List condense(const NumericVector& x, double origin, double binwidth,
              const NumericVector& z, const NumericVector& w,
              const Condenser& condenser) {

  BinnedVector group(x, origin, binwidth);
  int n_obs = group.size();
  int n_bins = group.nbins();

  const NumericVector& weight_ = (w.size() > 0) ? w :
    rep(NumericVector::create(1), n_obs);
  const NumericVector& z_ = (z.size() > 0) ? z :
    rep(NumericVector::create(1), n_obs);

  // Push values into condensers
  std::vector<Condenser> condensers(n_bins, condenser);
  for(int i = 0; i < n_obs; ++i) {
    int bin = group.bin_i(i);
    // Rcout << "i: " << i << " bin: " << bin << "\n";
    condensers[bin].push(z_[i], weight_[i]);
  }

  // Compute values from condensers and determine bins
  int n_condensers = condenser.size();
  NumericVector bin(n_bins);
  NumericMatrix out(n_bins, n_condensers);

  for (int i = 0; i < n_bins; ++i) {
    bin[i] = group.unbin(i);

    for (int j = 0; j < n_condensers; ++j) {
      out(i, j) = condensers[i].compute(j);
    }
  }

  // Name output columns
  CharacterVector out_cols(n_condensers);
  for (int j = 0; j < n_condensers; ++j) {
    out_cols[j] = condenser.name(j);
  }
  out.attr("dimnames") = List::create(CharacterVector::create(), out_cols);

  return List::create(bin, out);
}

// [[Rcpp::export]]
List condense_count(const NumericVector& x, double origin, double binwidth,
                    const NumericVector& z, const NumericVector& w) {
  return condense(x, origin, binwidth, z, w, SumCondenser(0));
}

// [[Rcpp::export]]
List condense_sum(const NumericVector& x, double origin, double binwidth,
                  const NumericVector& z, const NumericVector& w) {
  return condense(x, origin, binwidth, z, w, SumCondenser(1));
}

// [[Rcpp::export]]
List condense_moments(const NumericVector& x, double origin, double binwidth,
                      const NumericVector& z, const NumericVector& w,
                      int moments) {
  return condense(x, origin, binwidth, z, w, MomentCondenser(moments));
}

// [[Rcpp::export]]
List condense_median(const NumericVector& x, double origin, double binwidth,
                      const NumericVector& z, const NumericVector& w) {
  return condense(x, origin, binwidth, z, w, MedianCondenser());
}
