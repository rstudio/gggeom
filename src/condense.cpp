#include <Rcpp.h>
#include "BinnedVector.hpp"
#include "condensers.hpp"
using namespace Rcpp;

template<typename Condenser>
List condense(const NumericVector& x, double origin, double width,
              bool pad, bool right_closed,
              const NumericVector& z, const NumericVector& w,
              const Condenser& condenser) {

  BinnedVector group(x, width, origin, pad, right_closed);
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
  List out(n_condensers + 4);
  CharacterVector out_cols(n_condensers + 4);

  // First four columns giving bin info
  NumericVector x_(n_bins), xmin_(n_bins), xmax_(n_bins), width_(n_bins);
  for (int i = 0; i < n_bins; ++i) {
    double x = group.unbin(i);
    x_[i] = x;
    xmin_[i] = x - width / 2;
    xmax_[i] = x + width / 2;
    width_[i] = width;
  }

  out[0] = x_;     out_cols[0] = "x_";
  out[1] = xmin_; out_cols[1] = "xmin_";
  out[2] = xmax_; out_cols[2] = "xmax_";
  out[3] = width_; out_cols[3] = "width_";

  // Last columns give summaries from condensers
  for (int j = 0; j < n_condensers; ++j) {
    NumericVector condensed(n_bins);
    for (int i = 0; i < n_bins; ++i) {
      condensed[i] = condensers[i].compute(j);
    }
    out[j + 4] = condensed;
    out_cols[j + 4] = condenser.name(j);
  }

  out.attr("names") = out_cols;
  return out;
}

// [[Rcpp::export]]
List condense_count(const NumericVector& x, double origin, double width,
                    bool pad, bool right_closed,
                    const NumericVector& w) {
  return condense(x, origin, width, pad, right_closed, NumericVector::create(),
    w, SumCondenser(0));
}

// [[Rcpp::export]]
List condense_sum(const NumericVector& x, double origin, double width,
                  bool pad, bool right_closed,
                  const NumericVector& z, const NumericVector& w) {
  return condense(x, origin, width, pad, right_closed, z, w, SumCondenser(1));
}

// [[Rcpp::export]]
List condense_moments(const NumericVector& x, double origin, double width,
                      bool pad, bool right_closed,
                      const NumericVector& z, const NumericVector& w,
                      int moments) {
  return condense(x, origin, width, pad, right_closed, z, w,
    MomentCondenser(moments));
}

// [[Rcpp::export]]
List condense_median(const NumericVector& x, double origin, double width,
                     bool pad, bool right_closed,
                     const NumericVector& z, const NumericVector& w) {
  return condense(x, origin, width, pad, right_closed, z, w, MedianCondenser());
}
