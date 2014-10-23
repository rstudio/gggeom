#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List count_lgl(LogicalVector x, NumericVector w) {
  double n_t = 0, n_f = 0, n_na = 0;

  int n = x.size();
  bool has_w = w.size() != 0;

  for (int i = 0; i < n; ++i) {
    int xi = x[i];
    double wi = has_w ? w[i] : 1;

    if (xi == 1) {
      n_t += wi;
    } else if (xi == 0) {
      n_f += wi;
    } else {
      n_na += wi;
    }
  }

  return List::create(
    _["x_"] = LogicalVector::create(true, false, NA_LOGICAL),
    _["count_"] = NumericVector::create(n_t, n_f, n_na)
  );
}

// [[Rcpp::export]]
List count_factor(IntegerVector x, NumericVector w) {
  CharacterVector levels = as<CharacterVector>(x.attr("levels"));
  int m = levels.size();
  NumericVector counts(m + 1);

  int n = x.size();
  bool has_w = w.size() != 0;

  for (int i = 0; i < n; ++i) {
    int xi = x[i];
    if (xi < 0) {
      xi = 0;
    }
    counts[xi] += has_w ? w[i] : 1;
  }

  if (!counts[0]) {
    // No missing values, so need to drop off first (0) element of counts
    NumericVector new_counts(m);
    for (int i = 0; i < m; i++) {
      new_counts[i] = counts[i + 1];
    }

    IntegerVector x = seq_len(m);
    x.attr("levels") = levels;
    x.attr("class") = "factor";

    return List::create(
      _["x_"] = x,
      _["count_"] = new_counts
    );

  } else {
    // Has missing values, and not included in levels, so need to add to levels
    CharacterVector new_levels(m + 1);
    new_levels[0] = NA_STRING;
    for (int i = 0; i < m; i++) {
      new_levels[i + 1] = levels[i];
    }

    IntegerVector x = seq_len(m + 1);
    x.attr("levels") = new_levels;
    x.attr("class") = "factor";

    return List::create(
      _["x_"] = x,
      _["count_"] = counts
    );
  }

}
