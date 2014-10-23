#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector weightedQuantile(NumericVector x, IntegerVector w,
                               NumericVector probs) {
  int n = x.size(), m = probs.size();
  double sum = 0;
  std::vector<std::pair<double, int> > values(n);

  if (w.size() == 0) {
    for(int i = 0; i < n; ++i) {
      values[i] = std::make_pair(x[i], 1);
      sum++;
    }
  } else if (w.size() == n) {
    for(int i = 0; i < n; ++i) {
      int wi = w[i];
      if (wi <= 0) continue;
      values[i] = std::make_pair(x[i], wi);
      sum += wi;
    }
  } else {
    stop("w is not the same size as x");
  }
  std::sort(values.begin(), values.end());

  std::vector<std::pair<double, int> >::iterator v_it = values.begin(),
    v_end = values.end();
  NumericVector quantiles(m);
  double cur_pos = 0;
  double next_q = 1 + probs[0] * (sum - 1);

  for(int q = 0; v_it != v_end; ++v_it) {
    cur_pos += v_it->second;

    if (cur_pos >= next_q) {
      if (cur_pos == next_q) {
        // Quantile is exactly on data value
        quantiles[q] = v_it->first;
      } else if (cur_pos > next_q) {
        // Just missed it - interpolation between this value and last
        double alpha = next_q - floor(next_q);
        quantiles[q] = (1 - alpha) * (v_it - 1)->first + alpha * v_it->first;
      }
      // Advance to next quantile
      q++;
      next_q = 1 + probs[q] * (sum - 1);
    }

    // Found all the quantiles we're looking for
    if (q >= m) break;
  }

  return quantiles;
}
// Q[i](p) = (1 - g) x[j] + g x[j+1]
// j = floor(np + m)
// g = np + m - j
//
// For type 7:
//   m = 1 - p =>
//   j = floor(1 + (n - 1) * p)
//   g = (np + 1 - p) - floor(1 + (n - 1) * p)

