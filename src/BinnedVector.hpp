#include <Rcpp.h>
using namespace Rcpp;

// Wrapper for numeric vector that makes it easy figure to out which
// bin each observation belongs to.
class BinnedVector {
    // This should probably be a const NumericVector&, but that doesn't work
    // with modules currently
    NumericVector x_;
    double width_;
    double origin_;
  public:
    BinnedVector(NumericVector x, double width, double origin = 0)
       : x_(x), width_(width), origin_(origin) {
    }

    int bin_i(int i) const {
      return bin(x_[i]);
    }

    int bin(double x) const {
      if (ISNAN(x) || x == INFINITY || x == -INFINITY) return 0;
      if (x < origin_) return 0;

      return (x - origin_) / width_ + 1;
    }

    double unbin(int bin) const {
      if (bin == 0) return(NA_REAL);
      return (bin - 1) * width_ + origin_ + width_ / 2;
    }

    int nbins() const;

    int size() const {
      return x_.size();
    }

    double origin() const {
      return origin_;
    }

    double width() const {
      return width_;
    }

};
