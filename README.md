# ggcomp

[![Build Status](https://travis-ci.org/rstudio/ggcomp.png?branch=master)](https://travis-ci.org/rstudio/ggcomp)

ggcomp provides a suite of efficient summary and computation packages that are useful for visualisation. In some sense, it's the next iteration of the [bigvis](https://github.com/hadley/bigvis), but it focusses only on the computation. [ggvis](https://github.com/rstudio/ggvis) will eventually use ggcomp for all computation.

## Installation

ggcomp is not currently available on CRAN, but you can install it from github with:

```R
# install.packages("devtools")
install_github("hadley/dplyr")
```

Note that since ggcomp makes extensive use of Rcpp for high-performance computations, you'll need a development environment with a C++ compiler.

## Design considerations

Many computations are composed of three pieces. For example, `compute_bin()` is made up of:

* `compute_bin(df, ~x)`: an interface that takes a complete object 
  (e.g. a data frame) and what variables to use. It has methods for
  data frames, grouped data frames and (in ggvis) ggvis objects.

* `compute_bin_vec(df$x)`: as interface that works with individual vectors.
  It potentially has methods for numeric, Date, POSIXct etc, although for
  binning we can do everything in one function (see `restore()` for details
  of approach).

* `param_bin(range(df$x))`: a way to generate default parameters from data.
  (Not exported). It has methods for NULL, numeric, Date and POSIXct, most
  of which are wrappers around the numeric method.

Both `compute_bin()` and `compute_bin_vec()` return a data frame. The data frame always has the same columns, and they are of consistent type (`x_`, `xmin_` and `xmax_` are the same type as the variable being binned, `count_` and `width_` are always numeric). This makes `compute_bin()` a stable foundation for other functions.
