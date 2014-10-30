# ggcomp

[![Build Status](https://travis-ci.org/rstudio/ggcomp.png?branch=master)](https://travis-ci.org/rstudio/ggcomp)

ggcomp provides a suite of efficient summary and computation packages that are useful for visualisation. In some sense, it's the next iteration of the [bigvis](https://github.com/hadley/bigvis), but it focusses only on the computation. [ggvis](https://github.com/rstudio/ggvis) will eventually use ggcomp for all computation.

## Installation

ggcomp is not currently available on CRAN, but you can install it from github with:

```R
# install.packages("devtools")
install_github("rstudio/ggcomp")
```

Note that since ggcomp makes extensive use of Rcpp for high-performance computations, you'll need a development environment with a C++ compiler.
