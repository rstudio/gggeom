# gggeom

[![Build Status](https://travis-ci.org/rstudio/gggeom.png?branch=master)](https://travis-ci.org/rstudio/gggeom)

gggeom provides data structures for describing the geometry primitives that underly all visualisations.

## Installation

gggeom is not currently available on CRAN, but you can install it from github with:

```R
# install.packages("devtools")
install_github("rstudio/gggeom")
```

Note that since gggeom makes extensive use of Rcpp for high-performance computations, you'll need a development environment with a C++ compiler.
