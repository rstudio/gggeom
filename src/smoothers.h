double smoothLinear(const std::vector<double>& x,
                    const std::vector<double>& y,
                    const std::vector<double>& w);

double smoothRobust(const std::vector<double>& x,
                    const std::vector<double>& y,
                    const std::vector<double>& w,
                    int iterations = 3);

class LinearSmoother {
  public:
      double compute(const std::vector<double>& x,
                     const std::vector<double>& y,
                     const std::vector<double>& w) const {
      return smoothLinear(x, y, w);
    }
};

class RobustSmoother {
  int iterations;

  public:
    RobustSmoother (int iterations_ = 3) : iterations(iterations_) {
      if (iterations < 0) Rcpp::stop("Invalid iterations");
    }

    double compute(const std::vector<double>& x,
                   const std::vector<double>& y,
                   const std::vector<double>& w) const {
      return smoothRobust(x, y, w, iterations);
    }
};
