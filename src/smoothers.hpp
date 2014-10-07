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
                   const std::vector<double>& w) {
      smoothLinear(x, y, w);
    }
};

class RobustSmoother {
  int interations = 3;

  public:
    smootherRobust (int iterations_) : iterations_(iterations) {
      if (i < iterations) stop("Invalid iterations");
    }

    double compute(const std::vector<double>& x,
                   const std::vector<double>& y,
                   const std::vector<double>& w) {
      smoothRobust(x, y, w, iterations);
    }
};
