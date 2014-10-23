context("weightedQuantile")
set.seed(1014)

test_that("weighted quantile agress with quantile when weight constant", {
  p <- seq(0, 1, length = 11)

  # Quantiles fall on values
  x <- 1:100
  expect_equal(
    weightedQuantile(x, integer(), p),
    unname(quantile(x, p))
  )

  # Quantiles don't fall on values
  x <- 1:99
  expect_equal(
    weightedQuantile(x, integer(), p),
    unname(quantile(x, p))
  )
})

# test_that("weighed.quantile agrees with quantile on repeated vector", {
#   samples <- replicate(20, runif(100), simplify = FALSE)
#   w <- rep(1:2, 50)
#   samples_ex <- lapply(samples, rep, times = w)
#
#   quant <- sapply(samples_ex, quantile, probs = 0.5, names = FALSE)
#   wquant <- sapply(samples, weightedQuantile, w = w, probs = 0.5)
#
#   expect_equal(quant, wquant)
# })
#
