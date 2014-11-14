context("heap")

test_that("heap sort sorts data", {
  x <- runif(1e5)
  expect_equal(heap_sort(x), sort(x, decreasing = TRUE))
})
