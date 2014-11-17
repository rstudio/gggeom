context("heap")

test_that("heap sort sorts data", {
  x <- runif(1e5)
  expect_equal(heap_sort(x), sort(x))
})

test_that("updating heap sorts data", {
  x <- runif(1e5)
  # Remove duplicates - heap sort is not stable
  x <- x[!duplicated(x)]

  expect_equal(heap_update_sort(-x)$sort, sort(-x))
  expect_equal(heap_update_sort(x)$sort, sort(x))
  expect_equal(heap_update_sort(x)$order, order(x))
})
