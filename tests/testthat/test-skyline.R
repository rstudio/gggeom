context("Skyline")

test_that("one building skyline is idempotent", {
  out <- skyline(c(1, 2, 1))
  expect_equal(out$x, c(1, 2))
  expect_equal(out$h, c(1, 0))
})

test_that("non-overlapping building adds two edges", {
  out1 <- skyline(c(1, 2, 1), c(3, 4, 1))
  expect_equal(out1$x, 1:4)
  expect_equal(out1$h, c(1, 0, 1, 0))

  out2 <- skyline(c(3, 4, 1), c(1, 2, 1))
  expect_equal(out2$x, 1:4)
  expect_equal(out2$h, c(1, 0, 1, 0))
})
