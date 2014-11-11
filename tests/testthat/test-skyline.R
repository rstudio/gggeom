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

test_that("small building eclipsed by tall building", {
  out1 <- skyline(c(3, 4, 1), c(1, 5, 2))
  out2 <- skyline(c(1, 5, 2), c(3, 4, 1))

  expect_equal(out1, out2)
  expect_equal(out1$x, c(1, 5))
  expect_equal(out1$h, c(2, 0))
})

test_that("narrow-tall building in middle of long-short building", {
  out1 <- skyline(c(2, 4, 2), c(1, 5, 1))
  out2 <- skyline(c(1, 5, 1), c(2, 4, 2))

  expect_equal(out1, out2)
  expect_equal(out1$h, c(1, 2, 1, 0))
  expect_equal(out1$x, c(1, 2, 4, 5))
})

test_that("taller builder overlapping start of existing", {
  out1 <- skyline(c(1, 3, 1), c(0, 2, 2))
  out2 <- skyline(c(0, 2, 2), c(1, 3, 1))

  expect_equal(out1, out2)
  expect_equal(out1$h, c(2, 1, 0))
  expect_equal(out1$x, c(0, 2, 3))
})

test_that("taller builder overlapping end of existing", {
  out1 <- skyline(c(1, 3, 1), c(2, 4, 2))
  out2 <- skyline(c(2, 4, 2), c(1, 3, 1))

  expect_equal(out1, out2)
  expect_equal(out1$h, c(1, 2, 0))
  expect_equal(out1$x, c(1, 2, 4))
})
