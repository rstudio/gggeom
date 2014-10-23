context("count")

test_that("empty factor levels preserved", {
  x <- factor(c("A", "A", "C"), levels = LETTERS[1:3])
  out <- compute_count_vec(x)
  expect_equal(out$x_, factor(LETTERS[1:3]))
  expect_equal(out$count_, c(2, 0, 1))
})

test_that("order of levels preserved", {
  x <- factor(c("A", "A", "B"), levels = c("B", "A"))
  out <- compute_count_vec(x)
  expect_equal(out$x_, factor(LETTERS[2:1], levels = LETTERS[2:1]))
  expect_equal(out$count_, c(1, 2))
})

test_that("missing values in factors are counted", {
  x <- factor(c(NA, NA, "a", "b"))
  out1 <- compute_count_vec(x) # NA in vector
  out2 <- compute_count_vec(factor(x, levels = c(NA, "a", "b"))) # NA in levels

  expect_equal(out1$count_, c(2, 1, 1))
  expect_equal(out2$count_, c(2, 1, 1))
})

test_that("dates preserved", {
  x <- as.Date("2013-07-01") + 1:5
  out <- compute_count_vec(x)
  expect_is(out$x_, "Date")
})

test_that("times preserved", {
  x <- Sys.time() + 1:5
  out <- compute_count_vec(x)
  expect_is(out$x_, "POSIXct")
})

