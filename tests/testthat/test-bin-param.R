context("bin_params()")

# Setting boundary/center ------------------------------------------------------

test_that("can't set both boundary and center", {
  expect_error(bin_params(c(0, 100), boundary = 0, center = 0), "Only one")
})

test_that("boundary sets edge of bin", {
  p <- bin_params(c(1, 30), boundary = 0)
  expect_equal(p$origin, 0)

  p <- bin_params(c(1, 30), boundary = 10)
  expect_equal(p$origin, 0)
})

test_that("center sets middle of bin", {
  p <- bin_params(c(0, 30), center = 0)
  expect_equal(p$origin, -0.5)

  p <- bin_params(c(0, 30), center = 10)
  expect_equal(p$origin, -0.5)
})

test_that("left-open needs extra bin", {
  p <- bin_params(0:1, boundary = 0, width = 1)
  expect_equal(p$origin, -1)

  p <- bin_params(0:1 + 1e-9, boundary = 0, width = 1)
  expect_equal(p$origin, -1)
})

# Other numeric types ----------------------------------------------------------

test_that("treats dates like numbers", {
  x <- as.Date("2013-06-01") + c(0, 30)

  p <- bin_params(x, boundary = x[[1]])
  expect_equal(p$origin, as.numeric(x[[1]]) - 1)
  expect_equal(p$width, 1)
})

test_that("treats times like numbers", {
  x <- as.POSIXct('2001-06-01 21:00', tz = 'America/New_York') + c(0, 30 * 24 * 60 * 60)

  p <- bin_params(x, boundary = x[[1]])
  expect_equal(p$origin, as.numeric(x[[1]]) - 24 * 60 * 60)
  expect_equal(p$width, 24 * 60 * 60)
})

# Guessing width ---------------------------------------------------------------

test_that("Automatic width", {
  # Approximately 30 bins, at round numbers -> width 1
  expect_equal(bin_params(c(0, 25.0))$width, 1)
  expect_equal(bin_params(c(1L, 25L))$width, 1)

  expect_equal(bin_params(c(0, 50))$width, 2)
  expect_equal(bin_params(c(1L, 50L))$width, 2)
})
