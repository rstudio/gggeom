context("stack")

test_that("uses height of bars", {
  rects <- data.frame(x = c(1, 1), y = c(1, 3), width = 1, height = 1) %>%
    render_tile(~x, ~y)

  stacked <- rects %>% geometry_stack()

  expect_equal(stacked$y1, c(0, 1))
  expect_equal(stacked$y2, c(1, 2))

})
