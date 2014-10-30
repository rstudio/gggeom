library(dplyr)
data("mpg", package = "ggplot2")

scatter_ex <- render_point(mpg, ~cty, ~hwy)
scatter_ex %>% plot()
scatter_ex %>% geometry_jitter() %>% plot(col = "red")
scatter_ex %>% geometry_rotate() %>% plot()
scatter_ex %>% geometry_flip() %>% plot()
devtools::use_data(scatter_ex)

bar_ex <- render_bar(mpg, ~cyl, 1)
bar_ex %>% plot()
bar_ex %>% geometry_stack() %>% plot()
bar_ex %>% geometry_stack() %>% geometry_rotate() %>% plot()
bar_ex %>% geometry_rotate() %>% geometry_stack("x") %>% plot()

devtools::use_data(bar_ex)


histogram_ex <- mpg %>%
  compute_bin(~hwy, width = 1) %>%
  render_rect(~xmin_, 0, ~xmax_, ~count_)
histogram_ex %>% plot()
histogram_ex %>% geometry_flip("y") %>% plot()
devtools::use_data(histogram_ex)
