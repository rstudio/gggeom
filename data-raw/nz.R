nz_raw <- maps::map("nz", plot = FALSE)
group <- cumsum(is.na(nz_raw$x)) + 1

x <- nz_raw$x %>% split(group) %>% lapply(function(x) x[!is.na(x)]) %>% unname
y <- nz_raw$y %>% split(group) %>% lapply(function(x) x[!is.na(x)]) %>% unname
island <- gsub(".Island ", "", nz_raw$names)

nz <- dplyr::data_frame(x_ = x, y_ = y, island)
class(nz) <- c("geom_polygon", "geom", class(nz))
plot(nz)
devtools::use_data(nz, overwrite = TRUE)
