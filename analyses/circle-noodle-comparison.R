library(tidyverse)
library(sf)

# build some shapes -------------------------------------------------------

# a circle
pt <- st_point(x = c(0, 0))

radius <- 30
circle <- st_buffer(pt, radius)
circle_linestring <- st_cast(circle, "MULTILINESTRING")

# something more abstract
nc <- st_read(system.file("shape/nc.shp", package="sf"))[4, ]
st_area(nc)
plot(nc$geometry)


# convert each to a multilinestring ---------------------------------------
circle_linestring <- st_cast(circle, "MULTILINESTRING")
nc_linestring <- st_cast(nc, "MULTILINESTRING")

# sample from each polygons -----------------------------------------------
circle_samps <- st_sample(circle, size = 1000)
nc_samps <- st_sample(nc, size = 1000)


# calculate distance to edge ----------------------------------------------
circle_dist_to_edge <-
  st_distance(circle_samps, circle_linestring)[,1] %>% 
  sort()

nc_dist_to_edge <- 
  st_distance(nc_samps, nc_linestring)[, 1] %>% 
  sort() %>% 
  as.numeric()


# create cumulative distribution function ---------------------------------
circle_cdf <- ecdf(circle_dist_to_edge)
circle_pr_X_farther_from_edge <- 1 - circle_cdf(circle_dist_to_edge)

nc_cdf <- ecdf(nc_dist_to_edge)
nc_pr_X_farther_from_edge <- 1 - nc_cdf(nc_dist_to_edge)


# some plots --------------------------------------------------------------
# geometry plots
par(mfrow = c(1, 2))
plot(circle)
plot(circle_samps, add = TRUE, pch = 19)

plot(nc$geometry)
plot(nc_samps, add = TRUE, pch = 19)
dev.off()

# cdf plots
par(mfrow = c(1, 2))
plot(circle_dist_to_edge, circle_pr_X_farther_from_edge)
plot(nc_dist_to_edge, nc_pr_X_farther_from_edge)
dev.off()

# theoretical relationships plots
par(mfrow = c(1, 2))
circle_prop_rad <- circle_dist_to_edge / max(circle_dist_to_edge)
plot(circle_dist_to_edge, circle_pr_X_farther_from_edge)
lines(circle_dist_to_edge, y = (circle_prop_rad - 1)^2, col = "red", lwd = 3)

nc_prop_rad <- as.numeric(nc_dist_to_edge / max(nc_dist_to_edge))
plot(nc_dist_to_edge, nc_pr_X_farther_from_edge)
lines(nc_dist_to_edge, y = (nc_prop_rad - 1)^2, col = "red", lwd = 3)
# dev.off()

# fit some models ---------------------------------------------------------

circle_fm <- nls(circle_pr_X_farther_from_edge ~ ((circle_dist_to_edge - max_rad) / max_rad) ^ 2, start = list(max_rad = 25))

nc_fm <- nls(nc_pr_X_farther_from_edge ~ ((nc_dist_to_edge - max_rad) / max_rad) ^ 2, start = list(max_rad = 5000))
lines(nc_dist_to_edge, y = ((nc_dist_to_edge / coef(nc_fm)) - 1)^2, col = "blue", lwd = 3)


# original SDC calculations -----------------------------------------------

# buffer_dist <- 0:30
# 
# buffered_geoms <- lapply(buffer_dist, FUN = function(x) st_buffer(circle, -x))
# props <- lapply(buffered_geoms, FUN = function(x) 1 - (st_area(x) / st_area(circle))) %>% do.call("c", .)
# 
# lines(buffer_dist, props, col = "blue", lwd = 3)
