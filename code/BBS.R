library(rgdal)
## bbs routes https://earthworks.stanford.edu/catalog/stanford-vy474dv5024  1966-1998
bbs_rou <- readOGR(file.path("data/BBS/BBS_route_shp/bbsrtsl020.shp")) #st_read("park_raster/nps_boundary/nps_boundary.shp")
park_bound <- readRDS("data/out/park_bound.rds")
pb <- subset(park_bound, UNIT_CODE %in% c("ACAD", "MABI", "MORR"))

plot(pb, border="red", lwd=3, xlim = c(-75, -68), ylim = c(38, 46))
plot(bbs_rou, add = T)


