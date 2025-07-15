#? *********************************************************************************
#? ------------------------------   get_park_plot_limits.R   ------------------------------
#? *********************************************************************************
#! Code to get park boundaries for plotring purposes, speciealy for the ROVA parks
#
#! Input ----------------------------------------------
#           - data/out/NETNtib.rds :
#
#! Output ----------------------------------------------
#           - data/out/park_plot_lims.rds : tible with limits to plot each park
#
# detach packages and clear workspace
freshr::freshr()
#
#! Load packages ---------------------------------------
#library(conflicted)
library(tidyverse)
library(glue)
library(sp)
library(sf)
library(reshape2)

#! Make functions --------------------------------------
colanmes <- colnmaes <- colnames
lenght <- length
`%!in%` <- Negate(`%in%`)

#! Import data -----------------------------------------
## file paths
BIRD_SITE_PATH <- "data/out/NETNtib.rds"
PARK_SITE_PATH <- "data/src/key_park.rds"

## read files
bird_sit <- read_rds(file = BIRD_SITE_PATH)
parks    <- read_rds(file = PARK_SITE_PATH) 

#! get coordinates from the bird plots ------------------------------
parks <- parks %>% 
  dplyr::select(parks) %>% 
  distinct() %>% 
  filter(parks %!in% c("ACAD", "ELRO", "SAIR")) %>%
  pull()

parks <- sort(parks) 

for(ii in 1:length(bird_sit$points)){
  coord_loop <- 
    bird_sit$points[ii][[1]] %>% 
      select(Admin_Unit_Code,
            NETN_Point_Name,
            Point_Name,
            Latitude,
            Longitude,
            UTM_ZONE)
  if(ii == 1) {
    park_site <- coord_loop
  } else {
    park_site <- rbind(park_site, coord_loop)
  }
}

# get coordinates from bird plots
bird_sit_coord <- park_site %>% 
  select(Point_Name,
          Latitude,
          Longitude, 
          UTM_ZONE) %>% 
  rename(bird_sit = Point_Name,
          lat = Latitude,
          lon = Longitude,
          UTMZone = UTM_ZONE) 

#? convert all bird coordinates to UTM to get distances in meters --------------------
xy <- data.frame(ID = 1:nrow(bird_sit_coord), 
                  X = bird_sit_coord$lon, 
                  Y = bird_sit_coord$lat)
coordinates(xy) <- c("X", "Y")
proj4string(xy) <- CRS("+proj=longlat +datum=WGS84")

# write_rds(xy, file = "data/out/bird_site_coords.rds")

for(ii in 1:nrow(xy)){
  band <- as.numeric(bird_sit_coord$UTMZone[ii])
  y <- spTransform(xy[ii,], CRS(glue("+proj=utm +zone={band} +datum=WGS84 +units=m")))
  y2 <- as(y, "SpatialPoints") %>% 
        as_tibble()
  y2$UTMZone <- band
  
  if(ii == 1) {
    utm_bir <- y2
  } else {
    utm_bir <- rbind(utm_bir, y2)
  }
  rm(y)
}

colnames(utm_bir) <- c("lonutm", "latutm", "UTMZone")
table(bird_sit_coord$UTMZone == utm_bir$UTMZone)

bird_sit_coord <- cbind(bird_sit_coord, utm_bir[,1:2])

bird_sit_coord2 <- 
    bird_sit_coord %>% 
        mutate(park = substr(bird_sit, 1, 4)) %>% 
        select(-lat, -lon, -UTMZone) %>% 
        filter(park == "WEFA")

(minx <- min(bird_sit_coord2$lonutm, na.rm = TRUE) - 100)
(maxx <- max(bird_sit_coord2$lonutm, na.rm = TRUE) + 100)
(miny <- min(bird_sit_coord2$latutm, na.rm = TRUE) - 100)
(maxy <- max(bird_sit_coord2$latutm, na.rm = TRUE) + 100)

print(glue("{round(minx,0)}, {round(maxx,0)}, {round(miny,0)}, {round(maxy,0)}"))

ggplot() +
  geom_point(aes(x = bird_sit_coord2$lonutm, 
                 y = bird_sit_coord2$latutm),
             size = 2,
             color = "red") +
  theme_bw() +
  coord_cartesian(xlim = c(minx, maxx), 
                  ylim = c(miny, maxy)) +
  ggtitle(glue("{bird_sit_coord2$park[1]}"))

# mapo limits/ boundaries
n_parks <- length(parks) 

park_bounds <- tibble(
  park = character(n_parks),
  xmin = numeric(n_parks),
  xmax = numeric(n_parks),
  ymin = numeric(n_parks),
  ymax = numeric(n_parks)
)
park_bounds$park <- parks

park_bounds[which(park_bounds$park == "HOFR"), 2:5] <- list(587596, 588444, 4623623, 4625111)
park_bounds[which(park_bounds$park == "MABI"), 2:5] <- list(697200, 700200, 4833300, 4835000)
park_bounds[which(park_bounds$park == "MIMA"), 2:5] <- list(309048, 313617, 4701850, 4703865)
park_bounds[which(park_bounds$park == "MORR"), 2:5] <- list(538396, 541212, 4512025, 4513859)
park_bounds[which(park_bounds$park == "SAGA"), 2:5] <- list(711850, 712625, 4818650, 4820100)
park_bounds[which(park_bounds$park == "SARA"), 2:5] <- list(609609, 612850, 4759400, 4763102)
park_bounds[which(park_bounds$park == "VAMA"), 2:5] <- list(587520, 587950, 4627120, 4628606)
park_bounds[which(park_bounds$park == "WEFA"), 2:5] <- list(629070, 629777, 4568205, 4569008)

write_rds(park_bounds, file = "data/out/park_plot_lims.rds")
