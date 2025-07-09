#? *********************************************************************************
#? ------------------------------   park_rova_find.R   ------------------------------
#? *********************************************************************************
#! Code to ...
#
#
#! Source ---------------------------------------------
#           - :
#           - :
#
#! Input ----------------------------------------------
#           - data/out/NETNtib.rds :
#           - data/src/key_park.rds :
#           - data/veg_kateaaron/NETN_forest_data_2006-2023.rds :
#           - data/veg_kateaaron/NETN_tree_dens_spp_2006-2023.rds :
#           - data/veg_kateaaron/for_sites.rds : 
#           - data/out/key_bsite.rds : 
#           - data/out/key_fsite.rds :
#
#! Output ----------------------------------------------
#           - data/out/close_points_f.rds : tibble with combinations of forest and bird sites, and the distances between them
#!          - data/out/park_site.rds :
#!          - data/out/for_sit_coord.rds :
#!          - data/out/bird_site_coords.rds :
#           - data/out/site_covs.rds : output file with the covariate values
#!          - data/out/for_sit2.rds :
#!          - data/out/close_points_fcovs.rds :
#
# detach packages and clear workspace
freshr::freshr()
#
#! Load packages ---------------------------------------
#library(conflicted)
library(tidyverse)
library(glue)
library(sp)
#library(rgdal)
library(sf)
library(reshape2)
library(ggplot2)
library(ggh4x)
#library("MetBrewer")
library(forestNETN)

#conflicts_prefer(dplyr::select)
#conflicts_prefer(dplyr::filter)
# conflicts_prefer(scales::alpha)
#
#! Make functions --------------------------------------
colanmes <- colnmaes <- colnames
lenght <- length
`%!in%` <- Negate(`%in%`)

Modes <- function(x) {
  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  if(length(ux[tab == max(tab)]) > 1) {
    sample(ux[tab == max(tab)], 1)
    } else {
      ux[tab == max(tab)]}
}
#! Define settings -------------------------------------
radi_dist <- 20000

#! Import data -----------------------------------------
## file paths
BIRD_SITE_PATH    <- "data/out/NETNtib.rds"
PARK_SITE_PATH    <- "data/src/key_park.rds"
FORCOVS_SITE_PATH <- "data/veg_kateaaron/NETN_forest_data_2006-2023.rds"
FORSPS_SITE_PATH  <- "data/veg_kateaaron/NETN_tree_dens_spp_2006-2023.rds"
FOR_SITE_PATH     <- "data/veg_kateaaron/for_sites.rds"
BIRD_FOR_PATH     <- "data/out/key_bsite.rds"
FOR_FOR_PATH      <- "data/out/key_fsite.rds"
VEG_TYP_PATH      <- "data/out/tab_veg3_AW.csv"    #  "data/veg_parks.csv"

## read files
bird_sit      <- read_rds(file = BIRD_SITE_PATH)
parks         <- read_rds(file = PARK_SITE_PATH) 
for_sit       <- read_rds(file = FORCOVS_SITE_PATH)
fordiv_sit    <- read_rds(file = FORSPS_SITE_PATH)
for_sit_coord <- read_rds(file = FOR_SITE_PATH)
key_bsite     <- read_rds(file = BIRD_FOR_PATH)
key_fsite     <- read_rds(file = FOR_FOR_PATH)
veg_type      <- read_csv(file = VEG_TYP_PATH)

#! get coordinates from the bird plots ------------------------------
parks <- parks %>% 
  dplyr::select(parks) %>% 
  distinct() %>% 
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

nrow(park_site)

## write_rds "data/out/park_site.rds"
# write_rds(park_site, file = "data/out/park_site.rds")

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

#! get coordinates from the forest plots ------------------------------
for_sit_coord <- for_sit_coord %>% 
  rename(for_sit = Plot_Name,
          latutm = Y,
          lonutm = X) %>% 
  relocate(for_sit, latutm, lonutm, UTMZone) %>% 
  mutate(UTMZone = substr(UTMZone, 1 , 2))

colnmaes(for_sit_coord); colnmaes(bird_sit_coord)
# write_rds(for_sit_coord, file = "data/out/for_sit_coord.rds")

par(mfrow = c(1,2))
plot(for_sit_coord$lonutm, for_sit_coord$latutm, col = "darkgreen")
plot(bird_sit_coord$lon, bird_sit_coord$lat, col = "violet")

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

park_plot_nam <- bird_sit_coord %>% 
              mutate(park = substr(bird_sit, 1 , 4)) %>% 
              group_by(park) %>% 
              filter(row_number()==1) %>%
              ungroup()

library(ggplot2)

ggplot() +
  geom_point(aes(x = bird_sit_coord$lonutm, 
                 y = bird_sit_coord$latutm),
             size = 2,
             color = "red") +
  geom_point(aes(x = for_sit_coord$lonutm, 
                 y = for_sit_coord$latutm), 
             color = "darkgreen") +
  #geom_text(aes(x= park_plot_nam$lonutm, y =park_plot_nam$latutm),
  #          label = park_plot_nam$park, size = 3, vjust = -1.3) +
  theme_bw() +
  coord_cartesian(xlim = c(697200, 700200), 
                  ylim = c(4833300, 4835000))

# great - it all alligns very well; now let's get the closest 3 sites

#? Connect forest sites and bird sites -----------------------------------
# there is no SAIR site level data
bird_sit_coord2 <- bird_sit_coord %>% 
    as_tibble() %>% 
    mutate(park = substr(bird_sit, 1 , 4)) %>%
    filter(park != "SAIR") %>% 
    select(-park)

# link forest types with sites
bird_sit_coord2 <- left_join(bird_sit_coord2, 
                              key_bsite %>% 
                                rename(bird_sit = Point_Name,
                                        b_for = MapUnit_ID) %>% 
                                select(bird_sit, b_for),
                              by = "bird_sit") %>% 
                    filter(!is.na(b_for))

for_sit_coord2 <- left_join(for_sit_coord  %>% 
                              mutate(park = substr(for_sit, 1, 4)), #%>% 
                              #filter(park != "ACAD") %>% 
                              #filter(park != "SAIR"), 
                            key_fsite %>% 
                              rename(for_sit = ID,
                                      f_for = MapUnit_ID) %>% 
                              select(for_sit, f_for, geometry),
                            by = "for_sit") %>% 
                    filter(!is.na(f_for))

# make sure parks patch with forest sites only within the same park
for_sit_coord_rova <- for_sit_coord2 %>% 
  filter(park == "ROVA") %>% 
  select(-park)

for_sit_coord_elro <- for_sit_coord_rova %>% 
  mutate(park = "ELRO")
for_sit_coord_vama <- for_sit_coord_rova %>% 
  mutate(park = "VAMA")
for_sit_coord_hofr <- for_sit_coord_rova %>% 
  mutate(park = "HOFR")

for_sit_coord3 <- rbind(for_sit_coord_elro, 
                        for_sit_coord_vama, 
                        for_sit_coord_hofr)

#bird_sit_coord2 <- bird_sit_coord2[1:11,]

# get a table with all the forest types to compare to the key, 
#    and possible combine them into smaller groups
veg_type_indata <- c(unique(for_sit_coord3$f_for),
                      unique(bird_sit_coord2$b_for)) %>% 
                      unique() %>% 
                      sort() %>% 
                      as_tibble() %>% 
                      rename(MapUnit_ID = value) %>% 
                      left_join(veg_type, by = "MapUnit_ID")
dim(veg_type_indata)
length(unique(veg_type_indata$MapUnit_ID))
#writexl::write_xlsx(tab_veg3, path = "data/out/tab_veg3.xlsx")

bird_sit_coord2 <- bird_sit_coord2 %>% mutate(park = substr(bird_sit,1,4)) %>% filter(park %in% c("ELRO", "HOFR", "VAMA")) %>% select(-park)
bird_sit_coord2 <- bird_sit_coord2 %>% 
      rename(MapUnit_ID = b_for) %>% 
      left_join(., veg_type, by = "MapUnit_ID")  %>% 
      rename(bir_veg = Cover_Type)

for_sit_coord3 <- for_sit_coord3 %>% 
      rename(MapUnit_ID = f_for) %>% 
      left_join(., veg_type, by = "MapUnit_ID") %>% 
      rename(for_veg = Cover_Type)

rbind(
  bird_sit_coord2 %>% select(MapUnit_ID) %>% distinct(),
  for_sit_coord3 %>% select(MapUnit_ID) %>% distinct()) %>% 
  distinct() %>% 
  arrange(MapUnit_ID)

# test sites
for_test <- for_sit_coord3 %>% 
              select(for_sit, park, lonutm, latutm, for_veg)
bir_test <- bird_sit_coord2 %>% 
              mutate(park = substr(bird_sit,1,4)) %>% 
              select(park, lonutm, latutm, bir_veg)
parks_sub <- sort(unique(for_test$park))
par(mfrow = c(1,1))

for(ii in 1:length(parks_sub)){
  for_test2 <- for_test %>% filter(park == parks_sub[ii])
  bir_test2 <- bir_test %>% filter(park == parks_sub[ii])

  plot(for_test2$lonutm, for_test2$latutm, col = "darkgreen", 
      main = for_test2$park[1], xlim = c(587500, 592000), ylim = c(4623000, 4629000))
  points(bir_test2$lonutm, bir_test2$latutm, col = "violet")
}
# get which sites are in which park
VAMA_sites <- for_test %>% 
                  filter(latutm > 4626000,
                         lonutm < 589000) %>% 
                  mutate(park = "VAMA")

plot(VAMA_sites$lonutm, VAMA_sites$latutm,  col = "darkgreen", xlim = c(587500, 592000), ylim = c(4623000, 4629000))
points(bir_test$lonutm, bir_test$latutm, col = "violet")

HOFR_sites <- for_test %>% 
                  filter(latutm < 4626000,
                         lonutm < 589000) %>% 
                  mutate(park = "HOFR")

plot(HOFR_sites$lonutm, HOFR_sites$latutm,  col = "darkgreen", xlim = c(587500, 592000), ylim = c(4623000, 4629000))
points(bir_test$lonutm, bir_test$latutm, col = "violet")

ELRO_sites <- for_test %>% 
                  filter(latutm < 4626000,
                         lonutm > 591000) %>% 
                  mutate(park = "ELRO")

plot(ELRO_sites$lonutm, ELRO_sites$latutm,  col = "darkgreen", xlim = c(587500, 592000), ylim = c(4623000, 4629000))
points(bir_test$lonutm, bir_test$latutm, col = "violet")


VAMA_sites2 <- VAMA_sites %>% left_join(., for_sit_coord3, by = c("for_sit", "park", "lonutm", "latutm", "for_veg")) %>% distinct()

HOFR_sites2 <- HOFR_sites %>%  left_join(., for_sit_coord3, by = c("for_sit", "park", "lonutm", "latutm", "for_veg")) %>% distinct()

ELRO_sites2 <- ELRO_sites %>%  left_join(., for_sit_coord3, by = c("for_sit", "park", "lonutm", "latutm", "for_veg")) %>% distinct()

write_rds(VAMA_sites2, file = "data/VAMA_sites.rds")
write_rds(HOFR_sites2, file = "data/HOFR_sites.rds")
write_rds(ELRO_sites2, file = "data/ELRO_sites.rds")
