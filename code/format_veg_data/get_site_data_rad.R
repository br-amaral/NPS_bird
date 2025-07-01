#? *********************************************************************************
#? ------------------------------   get_site_data.R   ------------------------------
#? *********************************************************************************
#! Code to get site values for forest covariates. First, it gets the locations and forest types
#!    of each bird site and forest plot, and connect them. Bird sites are characterized with forest
#!    structure variables for all the forest plots that are within the same forest type as all the
#!    forest plots.
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
# radius distance in meters
radi_dist <- 250

#! Import data -----------------------------------------
## file paths
BIRD_SITE_PATH    <- "data/out/NETNtib.rds"
PARK_SITE_PATH    <- "data/src/key_park.rds"
FORCOVS_SITE_PATH <- "data/veg_kateaaron/NETN_forest_data_2006-2023.rds"
FORSPS_SITE_PATH  <- "data/veg_kateaaron/NETN_tree_dens_spp_2006-2023.rds"
FOR_SITE_PATH     <- "data/veg_kateaaron/for_sites.rds"
BIRD_FOR_PATH     <- "data/out/key_bsite.rds"
FOR_FOR_PATH      <- "data/out/key_fsite.rds"
VEG_TYP_PATH      <- "data/out/updated_for_cats.csv"    # vegetation types (3) of the parks
VAMA_PARK_PATH    <- "data/VAMA_sites.rds"
HOFR_PARK_PATH    <- "data/HOFR_sites.rds"
ELRO_PARK_PATH    <- "data/ELRO_sites.rds"

## read files
## get site names for ROVA parks 
VAMA_sites <- read_rds(file = VAMA_PARK_PATH) %>% 
                select(park, for_sit)

HOFR_sites <- read_rds(file = HOFR_PARK_PATH) %>% 
                select(park, for_sit)

ELRO_sites <- read_rds(file = ELRO_PARK_PATH) %>% 
                select(park, for_sit)

ROVA_sites <- rbind(VAMA_sites, HOFR_sites, ELRO_sites)  %>% 
                rename(ParkUnit = park, Plot_Name = for_sit)

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

nrow(park_site)

#? write_rds "data/out/park_site.rds"
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
#? write_rds for_sit_coord
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
    filter(park != "ACAD",
           park != "ELRO",
           park != "SAIR") %>% 
    select(-park)

# link forest types with sites
bird_sit_coord2 <- left_join(bird_sit_coord2, 
                              key_bsite %>% 
                                rename(bird_sit = Point_Name,
                                        b_for = MapUnit_ID) %>% 
                                select(bird_sit, b_for),
                              by = "bird_sit") %>% 
                    filter(!is.na(b_for))

# change Rova names to actual parks
ROVA_sites <- ROVA_sites  %>% rename(park = ParkUnit, ID = Plot_Name)

for(ii in 1:nrow(key_fsite)){
    for(jj in 1:nrow(ROVA_sites)){
      if(key_fsite$ID[ii] == ROVA_sites$ID[jj]) {
         key_fsite$park[ii] <- ROVA_sites$park[jj]
      }
    }
}

for_sit_coord2 <- left_join(for_sit_coord, 
                            key_fsite %>% 
                              rename(for_sit = ID,
                                      f_for = MapUnit_ID) %>% 
                              select(for_sit, f_for, geometry, park),
                            by = "for_sit") %>% 
                    filter(!is.na(f_for)) %>% 
                    filter(park != "ROVA")

#bird_sit_coord2 <- bird_sit_coord2[1:11,]

# get a table with all the forest types to compare to the key, 
#    and possible combine them into two groups: forest not forest
for (ii in 1:nrow(veg_type)){
    if(!is.na(veg_type$Cover_Type[ii]) && veg_type$Cover_Type[ii] != "Not forest") {
        veg_type$Cover_Type[ii] <- "Forest"
        } 
}

veg_type_indata <- c(unique(for_sit_coord2$f_for),
                      unique(bird_sit_coord2$b_for)) %>% 
                      unique() %>% 
                      sort() %>% 
                      as_tibble() %>% 
                      rename(MapUnit_ID = value) %>% 
                      left_join(veg_type, by = "MapUnit_ID")
dim(veg_type_indata)
length(unique(veg_type_indata$MapUnit_ID))
#writexl::write_xlsx(tab_veg3, path = "data/out/tab_veg3.xlsx")

# bird_sit_coord2 <- bird_sit_coord2 %>% mutate(park = substr(bird_sit,1,4))  %>% filter(park %in% c("ELRO", "HOFR", "VAMA")) %>% select(-park)
bird_sit_coord2 <- bird_sit_coord2 %>% 
      rename(MapUnit_ID = b_for) %>% 
      left_join(., veg_type, by = "MapUnit_ID")  %>% 
      rename(bir_veg = Cover_Type)

for_sit_coord3 <- for_sit_coord2 %>% 
      rename(MapUnit_ID = f_for) %>% 
      left_join(., veg_type, by = "MapUnit_ID") %>% 
      rename(for_veg = Cover_Type)

rbind(
  bird_sit_coord2 %>% filter(!is.na(bir_veg)) %>% select(MapUnit_ID) %>% distinct(),
  for_sit_coord3 %>% filter(!is.na(for_veg)) %>% select(MapUnit_ID) %>% distinct()) %>% 
  distinct() %>% 
  arrange(MapUnit_ID)

# test sites
for_test <- for_sit_coord3 %>% 
              select(park, lonutm, latutm, for_veg)
bir_test <- bird_sit_coord2 %>% 
              mutate(park = substr(bird_sit,1,4)) %>% 
              select(park, lonutm, latutm, bir_veg)
parks_sub <- sort(unique(for_test$park))

par(mfrow = c(1,1))
for(ii in 1:length(parks_sub)){
  for_test2 <- for_test %>% filter(park == parks_sub[ii])
  bir_test2 <- bir_test %>% filter(park == parks_sub[ii])

  plot(for_test2$lonutm, for_test2$latutm, col = "darkgreen", 
      main = for_test2$park[1], pch = 19, cex = 3)
  points(bir_test2$lonutm, bir_test2$latutm, col = "violet", pch = 15, cex = 3)
}
# get ALL neighbors within the same park, and only keep the ones that have the forest type
# remove not forest first
for_sit_coord3  <- for_sit_coord3  %>% filter(for_veg == "Forest")
bird_sit_coord2 <- bird_sit_coord2  %>% filter(bir_veg == "Forest")

for (ii in 1:nrow(bird_sit_coord2)) {

  band <- as.numeric(bird_sit_coord2$UTMZone[ii])
  y <- spTransform(xy[ii,], CRS(glue("+proj=utm +zone={band} +datum=WGS84 +units=m")))

  bird <- st_as_sf(bird_sit_coord2[ii,5:6], 
                    coords=c("lonutm", "latutm"), 
                    crs = CRS(proj4string(y)))

  plop <- substr(bird_sit_coord2$bird_sit[ii], 1, 4)

  for_sit_coord4 <- for_sit_coord3 %>% 
                      filter(park == plop)
  
  print(bird_sit_coord2$bird_sit[ii])

  for(jj in 1:nrow(for_sit_coord4)) {
    # are the bird sites and forest plots in the same forest type?
    # if the answer is yes, we calculate the distance between them
    if((bird_sit_coord2$bir_veg[ii] == for_sit_coord4$for_veg[jj]) == TRUE){
      print(for_sit_coord4$for_sit[jj])
      print(jj)

      band2 <- as.numeric(for_sit_coord4$UTMZone[jj])
      x <- spTransform(xy[ii,], CRS(glue("+proj=utm +zone={band2} +datum=WGS84 +units=m")))

      fore <- st_as_sf(for_sit_coord4[,2:3], 
                    coords=c("lonutm", "latutm"), 
                    crs = CRS(proj4string(x)))

      distances <- st_distance(bird, fore[jj,], by_element = TRUE)
  
      distances2 <- cbind(as.numeric(distances), for_sit_coord4$for_sit[jj]) %>% 
          as_tibble() %>% 
          rename(dist = V1, 
                for_sit = V2) %>% 
          mutate(dist = as.numeric(dist),
                for_b = as.character(bird_sit_coord2$bir_veg[ii]),
                for_f = as.character(for_sit_coord4$for_veg[jj]),
                bird_sit = bird_sit_coord2$bird_sit[ii])
    
      if("dist1" %!in% ls()) {
          dist1 <- distances2
        } else {
          dist1 <- rbind(dist1, distances2)
        } 
    } 
  }
  
  # get only sites and plots that are 500m between each other
  if("dist1" %in% ls()) {
    dist_small <- dist1 %>% 
                    arrange(dist) %>% 
                    filter(dist <= radi_dist) %>% # diameter of the home range area of birds plus some slack if it is not circular 
                    arrange(bird_sit, dist)
    
    table(dist_small$bird_sit)

    rm(dist1)
  #!! ERROR: NOT REALLY AN ERROR, just choosing how many neighbours
    close_points <- dist_small %>%
                      group_by(bird_sit) %>%
                      arrange(dist) %>% 
                      slice(1:5) %>% # TODO: 
                      ungroup()

    table(close_points$bird_sit)

    if(ii == 1) {
      close_points_f <- close_points
      } else {
        close_points_f <- rbind(close_points_f, close_points)
      }
    rm(close_points)
  }
  print(ii)
}

# connects forest and bird sites
close_points_f <- close_points_f %>% 
                    arrange(bird_sit, dist) %>% 
                    distinct()

# write_rds data/out/close_points_f.rds
# write_rds(close_points_f, file = "data/out/close_points_f.rds")

# plot close points by park
## join coordinates
close_points_f2 <- close_points_f %>% 
  left_join(for_sit_coord2 %>% 
              rename(latutmf = latutm, lonutmf = lonutm) %>%
              select(for_sit, latutmf, lonutmf),
            by = "for_sit") %>% 
  left_join(bird_sit_coord2 %>% 
              rename(latutmb = latutm, lonutmb = lonutm) %>%
              select(bird_sit, latutmb, lonutmb),
            by = "bird_sit") 

for(ii in 1:lenght(parks)){
  (plop <- parks[ii])
  close_points_f3 <- close_points_f2 %>% 
                      filter(substr(bird_sit, 1, 4) == plop)

  p2 <- 
  ggplot(close_points_f3) +
    geom_segment(aes(x = lonutmb, y = latutmb, 
                      xend = lonutmf, yend = latutmf, 
                      colour = bird_sit)) +
    geom_point(aes(x = lonutmb, 
                    y = latutmb,
                    colour = bird_sit),
              size = 3) +
    geom_point(aes(x = lonutmf, 
                    y = latutmf),
              size = 3,
              color = "#186A3B",
              shape = 15) +
geom_text(aes(x = lonutmb, y = latutmb, 
              label = substr(bird_sit, 5, nchar(bird_sit))), 
              size = 5, vjust = -1.3) +
    theme_bw() +
    labs(title = parks[ii])
    #)
  print(p2)
  #library(plotly)
  #ggplotly(p2)
}

table(close_points_f$bird_sit) %>% dim()
table(close_points_f$bird_sit) %>% sort()

table(close_points_f$dist) %>% sort()

table(close_points_f$for_b) %>% sort()

table(close_points_f$for_f) %>% sort()


table(for_sit$SampleYear) %>% max()

#? get extra covariates
## canopy cover ---------------------------------------------------------
path <- glue("{getwd()}/data/veg_kateaaron") 
importCSV(path, zip_name = "NETN_Forest_20231106.zip")
can <- forestNETN::joinStandData(park = "all") %>%
          as_tibble() 

ROVA_sites <- ROVA_sites  %>% rename(ParkUnit = park, Plot_Name = ID)

for(ii in 1:nrow(can)){
    for(jj in 1:nrow(ROVA_sites)){
      if(can$Plot_Name[ii] == ROVA_sites$Plot_Name[jj]) {
         can$ParkUnit[ii] <-  ROVA_sites$ParkUnit[jj]
      }
    }
}

can <- can %>%        
          filter(ParkUnit != "ROVA") %>% 
          select(Plot_Name, SampleYear, ParkUnit, Pct_Crown_Closure) %>% 
          group_by(Plot_Name) %>% 
          mutate(can_m = mean(Pct_Crown_Closure, na.rm = T)) %>% 
          ungroup() %>% 
          select(-Pct_Crown_Closure) %>% 
          distinct()

## wood debris ----------------------------------------------------------
cwd <- joinCWDData(park = 'all') %>% # coarse wood debris
          as_tibble()          

for(ii in 1:nrow(cwd)){
    for(jj in 1:nrow(ROVA_sites)){
      if(cwd$Plot_Name[ii] == ROVA_sites$Plot_Name[jj]) {
         cwd$ParkUnit[ii] <-  ROVA_sites$ParkUnit[jj]
      }
    }
}

cwd <- cwd %>%        
          filter(ParkUnit != "ROVA") %>%    
          select(Plot_Name, SampleYear, ParkUnit, CWD_Vol) %>% 
          group_by(Plot_Name) %>% 
          mutate(deb_m = mean(CWD_Vol, na.rm = T)) %>% 
          ungroup() %>% 
          select(-CWD_Vol) %>% 
          distinct()


## snags ----------------------------------------------------------------
# stand_spp <- joinStandData()
# colnames(stand_spp)
# str(stand_spp)
# tree_den_spp <- joinTreeData()
# str(tree_den_spp)
# TREECLCD_NERS: Tree class code
# treeht <- subset(get("StandTreeHeights_NETN", envir = path),
#                               select = c(Plot_Name, PlotID, EventID, CrownClassCode, CrownClassLabel,
#                                          TagCode, Height))
                                         
#  treeht_sum <- treeht %>% mutate(crown = ifelse(CrownClassCode == 4, "Inter", "Codom")) %>%
#                              group_by(Plot_Name, PlotID, EventID, crown)

for_sit_extra <- for_sit %>% 
                    left_join(., can, by = c('Plot_Name', 'SampleYear', 'ParkUnit')) %>% 
                    left_join(., cwd, by = c('Plot_Name', 'SampleYear', 'ParkUnit'))

#? get means for all years ----------------------------------------------
## mean for all years
for_sit2 <- for_sit_extra %>% 
  group_by(Plot_Name) %>% 
  mutate(treeden_haM = mean(treeden_ha, na.rm = T),
          BA_m2haM = mean(BA_m2ha, na.rm = T),
          tree_richM = mean(tree_rich, na.rm = T),
          StageM = Modes(Stage),
          pctBA_poleM = mean(pctBA_pole, na.rm = T),
          pctBA_matureM = mean(pctBA_mature, na.rm = T),
          pctBA_largeM = mean(pctBA_large, na.rm = T),
          sap_den_m2M = mean(sap_den_m2, na.rm = T),
          shrub_covM = mean(shrub_cov, na.rm = T),
          canop_covM = mean(can_m, na.rm = T),
          debri_covM = mean(deb_m, na.rm = T),
          X_for = X,      
          Y_for = Y,
          UTMZone_for = UTMZone,
          for_sit = Plot_Name)  %>% 
  ungroup() %>% 
  select(for_sit, ParkUnit, X_for, Y_for, UTMZone_for,
          treeden_haM, BA_m2haM, tree_richM, StageM, pctBA_poleM, 
          pctBA_matureM, pctBA_largeM, sap_den_m2M, shrub_covM, 
          canop_covM, debri_covM) %>% 
  distinct()
  
close_points_f2 <- left_join(close_points_f, for_sit2, by = "for_sit") %>% 
    group_by(bird_sit) %>%
  mutate(treeden_haM = mean(treeden_haM, na.rm = T),
          BA_m2haM = mean(BA_m2haM, na.rm = T),
          tree_richM = mean(tree_richM, na.rm = T),
          StageM = Modes(StageM),
          pctBA_poleM = mean(pctBA_poleM, na.rm = T),
          pctBA_matureM = mean(pctBA_matureM, na.rm = T),
          pctBA_largeM = mean(pctBA_largeM, na.rm = T),
          sap_den_m2M = mean(sap_den_m2M, na.rm = T),
          shrub_covM = mean(shrub_covM, na.rm = T),
          canop_covM = mean(canop_covM, na.rm = T),
          debri_covM = mean(debri_covM, na.rm = T))  %>% 
  ungroup() %>% 
  mutate(park = substr(bird_sit, 1, 4)) %>%
  select(bird_sit, park,
          treeden_haM, BA_m2haM, tree_richM, StageM, 
          pctBA_poleM, pctBA_matureM, pctBA_largeM, 
          sap_den_m2M, 
          shrub_covM,
          canop_covM, debri_covM) %>% 
  distinct() %>% 
  rename(ParkUnit = park,
          siteDEN = treeden_haM, siteBA = BA_m2haM, 
          siteRICH = tree_richM, siteSTA = StageM,
          siteBA_pole = pctBA_poleM, siteBA_mature = pctBA_matureM, siteBA_large = pctBA_largeM,
          siteSAPden = sap_den_m2M, 
          siteSHRUden = shrub_covM,
          siteCANOden = canop_covM, 
          siteDEBRden = debri_covM)

neighbor <- left_join(close_points_f, for_sit2, by = "for_sit") %>% 
                      select(for_sit, bird_sit) %>% 
                      distinct()

#? get YEAR SPECIFIC means ----------------------------------------------
for_sit2_year <- for_sit_extra %>% 
  rename(X_for = X,      
         Y_for = Y,
         UTMZone_for = UTMZone,
         for_sit = Plot_Name,
         Year = SampleYear) %>% 
  select(for_sit, ParkUnit, Year, X_for, Y_for, UTMZone_for,
          treeden_ha, BA_m2ha, tree_rich, Stage, pctBA_pole, 
          pctBA_mature, pctBA_large, sap_den_m2, shrub_cov, 
          can_m, deb_m) %>% 
  distinct()
  
close_points_f2_year <- suppressWarnings(
  full_join(for_sit2_year, close_points_f, by = "for_sit") %>% 
    group_by(bird_sit, Year) %>%
    mutate(treeden_ha = mean(treeden_ha, na.rm = T),
            BA_m2ha = mean(BA_m2ha, na.rm = T),
            tree_rich = mean(tree_rich, na.rm = T),
            Stage = Modes(Stage),
            pctBA_pole = mean(pctBA_pole, na.rm = T),
            pctBA_mature = mean(pctBA_mature, na.rm = T),
            pctBA_large = mean(pctBA_large, na.rm = T),
            sap_den_m2 = mean(sap_den_m2, na.rm = T),
            shrub_cov = mean(shrub_cov, na.rm = T),
            canop_cov = mean(can_m, na.rm = T),
            debri_cov = mean(deb_m, na.rm = T))  %>% 
    ungroup() %>% 
    mutate(park = substr(bird_sit, 1, 4)) %>%
    select(bird_sit, park, Year,
            treeden_ha, BA_m2ha, tree_rich, Stage, 
            pctBA_pole, pctBA_mature, pctBA_large, 
            sap_den_m2, 
            shrub_cov,
            canop_cov, debri_cov) %>% 
    distinct() %>% 
    rename(ParkUnit = park,
          siteDENYR = treeden_ha, siteBAYR = BA_m2ha, 
          siteRICHYR = tree_rich, siteSTAYR = Stage,
          siteBA_poleYR = pctBA_pole, siteBA_matureYR = pctBA_mature, siteBA_largeYR = pctBA_large,
          siteSAPdenYR = sap_den_m2, 
          siteSHRUdenYR = shrub_cov,
          siteCANOdenYR = canop_cov, 
          siteDEBRdenYR = debri_cov) %>% 
    filter(!is.na(ParkUnit))
)

#! Output files ----------------------------------------------
print("save output files!")
# forest site information
write_rds(for_sit2, file = glue("data/out/for_sit2_nei_grp_{radi_dist}m.rds"))

# information of covariates for each bird site
write_rds(close_points_f2, file = glue("data/out/site_covs_nei_grp_{radi_dist}m.rds"))

# information of covariates for each bird site BY YEAR
write_rds(close_points_f2_year, file = glue("data/out/site_covs_nei_grp_{radi_dist}m_yr.rds"))

# who is who's neighbor
write_rds(neighbor, file = glue("data/out/neighbor_grp_{radi_dist}m.rds"))

cat(paste("\n\n Done \n\n\n"))

# neighbours
table(neighbor$for_sit)
table(neighbor$for_sit) %>% mean()
table(neighbor$for_sit) %>% sd()

# number of neightbours per park
uni_neigh <- full_join(for_sit2_year, close_points_f, by = "for_sit")  %>% 
                select(bird_sit, for_sit) %>% 
                filter(!is.na(bird_sit))  %>% 
                mutate(uni_nei = glue("{substr(bird_sit, 1, 4)}_{substr(for_sit, 6, 8)}")) %>% 
                mutate(park = glue("{substr(bird_sit, 1, 4)}"))

uni_neigh2 <- uni_neigh %>% 
                select(park, uni_nei) %>% 
                distinct() %>% 
                group_by(park) %>% 
                summarise(neigh = n()) %>% 
                ungroup() %>% 
                arrange(park)

# number of forest sites
uni_for <- full_join(for_sit2_year, close_points_f, by = "for_sit")  %>% 
                select(bird_sit, for_sit, ParkUnit) %>% 
                filter(ParkUnit != "ACAD")  %>%
                mutate(uni_nei = glue("{substr(bird_sit, 1, 4)}_{substr(for_sit, 6, 8)}")) %>% 
                mutate(park = glue("{substr(bird_sit, 1, 4)}"))  %>% 
                mutate(park2 = ifelse(is.na(bird_sit), 
                                      glue("{substr(for_sit, 1, 4)}"), 
                                      glue("{substr(uni_nei, 1, 4)}")))
view(uni_for)

uni_for %>% 
    select(park2, for_sit) %>% 
    distinct() %>% 
    group_by(park2) %>% 
    summarise(for_sit = n()) %>% 
    ungroup() %>% 
    arrange(park2)

for_sit2_year  %>% 
    group_by(ParkUnit) %>%
    summarise(min_yr = min(Year),
              max_yr = max(Year),
              n_years = length(unique(Year)))

