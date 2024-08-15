#! *********************************************************************************
#! -------------------------------   Amazing Title   -------------------------------
#! *********************************************************************************
# Code to get gdb files from parks and classifi all bird and forest sites
#   according to vegetation type
#
# Source ---------------------------------------------
#           - :
#           - :
#
# Input ----------------------------------------------
#           - data/veg_maps/ :
#           - data/src/key_park.rds :
#           - data/out/park_site.rds :
#           - data/out/bird_site_coords.rds :
#           - data/out/for_sit_coord.rds :
#           - :
#
# Output ----------------------------------------------
#           - data/out/key_fsite.rds : 
#           - data/out/key_bsite.rds :
#
# detach packages and clear workspace
if(!require(freshr)){install.packages('freshr')}
freshr::freshr()
#
# Load packages ---------------------------------------
library(conflicted)
library(tidyverse)
library(glue)
library(sf)
#library(rgdal)
#
conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::filter)
# conflicts_prefer(scales::alpha)
#
# Make functions --------------------------------------
colanmes <- colnames
lenght <- length
`%!in%` <- Negate(`%in%`)
#
# Source code -----------------------------------------
#
# Import data -----------------------------------------
## file paths
PATH_PARK_GDB   <- "data/veg_maps/"
PARK_KEY_PATH   <- "data/src/key_park.rds"
PARK_SITE_PATH  <- "data/out/park_site.rds"
BIRD_SITE_COORD <- "data/out/bird_site_coords.rds"
FOR_SITE_COORD  <- "data/out/for_sit_coord.rds"

## read files
parks <- read_rds(file = PARK_KEY_PATH) 
park_site <- read_rds(file = PARK_SITE_PATH) 
xy <- read_rds(file = BIRD_SITE_COORD)
xy2 <- read_rds(file = FOR_SITE_COORD)

## format files
parks <- parks %>% 
  dplyr::select(parks) %>% 
  distinct() %>% 
  pull()

parks_remove <- c("ACAD")

parks <- parks[which(parks %!in% parks_remove)]

xy_sf <- st_as_sf(xy)

# loop to load all park files
park_folder <- list.files(path = "data/veg_maps/")
folder_names <- substr(park_folder, 1, 4)

# get layer names
# scdl <- st_layers(dsn = PATH_PARK_LOOP)
# scdl$name

parks_ana <- c()

for(ii in 1:lenght(parks)){
    (park_loop <- tolower(parks[ii]))
    if(park_loop %in% folder_names) {

        if(park_loop == "mima"){ 

        vegp_map <- read_sf(dsn = "data/veg_maps/mimageodata/MIMA_Veg_shp/MIMA_Veg.shp") 

        assign(glue("{park_loop}_vegmap"), vegp_map)
        
        parks_ana <- c(parks_ana, park_loop)

    } else { 

        PATH_PARK_LOOP <- glue("{PATH_PARK_GDB}{park_loop}geodata/{park_loop}geodata.gdb")
        
        # get layer names
        # scdl <- st_layers(dsn = PATH_PARK_LOOP)
        # scdl$name

        vegp_map <- sf::st_read(PATH_PARK_LOOP, layer = glue("{toupper(park_loop)}_VegPolys"))

        assign(glue("{park_loop}_vegmap"), vegp_map)
        
        parks_ana <- c(parks_ana, park_loop)
        }
    }
    
    if(park_loop %in% c("elro", "hofr", "vama")) {

        PATH_PARK_LOOP <- glue("{PATH_PARK_GDB}rovageodata/rovageodata.gdb")
        
        vegp_map <- sf::st_read(PATH_PARK_LOOP, layer = glue("ROVA_VegPolys"))

        assign(glue("{park_loop}_vegmap"), vegp_map)

        parks_ana <- c(parks_ana, park_loop)

    }
}

#! get park X and Y coordinates ----------------------------------
xy$park <- park_site$Admin_Unit_Code
xy$Point_Name <- park_site$Point_Name
xy$UTM_ZONE <- park_site$UTM_ZONE

str(xy)

# Convert the SpatialPointsDataFrame to an sf object
xy_sf <- st_as_sf(xy)

parks_ana <- sort(parks_ana)

for(ii in 1:lenght(parks_ana)){
    (park_loop <- tolower(parks_ana[ii]))

    xy_sf_loop <- xy_sf %>% filter(park == toupper(park_loop))

    get_parkloop_veg <- get(glue("{park_loop}_vegmap"))

    xy_transformed <- st_transform(xy_sf_loop, crs = st_crs(get_parkloop_veg))

    bird_point_veg <- st_intersection(xy_transformed, get_parkloop_veg)

    # plot(xy_transformed)

    # plot(get_parkloop_veg)

    # plot(bird_point_veg)

    # dim(xy_transformed)

    # dim(bird_point_veg)

    # get_parkloop_veg$MapUnit_Name %>% unique()

    # bird_point_veg$MapUnit_Name %>% unique()

    assign(glue("{park_loop}_birdsite_vegmap"), bird_point_veg)

    if(park_loop == "mima") {
        bird_point_veg <- bird_point_veg  %>% 
            mutate(MapUnit_ID = GROUP_CODE,
                   MapUnit_Name = GROUP_NAME)
    }
    
    key_bsite_l <- bird_point_veg %>% 
        select(park, Point_Name, UTM_ZONE, 
            MapUnit_ID,  MapUnit_Name) %>% 
            as_tibble()

    if(ii == 1){key_bsite <- key_bsite_l
        } else {
            key_bsite <- rbind(key_bsite, key_bsite_l)
        }

    print(park_loop)
    print(nrow(key_bsite_l))
    rm(key_bsite_l)
    rm(bird_point_veg)
    rm(park_loop)

}

dim(key_bsite)

# OMG great! now do the same for forest plots!
xy3 <- data.frame(ID = xy2$for_sit, 
                  park = substr(xy2$for_sit, 1, 4),
                  X =  xy2$lonutm, 
                  Y =  xy2$latutm,
                  zone = as.numeric(xy2$UTMZone))

xy4 <- bcmaps::utm_convert(xy3, easting = "X", northing = "Y",
                   zone = "zone", crs = "WGS84")

projcrs <- "+proj=utm +datum=WGS84 +units=m"
xy_sf2 <- st_as_sf(xy4)

# load forest plots, add rova in the list and rova veg_map
parks_ana2 <- c(parks_ana[which(parks_ana %in% tolower(unique(xy3$park)))],
                "rova")
PATH_PARK_LOOP <- glue("{PATH_PARK_GDB}rovageodata/rovageodata.gdb")
rova_vegmap <- sf::st_read(PATH_PARK_LOOP, 
                           layer = glue("ROVA_VegPolys"))

for(ii in 1:lenght(parks_ana2)){
    (park_loop <- tolower(parks_ana2[ii]))

    # Now you can apply the st_transform function
    xy_sf_loop2 <- xy_sf2 %>% filter(park == toupper(park_loop))
    #st_crs(xy_sf_loop2) <- CRS("+proj=longlat +datum=WGS84")

    get_parkloop_veg2 <- get(glue("{park_loop}_vegmap"))

    xy_transformed2 <- sf::st_transform(xy_sf_loop2, 
                                        crs = st_crs(get_parkloop_veg2))

    # st_crs(xy_transformed2) == st_crs(xy_sf_loop2)
    # st_crs(xy_transformed2) == st_crs(get_parkloop_veg2)

    for_point_veg <- st_intersection(xy_transformed2, get_parkloop_veg2)

    # plot(xy_transformed2)
    # st_crs(xy_transformed2)

    # plot(xy_sf_loop2)
    # st_crs(xy_sf_loop2)

    # plot(get_parkloop_veg2)
    # st_crs(get_parkloop_veg2)

    # plot(for_point_veg)

    # dim(xy_transformed2)

    # dim(for_point_veg)

    # get_parkloop_veg$MapUnit_Name %>% unique()

    # bird_point_veg$MapUnit_Name %>% unique()

    assign(glue("{park_loop}_forsite_vegmap"), for_point_veg)
    
  if(park_loop == "mima") {
        for_point_veg <- for_point_veg %>% 
            mutate(MapUnit_ID = GROUP_CODE,
                   MapUnit_Name = GROUP_NAME)
    }

    key_fsite_l <- for_point_veg %>% 
        select(park, ID, 
               X, Y, zone, 
               MapUnit_ID) %>% 
               as_tibble()

    if(ii == 1){key_fsite <- key_fsite_l
        } else {
            key_fsite <- rbind(key_fsite, key_fsite_l)
        }
    print(park_loop)
    print(nrow(key_fsite_l))
    rm(key_fsite_l)
    rm(for_point_veg)
    rm(park_loop)
}

key_fsite %>% tail()

# write_rds(key_fsite, file = "data/out/key_fsite.rds")
# write_rds(key_bsite, file = "data/out/key_bsite.rds")


