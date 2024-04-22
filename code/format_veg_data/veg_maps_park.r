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
#           - :
#           - :
#
# Output ----------------------------------------------
#           - :
#           - :
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
PATH_PARK_GDB  <- "data/veg_maps/"
PARK_KEY_PATH  <- "data/src/key_park.rds"
PARK_SITE_PATH <-  "data/out/park_site.rds"

## read files
parks <- read_rds(file = PARK_KEY_PATH) 

park_site <- read_rds(file = PARK_SITE_PATH) 

## format files
parks <- parks %>% 
  dplyr::select(parks) %>% 
  distinct() %>% 
  pull()

parks <- parks[-1]
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

        PATH_PARK_LOOP <- glue("{PATH_PARK_GDB}{park_loop}geodata/{park_loop}geodata.gdb")
        
        # get layer names
        # scdl <- st_layers(dsn = PATH_PARK_LOOP)
        # scdl$name

        vegp_map <- sf::st_read(PATH_PARK_LOOP, layer = glue("{toupper(park_loop)}_VegPolys"))

        assign(glue("{park_loop}_vegmap"), vegp_map)
        
        parks_ana <- c(parks_ana, park_loop)
    }
    
    if(park_loop %in% c("elro", "hofr", "vama")) {

        PATH_PARK_LOOP <- glue("{PATH_PARK_GDB}rovageodata/rovageodata.gdb")
        
        vegp_map <- sf::st_read(PATH_PARK_LOOP, layer = glue("ROVA_VegPolys"))

        assign(glue("{park_loop}_vegmap"), vegp_map)

        parks_ana <- c(parks_ana, park_loop)

    }
}

xy <- read_rds(file = "data/out/bird_site_coords.rds")

xy$park <- park_site$Admin_Unit_Code
xy$Point_Name <- park_site$Point_Name
xy$UTM_ZONE <- park_site$UTM_ZONE

str(xy)

# ERROR: does not work, not sure if it necessary
#xy2 <- spTransform(xy, st_crs(vegp_map))

# Convert the SpatialPointsDataFrame to an sf object
xy_sf <- st_as_sf(xy)

parks_ana <- sort(parks_ana)

for(ii in 1:lenght(parks_ana)){
    (park_loop <- tolower(parks_ana[ii]))

    # Now you can apply the st_transform function
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





