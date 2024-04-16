#? *********************************************************************************
#? -------------------------------   Amazing Title   -------------------------------
#? *********************************************************************************
#! Code to ...
#
#
#! Source ---------------------------------------------
#           - :
#           - :
#
#! Input ----------------------------------------------
#           - data/veg_kateaaron/NETN_forest_data_2006-2023.rds : site forest covariates
#           - data/veg_kateaaron/for_sites.rds : name of all the sites with they XY coordinates
#           - data/veg_kateaaron/NETN_tree_dens_spp_2006-2023.rds : tree species abundance
#           - data/veg_kateaaron/NETN_forest_metadata.csv : meta data with info of the columns of the forest variables
#           -  : tibble with site names and coordinates
#
#! Output ----------------------------------------------
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

conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::filter)
# conflicts_prefer(scales::alpha)
#
# Make functions --------------------------------------
colanmes <- colnmaes <- colnames
lenght <- length
`%!in%` <- Negate(`%in%`)
#
# Source code -----------------------------------------
#
# Import data -----------------------------------------
## file paths
BIRD_SITE_PATH    <- "data/out/NETNtib.rds"
PARK_SITE_PATH    <- "data/src/key_park.rds"
FORCOVS_SITE_PATH <- "data/veg_kateaaron/NETN_forest_data_2006-2023.rds"
FORSPS_SITE_PATH    <- "data/veg_kateaaron/NETN_tree_dens_spp_2006-2023.rds"
FOR_SITE_PATH     <- "data/veg_kateaaron/for_sites.rds"

## read files
bird_sit <- read_rds(file = BIRD_SITE_PATH)
parks <- read_rds(file = PARK_SITE_PATH) 
for_sit <- read_rds(file = FORCOVS_SITE_PATH)
fordiv_sit <- read_rds(file = FORSPS_SITE_PATH)
for_sit_coord <- read_rds(file = FOR_SITE_PATH)

# get coordinates from the bird plots
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
# get coordinates from bird plots
bird_sit_coord <- park_site  %>% 
  select(Point_Name,
         Latitude,
         Longitude, 
         UTM_ZONE) %>% 
  rename(bird_sit = Point_Name,
         lat = Latitude,
         lon = Longitude
  )

# get coordinates from the forest plots
for_sit_coord <- for_sit_coord %>% 
  rename(for_sit = Point_Name,
         lat = Y,
         lon = X
  )

colnmaes(for_sit_coord); colnmaes(bird_sit_coord)
