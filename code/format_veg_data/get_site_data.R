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
#

library("landscapemetrics")
library("landscapetools")
library("remotes")
library(cowplot)
library(data.table)
library(dismo)
library(dplyr)
library(FedData)
library(glue)
library(maptools)
library(prettymapr)
library(raster)
library(rgdal)
library(rgeos)
library(sf)
library(sp)
library(stars)
library(terra)
library(tidyverse)


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
## read files

parks <- readRDS(file = "data/src/key_park.rds") %>% 
  dplyr::select(parks) %>% 
  distinct() %>% 
  pull()

parks <- sort(parks)


bird_sit <- read_rds(file = glue("data/park_raster/{parks[i]}/{parks[i]}_site.rds")) 
bird_sit <- read_rds(file = '/Users/bamaral/Library/Mobile Documents/com~apple~CloudDocs/Documents/GitHub/NPS_birds/data/park_raster/ELRO/ELRO_site.rds')
psit_sf <- bird_sit
sites_n <- dim(psit_sf)[1]
psit_sf <- st_as_sf(psit_sf)
psit_sf <- psit_sf %>% st_set_crs(st_crs(geom))