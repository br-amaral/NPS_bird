# P1_park_covariates
# script to load raster files, get park area, define buffer and crop map
# P1_park_raster
# script to load parker raster files, park boundaries and site coordinates, save those as .rds files to use to create the buffer
#
# Input:  - park_raster/{parks[i]}/NLCD_{years[j]}_Land_Cover{...}.tiff: tiff downloaded from mrlc with land use data
#         - park_raster/nps_boundary/nps_boundary.shp: shape file with the boundaries of US parks
#         - data/src/key_park.rds: tibble with the park names
#
#
# Output: -  data/park_raster/{parks[i]}/{parks[i]}{years[j]}_land{buffers_n[b]}_for_no.rds: raster file for one park of a different
#                                                                                                buffer size with only forest no forest habitat
#
#

# one layer for each park in each year

# load packages -------------------------------
library(dplyr)
library(raster)
library(sp)
library(rgdal)
library(rgeos)
library(maptools)
library(FedData)
library(data.table)
library("landscapemetrics")
library("landscapetools")
library("remotes")
library(tidyverse)
library(stars)
library(sf)
library(glue)
library(terra)
library(prettymapr)
library(cowplot)
library(dismo)

# set up working directory
# setwd("~/Documents/GitHub/NPS_birds/")

# set up code for parks, buffers and years
parks <- readRDS(file = "data/src/key_park.rds") %>% 
  dplyr::select(parks) %>% 
  distinct() %>% 
  pull()

years <- c(2001,2004, 2006, 2008, 2011, 2013, 2016, 2019)

#buffers <- c(100, 250, 500, 750, 1000)
#buffers_n <- gsub("\\+", "", as.character(buffers))


# ******************************************
# ----------- Import raster maps -----------
# ******************************************
# Input:  - park_raster/{park_name}/NLCD_{year}_Land_Cover{...}.tiff - tiff downloaded from mrlc with land use data
# Output: - park_raster/{park_name}_{year}_lc.rds - park raster file
#
# Import raster from tiff files (land use for 2019 to 2004) downloaded in Nov 2022 from 
#  https://www.mrlc.gov/viewer/

for(i in 1:length(parks)){
  # land use change index from 2004 to 2019
  name1 <- glue("{parks[i]}_ci")
  path_import1 <- glue("data/park_raster/{parks[i]}")
  name_import1 <- glue("NLCD_{years[1]}_{years[length(years)]}_change_index")
  xci <- list.files(             # find respective files
    path = path_import1,
    pattern = name_import1,          # regex
    full.names = TRUE
  )
  xci_r <- raster(xci) 
  xci_rp <- projectRaster(xci_r, crs = CRS("+proj=longlat +datum=NAD83 +no_defs"))
  assign(name1, xci_rp)
  write_rds(xci_rp, file = glue("data/park_raster/{parks[i]}/{name1}.rds"))
  rm(xci, xci_r, xci_rp)
  print(name_import1)
  
  for(j in 1:length(years)){
    
    skip_to_next <- FALSE
    
    tryCatch(
      {# land cover from 2004 to 2019
        name2 <- glue("{parks[i]}_lc_{years[j]}")
        path_import2 <- glue("data/park_raster/{parks[i]}")
        name_import2 <- glue("NLCD_{years[j]}_Land_Cover")
        xlc <- list.files(             # find respective files
          path = path_import2,
          pattern = name_import2,          # regex
          full.names = TRUE)
        xlc_r <- raster(xlc) 
        xlc_rp <- projectRaster(xlc_r, crs = CRS("+proj=longlat +datum=NAD83 +no_defs"))
        assign(name2, xlc_rp)
        write_rds(xlc_rp, file = glue("data/park_raster/{parks[i]}/{name2}.rds"))
        rm(xlc, xlc_r, xlc_rp)
        print(name_import2)}, 
      error = function(e) { skip_to_next <<- TRUE})
    
    if(skip_to_next) { next } 
    
  }
}

# ******************************************
# ---------- Get park area maps ------------
# ******************************************

## import nps park area maps -----------------------------------------------------------------
# park_bound <- readOGR(file.path("data/park_raster/nps_boundary/nps_boundary.shp")) #st_read("park_raster/nps_boundary/nps_boundary.shp")
park_bound <- readRDS("data/out/park_bound.rds")

for(i in 1:length(parks)){
  pb <- subset(park_bound, UNIT_CODE == parks[i])
  name3 <- glue("{parks[i]}_pb")
  path_export3 <- glue("data/park_raster/{parks[i]}")
  assign(name3, pb)
  write_rds(pb, file = glue("{path_export3}/{name3}.rds"))
  print(name3)
  rm(name3, path_export3, pb)
}

# MABI_pb <- readRDS(file = glue("{path_export3}/{name3}.rds"))

## get site coordinates ------------------------------------------------------------------------

NPS_DATA_PATH <- file.path("~/Documents/GitHub/NPS_birds/data/src/NETNtib.rds")
bdat <- read_rds(file = NPS_DATA_PATH)

for(i in 1:length(bdat$points)){
  if(i == 1){
    coords_sit <- cbind(bdat$points[[i]]$Point_Name,
                        bdat$points[[i]]$Latitude,
                        bdat$points[[i]]$Longitude) %>% as_tibble()
  } else {
    coords_sit <- rbind(coords_sit,
                        cbind(bdat$points[[i]]$Point_Name,
                              bdat$points[[i]]$Latitude,
                              bdat$points[[i]]$Longitude))
  }
}

colnames(coords_sit) <- c("Point_Name", "Latitude", "Longitude")

nrow(coords_sit)
coords_sit <- distinct(coords_sit)
nrow(coords_sit)

for(i in 1:length(parks)){
  psit <- coords_sit %>% 
    filter(substr(Point_Name, 1, 4) == parks[i])
  psit_sf <- st_as_sf(psit[,2:3], coords = c("Longitude", "Latitude"))
  write_rds(psit_sf, file = glue("data/park_raster/{parks[i]}/{parks[i]}_site.rds"))
  rm(psit, psit_sf)
}

# MABI_lc_2019 %>% plot(xlim = c(-72.57, -72.5), ylim = c(43.62, 43.65)) ; plot(MABI_pb, add = T) ; psit_sf %>% plot(add = T, size = 1)

