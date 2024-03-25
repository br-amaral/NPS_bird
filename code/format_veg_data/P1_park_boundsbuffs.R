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
# library(prettymapr)
library(raster)
library(rgdal)
library(rgeos)
library(sf)
library(sp)
library(stars)
library(terra)
library(tidyverse)

# set up code for parks, buffers and years
parks <- readRDS(file = "data/src/key_park.rds") %>% 
  dplyr::select(parks) %>% 
  distinct() %>% 
  pull()

years <- c(2001,2004, 2006, 2008, 2011, 2013, 2016, 2019)

# Choose buffer extends and if the buffer is around the park area or the sites
# ext_b <- "park"    ;     
ext_b <- "site"

if (ext_b == "site") {
  buffers <- c(50, 150, 300)  ## distance is in meters!!!
}

if (ext_b == "park") {
  buffers <- c(100, 500, 1000, 2000)  ## distance is in meters!!!
}

buffers_n <- gsub("\\+", "", as.character(buffers))

nsite_pk <- read_csv(file = "data/nsite_pk.csv") %>% 
  pull() %>% 
  as.vector()

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

# park buffers
dist_buf <- buffers

for(i in 1:length(parks)){
  print(parks[i])
  
  if(ext_b == "park") {
    sf_use_s2(FALSE)
    geom <- st_as_sfc(get(glue("{parks[i]}_pb")))
    geom <- st_as_sf(geom)
    
    for(b in 1:length(buffers)){
      buf <- sf::st_buffer(geom, dist = buffers[b]/100000)
      name4 <- glue("{parks[i]}_buf{buffers_n[b]}")
      path_export3 <- glue("data/park_raster/{parks[i]}")
      assign(name4, buf)
      write_rds(buf, file = glue("{path_export3}/{name4}.rds"))
      print(name4)
    }
  }
  
  if(ext_b == "site") {
    
    geom <- st_as_sfc(get(glue("{parks[i]}_pb")))
    geom <- st_as_sf(geom)
    
    for(b in 1:length(buffers)){
      
      psit_sf <- read_rds(file = glue("data/park_raster/{parks[i]}/{parks[i]}_site.rds")) 
      sites_n <- dim(psit_sf)[1]
      psit_sf <- st_as_sf(psit_sf)
      psit_sf <- psit_sf %>% st_set_crs(st_crs(geom))
      
      for(s in 1:sites_n){
        buf <- suppressMessages(sf::st_buffer(psit_sf[s,], dist = dist_buf[b], warnings = FALSE)) %>%  #buffers[b]) %>% 
          st_as_sfc()
        s2 <- as.character(s)
        if(nchar(s2) < 2){s2 <- glue("0{s2}")}
        name4 <- glue("{parks[i]}_buf{buffers_n[b]}_site{s2}")
        path_export3 <- glue("data/park_raster/{parks[i]}")
        assign(name4, buf)
        #  plot(MABI_buf300_site1, col = "red", add = T)
        write_rds(buf, file = glue("{path_export3}/{name4}.rds"))
        print(name4)
      }
    }
  }
}


