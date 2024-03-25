# P2_park_buffer
# script to load parker raster files and park boundaries, save those as .rds files to use to create the buffer
# https://www.mrlc.gov/data/legends/national-land-cover-database-class-legend-and-description
#
# Input:  - data/src/key_park.rds: tibble with the park names
#
#
#
# Output: -  data/park_raster/{parks[i]}/{parks[i]}{years[j]}_land{buffers_n[b]}_for_no.rds: raster file for one park of a different
#                                                                                                buffer size with only forest no forest habitat
#
#


# load packages -------------------------------
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

# set up working directory
#setwd("~/Documents/GitHub/NPS_birds/")
rm(list = ls())

# Choose buffer extends and if the buffer is around the park area or the sites
# ext_b <- "park"    ;     
ext_b <- "park"

if (ext_b == "site") {
  buffers <- c(50, 150, 300)  ## distance is in meters!!!
}

if (ext_b == "park") {
  buffers <- c(100, 1000, 2000)  ## distance is in meters!!!
}

buffers_n <- gsub("\\+", "", as.character(buffers))

# load park and year info
parks <- readRDS(file = "data/src/key_park.rds") %>% 
  dplyr::select(parks) %>% 
  distinct() %>% 
  pull()

parks <- sort(parks)

years <- c(2019)

nsite_pk <- read_csv(file = "data/nsite_pk.csv") %>% 
  pull() %>% 
  as.vector()

# load park boundary and raster files for different years land cover and land use change index
for(i in 1:length(parks)){
  name1 <- glue("{parks[i]}_ci")
  name3 <- glue("{parks[i]}_pb")
  
  xci_rp <- read_rds(file = glue("data/park_raster/{parks[i]}/{name1}.rds"))
  assign(name1, xci_rp)
  
  pb <- read_rds(file = glue("data/park_raster/{parks[i]}/{name3}.rds"))
  assign(name3, pb)
  
  for(j in 1:length(years)){
    name2 <- glue("{parks[i]}_lc_{years[j]}")
    xlc_rp <- read_rds(file = glue("data/park_raster/{parks[i]}/{name2}.rds"))
    assign(name2, xlc_rp)
  }
}

# ***************************************************
# ------ Create buffer around parks or sites --------
# ***************************************************
# Create a buffer around every park to crop the land use map accordingly
# not cropping anything yet, JUST creating the buffer

#sf_use_s2(FALSE)

# choose extent - park boundary or site coordinates (moved up on the script)
#    ext_b <- "park"    ;     ext_b <- "site"
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
        buf <- suppressMessages(sf::st_buffer(psit_sf[s,], dist = dist_buf[b]#, warnings = FALSE
                                              )) %>%  #buffers[b]) %>% 
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

# *****************************************
# - Crop the different year and park maps -
# *****************************************

lcc.proj <- CRS("+proj=lcc +lat_0=41.75 +lon_0=-120.5 +lat_1=43 +lat_2=45.5 +x_0=400000 +y_0=0 +datum=NAD83 +units=ft +no_defs")

# park area
if(ext_b == "park") { 
  for(i in 1:length(parks)) {
    print(parks[i])
    for(j in 1:length(years)){
      print(years[j])
      
      name2 <- glue("{parks[i]}_lc_{years[j]}")
      path_import2 <- glue("data/park_raster/{parks[i]}")
      name_import2 <- glue("NLCD_{years[j]}_Land_Cover")
      xlc <- list.files(             # find respective files
        path = path_import2,
        pattern = name_import2,          # regex
        full.names = TRUE)
      xlc_r <- raster(xlc) 
      park_map_year <- projectRaster(xlc_r, crs = CRS("+proj=longlat +datum=NAD83 +no_defs"))
      
      for(b in 1:length(buffers)){
        buf <- get(glue("{parks[i]}_buf{buffers_n[b]}"))
        print(glue("{parks[i]}_buf{buffers_n[b]}"))
        # land cover
        # if(parks[i] == "HOFR" & years[j] == 2011) {
        #   # for some reason some specific raster do not work on loop
        #   hofr_2011_lc <- raster("data/park_raster/HOFR/NLCD_2011_Land_Cover_L48_20210604_FXrB2kVAzgyCFr4fLALu.tiff")
        #   hofr_2011_lc <- projectRaster(hofr_2011_lc,
        #                                 crs = CRS("+proj=longlat +datum=NAD83 +no_defs"))
        #   park_map_year <- hofr_2011_lc
        # } else if (parks[i] == "MABI" & years[j] == 2016){
        #   # for some reason mabi_2016_lc does not work on loop
        #   mabi_2016_lc <- raster("data/park_raster/MABI/NLCD_2016_Land_Cover_L48_20210604_NQqfiy9JRFKBGlEHX0pL.tiff")
        #   mabi_2016_lc <- projectRaster(mabi_2016_lc,
        #                                 crs = CRS("+proj=longlat +datum=NAD83 +no_defs"))
        #   park_map_year <- mabi_2016_lc
        # } else { 
        #   park_map_year <- readRDS(file = glue("data/park_raster/{parks[i]}/{parks[i]}_lc_{years[j]}.rds"))
        # }
        
        buf_ext <- raster::extent(st_as_sf(buf))
        int_c <- crop(park_map_year, buf_ext) 
        int <- mask(int_c, st_as_sf(buf)) 
        
        write_rds(int, file = glue("data/park_raster/{parks[i]}/{parks[i]}{years[j]}_land_buf{buffers_n[b]}_park.rds"))
        
        # forest non-forest data
        int2 <- int
        raster::values(int2) <- raster::values(int2) %>% round(digits = 0)
        # selecting only forest
        int2[int2 < 41] <- 0 # make everything below 41 zero
        int2[int2 > 43] <- 0 # make everything above 43 zero
        int2[int2 == 41 | int2 == 42 | int2 == 43] <- 1
        
        write_rds(int2, file = glue("data/park_raster/{parks[i]}/{parks[i]}{years[j]}_land_buf{buffers_n[b]}_park_int2.rds"))
        
        # resolution on x and y is different
        # raster::res(hood_for)
        int3 <- projectRaster(int2, crs = lcc.proj, res=c(50,50))
        int3[int3 > 0.5] <- 1
        int3[int3 < 0.5] <- 0
        write_rds(int3, file = glue("data/park_raster/{parks[i]}/{parks[i]}{years[j]}_land_buf{buffers_n[b]}_for_no_park.rds"))
      } 
    }
  }
}

# for site
if(ext_b == "site") {
  for(i in 1:length(parks)) {
    print(parks[i])
    for(j in 1:length(years)){
      print(years[j])
      
      # from P1
      name2 <- glue("{parks[i]}_lc_{years[j]}")
      path_import2 <- glue("data/park_raster/{parks[i]}")
      name_import2 <- glue("NLCD_{years[j]}_Land_Cover")
      xlc <- list.files(             # find respective files
        path = path_import2,
        pattern = name_import2,          # regex
        full.names = TRUE)
      xlc_r <- raster(xlc) 
      park_map_year <- projectRaster(xlc_r, crs = CRS("+proj=longlat +datum=NAD83 +no_defs"))
      
      for(b in 1:length(buffers)){
        max_p <- nsite_pk[i]
        for(s in 1:max_p){
          # i <- 4 ; j <- b <- s <- 1
          s2 <- as.character(s)
          if(nchar(s2) < 2){s2 <- glue("0{s2}")}
          
          buf <- get(glue("{parks[i]}_buf{buffers_n[b]}_site{s2}"))
          
          # # land cover
          # if(parks[i] == "HOFR" & years[j] == 2011) {
          #   # for some reason some specific raster do not work on loop
          #   hofr_2011_lc <- raster("data/park_raster/HOFR/NLCD_2011_Land_Cover_L48_20210604_FXrB2kVAzgyCFr4fLALu.tiff")
          #   hofr_2011_lc <- projectRaster(hofr_2011_lc,
          #                                 crs = CRS("+proj=longlat +datum=NAD83 +no_defs"))
          #   park_map_year <- hofr_2011_lc
          # } else if (parks[i] == "MABI" & years[j] == 2016){
          #   # for some reason mabi_2016_lc does not work on loop
          #   mabi_2016_lc <- raster("data/park_raster/MABI/NLCD_2016_Land_Cover_L48_20210604_NQqfiy9JRFKBGlEHX0pL.tiff")
          #   mabi_2016_lc <- projectRaster(mabi_2016_lc,
          #                                 crs = CRS("+proj=longlat +datum=NAD83 +no_defs"))
          #   park_map_year <- mabi_2016_lc
          # } else { 
          #   park_map_year <- readRDS(file = glue("data/park_raster/{parks[i]}/{parks[i]}_lc_{years[j]}.rds"))
          # }
          
          ## original
          buf_ext <- raster::extent(st_as_sf(buf))
          int_c <- crop(park_map_year, buf_ext) 
          int <- mask(int_c, st_as_sf(buf)) 
          
          write_rds(int, file = glue("data/park_raster/{parks[i]}/{parks[i]}{years[j]}_land_buf{buffers_n[b]}_site{s2}.rds"))
          
          # forest non-forest data
          int2 <- int
          raster::values(int2) <- raster::values(int2) %>% round(digits = 0)
          # selecting only forest (all forest types availabe are here)
          int2[int2 < 41] <- 0 # make everything below 41 zero
          int2[int2 > 43] <- 0 # make everything above 43 zero
          int2[int2 == 41 | int2 == 42 | int2 == 43] <- 1
          
          # resolution on x and y is different
          # raster::res(hood_for)
          int3 <- projectRaster(int2, crs = lcc.proj, res=c(50,50))
          int3[int3 > 0.5] <- 1
          int3[int3 < 0.5] <- 0
          write_rds(int3, file = glue("data/park_raster/{parks[i]}/{parks[i]}{years[j]}_land_buf{buffers_n[b]}_for_no_site{s2}.rds")) 
        }
      }
    }
  }
}

# plot(pb)
#plot(xci_rp, add = T)
int_poly <- rasterToPolygons(int)

plot(int2, legend=FALSE)
plot(pb, add = T, lwd=3)
psit_sf %>% plot(add = T, cex = 0.7, pch = 16, col = "blue")

plot(MABI_buf1000, add = T, border = "red")
plot(MABI_buf750,  add = T, border = "red")
plot(MABI_buf500,  add = T, border = "red")
plot(MABI_buf250,  add = T, border = "red")
plot(MABI_buf100,  add = T, border = "red")

r.spdf <- as(int2, "SpatialPixelsDataFrame")
r.df <- as.data.frame(r.spdf) %>% fortify()

colors <- c("white", "springgreen3")

ggplot(r.df, aes(x=x, y=y)) + 
  geom_tile(aes(fill = factor(Layer_1))) + 
  scale_fill_manual(values=colors) +
  coord_equal() +
  geom_polygon(data = pb, aes(long, lat, group=factor(group)), colour='black', fill= NA) +
  theme_bw() 

ggplot() +
  geom_stars(sf_as_st(MABI_buf1000) )

# let me think about that for a while
# guidelines - eat the meals separetly; noise, where to sleep
# this is what I wan to happen and this is not what i wan t o happen

  
