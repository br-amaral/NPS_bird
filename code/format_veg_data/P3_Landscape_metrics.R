# *****************************************
# ----------- Landscape metrics -----------
# *****************************************
# Script to get the park buffer areas with forest class and calculate landscape metrics

# input:    - data/src/sites_park_tib.rds: tibble with parks, number of sites and site numbers
#           - :
# output:   - data/park_raster/{parks[k]}/{name3}_metrics_clu.rds:
#           - data/park_raster/{parks[k]}/{name3}_metrics_core.rds:
#           - data/park_raster/{parks[k]}/{name3}_metrics_area.rds:


# load packages -------------------------------
library(conflicted)
library(dplyr)
library(ggplot2)
library(raster)
library(sp)
library(rgdal)
library(rgeos)
library(maptools)
library(landscapemetrics)
library(landscapetools)
library(FedData)
library(data.table)
library("landscapemetrics")
library("landscapetools")
library("remotes")
library(prettymapr)
library(cowplot)
library(dismo)
library(tidyverse)
library(glue)

rm(list = ls(all.names = TRUE))

conflicts_prefer(dplyr::filter)

# set up working directory
#setwd("~/Documents/GitHub/NPS_birds/")

sites_park_tib <- read_rds(file = "data/src/sites_park_tib.rds")
ext_b <- "park"

# Choose buffer extends and if the buffer is around the park area or the sites
# ext_b <- "park"    ;     ext_b <- "site"
if (ext_b == "site") {
  buffers <- c(50, 150)  ## distance is in meters!!!
}

if (ext_b == "park") {
  buffers <- c(100, 500, 1000, 2000)  ## distance is in meters!!!
}

buffers_n <- gsub("\\+", "", as.character(buffers))

# set up code for parks, buffers and years
parks <- readRDS(file = "data/src/key_park.rds") %>% 
  dplyr::select(parks) %>% 
  distinct() %>% 
  pull()

parks <- sort(parks)
#parks <- parks[-1]

years <- years_sat <- c(#2004, 2006, 2008, 2011, 2013, 2016, 
                        2019)

# load forest layer data for parks or sites ----------------------------
## park level area covariates ------------------------------------------

if(ext_b == "park") { 
  arr_clu_park <- array(NA, 
                        dim = c(length(parks),                    # park
                                length(years_sat),                # years
                                length(buffers)),                 # buffer
                        dimnames = list(parks,
                                        as.character(years_sat),
                                        buffers_n)) 
  
  arr_area_park <- arr_core_park <- arr_areap_park <- arr_clu_park
  for(i in 1:length(parks)){
    print(parks[i])
    for(j in 1:length(years)){
      print(years[j])
      for(b in 1:length(buffers)){
        
        for_b <- read_rds(file = glue("data/park_raster/{parks[i]}/{parks[i]}{years[j]}_land_buf{buffers_n[b]}_for_no_park.rds"))
        
        name1 <- glue("{parks[i]}{years[j]}_buf{b}_park")
        
        for_metrics_clu <- lsm_c_clumpy(for_b)
        for_metrics_core <- lsm_c_tca(for_b)
        for_metrics_area <- lsm_c_ca(for_b)
        
        write_rds(for_metrics_clu, file = glue("data/park_raster/{parks[i]}/{name1}_metrics_clu_{length(list.files(path = file.path(getwd(),'/data/park_raster'),pattern = glue('{name1}_metrics_clu'), full.names = FALSE)) + 1}.rds"))
        write_rds(for_metrics_core, file = glue("data/park_raster/{parks[i]}/{name1}_metrics_core_{length(list.files(path = file.path(getwd(),'/data/park_raster'),pattern = glue('{name1}_metrics_core'), full.names = FALSE)) + 1}.rds"))
        write_rds(for_metrics_area, file = glue("data/park_raster/{parks[i]}/{name1}_metrics_area_{length(list.files(path = file.path(getwd(),'/data/park_raster'),pattern = glue('{name1}_metrics_area'), full.names = FALSE)) + 1}.rds"))
        
        clu <- for_metrics_clu %>% 
          filter(class == 1) %>% 
          dplyr::select(value) %>% 
          pull() %>% 
          as.numeric()
        
        core <- for_metrics_core  %>% 
          filter(class == 1) %>% 
          dplyr::select(value) %>% 
          pull() %>% 
          as.numeric()
        
        area_o <- for_metrics_area  %>% 
          filter(class == 1) %>% 
          dplyr::select(value) %>% 
          pull() %>% 
          as.numeric()
        
        area_o_zero <- for_metrics_area  %>% 
          filter(class == 0) %>% 
          dplyr::select(value) %>% 
          pull() %>% 
          as.numeric()
        
        if(length(area_o)==0) {area_o <- 0}
        if(length(area_o_zero)==0) {area_o_zero <- 0}
        
        area_o2 <- area_o/(area_o_zero+area_o)
        
        if(length(clu)==0) {clu <- NA}
        if(length(core)==0) {core <- NA}
        if(length(area_o)==0) {area_o <- 0}
        if(length(area_o2)==0) {area_o2 <- 0}
        
        arr_clu_park[i,j,b] <- clu
        arr_core_park[i,j,b] <- core
        arr_area_park[i,j,b] <- area_o
        arr_areap_park[i,j,b] <- area_o2
      
      }
    }
  }
  write_rds(arr_clu_park, file = "data/park_raster/arr_clu_park.rds")
  write_rds(arr_core_park, file = "data/park_raster/arr_core_park.rds")
  write_rds(arr_area_park, file = "data/park_raster/arr_area_park.rds")
  write_rds(arr_areap_park, file = "data/park_raster/arr_areap_park.rds")
}

# write_rds(arr_clu_park, file = glue("data/park_raster/arr_clu_park_{length(list.files(path = file.path(getwd(),'/data/park_raster'),pattern = glue('arr_clu_park'), full.names = FALSE)) + 1}.rds"))
# write_rds(arr_core_park, file = glue("data/park_raster/arr_core_park_{length(list.files(path = file.path(getwd(),'/data/park_raster'),pattern = glue('arr_core_park'), full.names = FALSE)) + 1}.rds"))
# write_rds(arr_area_park, file = glue("data/park_raster/arr_area_park_{length(list.files(path = file.path(getwd(),'/data/park_raster'),pattern = glue('arr_area_park'), full.names = FALSE)) + 1}.rds"))
# write_rds(arr_areap_park, file = glue("data/park_raster/arr_areap_park_{length(list.files(path = file.path(getwd(),'/data/park_raster'),pattern = glue('arr_areap_park'), full.names = FALSE)) + 1}.rds"))




## site level area covariates ---------------------------------

if(ext_b == "site") { 
  arr_clu_site <- array(NA,
                        dim = c(nrow(sites_park_tib),        # park
                                length(years_sat),           # years
                                length(buffers),             # buffer
                                max(sites_park_tib$nsite)),  # site
                        dimnames = list(sites_park_tib$park,
                                        as.character(years_sat),
                                        buffers_n,
                                        as.character(seq(1:max(sites_park_tib$nsite))) # site
                        ))
  
  arr_area_site <- arr_core_site <- arr_areap_site <- arr_clu_site 
  
  for(i in 1:length(parks)) {              # park I
    print(parks[i])
    for(j in 1:length(years_sat)){         # years J
      print(years[j])
      for(b in 1:length(buffers)){         # buffer B
        max_p <- sites_park_tib$nsite[i]
        for(s in 1:max_p){                 # site S
          
          # i <- 4 ; j <- b <- s <- 1
          s2 <- as.character(s)
          if(nchar(s2) < 2){s2 <- glue("0{s2}")}
          for_b <- read_rds(file = glue("data/park_raster/{parks[i]}/{parks[i]}{years_sat[j]}_land_buf{buffers_n[b]}_for_no_site{s2}.rds"))
          
          name1 <- glue("{parks[i]}{years_sat[j]}_buf{b}_site{s2}")
          
          for_metrics_clu <- lsm_c_clumpy(for_b)
          for_metrics_core <- lsm_c_tca(for_b)
          for_metrics_area <- lsm_c_ca(for_b)
          
            clu <- for_metrics_clu %>% 
            filter(class == 1) %>% 
            dplyr::select(value) %>% 
            pull() %>% 
            as.numeric()
          
          core <- for_metrics_core  %>% 
            filter(class == 1) %>% 
            dplyr::select(value) %>% 
            pull() %>% 
            as.numeric()
          
          area_o <- for_metrics_area  %>% 
            filter(class == 1) %>% 
            dplyr::select(value) %>% 
            pull() %>% 
            as.numeric()
          
          area_o_zero <- for_metrics_area  %>% 
            filter(class == 0) %>% 
            dplyr::select(value) %>% 
            pull() %>% 
            as.numeric()
          
          if(length(area_o)==0) {area_o <- 0}
          if(length(area_o_zero)==0) {area_o_zero <- 0}
          
          area_o2 <- area_o/(area_o_zero + area_o)
          
          if(length(clu) != 0) {arr_clu_site[i,j,b,s] <- clu}
          if(length(core) != 0) {arr_core_site[i,j,b,s] <- core}
          if(length(area_o) != 0) {arr_area_site[i,j,b,s] <- area_o}
          if(length(area_o2) != 0) {arr_areap_site[i,j,b,s] <- area_o2}
          
        }
      }
    }
  }
  write_rds(arr_clu_site, file = "data/park_raster/arr_clu_site_.rds")
  write_rds(arr_core_site, file = "data/park_raster/arr_core_site.rds")
  write_rds(arr_area_site, file = "data/park_raster/arr_area_site.rds")
  write_rds(arr_areap_site, file = "data/park_raster/arr_areap_site.rds")
}

# write_rds(arr_clu_site, file = glue("data/park_raster/arr_clu_site_{length(list.files(path = file.path(getwd(),'/data/park_raster'),pattern = glue('arr_clu_site'), full.names = FALSE)) + 1}.rds"))
# write_rds(arr_core_site, file = glue("data/park_raster/arr_core_site_{length(list.files(path = file.path(getwd(),'/data/park_raster'),pattern = glue('arr_core_site'), full.names = FALSE)) + 1}.rds"))
# write_rds(arr_area_site, file = glue("data/park_raster/arr_area_site_{length(list.files(path = file.path(getwd(),'/data/park_raster'),pattern = glue('arr_area_site'), full.names = FALSE)) + 1}.rds"))
# write_rds(arr_areap_site, file = glue("data/park_raster/arr_areap_site_{length(list.files(path = file.path(getwd(),'/data/park_raster'),pattern = glue('arr_areap_site'), full.names = FALSE)) + 1}.rds"))








