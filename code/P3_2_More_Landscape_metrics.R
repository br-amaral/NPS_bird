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

conflicts_prefer(dplyr::filter)
rm(list = ls(all.names = TRUE))

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

years <- years_sat <- c(#2004, 2006, 2008, 2011, 2013, 2016, 
  2019)

# load forest layer data for parks or sites ----------------------------
## park level area covariates ------------------------------------------

if(ext_b == "park") { 
  arr_cai_park <- array(NA, 
                        dim = c(length(parks),             # park
                                length(years_sat),                # years
                                length(buffers)),                 # buffer
                        dimnames = list(parks,
                                        as.character(years_sat),
                                        buffers_n)) 
  
  arr_coh_park <- arr_enn_park <- arr_ai_park <- arr_pafrac_park <-
    arr_nlsi_park <- arr_iji_park <- arr_con_park <- arr_tca_park <- 
    arr_te_park <- arr_np_park <- arr_cai_park
  for(i in 1:length(parks)){
    print(parks[i])
    for(j in 1:length(years)){
      print(years[j])
      for(b in 1:length(buffers)){
        
        for_b <- read_rds(file = glue("data/park_raster/{parks[i]}/{parks[i]}{years[j]}_land_buf{buffers_n[b]}_for_no_park.rds"))
        
        name1 <- glue("{parks[i]}{years[j]}_buf{b}_park")
        
        # CAI_MN - Mean Core Area Index (CORE AREA)
        for_metrics_cai <- lsm_l_cai_mn(for_b)
        
        # COHESION - Patch Cohesion Index
        for_metrics_coh <- lsm_l_cohesion(for_b)
        
        # ENN_MN - Mean Euclidean Nearest Neighbour Index
        for_metrics_enn <- lsm_l_enn_mn(for_b)
        
        # AI - Aggregation Index (CONTAGION/INTERSPERSION)
        for_metrics_ai <- lsm_l_ai(for_b)
        
        # nLSI - Normalized Landscape Shape Index (AREA/EDGE/DENSITY)
        for_metrics_nlsi <- lsm_c_nlsi(for_b)
        
        # PAFRAC - Perimeter Area Fractal Dimension (SHAPE)
        for_metrics_pafrac <- lsm_l_pafrac(for_b)
        
        # Interspersion and Juxtaposition index (percent)
        #for_metrics_iji <- lsm_l_iji(for_b)
        
        # Contagion index (percent)
        for_metrics_con <- lsm_l_contag(for_b)
        
        # Total core area (ha)
        for_metrics_tca <- lsm_l_tca(for_b)
        
        # Total edge (m)
        for_metrics_te <- lsm_c_te(for_b)
        
        # number of patches
        for_metrics_np <- lsm_l_np(for_b)
     
        write_rds(for_metrics_cai, file = glue("data/park_raster/{parks[i]}/{name1}_metrics_cai.rds"))
        write_rds(for_metrics_coh, file = glue("data/park_raster/{parks[i]}/{name1}_metrics_coh.rds"))
        write_rds(for_metrics_enn, file = glue("data/park_raster/{parks[i]}/{name1}_metrics_enn.rds"))
        write_rds(for_metrics_ai, file = glue("data/park_raster/{parks[i]}/{name1}_metrics_ai.rds"))
        write_rds(for_metrics_nlsi, file = glue("data/park_raster/{parks[i]}/{name1}_metrics_nlsi.rds"))
        write_rds(for_metrics_pafrac, file = glue("data/park_raster/{parks[i]}/{name1}_metrics_pafrac.rds"))
        #write_rds(for_metrics_iji, file = glue("data/park_raster/{parks[i]}/{name1}_metrics_iji.rds"))
        write_rds(for_metrics_con, file = glue("data/park_raster/{parks[i]}/{name1}_metrics_con.rds"))
        write_rds(for_metrics_tca, file = glue("data/park_raster/{parks[i]}/{name1}_metrics_tca.rds"))
        write_rds(for_metrics_te, file = glue("data/park_raster/{parks[i]}/{name1}_metrics_te.rds"))
        write_rds(for_metrics_np, file = glue("data/park_raster/{parks[i]}/{name1}_metrics_np.rds"))

        cai <- for_metrics_cai %>% 
          dplyr::select(value) %>% 
          pull() %>% 
          as.numeric()
        
        coh <- for_metrics_coh %>% 
          dplyr::select(value) %>% 
          pull() %>% 
          as.numeric()
        
        enn <- for_metrics_enn %>% 
          pull() %>% 
          as.numeric()
        
        ai <- for_metrics_ai %>% 
          dplyr::select(value) %>% 
          pull() %>% 
          as.numeric()
        
        nlsi <- for_metrics_nlsi %>% 
          filter(class == 1) %>% 
          dplyr::select(value) %>% 
          pull() %>% 
          as.numeric()
        
        pafrac <- for_metrics_pafrac %>% 
          dplyr::select(value) %>% 
          pull() %>% 
          as.numeric()
        
        # iji <-for_metrics_iji %>% 
        #   dplyr::select(value) %>% 
        #   pull() %>% 
        #   as.numeric()
        
        con <- for_metrics_con %>% 
          dplyr::select(value) %>% 
          pull() %>% 
          as.numeric()
        
        tca <- for_metrics_tca %>% 
          dplyr::select(value) %>% 
          pull() %>% 
          as.numeric()
        
        te <- for_metrics_te  %>% 
          filter(class == 1) %>% 
          dplyr::select(value) %>% 
          pull() %>% 
          as.numeric()
        
        np <- for_metrics_np %>% 
          dplyr::select(value) %>% 
          pull() %>% 
          as.numeric()
        
        if(length(cai)==0) {cai <- NA}
        if(length(coh)==0) {coh <- NA}
        if(length(enn)==0) {enn <- NA}
        if(length(ai)==0) {ai <- NA}
        if(length(nlsi)==0) {nlsi <- NA}
        if(length(pafrac)==0) {pafrac <- NA}
        #if(length(iji)==0) {iji <- NA}
        if(length(con)==0) {con <- NA}
        if(length(tca)==0) {tca <- NA}
        if(length(te)==0) {te <- NA}
        if(length(np)==0) {np <- NA}

      
        arr_cai_park[i,j,b] <- cai
        arr_coh_park[i,j,b] <- coh
        arr_enn_park[i,j,b] <- enn
        arr_ai_park[i,j,b] <- ai
        arr_pafrac_park[i,j,b] <- pafrac
        arr_nlsi_park[i,j,b] <- nlsi        
        #arr_iji_park[i,j,b] <- iji
        arr_con_park[i,j,b] <- con
        arr_tca_park[i,j,b] <- tca        
        arr_te_park[i,j,b] <- te
        arr_np_park[i,j,b] <- np

      }
    }
  }
  write_rds(arr_cai_park, file = "data/park_raster/arr_cai_park.rds")
  write_rds(arr_coh_park, file = "data/park_raster/arr_coh_park.rds")
  write_rds(arr_enn_park, file = "data/park_raster/arr_enn_park.rds")
  write_rds(arr_ai_park, file = "data/park_raster/arr_ai_park.rds")
  write_rds(arr_pafrac_park, file = "data/park_raster/arr_pafrac_park.rds")
  write_rds(arr_nlsi_park, file = "data/park_raster/arr_nlsi_park.rds")
  #write_rds(arr_iji_park, file = "data/park_raster/arr_iji_park.rds")
  write_rds(arr_con_park, file = "data/park_raster/arr_con_park.rds")
  write_rds(arr_tca_park, file = "data/park_raster/arr_tca_park.rds")
  write_rds(arr_te_park, file = "data/park_raster/arr_te_park.rds")
  write_rds(arr_np_park, file = "data/park_raster/arr_np_park.rds")
}

## site level area covariates ---------------------------------

if(ext_b == "site") { 
  arr_cai_site <- array(NA,
                        dim = c(nrow(sites_park_tib),        # park
                                length(years_sat),           # years
                                length(buffers),             # buffer
                                max(sites_park_tib$nsite)),  # site
                        dimnames = list(sites_park_tib$park,
                                        as.character(years_sat),
                                        buffers_n,
                                        as.character(seq(1:max(sites_park_tib$nsite))) # site
                        ))
  
  arr_enn_site <- arr_ai_site <- arr_pafrac_site <- arr_nlsi_site <- arr_iji_site <- 
    arr_con_site <- arr_tca_site <- arr_te_site <- arr_np_site <- arr_coh_site <- arr_cai_site
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
          
          # CAI_MN - Mean Core Area Index (CORE AREA)
          cai <- lsm_l_cai_mn(for_b)

          # COHESION - Patch Cohesion Index
          coh <- lsm_l_cohesion(for_b)

          # ENN_MN - Mean Euclidian Nearest Neighbour Index
          enn <- lsm_l_enn_mn(for_b)

          # AI - Aggregation Index (CONTAGION/INTERSPERSION)
          ai <- lsm_l_ai(for_b)

          # nLSI - Normalized Landscape Shape Index (AREA/EDGE/DENSITY)
          nlsi <- lsm_c_nlsi(for_b)

          # PAFRAC - Perimeter Area Fractal Dimension (SHAPE)
          pafrac <- lsm_l_pafrac(for_b)

          # Interspersion and Juxtaposition index (percent)
          #iji <- lsm_l_iji(for_b)

          # Contagion index (percent)
          con <- lsm_l_contag(for_b)

          # Total core area (ha)
          tca <- lsm_l_tca(for_b)

          # Total edge (m)
          te <- lsm_c_te(for_b)

          # number of patches
          np <- lsm_l_np(for_b)

          if(length(cai)==0) {cai <- NA}
          if(length(coh)==0) {coh <- NA}
          if(length(enn)==0) {enn <- NA}
          if(length(ai)==0) {ai <- NA}
          if(length(nlsi)==0) {nlsi <- NA}
          if(length(pafrac)==0) {pafrac <- NA}
          #if(length(iji)==0) {iji <- NA}
          if(length(con)==0) {con <- NA}
          if(length(tca)==0) {tca <- NA}
          if(length(te)==0) {te <- NA}
          if(length(np)==0) {np <- NA}
          
          arr_cai_site[i,j,b,s] <- cai$value
          arr_coh_site[i,j,b,s] <- coh$value
          arr_enn_site[i,j,b,s] <- enn$value
          arr_ai_site[i,j,b,s] <- ai$value
          arr_pafrac_site[i,j,b,s] <- pafrac$value
          arr_nlsi_site[i,j,b,s] <- nlsi$value[2]       
          #arr_iji_site[i,j,b,s] <- iji$value
          arr_con_site[i,j,b,s] <- con$value
          arr_tca_site[i,j,b,s] <- tca$value       
          arr_te_site[i,j,b,s] <- te$value[2]
          arr_np_site[i,j,b,s] <- np$value 
        }
      }
    }
  }
  write_rds(arr_cai_site, file = "data/park_raster/arr_cai_site.rds")
  write_rds(arr_coh_site, file = "data/park_raster/arr_coh_site.rds")
  write_rds(arr_enn_site, file = "data/park_raster/arr_enn_site.rds")
  write_rds(arr_ai_site, file = "data/park_raster/arr_ai_site.rds")
  write_rds(arr_pafrac_site, file = "data/park_raster/arr_pafrac_site.rds")
  write_rds(arr_nlsi_site, file = "data/park_raster/arr_nlsi_site.rds")
  #write_rds(arr_iji_site, file = "data/park_raster/arr_iji_site.rds")
  write_rds(arr_con_site, file = "data/park_raster/arr_con_site.rds")
  write_rds(arr_tca_site, file = "data/park_raster/arr_tca_site.rds")
  write_rds(arr_te_site, file = "data/park_raster/arr_te_site.rds")
  write_rds(arr_np_site, file = "data/park_raster/arr_np_site.rds")
}

