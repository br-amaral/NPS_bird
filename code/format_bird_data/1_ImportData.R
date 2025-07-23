# 1_ImportDara.R  ------------------------------------------------------------
# Script to import the NPS database using the NCRN package and the files on my computer

# INPUT
# data is in the data/source folder, and within that that is a folder with different spans
#   of when the data was collected, i.e. 2020 is data from begining until 2020
# NCRNbirds package requires the following data files to import your data:
#   data/NETN/FieldData.csv: Raw bird detection data
#   data/NETN/Visits.csv: List of unique surveys by date for each point count station (events)
#   data/NETN/Points.csv: List of point count station names and locations
#   data/NETN/XXXXbands.csv: List of distance bands used during point count surveys for a particular network. 
#      ‘XXXX’ refers to the 4 letter network acronym.
#   data/NETN/XXXXintervals.csv: List of time intervals used during point count surveys for a particular network. 
#      ‘XXXX’ refers to the 4 letter network acronym.
#   data/NETN/BirdSpecies.csv: Species taxonomy table
#   data/NETN/BirdGuildAssignments.csv: Bird Guild Table

# OUTPUT:
#   data/NETNtib.rds: single master tibble with all data imported using the NCRN package
#   data/key_park.rds: tibble with infor about park, netwrok and id

## Load packages -------------------------------------------------------------------------------- 
library(tidyverse)
library(NCRNbirds)
library(lubridate)
library(glue)

## import data ----------------------------------------------------------------------------------
data_year <- "NETN_2020"
NETN <- getAKNData(Dir =glue("data/src/original/{data_year}"), import= TRUE) 

colnmaes <- colnames

## Explore data formats -------------------------------------------------------------------------
class(NETN)
length(NETN)

## Key table with network, park name, years sampled ---------------------------------------------------
parks <- NA
for(i in 1:length(NETN)){
  parks <- c(parks, NETN[[i]]@ParkCode)
}
parks <- parks[!is.na(parks)]

network <- NA
for(i in 1:length(NETN)){
  network <- c(network, NETN[[i]]@Network)
}
network <- network[-1]

park_long <- NA
for(i in 1:length(NETN)){
  park_long <- c(park_long, NETN[[i]]@LongName)
}
park_long <- park_long[-1]

key_tib <- as_tibble(cbind(network, parks, park_long)) %>% 
  mutate(id = row_number())

## create a master tibble --------------------------------------------------------------------------
NETNtib <- as_tibble(matrix(ncol = 12, nrow = nrow(key_tib)))
colnames(NETNtib) <- c('park_code', 'p_short_name', 'p_long_name', 'network', 'visit_number',
                       'field_data', 'visits', 'points', 'bands', 'intervals', 'bird_species','bird_guild')
NETNtib$park_code <- key_tib$parks

## function to populate the tibble --------------------------------------------------------------
for(i in 1:nrow(key_tib)){
  
  NETNobj <- NETN[[i]]
  
  (park_name <- NETNobj@ParkCode)
  
  numb <- key_tib %>% 
    filter(parks == park_name) %>% 
    dplyr::select(id) %>% 
    pull()
  
  # Split into 5 separate assignments with NA handling
  NETNtib$park_code[numb] <- ifelse(is.null(NETNobj@ParkCode) || length(NETNobj@ParkCode) == 0, NA, NETNobj@ParkCode)
  
  NETNtib$p_short_name[numb] <- ifelse(is.null(NETNobj@ShortName) || length(NETNobj@ShortName) == 0, NA, NETNobj@ShortName)
  
  NETNtib$p_long_name[numb] <- ifelse(is.null(NETNobj@LongName) || length(NETNobj@LongName) == 0, NA, gsub(" ", "_", NETNobj@LongName))
  
  NETNtib$network[numb] <- ifelse(is.null(NETNobj@Network) || length(NETNobj@Network) == 0, NA, NETNobj@Network)
  
  NETNtib$visit_number[numb] <- ifelse(is.null(NETNobj@VisitNumber) || length(NETNobj@VisitNumber) == 0, NA, NETNobj@VisitNumber)
  
  # Handle list columns with NA for empty objects
  NETNtib$field_data[numb] <- if(is.null(NETNobj@Birds) || nrow(NETNobj@Birds) == 0) {
    list(NA)
  } else {
    forest_birds <- NETNobj@Birds %>% filter(Survey_Type == "Forest")
    if(nrow(forest_birds) == 0) list(NA) else list(forest_birds)
  }
  
  NETNtib$visits[numb] <- if(is.null(NETNobj@Visits) || length(NETNobj@Visits) == 0) {
    list(NA)
  } else {
    list(NETNobj@Visits)
  }
  
  NETNtib$points[numb] <- if(is.null(NETNobj@Points) || length(NETNobj@Points) == 0) {
    list(NA)
  } else {
    list(NETNobj@Points)
  }
  
  NETNtib$bands[numb] <- if(is.null(NETNobj@Bands) || length(NETNobj@Bands) == 0) {
    list(NA)
  } else {
    list(NETNobj@Bands)
  }
  
  NETNtib$intervals[numb] <- if(is.null(NETNobj@Intervals) || length(NETNobj@Intervals) == 0) {
    list(NA)
  } else {
    list(NETNobj@Intervals)
  }
  
  NETNtib$bird_species[numb] <- if(is.null(NETNobj@Species) || length(NETNobj@Species) == 0) {
    list(NA)
  } else {
    list(NETNobj@Species)
  }
  
  NETNtib$bird_guild[numb] <- if(is.null(NETNobj@Guilds) || length(NETNobj@Guilds) == 0) {
    list(NA)
  } else {
    list(NETNobj@Guilds)
  }
}

## Check tibble and export as an RDS file-------------------------------------------------
NETNtib <- NETNtib %>% 
            filter(park_code != "ROVA")
key_tib <- key_tib %>% 
            filter(parks != "ROVA") %>% 
            mutate(id = row_number())

write_rds(NETNtib, file = "data/out/NETNtib.rds")
write_rds(key_tib, file = "data/key_park.rds")






