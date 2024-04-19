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
parks <- parks[-1]

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
giant_tib <- function(NETNf){
  
  NETNobj <- NETNf
  
  park_name <- NETNobj@ParkCode
  
  numb <- key_tib %>% 
    filter(parks == park_name) %>% 
    dplyr::select(id) %>% 
    pull()
  
  NETNtib[numb,1:5] <<- c(NETNobj@ParkCode, NETNobj@ShortName, gsub(" ", "_", NETNobj@LongName), 
                          NETNobj@Network, NETNobj@VisitNumber) %>% 
    as.data.frame() %>% t()
  
  NETNtib$field_data[numb] <<- list(NETNobj@Birds)
  NETNtib$visits[numb] <<- list(NETNobj@Visits)
  NETNtib$points[numb] <<- list(NETNobj@Points)
  NETNtib$bands[numb] <<- list(NETNobj@Bands)
  NETNtib$intervals[numb] <<- list(NETNobj@Intervals)
  NETNtib$bird_species[numb] <<- list(NETNobj@Species)
  NETNtib$bird_guild[numb] <<- list(NETNobj@Guilds)
  
}

for(i in 1:nrow(key_tib)){
  giant_tib(NETN[[i]])
}

## Check tibble and export as an RDS file-------------------------------------------------
NETNtib

saveRDS(NETNtib, file = "data/out/NETNtib.rds")
saveRDS(key_tib, file = "data/key_park.rds")






