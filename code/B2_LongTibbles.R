# 2_LongTibbles.R
# Script to collapse all bird data (field_data) in one long tibble --------------------------

# INPUT
#   data/NETNtib.rds: single master tibble with all data imported using the NCRN package
#   data/key_park.rds: tibble with infor about park, netwrok and id


# OUTPUT:
#  

## Load packages -------------------------------------------------------------------------------- 
library(tidyverse)
library(lubridate)
library(glue)

## Import data -----------------------------------------
NETNtib <- read_rds(file = "data/NETNtib.rds")
kpark <- readRDS(file = "data/key_park.rds")

colnmaes <- colnames

## loop in key tibble put all bird data in same object
for(i in 1:nrow(NETNtib)) {
  ltib <- NETNtib$field_data[i][[1]]
  if(i == 1) {
    birdpoint <- ltib
  } else {
    birdpoint <- rbind(birdpoint, ltib)
  }
}

## short column names that easy to type ------------------------------------
# refer to NCRNBirds_vignette.html for details about columns
birdpoint <- birdpoint %>% 
  rename(park = Admin_Unit_Code,
         transect = Transect_Name,
         point = Point_Name,
         date = EventDate,
         visit = Visit,
         year = Year,
         species = AOU_Code,
         count = Bird_Count,
         sps_sci = Scientific_Name,
         sps_com = Common_Name,
         interval = Interval,                # ?
         interval_len = Interval_Length,     # ?
         id_met_id = ID_Method_Code,         # A=Audio, V=Visual, B=Both, F=Flyover
         id_met = ID_Method,
         dist_id = Distance_id,              # ? Distance band code
         distance = Distance,                # ? Distance band of detection
         flyover = Flyover_Observed,         # 0=No; 1=Yes
         init3min = Initial_Three_Min_Cnt,   # 0=No; 1=Yes
         sur_typ = Survey_Type,              # Forest, Grassland
         data_sta = Data.Status,
         poin_note = Point.Note,
         obs = Observer,                     # Unique code to identify observer
         ski_lev = Skill_Level,              # 
         ski_note = Skill_Notes
         ) %>% 
  relocate(year, .after = date)# %>% 
  # mutate(julian_d = as.numeric(format(date, "%j")),
  #       month = as.numeric(format(date, "%m")))


## look at some general patterns! --------------------------------------------

# diversity changing with time

# abundance changing with time

# forest height - affects diversity (focus on warblers) - height is more important locally, and sth else regionally

# birds that are being highly affected by forest pests (habitat specialists)

# look how migrants (long distance) abundance]

# generalist are early succession birds?

## turnover, invasions, matrix and park size

# Lessons from Doser:
##  - trends similar between guilds, different between parks:
##      -- "ecological processes, biological invasions, and management activities that affect local forest
##         condition appear to have consistent effects on local forest bird communities"
##      -- "life-history characteristics, including diet, foraging strategy, habitat preference, and nesting location, do not predict the
##         effects of climate change on bird species distributions in northeastern North America responses of bird species to stressors 
##         such as climate change might be independent of life-history traits" hhmmmmmmmmmmmmmm
##      -- This result potentially suggests communities of birds within these parks might respond differently over time as a result of differences in
##         **local environmental stressors** and **interactions with other species**
##      -- future analyses and management efforts should focus on declining specialist guilds in these three parks that require the interior and 
##         older forest habitat these parks are designed to protect
##      -- habitat surrounding the park - make sure that park by itself would be able to sustain, "[birds are] likely affected by interactions 
##         between local forest structure, surrounding land use, local community interactions, climate, and local stand dynamics (e.g., pest
##         outbreaks, disturbances, succession"; "Overall, these results suggest management should focus on limiting forest
##         fragmentation and maintaining or increasing the amount of forested cover in the surrounding landscape
##         matrix, followed by maintenance of forest structure and diversity."

## BUT: guilds dont have a counterpart (temperate migrants VS ???)
## 

# forest variables and bird abundance (specially MABI, ACAD and MORR that are declining in Doser 2021) 
#   forest/tree structural stage and diversity

# abundance peak with area of forest and regeneration - is regeneration associated with increase, but not necessarily peak?

# turnover rate with regeneration?

# turnover showing how species might be moving ranges northwise?


