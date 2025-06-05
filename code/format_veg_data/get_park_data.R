#? *********************************************************************************
#? ------------------------------   get_park_data.R   ------------------------------
#? *********************************************************************************
# Code to get the environmental variables at the park level
#
#! Input ----------------------------------------------
#           - data/out/site_covs.rds :
#
#! Output ----------------------------------------------
#           - data/out/park_covs.rds : tibble with park level environmental variables
#
# detach packages and clear workspace
if(!require(freshr)){install.packages('freshr')}
freshr::freshr()
#
#! Load packages ---------------------------------------
library(conflicted)
library(tidyverse)
library(forestNETN)
library(glue)
#
conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::filter)
# conflicts_prefer(scales::alpha)
#
#! Make functions --------------------------------------
colanmes <- colnames
lenght <- length
`%!in%` <- Negate(`%in%`)

Modes <- function(x) {
  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  if(length(ux[tab == max(tab)]) > 1) {
    sample(ux[tab == max(tab)], 1)
    } else {
      ux[tab == max(tab)]}
}

#! Import data -----------------------------------------
## file paths

FORCOVS_PARK_PATH <- "data/veg_kateaaron/NETN_forest_data_2006-2023.rds"
VAMA_PARK_PATH <- "data/VAMA_sites.rds"
HOFR_PARK_PATH <- "data/HOFR_sites.rds"
ELRO_PARK_PATH <- "data/ELRO_sites.rds"

## read files
## get site names for ROVA parks 
VAMA_sites <- read_rds(file = VAMA_PARK_PATH) %>% 
                select(park, for_sit)

HOFR_sites <- read_rds(file = HOFR_PARK_PATH) %>% 
                select(park, for_sit)

ELRO_sites <- read_rds(file = ELRO_PARK_PATH) %>% 
                select(park, for_sit)

ROVA_sites <- rbind(VAMA_sites, HOFR_sites, ELRO_sites)  %>% 
                rename(ParkUnit = park, Plot_Name = for_sit)

## all fost data with covariates but WRONG rova name
for_park <- read_rds(file = FORCOVS_PARK_PATH)

for(ii in 1:nrow(for_park)){
    for(jj in 1:nrow(ROVA_sites)){
      if(for_park$Plot_Name[ii] == ROVA_sites$Plot_Name[jj]) {
         for_park$ParkUnit[ii] <-  ROVA_sites$ParkUnit[jj]
      }
    }
}

for_park <- for_park %>%        
          filter(ParkUnit != "ROVA",
                 ParkUnit != "ELRO",
                 ParkUnit != "ACAD")   

## canopy cover ---------------------------------------------------------
path <- glue("{getwd()}/data/veg_kateaaron") 
importCSV(path, zip_name = "NETN_Forest_20231106.zip")
can <- forestNETN::joinStandData(park = "all") %>%
          as_tibble() 

for(ii in 1:nrow(can)){
    for(jj in 1:nrow(ROVA_sites)){
      if(can$Plot_Name[ii] == ROVA_sites$Plot_Name[jj]) {
         can$ParkUnit[ii] <-  ROVA_sites$ParkUnit[jj]
      }
    }
}

can <- can %>%        
          filter(ParkUnit != "ROVA") %>%   
          select(ParkUnit, Pct_Crown_Closure) %>% 
          group_by(ParkUnit) %>% 
          mutate(parkCAN = mean(Pct_Crown_Closure, na.rm = T)) %>% 
          ungroup() %>% 
          select(-Pct_Crown_Closure) %>% 
          distinct()

## wood debris ----------------------------------------------------------
cwd <- joinCWDData(park = 'all') %>% # coarse wood debris
          as_tibble() 

for(ii in 1:nrow(cwd)){
    for(jj in 1:nrow(ROVA_sites)){
      if(cwd$Plot_Name[ii] == ROVA_sites$Plot_Name[jj]) {
         cwd$ParkUnit[ii] <-  ROVA_sites$ParkUnit[jj]
      }
    }
}

cwd <- cwd %>%        
          filter(ParkUnit != "ROVA") %>%    
          select(ParkUnit, CWD_Vol) %>% 
          group_by(ParkUnit) %>% 
          mutate(parkDEB = mean(CWD_Vol, na.rm = T)) %>% 
          ungroup() %>% 
          select(-CWD_Vol) %>% 
          distinct()

## snags ----------------------------------------------------------------





#! calculate park means --------------------------------
for_park2 <- for_park %>% 
  group_by(ParkUnit) %>% 
  mutate(parkDEN = mean(treeden_ha, na.rm = T),
         parkBA = mean(BA_m2ha, na.rm = T),
         parkRICH = mean(tree_rich, na.rm = T),
         parkSTA = Modes(Stage),
         parkBA_pole = mean(pctBA_pole, na.rm = T),
         parkBA_mature = mean(pctBA_mature, na.rm = T),
         parkBA_large = mean(pctBA_large, na.rm = T),
         #parkDIV = mean(, na.rm = T),
         parkSAPden = mean(sap_den_m2, na.rm = T),
         parkSHRUden = mean(shrub_cov, na.rm = T))  %>% 
  ungroup() %>% 
  select(ParkUnit,
         parkDEN, parkBA, parkRICH, parkSTA,
         parkBA_pole, parkBA_mature, parkBA_large,
         parkSAPden, 
         parkSHRUden) %>% 
  distinct() %>% 
  left_join(., can, by = "ParkUnit") %>% 
  left_join(., cwd, by = "ParkUnit")

#! Output files ----------------------------------------------
write_rds(for_park2, file = "data/out/park_covs.rds")

cat(paste("\n\n Done \n\n\n"))

ggplot(for_park2 %>% 
      filter(ParkUnit != "ACAD")%>% 
      filter(ParkUnit != "ELRO")%>% 
      filter(ParkUnit != "SAIR")) +
  geom_point(aes(y = parkBA, x = ParkUnit)) +
  theme_bw()


