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
freshr::freshr()
#
#! Load packages ---------------------------------------
library(conflicted)
library(tidyverse)
#library(forestNETN)
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
FOR_PLOT_COVS     <- "data/out/for_plot_covs.rds"
VAMA_PARK_PATH    <- "data/VAMA_sites.rds"
HOFR_PARK_PATH    <- "data/HOFR_sites.rds"
ELRO_PARK_PATH    <- "data/ELRO_sites.rds"

#? read files
for_park <- read_rds(FOR_PLOT_COVS)

## get site names for ROVA parks 
VAMA_sites <- read_rds(file = VAMA_PARK_PATH) %>% 
                select(park, for_sit)

HOFR_sites <- read_rds(file = HOFR_PARK_PATH) %>% 
                select(park, for_sit)

ELRO_sites <- read_rds(file = ELRO_PARK_PATH) %>% 
                select(park, for_sit)

ROVA_sites <- rbind(VAMA_sites, HOFR_sites, ELRO_sites)  %>% 
                rename(ParkUnit = park, Plot_Name = for_sit)

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
                 ParkUnit != "ACAD",
                 ParkUnit != "SAIR")   

nrow(for_park)
length(unique(for_park$Plot_Name))

#! calculate park means --------------------------------
for_park2 <- for_park %>% 
  group_by(ParkUnit) %>% 
  summarise(BA_m2ha =             mean(BA_m2ha, na.rm = T),         
            BA_m2ha_Conifer =     mean(BA_m2ha_Conifer, na.rm = T),        
            BA_m2ha_Hardwood =    mean(BA_m2ha_Hardwood, na.rm = T),         
            BA_m2ha_large =       mean(BA_m2ha_large, na.rm = T),         
            BA_m2ha_mature =      mean(BA_m2ha_mature, na.rm = T),         
            BA_m2ha_pole =        mean(BA_m2ha_pole, na.rm = T),         
            treeden_ha =          mean(treeden_ha, na.rm = T),         
            treeden_ha_Conifer =  mean(treeden_ha_Conifer, na.rm = T),         
            treeden_ha_Hardwood = mean(treeden_ha_Hardwood, na.rm = T),   
            BA_m2ha_perc_con =    mean(BA_m2ha_perc_con, na.rm = T),    
            treeden_ha_large =    mean(treeden_ha_large, na.rm = T),        
            treeden_ha_mature =   mean(treeden_ha_mature, na.rm = T),    
            treeden_ha_pole =     mean(treeden_ha_pole, na.rm = T),         
            # seed_den_m2 =         mean(seed_den_m2, na.rm = T),        
            # sap_den_m2 =          mean(sap_den_m2, na.rm = T),       
            # regen_den_m2 =        mean(regen_den_m2, na.rm = T),    
            shrub_avg_cov =       mean(shrub_avg_cov, na.rm = T) 
            # shrub_cov_nat =       mean(shrub_cov_nat, na.rm = T),     
            # shrub_cov_nonat =     mean(shrub_cov_nonat, na.rm = T),        
            #cwd =                 mean(cwd, na.rm = T)
            )  

for_park2 %>% 
  summarise(across(everything(), ~sum(is.na(.)))) %>% 
  t() %>% 
  as.data.frame() %>%
  tibble::rownames_to_column("column") %>%
  arrange(-V1)

#! Output files ----------------------------------------------
write_rds(for_park2, file = "data/out/park_covs.rds")

cat(paste("\n\n Done \n\n\n"))
