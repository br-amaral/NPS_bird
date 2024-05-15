#? *********************************************************************************
#? ------------------------------   get_park_data.R   ------------------------------
#? *********************************************************************************
# Code to get the environmental variables at the park level
#
#
#! Source ---------------------------------------------
#           - :
#           - :
#
#! Input ----------------------------------------------
#           - :
#           - :
#
#! Output ----------------------------------------------
#           - :
#           - :
#
# detach packages and clear workspace
if(!require(freshr)){install.packages('freshr')}
freshr::freshr()
#
#! Load packages ---------------------------------------
library(conflicted)
library(tidyverse)
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

#! Source code -----------------------------------------
#
#! Import data -----------------------------------------
## file paths
FORCOVS_SITE_PATH <- "data/veg_kateaaron/NETN_forest_data_2006-2023.rds"
FORSPS_SITE_PATH  <- "data/veg_kateaaron/NETN_tree_dens_spp_2006-2023.rds"

## read files
for_sit       <- read_rds(file = FORCOVS_SITE_PATH)
fordiv_sit    <- read_rds(file = FORSPS_SITE_PATH)

#! calculate park means --------------------------------
for_sit2 <- for_sit %>% 
  #filter(SampleYear == 2022) %>% 
  group_by(ParkUnit) %>% 
  mutate(treeden_haM = mean(treeden_ha, na.rm = T),
         BA_m2haM = mean(BA_m2ha, na.rm = T),
         tree_richM = mean(tree_rich, na.rm = T),
         StageM = Modes(Stage),
         pctBA_poleM = mean(pctBA_pole, na.rm = T),
         pctBA_matureM = mean(pctBA_mature, na.rm = T),
         pctBA_largeM = mean(pctBA_large, na.rm = T),
         sap_den_m2M = mean(sap_den_m2, na.rm = T),
         shrub_covM = mean(shrub_cov, na.rm = T),
         X_for = X,      
         Y_for = Y,
         UTMZone_for = UTMZone,
         for_sit = Plot_Name)  %>% 
  ungroup() %>% 
  select(for_sit, ParkUnit, X_for, Y_for, UTMZone_for,
         treeden_haM, BA_m2haM, tree_richM, StageM, pctBA_poleM, 
         pctBA_matureM, pctBA_largeM, sap_den_m2M, shrub_covM) %>% 
  distinct()


