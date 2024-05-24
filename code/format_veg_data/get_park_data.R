#? *********************************************************************************
#? ------------------------------   get_park_data.R   ------------------------------
#? *********************************************************************************
# Code to get the environmental variables at the park level
#
#! Source ---------------------------------------------
#
#! Input ----------------------------------------------
#           - data/veg_kateaaron/NETN_forest_data_2006-2023.rds :
#           - data/veg_kateaaron/NETN_tree_dens_spp_2006-2023.rds :
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
FORCOVS_PATH <- "data/veg_kateaaron/NETN_forest_data_2006-2023.rds"
FORSPS_PATH  <- "data/veg_kateaaron/NETN_tree_dens_spp_2006-2023.rds"

## read files
for_park       <- read_rds(file = FORCOVS_PATH)
fordiv_park    <- read_rds(file = FORSPS_PATH)

#! calculate park means --------------------------------
for_park2 <- for_park %>% 
  #filter(SampleYear == 2022) %>% 
  group_by(ParkUnit) %>% 
  mutate(parkDEN = mean(treeden_ha, na.rm = T),
         parkBA = mean(BA_m2ha, na.rm = T),
         parkRICH = mean(tree_rich, na.rm = T),
         parkSTA = Modes(Stage),
         parkBA_pole = mean(pctBA_pole, na.rm = T),
         parkBA_mature = mean(pctBA_mature, na.rm = T),
         parkBA_large = mean(pctBA_large, na.rm = T),
         parkDIV = mean(tree_rich, na.rm = T),
         parkSAPden = mean(sap_den_m2, na.rm = T),
         parkSHRUden = mean(shrub_cov, na.rm = T))  %>% 
  ungroup() %>% 
  select(ParkUnit,
         parkDEN, parkBA, parkRICH, parkSTA,
         parkBA_pole, parkBA_mature, parkBA_large,
         parkSAPden, 
         parkSHRUden) %>% 
  distinct()

#! Output files ----------------------------------------------
write_rds(for_park2, file = "data/out/park_covs.rds")
