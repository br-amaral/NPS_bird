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
FORCOVS_PATH <- "data/out/site_covs.rds"

## read files
for_park <- read_rds(file = FORCOVS_PATH)

#! calculate park means --------------------------------
for_park2 <- for_park %>% 
  #filter(SampleYear == 2022) %>% 
  group_by(ParkUnit) %>% 
  mutate(parkDEN = mean(siteDEN, na.rm = T),
          parkBA = mean(siteBA, na.rm = T),
          parkRICH = mean(siteRICH, na.rm = T),
          parkSTA = Modes(siteSTA),
          parkBA_pole = mean(siteBA_pole, na.rm = T),
          parkBA_mature = mean(siteBA_mature, na.rm = T),
          parkBA_large = mean(siteBA_large, na.rm = T),
          #parkDIV = mean(, na.rm = T),
          parkSAPden = mean(siteSAPden, na.rm = T),
          parkSHRUden = mean(siteSHRUden, na.rm = T))  %>% 
  ungroup() %>% 
  select(ParkUnit,
         parkDEN, parkBA, parkRICH, parkSTA,
         parkBA_pole, parkBA_mature, parkBA_large,
         parkSAPden, 
         parkSHRUden) %>% 
  distinct()

#! Output files ----------------------------------------------
write_rds(for_park2, file = "data/out/park_covs.rds")
