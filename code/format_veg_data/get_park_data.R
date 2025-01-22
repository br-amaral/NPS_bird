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

## read files
for_park <- read_rds(file = FORCOVS_PARK_PATH)
## canopy cover ---------------------------------------------------------
path <- glue("{getwd()}/data/veg_kateaaron") 
importCSV(path, zip_name = "NETN_Forest_20231106.zip")
can <- forestNETN::joinStandData(park = "all") %>%
          as_tibble() %>% 
          select(ParkUnit, Pct_Crown_Closure) %>% 
          group_by(ParkUnit) %>% 
          mutate(parkCAN = mean(Pct_Crown_Closure, na.rm = T)) %>% 
          ungroup() %>% 
          select(-Pct_Crown_Closure) %>% 
          distinct()

can_yr <- forestNETN::joinStandData(park = "all") %>%
          as_tibble() %>% 
          select(SampleYear, ParkUnit, Pct_Crown_Closure) %>% 
          group_by(ParkUnit, SampleYear) %>% 
          mutate(parkCANyr = mean(Pct_Crown_Closure, na.rm = T)) %>% 
          ungroup() %>% 
          select(-Pct_Crown_Closure) %>% 
          distinct()

## wood debris ----------------------------------------------------------
cwd <- joinCWDData(park = 'all') %>% # coarse wood debris
          as_tibble() %>% 
          select(ParkUnit, CWD_Vol) %>% 
          group_by(ParkUnit) %>% 
          mutate(parkDEB = mean(CWD_Vol, na.rm = T)) %>% 
          ungroup() %>% 
          select(-CWD_Vol) %>% 
          distinct()

cwd_yr <- joinCWDData(park = 'all') %>% # coarse wood debris
            as_tibble() %>% 
            select(SampleYear, ParkUnit, CWD_Vol) %>% 
            group_by(ParkUnit, SampleYear) %>% 
            mutate(parkDEByr = mean(CWD_Vol, na.rm = T)) %>% 
            ungroup() %>% 
            select(-CWD_Vol) %>% 
            distinct()

## snags ----------------------------------------------------------------

#! calculate park means --------------------------------
for_park <- for_park %>% 
  filter(ParkUnit != "ACAD", 
         SampleYear == 2022) %>% 
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

#! calculate park means --------------------------------
for_park2_yr <- for_park %>% 
  group_by(ParkUnit, SampleYear) %>% 
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
  select(ParkUnit, SampleYear,
         parkDEN, parkBA, parkRICH, parkSTA,
         parkBA_pole, parkBA_mature, parkBA_large,
         parkSAPden, 
         parkSHRUden) %>% 
  distinct() %>% 
  left_join(., can_yr, by = c("ParkUnit", "SampleYear")) %>% 
  left_join(., cwd_yr, by = c("ParkUnit", "SampleYear"))

#! Output files ----------------------------------------------
write_rds(for_park2, file = "data/out/park_covs.rds")
write_rds(for_park2_yr, file = "data/out/park_covs_yr.rds")

cat(paste("\n\n Done \n\n\n"))


colnames(for_park)

colnames(for_park2_yr)

ggplot(for_park2_yr %>% 
      filter(ParkUnit != "ACAD")%>% 
      filter(ParkUnit != "ELRO")%>% 
      filter(ParkUnit != "SAIR")) +
  geom_point(aes(x = SampleYear, y = parkBA, col = ParkUnit)) +
  geom_line(aes(x = SampleYear, y = parkBA, col = ParkUnit)) +
  theme_bw()

ggplot(for_park2_yr %>% 
      filter(ParkUnit != "ACAD")%>% 
      filter(ParkUnit != "ELRO")%>% 
      filter(ParkUnit != "SAIR")) +
  geom_point(aes(x = SampleYear, y = parkDEN, col = ParkUnit)) +
  geom_line(aes(x = SampleYear, y = parkDEN, col = ParkUnit)) +
  theme_bw()

unique(for_park2_yr$ParkUnit)
library(ggplot2)
library(dplyr)

# Assuming for_park is your data frame
ggplot(for_park %>% filter(ParkUnit == "WEFA")) +
  geom_boxplot(aes(x = as.factor(SampleYear), y = BA_m2ha, col = ParkUnit)) +
  ylim(c(0, 80)) +
  labs(x = "Sample Year", y = "Basal Area", 
  title = "Boxplot of Basal Area by Year for WEFA") +
  theme_bw() +
  theme(legend.position = "none")

ggplot(for_park %>% filter(ParkUnit == "MIMA")) +
  geom_boxplot(aes(x = as.factor(SampleYear), y = BA_m2ha, col = ParkUnit)) +
  ylim(c(0, 80)) +
  labs(x = "Sample Year", y = "Basal Area", title = "Boxplot of Basal Area by Year for MIMA") +
  theme_bw() +
  theme(legend.position = "none")
