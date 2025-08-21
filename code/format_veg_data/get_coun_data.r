#? *********************************************************************************
#? ------------------------------   get_coun_data.R   ------------------------------
#? *********************************************************************************
# Code to get the environmental variables at the county level
#
#! Source ---------------------------------------------
#           - getFIA function: from the rFIA package, get FIA data for states
#
#! Input ----------------------------------------------
#
#
#! Output ----------------------------------------------
#           - data/out/coun_covs.rds - county covariate data for model
#
# detach packages and clear workspace
freshr::freshr()
#
options(tigris_use_cache = TRUE)
#! Load packages ---------------------------------------
library(conflicted)
library(tidyverse)
library(glue)
library(rFIA)
library(tigris)
library(forestNETN)
library(rFIA)
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
## Download the state subset or Connecticut (requires an internet connection)
## Save as an object to automatically load the data into your current R session!
# options(timeout = 15000)
# vt <- getFIA(states = 'VT', dir = 'data/FIA', load = FALSE)
# me <- getFIA(states = 'ME', dir = 'data/FIA', load = FALSE)
# nh <- getFIA(states = 'NH', dir = 'data/FIA', load = FALSE)
# ny <- getFIA(states = 'NY', dir = 'data/FIA', load = FALSE)
# ct <- getFIA(states = 'CT', dir = 'data/FIA', load = FALSE)
# ma <- getFIA(states = 'MA', dir = 'data/FIA', load = FALSE)
# ri <- getFIA(states = 'RI', dir = 'data/FIA', load = FALSE)
# nj <- getFIA(states = 'NJ', dir = 'data/FIA', load = FALSE)

#! Import data -----------------------------------------
# master table with parks and counties
# county location of each park
park_county <- matrix(c(
  #'ACAD', 'Hancock County', 'Maine','ME',
  'ELRO', 'Dutchess County', 'New York','NY',
  'HOFR', 'Dutchess County', 'New York','NY',
  'MABI', 'Windsor County', 'Vermont','VT',
  'MIMA', 'Middlesex County', 'Massachusetts','MA',  
  'MORR', 'Morris County', 'New Jersey','NJ',
  'SAGA', 'Sullivan County', 'New Hampshire','NH',
  #'SAIR', 'Essex County', 'Massachusetts','MA',
  'SARA', 'Saratoga County', 'New York','NY',
  'VAMA', 'Dutchess County', 'New York','NY',
  'WEFA', 'Western Connecticut Planning Region', 'Connecticut','CT'), # used to be 'Fairfield County'
  #'WEFA', 'Fairfield County', 'Connecticut','CT'), 
  ncol = 4, byrow = T) %>% 
  as_tibble()
colnames(park_county) <- c("park", "county", "state", "state_abbr")

parks <- park_county$park

### Get multiple states worth of data (not saved since 'dir' is not specified)
### Load FIA Data from a local directory
db <- readFIA('data/FIA/')

#? get county shapefiles and the values for the variables for that county -----------------
for(ii in 1:nrow(park_county)){
  # get county shapefile
  county_sp <- counties(park_county$state[ii], cb = TRUE)
  
  county_sp2 <- county_sp %>% filter(NAMELSAD == park_county$county[ii])
  
  gg <- ggplot()
  gg <- gg + geom_sf(data = county_sp, color="black",
                     fill="white", linewidth=1) + 
    theme_bw() + 
    theme(panel.border = element_blank(), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          axis.line = element_blank()) 
  gg + geom_sf(data = county_sp2, color="black",
                     fill="pink", linewidth=2)
  
  # get the data for the county from FIA
  dbclip2 <- clipFIA(db, mask = county_sp2, mostRecent = FALSE)
  
  name1 <- glue("fia_{park_county$park[ii]}")
  
  assign(name1, dbclip2)

}

# conifer area Estimate forest land area from FIADB
## or TPA by bySpecies
## TPA bySizeClass

###? Tree basal area and density -----------------------------------------
# conversion to stems/ha = 10000/400
# cm2 to m2 cancels out, so just /400m2 plot.
#todo: check hardwood
# by species
for(ii in 1:nrow(park_county)){
  tpaRI <- tpa(get(glue("fia_{park_county$park[ii]}")), 
               #totals = TRUE, 
               byPlot = TRUE, 
               treeType = 'live',
               bySpecies = TRUE,
               treeDomain =  DIA > 5.0) %>%    # exclude saplings
               group_by(pltID, YEAR, SCIENTIFIC_NAME) %>%
               # sum all trees of the same sps to get sps ba and den
               summarize(treeden_ha = sum(TPA, na.rm = T) * 2.47105,    # convert trees per acre to trees per hectare 
                         BA_m2ha = sum(BAA, na.rm = T) * 0.229568) %>%  # convert ft²/acre to m²/ha
               group_by(pltID, SCIENTIFIC_NAME) %>%
               # take the mean over the years to capture diferences between plots
               summarize(treeden_ha = mean(treeden_ha, na.rm = T),  
                         BA_m2ha = mean(BA_m2ha, na.rm = T)) %>% 
               mutate(park = park_county$park[ii])  %>% 
               ungroup() %>% 
               distinct()

  if(ii == 1){
    tpa_tab_sps <- tpaRI
  }
  
  if(ii > 1){
    tpa_tab_sps <- rbind(tpa_tab_sps, tpaRI)
  }
}

tree_cat <- read_csv("data/tree_sps_harcon.csv")

tree_sps <- as_tibble(sort(unique((tpa_tab_sps$SCIENTIFIC_NAME))))  %>% 
                rename(sps = value) %>% 
                mutate(genus = word(sps, 1)) %>% 
                filter(genus != "Unknown",
                       genus != "None")

tree_sps$genus %>% unique()

tree_sps <- left_join(tree_sps, tree_cat, by = "genus")  %>% 
                select(-genus) %>% 
                rename(SCIENTIFIC_NAME = sps)

table(is.na(tree_sps$type))

tree_sps %>% filter(is.na(type))

tpa_covs <- tpa_tab_sps  %>% 
                  left_join(., tree_sps, by = "SCIENTIFIC_NAME")  %>% 
                  group_by(pltID, type) %>% 
                  mutate(treeden_ha = sum(treeden_ha, na.rm = T), 
                         BA_m2ha = sum(BA_m2ha, na.rm = T)) %>% 
                  ungroup() %>% 
                  select(-SCIENTIFIC_NAME) %>% 
                  distinct() %>% 
                  arrange(pltID) %>% 
                  pivot_wider(names_from = "type", values_from = c("treeden_ha", "BA_m2ha"), values_fill = 0) %>% 
# total
                  mutate(treeden_ha = (treeden_ha_Hardwood + treeden_ha_Conifer),
                         BA_m2ha = (BA_m2ha_Hardwood + BA_m2ha_Conifer)) %>% 
                  group_by(park) %>% 
                  summarise(treeden_ha_Hardwood = mean(treeden_ha_Hardwood, na.rm = T),
                            treeden_ha_Conifer =  mean(treeden_ha_Conifer, na.rm = T),
                            BA_m2ha_Hardwood =    mean(BA_m2ha_Hardwood, na.rm = T),
                            BA_m2ha_Conifer =     mean(BA_m2ha_Conifer, na.rm = T),
                            treeden_ha =          mean(treeden_ha, na.rm = T),
                            BA_m2ha =             mean(BA_m2ha, na.rm = T))  %>% 
                  mutate(BA_m2ha_perc_con = BA_m2ha_Conifer/BA_m2ha)

# by size class: classify each tree accoding to DBH in BA and density of pole, mature, and large
# size classes are 10-25.9 cm DBH (pole), 26-45.9 cm DBH (mature) and ≥ 46 cm DBH (large).

for(ii in 1:nrow(park_county)){
    tpa_pole <- tpa(get(glue("fia_{park_county$park[ii]}")), 
                  #totals = TRUE, 
                  byPlot = TRUE,
                  treeDomain =  DIA >= 10/2.54 & DIA < 26/2.54,   ## cm to inches
                  treeType = 'live') %>% 
                  mutate(siz_cla = "pole")  %>% 
                  group_by(pltID) %>% 
                  summarise(treeden_ha_pole = mean(TPA, na.rm = T) * 2.47105,    # convert trees per acre to trees per hectare 
                            BA_m2ha_pole = mean(BAA, na.rm = T) * 0.229568) %>% 
                  ungroup() %>% 
                  select(treeden_ha_pole, BA_m2ha_pole) %>% 
                  summarise(treeden_ha_pole = mean(treeden_ha_pole, na.rm = T),
                            BA_m2ha_pole = mean(BA_m2ha_pole, na.rm = T))%>% 
                  mutate(park = park_county$park[ii]) 

    tpa_mature <- tpa(get(glue("fia_{park_county$park[ii]}")), 
                  #totals = TRUE, 
                  byPlot = TRUE,
                  treeDomain =  DIA >= 26/2.54 & DIA < 46/2.54,   ## cm to inches
                  treeType = 'live') %>% 
                  mutate(siz_cla = "mature")  %>% 
                  group_by(pltID) %>% 
                  summarise(treeden_ha_mature = mean(TPA, na.rm = T) * 2.47105,    # convert trees per acre to trees per hectare 
                            BA_m2ha_mature = mean(BAA, na.rm = T) * 0.229568) %>% 
                  ungroup() %>% 
                  select(treeden_ha_mature, BA_m2ha_mature) %>% 
                  summarise(treeden_ha_mature = mean(treeden_ha_mature, na.rm = T),
                            BA_m2ha_mature = mean(BA_m2ha_mature, na.rm = T))%>% 
                   mutate(park = park_county$park[ii]) 

    tpa_large <- tpa(get(glue("fia_{park_county$park[ii]}")), 
                  #totals = TRUE, 
                  byPlot = TRUE,
                  treeDomain = DIA >= 46/2.54,   ## cm to inches
                  treeType = 'live') %>% 
                  mutate(siz_cla = "large")  %>% 
                  group_by(pltID) %>% 
                  summarise(treeden_ha_large = mean(TPA, na.rm = T) * 2.47105,    # convert trees per acre to trees per hectare 
                            BA_m2ha_large = mean(BAA, na.rm = T) * 0.229568) %>% 
                  ungroup() %>% 
                  select(treeden_ha_large, BA_m2ha_large) %>% 
                  summarise(treeden_ha_large = mean(treeden_ha_large, na.rm = T),
                            BA_m2ha_large = mean(BA_m2ha_large, na.rm = T)) %>% 
                  mutate(park = park_county$park[ii]) 

    tpa_stage_loop <- full_join(tpa_pole, tpa_mature, by = "park") %>% 
                          full_join(., tpa_large, by = "park") 

    if(ii == 1){
      tpa_stage <- tpa_stage_loop
    }
    
    if(ii > 1){
      tpa_stage <- rbind(tpa_stage, tpa_stage_loop)
    }
  }


###? Stand structure ----------------------------------------------------
for(ii in 1:nrow(park_county)){
    stastr <- standStruct(get(glue("fia_{park_county$park[ii]}")), 
                  byPlot = TRUE) %>% arrange(pltID) %>% 
                  group_by(pltID, STAGE) %>% 
                  summarise(percen_cov = mean(PROP_STAGE)) %>% 
                  pivot_wider(names_from = STAGE, values_from = percen_cov, values_fill = 0) %>% 
                  select(-MOSAIC) %>% 
                  rename(pctCANCOV_pole = POLE,
                         pctCANCOV_mature = MATURE,
                         pctCANCOV_late = LATE) %>% 
                  ungroup() %>% 
                  select(pctCANCOV_pole, pctCANCOV_mature, pctCANCOV_late) %>% 
                  summarise(pctCANCOV_pole =   mean(pctCANCOV_pole, na.rm = T),
                            pctCANCOV_mature = mean(pctCANCOV_mature, na.rm = T),
                            pctCANCOV_late =   mean(pctCANCOV_late, na.rm = T)) %>% 
                  mutate(park = park_county$park[ii])

    if(ii == 1){
      stastr_tab2 <- stastr
    }
    
    if(ii > 1){
      stastr_tab2 <- rbind(stastr_tab2, stastr)
    }
  }

# seedling
for(ii in 1:nrow(park_county)){

    seed_cov_loop <- seedling(get(glue("fia_{park_county$park[ii]}")), 
                        byPlot = TRUE) %>% 
                        group_by(pltID) %>% 
                        summarise(seed_tpa = mean(TPA, na.rm = T) * 2.47105) %>%   ## acre to ha
                        ungroup() %>% 
                        select(seed_tpa) %>% 
                        summarise(seed_tpa = mean(seed_tpa, na.rm = T)) %>% 
                        mutate(park = park_county$park[ii]) 

    if(ii == 1){
      seed_cov <- seed_cov_loop
    }
    
    if(ii > 1){
      seed_cov <- rbind(seed_cov, seed_cov_loop)
    }
  }

# saplings: saplings per acre unadjusted (TREE.TPA_UNADJ where TREE.DIA <5.0)
for(ii in 1:nrow(park_county)){
    tpa_sap_loop <- tpa(get(glue("fia_{park_county$park[ii]}")), 
                        #totals = TRUE, 
                        byPlot = TRUE,
                        treeDomain =  DIA < 5.0,   ## cm to inches
                        treeType = 'live') %>% 
                        mutate(siz_cla = "sap") %>% 
                        group_by(pltID) %>% 
                        summarise(treeden_ha_sap = mean(TPA, na.rm = T) * 2.47105,    # convert trees per acre to trees per hectare 
                                  BA_m2ha_sap = mean(BAA, na.rm = T) * 0.229568) %>% 
                        ungroup() %>% 
                        select(treeden_ha_sap, BA_m2ha_sap) %>% 
                        summarise(treeden_ha_sap = mean(treeden_ha_sap, na.rm = T),
                                  BA_m2ha_sap = mean(BA_m2ha_sap, na.rm = T)) %>% 
                        mutate(park = park_county$park[ii])

      
    if(ii == 1){
      tpa_sap <- tpa_sap_loop
    }
    
    if(ii > 1){
      tpa_sap <- rbind(tpa_sap, tpa_sap_loop)
    }
  }

###? Shrub cover ---------------------------------------------------------
for(ii in 1:nrow(park_county)){
    shrub_loop <- vegStruct(get(glue("fia_{park_county$park[ii]}")), 
                            #totals = TRUE, 
                            byPlot = TRUE)  %>% 
                  filter(GROWTH_HABIT == 'Shrubs/vines', # maybe also 'Forbs'
                         LAYER %in% c("0 to 2.0 feet", "2.1 to 6.0 feet")) %>% 
                  group_by(pltID) %>% 
                  summarise(shrub_cov = mean(PROP_COVER, na.rm = T)) %>% 
                  ungroup() %>% 
                  select(shrub_cov) %>% 
                  summarise(shrub_cov = mean(shrub_cov, na.rm = T)) %>% 
                  mutate(park = park_county$park[ii])

    if(ii == 1){
      shrub <- shrub_loop
    }
    
    if(ii > 1){
      shrub <- rbind(shrub, shrub_loop)
    }
}
## check number of NAs - check the number of plots with replies

###? Down wood debris ----------------------------------------- 
# BIO_ACRE: estimate of mean biomass per acre of dwm (short tons/acre)
for(ii in 1:nrow(park_county)){
    deb_cov_loop <- dwm(get(glue("fia_{park_county$park[ii]}")), 
                        byPlot = TRUE) %>% 
                      group_by(pltID) %>%
                      summarise(dwd_bio = mean(BIO_ACRE, na.rm = T)) %>% 
                      ungroup() %>% 
                      select(dwd_bio) %>% 
                      summarise(dwd_bio = mean(dwd_bio, na.rm = T)) %>% 
                      mutate(park = park_county$park[ii])

    if(ii == 1){
      deb_cov <- deb_cov_loop
    }
    
    if(ii > 1){
      deb_cov <- rbind(deb_cov, deb_cov_loop)
    }
}

#? put everyhing in the same dataframe by county (park) -----------------------------------------
coun_covs <- full_join(tpa_covs, tpa_stage, by = "park") %>% 
  full_join(stastr_tab2, by = "park") %>% 
  full_join(shrub, by = "park") %>% 
  full_join(deb_cov, by = "park") %>% 
  full_join(seed_cov, by = "park") %>% 
  full_join(tpa_sap, by = "park")

#! Output files -----------------------------------------
write_rds(coun_covs, file = "data/out/coun_covs.rds")

# #? summarize the files by park AND YEAR and merge them -----------------------------------------
# tpa_tab3 <- tpa_tab %>% 
#   group_by(park, YEAR) %>% 
#   summarise(tpa = mean(TPA, na.rm = T),
#             baa = mean(BAA, na.rm = T),
#             tree_total = mean(TREE_TOTAL, na.rm = T),
#             ba_total = mean(BA_TOTAL, na.rm = T),
#             tpa_se = mean(TPA_SE, na.rm = T),
#             baa_se = mean(BAA_SE, na.rm = T),
#             tree_total_se = mean(TREE_TOTAL_SE, na.rm = T)) %>% 
#   rename(ParkUnit = park,
#          Year = YEAR,
#          counDEN = tpa, counBA = baa) %>% 
#   select(ParkUnit, Year, counDEN, counBA)

# shr_tab3 <- shr_tab %>% 
#   group_by(park, INVYR) %>% 
#   summarise(shr_per = mean(shr_per, na.rm = T),
#             shr_ht = mean(shr_ht, na.rm = T)) %>% 
#   rename(ParkUnit = park,
#          Year = INVYR,
#          counSHRUden = shr_per) %>% 
#   select(ParkUnit, Year, counSHRUden)

# stastr_tab3 <- stastr_tab %>%
#   select(-COVER_PCT_SE) %>%
#   pivot_wider(names_from = STAGE, values_from = COVER_PCT) %>% 
#   group_by(park, YEAR) %>% 
#   summarise(counPER_late = mean(LATE, na.rm = T),
#             counPER_matu = mean(MATURE, na.rm = T),
#             counPER_mosc = mean(MOSAIC, na.rm = T),
#             counPER_pole = mean(POLE, na.rm = T)) %>% 
#   rename(ParkUnit = park,
#          Year = YEAR)

# div_tab3 <- div_tab %>%
#   select(-Eh_a_SE, -S_a_SE) %>%
#   group_by(park, YEAR) %>% 
#   summarise(counH_a = mean(H_a, na.rm = T),
#             counH_b = mean(H_b, na.rm = T),
#             counH_g = mean(H_g, na.rm = T),
#             counEh_a = mean(Eh_a, na.rm = T),
#             counEh_b = mean(Eh_b, na.rm = T),
#             counEh_g = mean(Eh_g, na.rm = T),
#             counS_a = mean(S_a, na.rm = T),
#             counS_b = mean(S_b, na.rm = T),
#             counS_g = mean(S_g, na.rm = T)) %>%
#   rename(ParkUnit = park,
#          Year = YEAR)

# can_tab3 <- can_tab %>% 
#   select(park, INVYR, LIVE_CANOPY_CVR_PCT) %>% 
#   group_by(park, INVYR) %>% 
#   summarise(Can_cov = mean(LIVE_CANOPY_CVR_PCT, na.rm = T))%>%
#   rename(ParkUnit = park,
#          Year = INVYR)

# deb_tab3 <- deb_tab %>% 
#   select(park, BIO_ACRE) %>% 
#   group_by(park) %>% 
#   summarise(Dwn_Dbr = mean(BIO_ACRE, na.rm = T))%>%
#   rename(ParkUnit = park)

# #! TODO: make the snag work!!!!!!! TODO:
# # sna_tab2 <- sna_tab

# # put everyhing in the same dataframe by county (park) -----------------------------------------
# coun_covs <- left_join(tpa_tab3, stastr_tab3, by = c("ParkUnit","Year")) %>% 
#   left_join(div_tab3, by = c("ParkUnit")) %>% 
#   left_join(shr_tab3, by = c("ParkUnit")) %>% 
#   left_join(can_tab3, by = c("ParkUnit")) %>% 
#   left_join(deb_tab3, by = c("ParkUnit"))

# #! Output files -----------------------------------------
# # write_rds(coun_covs, file = "data/out/coun_covs_yr.rds")

# cat(paste("\n\n Done \n\n\n"))


# ggplot(coun_covs %>% 
#       filter(ParkUnit != "ACAD")%>% 
#       filter(ParkUnit != "ELRO")%>% 
#       filter(ParkUnit != "SAIR")) +
#   geom_point(aes(x = YEAR, y = TPA, col = park)) +
#   geom_line(aes(x = YEAR, y = TPA, col = park)) +
#   theme_bw()

# ggplot(coun_covs %>% 
#       filter(ParkUnit != "ACAD")%>% 
#       filter(park != "ELRO")%>% 
#       filter(park != "SAIR")) +
#   geom_point(aes(x = YEAR, y = BAA, col = park)) +
#   geom_line(aes(x = YEAR, y = BAA, col = park)) +
#   theme_bw()

#   ggplot(coun_covs %>% 
#       filter(park != "ACAD")%>% 
#       filter(park != "ELRO")%>% 
#       filter(park != "SAIR")) +
#   geom_point(aes(x = YEAR, y = Eh_a, col = park)) +
#   geom_line(aes(x = YEAR, y = Eh_a, col = park)) +
#   theme_bw()

#     ggplot(coun_covs %>% 
#       filter(park != "ACAD")%>% 
#       filter(park != "ELRO")%>% 
#       filter(park != "SAIR") %>% 
#       filter(STAGE ==  "LATE")) +
#   geom_point(aes(x = YEAR, y = COVER_PCT, col = park)) +
#   geom_line(aes(x = YEAR, y = COVER_PCT, col = park)) +
#   theme_bw()

#     ggplot(coun_covs %>% 
#       filter(park != "ACAD")%>% 
#       filter(park != "ELRO")%>% 
#       filter(park != "SAIR") %>% 
#       filter(STAGE ==  "MATURE")) +
#   geom_point(aes(x = YEAR, y = COVER_PCT, col = park)) +
#   geom_line(aes(x = YEAR, y = COVER_PCT, col = park)) +
#   theme_bw()

#     ggplot(coun_covs %>% 
#       filter(park != "ACAD")%>% 
#       filter(park != "ELRO")%>% 
#       filter(park != "SAIR") %>% 
#       filter(STAGE ==  "POLE")) +
#   geom_point(aes(x = YEAR, y = COVER_PCT, col = park)) +
#   geom_line(aes(x = YEAR, y = COVER_PCT, col = park)) +
#   theme_bw()
