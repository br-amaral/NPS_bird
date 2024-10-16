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
if(!require(freshr)){install.packages('freshr')}
freshr::freshr()
#
#! Load packages ---------------------------------------
library(conflicted)
library(tidyverse)
library(glue)
library(rFIA)
library(tigris)
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
  'ACAD', 'Hancock County', 'Maine','ME',
  'ELRO', 'Dutchess County', 'New York','NY',
  'HOFR', 'Dutchess County', 'New York','NY',
  'MABI', 'Windsor County', 'Vermont','VT',
  'MIMA', 'Middlesex County', 'Massachusetts','MA',  
  'MORR', 'Morris County', 'New Jersey','NJ',
  'SAGA', 'Sullivan County', 'New Hampshire','NH',
  'SAIR', 'Essex County', 'Massachusetts','MA',
  'SARA', 'Saratoga County', 'New York','NY',
  'VAMA', 'Dutchess County', 'New York','NY',
  'WEFA', 'Western Connecticut Planning Region', 'Connecticut','CT'), # used to be 'Fairfield County'
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

###? Tree basal area and density -----------------------------------------
for(ii in 1:nrow(park_county)){
  tpaRI <- tpa(get(glue("fia_{park_county$park[ii]}")), totals = TRUE) %>% 
    select(YEAR, TPA, BAA, TREE_TOTAL, BA_TOTAL, TPA_SE, BAA_SE, TREE_TOTAL_SE) %>% 
    mutate(park = park_county$park[ii])
  
  if(ii == 1){
    tpa_tab <- tpaRI
  }
  
  if(ii > 1){
    tpa_tab <- rbind(tpa_tab, tpaRI)
  }
}

### Stand structure
for(ii in 1:nrow(park_county)){
  stastr <- standStruct(get(glue("fia_{park_county$park[ii]}"))) %>% 
    select(YEAR, STAGE, COVER_PCT, COVER_PCT_SE) %>% 
    mutate(park = park_county$park[ii])
  if(ii == 1){
    stastr_tab <- stastr
  }
  
  if(ii > 1){
    stastr_tab <- rbind(stastr_tab, stastr)
  }
}

###? Tree richness ----------------------------------------- 
for(ii in 1:nrow(park_county)){
  div <- diversity(get(glue("fia_{park_county$park[ii]}"))) %>% 
    select(YEAR, H_a, Eh_a, S_a, H_b, Eh_b, S_b, H_g, Eh_g, S_g,
           Eh_a_SE, S_a_SE) %>% 
    mutate(park = park_county$park[ii])
  if(ii == 1){
    div_tab <- div
  }
  
  if(ii > 1){
    div_tab <- rbind(div_tab, div)
  }
}

###? Shrub cover ---------------------------------------------------------
# shrub data
# LVSHRBCD: Live shrub code. A cover class code indicating the percent cover of 
#           the forested microplot area covered with live shrubs.
# LVSHRBHT: Live shrub height. Indicates the height of the tallest live shrub to the
#           nearest 0.1 foot. Heights <6 feet are measured and heights 6 feet are estimated.

for(ii in 1:nrow(park_county)){
  
  county_l <- park_county$state_abbr[ii]
  
  county_shr <- read_csv(glue("data/FIA/{county_l}_DWM_MICROPLOT_FUEL.csv"),
                         col_types = cols(PLT_CN = col_character(), 
                                          CN = col_character())) %>% 
    select(INVYR, STATECD, COUNTYCD, PLOT, SUBP, MEASYEAR,
           LVSHRBCD, LVSHRBHT) %>%  # , DSHRBCD, DSHRBHT) # the D's are dead shrubs
           mutate(park = park_county$park[ii])  %>% 
           rename(shr_per = LVSHRBCD, 
                  shr_ht = LVSHRBHT)
  if(ii == 1){
    shr_tab <- county_shr
  }
  
  if(ii > 1){
    shr_tab <- rbind(shr_tab, county_shr)
  }

}

###? Canopy cover ---------------------------------------------------------
# shrub data
# LIVE_CANOPY_CVR_PCT: Live canopy cover percent. The percentage of live canopy cover for 
#                      the condition. Included are live tally trees, saplings, and seedlings 
#                      that cover the sample area.

for(ii in 1:nrow(park_county)){
  cancov <- get(glue("fia_{park_county$park[ii]}"))$COND %>% 
                as_tibble() %>% 
                select(INVYR, STATECD, COUNTYCD, PLOT, LIVE_CANOPY_CVR_PCT) %>% 
                mutate(park = park_county$park[ii])

  if(ii == 1){
    can_tab <- cancov
  }
  
  if(ii > 1){
    can_tab <- rbind(can_tab, cancov)
  }
}

# can_tab  %>% select(LIVE_CANOPY_CVR_PCT, park) %>% mutate(LIVE_CANOPY_CVR_PCT = ifelse(is.na(LIVE_CANOPY_CVR_PCT),0,1)) %>% table()





fia_WEFA$COND$LIVE_CANOPY_CVR_PCT
fia_WEFA$TREE$TREECLCD_NERS

downwood <- dwm(get(glue("fia_{park_county$park[ii]}"))) %>% 
    select(YEAR, FUEL_TYPE, VOL_ACRE, BIO_ACRE, CARB_ACRE) %>% 
    mutate(park = park_county$park[ii])

#? summarize the files by park and merge them -----------------------------------------
tpa_tab2 <- tpa_tab %>% 
  group_by(park) %>% 
  summarise(tpa = mean(TPA, na.rm = T),
            baa = mean(BAA, na.rm = T),
            tree_total = mean(TREE_TOTAL, na.rm = T),
            ba_total = mean(BA_TOTAL, na.rm = T),
            tpa_se = mean(TPA_SE, na.rm = T),
            baa_se = mean(BAA_SE, na.rm = T),
            tree_total_se = mean(TREE_TOTAL_SE, na.rm = T)) %>% 
  rename(ParkUnit = park,
         counDEN = tpa, counBA = baa) %>% 
  select(ParkUnit, counDEN, counBA)

shr_tab2 <- shr_tab %>% 
  group_by(park) %>% 
  summarise(shr_per = mean(shr_per, na.rm = T),
            shr_ht = mean(shr_ht, na.rm = T)) %>% 
  rename(ParkUnit = park,
         counSHRUden = shr_per) %>% 
  select(ParkUnit, counSHRUden)

stastr_tab2 <- stastr_tab %>%
  select(-COVER_PCT_SE) %>%
  pivot_wider(names_from = STAGE, values_from = COVER_PCT) %>% 
  group_by(park) %>%
  summarise(counPER_late = mean(LATE, na.rm = T),
            counPER_matu = mean(MATURE, na.rm = T),
            counPER_mosc = mean(MOSAIC, na.rm = T),
            counPER_pole = mean(POLE, na.rm = T)) %>% 
  rename(ParkUnit = park)

div_tab2 <- div_tab %>%
  select(-Eh_a_SE, -S_a_SE) %>%
  group_by(park) %>%
  summarise(counH_a = mean(H_a, na.rm = T),
            counH_b = mean(H_b, na.rm = T),
            counH_g = mean(H_g, na.rm = T),
            counEh_a = mean(Eh_a, na.rm = T),
            counEh_b = mean(Eh_b, na.rm = T),
            counEh_g = mean(Eh_g, na.rm = T),
            counS_a = mean(S_a, na.rm = T),
            counS_b = mean(S_b, na.rm = T),
            counS_g = mean(S_g, na.rm = T)) %>%
  rename(ParkUnit = park)

coun_covs <- left_join(tpa_tab2, stastr_tab2, by = "ParkUnit") %>% 
  left_join(div_tab2, by = "ParkUnit") %>% 
  left_join(shr_tab2, by = "ParkUnit")

#! Output files -----------------------------------------
write_rds(coun_covs, file = "data/out/coun_covs.rds")

cat(paste("\n\n Done \n\n\n"))



