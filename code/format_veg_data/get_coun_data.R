#? *********************************************************************************
#? ------------------------------   get_coun_data.R   ------------------------------
#? *********************************************************************************
# Code to get the environmental variables at the county level
#
#
#! Source ---------------------------------------------
#           -  :
#           -  :
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
#
#! Import data -----------------------------------------
## file paths
#FORCOVS_COUNTY_PATH <- "data/"
#FORSPS_COUNTY_PATH  <- "data/"
PARK_COUNTY_PATH <- "data/park_county.rds"
PARKS_LIST_PATH <- "data/src/key_park.rds"

## read files
for_cou       <- read_rds(file = FORCOVS_COUNTY_PATH)
fordiv_cou    <- read_rds(file = FORSPS_COUNTY_PATH)
park_county <- read_rds(file = PARK_COUNTY_PATH)
parks <- readRDS(file = PARKS_LIST_PATH) %>% 
  dplyr::select(parks) %>% 
  distinct() %>% 
  pull()

### Get multiple states worth of data (not saved since 'dir' is not specified)
### Load FIA Data from a local directory
db <- readFIA('data/FIA/')

#! calculate park means --------------------------------
for(ii in 1:nrow(park_county)){
  # get county shapefile
  county_sp <- counties(park_county$state[ii], cb = TRUE)
  
  county_sp2 <- county_sp %>% filter(NAMELSAD == park_county$county[ii])
  
  # gg <- ggplot()
  # gg <- gg + geom_sf(data = county_sp2, color="black",
  #                    fill="white", linewidth=2) + 
  #   theme_bw() + 
  #   theme(panel.border = element_blank(), 
  #         panel.grid.major = element_blank(),
  #         panel.grid.minor = element_blank(), 
  #         axis.line = element_blank()) 
  # gg 
  
  # get the data for the county from FIA
  dbclip2 <- clipFIA(db, mask = county_sp2, mostRecent = FALSE)
  
  name1 <- glue("fia_{park_county$park[ii]}")
  
  assign(name1, dbclip2)

}

### Tree basal area
### Tree density
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

### Tree richness
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

### Stage of stand (mode)

### Sapling density


### Shrub cover ---------------------------------------------------------

# SV%L1 - 4 or all together: [SV%Ar]]
# 8.5 Vegetation Structure 8.5.12 - 15
STND_COND_CD_PNWRS
LAND_COVER_CLASS_CD
GROWTH_HABIT_CD

park_county_l <- c("CT", "MA", "ME", "NH", "NJ", "RI", "VT", "NY")

# shrub data
## LVSHRBCD: Live shrub code. A cover class code indicating the percent cover of the forested microplot area covered with live shrubs.
## LVSHRBHT: Live shrub height. Indicates the height of the tallest live shrub to the nearest 0.1 foot. Heights <6 feet are measured and heights 6 feet are estimated.

for(ii in 1:length(park_county_l)){
  
  county_l <- park_county_l[ii]
  
  county_shr <- read_csv(glue("data/FIA/{county_l}_DWM_MICROPLOT_FUEL.csv"),
                         col_types = cols(PLT_CN = col_character())) %>% 
    select(CN, PLT_CN, INVYR, STATECD, COUNTYCD, PLOT, SUBP, MEASYEAR,
           LVSHRBCD, LVSHRBHT) # , DSHRBCD, DSHRBHT) # the D's are dead shrubs
  
  name1 <- glue("shr_{park_county_l[ii]}")
  
  assign(name1, county_shr)

}




str(fia_MABI)
summary(fia_MABI)
length(fia_MABI)
fia_colnam <- as_tibble(matrix(NA, nrow = length(colnames(fia_MABI[[1]])), ncol = 3))
colnames(fia_colnam) <- c("colname", "tibble", "tib_num")
fia_colnam <- fia_colnam %>%
                mutate(colname = as.character(colnames(fia_MABI[[1]])),
                       tibble = as.character(names(fia_MABI)[1]),
                       tib_num = as.numeric(1))

for(i in 2:(length(fia_MABI)-1)){
  rbind(fia_colnam, 
        as_tibble(cbind(colnames(fia_MABI[[i]]), 
                        rep(names(fia_MABI)[i], length(colnames(fia_MABI[[i]]))), 
                        rep(i, length(colnames(fia_MABI[[i]]))))) %>% 
          rename(colname = V1, tibble = V2, tib_num = V3) %>% 
          mutate(colname = as.character(colname),
                 tibble = as.character(tibble),
                 tib_num = as.numeric(tib_num)))
}


get("fia_{park_county$park[ii]}")

 db2_cond <- dbclip2$COND[c("CN", "PLT_CN", "INVYR", "UNITCD", "PLOT", "CONDID","COUNTYCD", "STATECD",
                            "COND_STATUS_CD", "OWNCD", "OWNGRPCD", "FORTYPCD",
                            "FLDTYPCD", "STDAGE", "STDSZCD", "SIBASE", "STDORGCD",
                            "ALSTKCD", "DSTRBCD1", "DSTRBYR1", "DSTRBCD2", "TRTCD1",
                            "PRESNFCD", "BALIVE", "FLDAGE", "ALSTK", "GSSTK", "FORTYPCDCALC",
                            "HABTYPCD1", "CARBON_LITTER", "GRAZING_SRS", "HARVEST_TYPE1_SRS",
                            "LIVE_CANOPY_CVR_PCT", "NBR_LIVE_STEMS", "DSTRBCD1_P2A", "TRTCD1_P2A",
                            "LAND_COVER_CLASS_CD")]


