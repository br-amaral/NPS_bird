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
 # 'WEFA', 'Fairfield County', 'Connecticut','CT'), 

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

###? proportion of snags ---------------------------------------------------------
# TREECLCD_NERS: Tree class code, Northeastern Research Station. In annual inventory, this code
#                represents a classification of the overall quality of a tree that is >5.0 inches d.b.h. It
#                classifies the quality of a sawtimber tree based on the present condition, or it classifies the
#                quality of a poletimber tree as a prospective determination (i.e., a forecast of potential
#                quality when and if the tree becomes sawtimber size). 
#                Code 6 Snag - Dead tree, or what remains of a dead tree, that is at least 4.5 feet tall and is
#                missing most of its bark. This category includes a tree covered with bark that is very
#                loose. This bark can usually be removed, often times in big strips, with very little
#                effort. A snag is not a recently dead tree. Most often, it has been dead for several
#                years - sometimes, for more than a decade.

for(ii in 1:nrow(park_county)){
  snacov <- get(glue("fia_{park_county$park[ii]}"))$TREE %>% 
                as_tibble() %>% 
                select(INVYR, STATECD, COUNTYCD, PLOT, TREECLCD_NERS) %>% 
                mutate(park = park_county$park[ii])

  if(ii == 1){
    sna_tab <- snacov
  }
  
  if(ii > 1){
    sna_tab <- rbind(sna_tab, snacov)
  }
}
sna_tab %>% 
  select(TREECLCD_NERS, park) %>% 
  mutate(TREECLCD_NERS = ifelse(TREECLCD_NERS < 6, 1,TREECLCD_NERS)) %>% 
  mutate(TREECLCD_NERS = ifelse(is.na(TREECLCD_NERS),0,TREECLCD_NERS)) %>% 
  table()

###? Down wood debris ----------------------------------------- 
# BIO_ACRE: estimate of mean biomass per acre of dwm (short tons/acre)

for(ii in 1:nrow(park_county)){
  deb <- dwm(get(glue("fia_{park_county$park[ii]}"))) %>% 
    select(YEAR, FUEL_TYPE, VOL_ACRE, BIO_ACRE, CARB_ACRE) %>% 
    mutate(park = park_county$park[ii])

  if(ii == 1){
    deb_tab <- deb
  }
  
  if(ii > 1){
    deb_tab <- rbind(deb_tab, deb)
  }
}

#? summarize the files by park and merge them -----------------------------------------
tpa_tab2 <- tpa_tab %>% 
  filter(YEAR == 2020) %>% 
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
  filter(INVYR == 2010) %>% 
  group_by(park) %>% 
  summarise(shr_per = mean(shr_per, na.rm = T),
            shr_ht = mean(shr_ht, na.rm = T)) %>% 
  rename(ParkUnit = park,
         counSHRUden = shr_per) %>% 
  select(ParkUnit, counSHRUden)

stastr_tab2 <- stastr_tab %>%  
  filter(YEAR == 2020) %>% 
  select(-COVER_PCT_SE) %>%
  pivot_wider(names_from = STAGE, values_from = COVER_PCT) %>% 
  group_by(park) %>%
  summarise(counPER_late = mean(LATE, na.rm = T),
            counPER_matu = mean(MATURE, na.rm = T),
            counPER_mosc = mean(MOSAIC, na.rm = T),
            counPER_pole = mean(POLE, na.rm = T)) %>% 
  rename(ParkUnit = park)

div_tab2 <- div_tab %>%
  filter(YEAR == 2020) %>% 
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

can_tab2 <- can_tab %>% 
  filter(INVYR == 2020) %>% 
  select(park, LIVE_CANOPY_CVR_PCT) %>% 
  group_by(park) %>%
  summarise(Can_cov = mean(LIVE_CANOPY_CVR_PCT, na.rm = T))%>%
  rename(ParkUnit = park)

deb_tab2 <- deb_tab %>% 
  filter(YEAR == 2020) %>% 
  select(park, BIO_ACRE) %>% 
  group_by(park) %>%
  summarise(Dwn_Dbr = mean(BIO_ACRE, na.rm = T))%>%
  rename(ParkUnit = park)

#! TODO: make the snag work!!!!!!! TODO:
# sna_tab2 <- sna_tab

nrow(tpa_tab2)
nrow(stastr_tab2)
nrow(div_tab2)
nrow(shr_tab2)
nrow(can_tab2)
nrow(deb_tab2)

# put everyhing in the same dataframe by county (park) -----------------------------------------
coun_covs <- left_join(tpa_tab2, stastr_tab2, by = "ParkUnit") %>% 
  left_join(div_tab2, by = "ParkUnit") %>% 
  left_join(shr_tab2, by = "ParkUnit") %>% 
  left_join(can_tab2, by = "ParkUnit") %>% 
  left_join(deb_tab2, by = "ParkUnit")

#! Output files -----------------------------------------
write_rds(coun_covs, file = "data/out/coun_covs.rds")

#? summarize the files by park AND YEAR and merge them -----------------------------------------
tpa_tab3 <- tpa_tab %>% 
  group_by(park, YEAR) %>% 
  summarise(tpa = mean(TPA, na.rm = T),
            baa = mean(BAA, na.rm = T),
            tree_total = mean(TREE_TOTAL, na.rm = T),
            ba_total = mean(BA_TOTAL, na.rm = T),
            tpa_se = mean(TPA_SE, na.rm = T),
            baa_se = mean(BAA_SE, na.rm = T),
            tree_total_se = mean(TREE_TOTAL_SE, na.rm = T)) %>% 
  rename(ParkUnit = park,
         Year = YEAR,
         counDEN = tpa, counBA = baa) %>% 
  select(ParkUnit, Year, counDEN, counBA)

shr_tab3 <- shr_tab %>% 
  group_by(park, INVYR) %>% 
  summarise(shr_per = mean(shr_per, na.rm = T),
            shr_ht = mean(shr_ht, na.rm = T)) %>% 
  rename(ParkUnit = park,
         Year = INVYR,
         counSHRUden = shr_per) %>% 
  select(ParkUnit, Year, counSHRUden)

stastr_tab3 <- stastr_tab %>%
  select(-COVER_PCT_SE) %>%
  pivot_wider(names_from = STAGE, values_from = COVER_PCT) %>% 
  group_by(park, YEAR) %>% 
  summarise(counPER_late = mean(LATE, na.rm = T),
            counPER_matu = mean(MATURE, na.rm = T),
            counPER_mosc = mean(MOSAIC, na.rm = T),
            counPER_pole = mean(POLE, na.rm = T)) %>% 
  rename(ParkUnit = park,
         Year = YEAR)

div_tab3 <- div_tab %>%
  select(-Eh_a_SE, -S_a_SE) %>%
  group_by(park, YEAR) %>% 
  summarise(counH_a = mean(H_a, na.rm = T),
            counH_b = mean(H_b, na.rm = T),
            counH_g = mean(H_g, na.rm = T),
            counEh_a = mean(Eh_a, na.rm = T),
            counEh_b = mean(Eh_b, na.rm = T),
            counEh_g = mean(Eh_g, na.rm = T),
            counS_a = mean(S_a, na.rm = T),
            counS_b = mean(S_b, na.rm = T),
            counS_g = mean(S_g, na.rm = T)) %>%
  rename(ParkUnit = park,
         Year = YEAR)

can_tab3 <- can_tab %>% 
  select(park, INVYR, LIVE_CANOPY_CVR_PCT) %>% 
  group_by(park, INVYR) %>% 
  summarise(Can_cov = mean(LIVE_CANOPY_CVR_PCT, na.rm = T))%>%
  rename(ParkUnit = park,
         Year = INVYR)

deb_tab3 <- deb_tab %>% 
  select(park, YEAR, BIO_ACRE) %>% 
  group_by(park, YEAR) %>% 
  summarise(Dwn_Dbr = mean(BIO_ACRE, na.rm = T))%>%
  rename(ParkUnit = park,
         Year = YEAR)

#! TODO: make the snag work!!!!!!! TODO:
# sna_tab2 <- sna_tab

# put everyhing in the same dataframe by county (park) -----------------------------------------
coun_covs_year <- left_join(tpa_tab3, stastr_tab3, by = c("ParkUnit", "Year")) %>% 
  left_join(div_tab3, by = c("ParkUnit", "Year")) %>% 
  left_join(shr_tab3, by = c("ParkUnit", "Year")) %>% 
  left_join(can_tab3, by = c("ParkUnit", "Year")) %>% 
  left_join(deb_tab3, by = c("ParkUnit", "Year"))

#! Output files -----------------------------------------
write_rds(coun_covs_year, file = "data/out/coun_covs_yr.rds")

cat(paste("\n\n Done \n\n\n"))

coun_covs_year <- read_rds("data/out/coun_covs_yr.rds")
coun_covs_year <- coun_covs_year %>% 
      filter(ParkUnit != "ACAD")%>% 
      filter(ParkUnit != "ELRO")%>% 
      filter(ParkUnit != "SAIR")
table(coun_covs_year$Year,coun_covs_year$ParkUnit)


coun_covs <- left_join(tpa_tab, stastr_tab, by = c("park","YEAR")) %>% 
  left_join(div_tab, by =  c("park","YEAR"))  
table(coun_covs$YEAR,coun_covs$park)

colnames(coun_covs)

ggplot(coun_covs %>% 
      filter(park != "ACAD")%>% 
      filter(park != "ELRO")%>% 
      filter(park != "SAIR")) +
  geom_point(aes(x = YEAR, y = TPA, col = park)) +
  geom_line(aes(x = YEAR, y = TPA, col = park)) +
  theme_bw()

ggplot(coun_covs %>% 
      filter(park != "ACAD")%>% 
      filter(park != "ELRO")%>% 
      filter(park != "SAIR")) +
  geom_point(aes(x = YEAR, y = BAA, col = park)) +
  geom_line(aes(x = YEAR, y = BAA, col = park)) +
  theme_bw()

  ggplot(coun_covs %>% 
      filter(park != "ACAD")%>% 
      filter(park != "ELRO")%>% 
      filter(park != "SAIR")) +
  geom_point(aes(x = YEAR, y = Eh_a, col = park)) +
  geom_line(aes(x = YEAR, y = Eh_a, col = park)) +
  theme_bw()

    ggplot(coun_covs %>% 
      filter(park != "ACAD")%>% 
      filter(park != "ELRO")%>% 
      filter(park != "SAIR") %>% 
      filter(STAGE ==  "LATE")) +
  geom_point(aes(x = YEAR, y = COVER_PCT, col = park)) +
  geom_line(aes(x = YEAR, y = COVER_PCT, col = park)) +
  theme_bw()

    ggplot(coun_covs %>% 
      filter(park != "ACAD")%>% 
      filter(park != "ELRO")%>% 
      filter(park != "SAIR") %>% 
      filter(STAGE ==  "MATURE")) +
  geom_point(aes(x = YEAR, y = COVER_PCT, col = park)) +
  geom_line(aes(x = YEAR, y = COVER_PCT, col = park)) +
  theme_bw()

    ggplot(coun_covs %>% 
      filter(park != "ACAD")%>% 
      filter(park != "ELRO")%>% 
      filter(park != "SAIR") %>% 
      filter(STAGE ==  "POLE")) +
  geom_point(aes(x = YEAR, y = COVER_PCT, col = park)) +
  geom_line(aes(x = YEAR, y = COVER_PCT, col = park)) +
  theme_bw()
  "LATE"   "MATURE" "MOSAIC" "POLE" 