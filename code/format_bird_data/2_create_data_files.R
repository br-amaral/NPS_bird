#! *********************************************************************************
#! ----------------------------- 3_create_bird_data.R ------------------------------
#! *********************************************************************************
# Code to create the y data file to run in the JAGS model 
#   can be run for 1, all or groups f species, in one or all parks
#
#! Source ---------------------------------------------
#           - code/format_bird_data/2_format_data.R:
#
#! Input ----------------------------------------------
#           from here: 
#           - data/out/coun_covs.rds - covariate data from county level
#           - data/out/park_covs.rds - covariate data from park level
#           - data/out/site_covs.rds - covariate data from site level
#           - data/park_raster/{pk[i]}_pb.rds : raster of each park to get park size 
#           - data/out/site_div.rds : diversity of forest for park and sites
#
#           from code/format_bird_data/format_data.R:
#             -- y1: table of ones and zeros for sps detections
#             -- visits (data/out/visits.rds): data from the visit files
#             -- yr_pk: number of years sampled in each park
#
#! Output ---------------------------------------------
#           - data/src/sites_park_tib.rds: tibble with park, number of sites and site numbers and codes
#           - data/out/site_n_key.rds: park and site unique key
#           - data/out/y_dat3.rds: first tibble with ALL occasions for sps, park, site, year and interval
#           - data/y_dat8.rds: birds data for each occasion, with park, species and site indexes
#           - data/X10.rds: environmental variables for all scales for each occasion, same dim() as y_dat6.rds
#           - data/sps_pk_nth.rds: species code in each park

## detach packages and clear workspace
if(!require(freshr)){install.packages("freshr")}
freshr::freshr()

#! Load packages --------------------------------------
library(conflicted)
library(tidyverse)
library(glue)
library(hms)
library(lubridate)
library(splitstackshape)

conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::filter)
# conflicts_prefer(scales::alpha)

#! Make functions --------------------------------------
colanmes <- colnames
lenght <- length
`%!in%` <- Negate(`%in%`)

#! Source code -----------------------------------------
## Create empty matrix with all parks, species, years, sites and intervals --------------------------
source("code/format_bird_data/format_data.R")

yog <- y1 # reset safety ;)
#! Define settings -------------------------------------
radi_dist <- 500

#! Import data -----------------------------------------
## file paths
PATH_COVS_COUN <- "data/out/coun_covs.rds"
PATH_COVS_PARK <- "data/out/park_covs.rds"
PATH_COVS_SITE <- glue("data/out/site_covs_nei_grp_{radi_dist}m.rds")
PATH_DIV_SITE_COVS <- "data/out/site_div.rds"
PATH_DIV_PARK_COVS <- "data/out/park_div.rds"

## parks -------------------------------------------------------------------------------------------
pk_list <- visits %>% 
    select(Admin_Unit_Code) %>% 
    distinct() %>%  
    arrange(Admin_Unit_Code) %>% 
    filter(Admin_Unit_Code %!in% c("ACAD", "SAIR", "ELRO")) %>% 
    pull()

npk <- length(pk_list)

## years -------------------------------------------------------------------------------------------
# year_n gives an ordinal number for each calendar year, while year_n_gap jumps years 
#  that have no sampling: year = 2007, 2009 ; year_n = 1, 2 ; year_n_gap = 1,3
yr_pk <- yr_pk %>% 
  arrange(Admin_Unit_Code,Year) %>% 
  filter(Admin_Unit_Code %!in% c("ACAD", "SAIR", "ELRO"))

yr_pk_min <- yr_pk %>% 
  group_by(Admin_Unit_Code) %>% 
  mutate(year_min = min(Year),
          park = Admin_Unit_Code) %>% 
  ungroup() %>% 
  select(Admin_Unit_Code, park, year_min) %>% 
  distinct()

nyr_pk <- nyr_pk %>% cbind(.,pk) %>% 
  as_tibble()  
colnames(nyr_pk) <- c("nyr", "park")

nyr_pk <- nyr_pk %>% 
  filter(park %!in% c("ACAD", "SAIR", "ELRO")) %>% 
  select(nyr) %>% 
  pull() %>% 
  as.numeric()

for(i in 1:lenght(nyr_pk)) {
  nth_year <- yr_pk %>% 
    filter(Admin_Unit_Code == pk_list[i]) %>% 
    mutate(year_n = seq(1, nrow(.),1))
  
  if(i == 1){
    yr_pk_nth <- nth_year
  } else {
    yr_pk_nth <- rbind(yr_pk_nth, nth_year)
  }
}

y1 <- yog %>% 
  left_join(., yr_pk_min, by = c("park", "Admin_Unit_Code")) %>% 
  mutate(year_n_gap = (Year - (year_min - 1))) %>% 
  left_join(., yr_pk_nth, by = c("Year", "Admin_Unit_Code"))

## sites --------------------------------------------------------------------------------------
(nsites <- length(visits$Point_Name %>% unique()))
point_names <- visits$Point_Name %>% unique() %>% sort()

point_names <- point_names[!(substr(point_names,1,4) %in% c("ACAD", "SAIR", "ELRO"))]

point_name_pk <- visits %>% 
                  dplyr::select(Admin_Unit_Code, Point_Name) %>% 
                  filter(Admin_Unit_Code %!in% c("ACAD", "SAIR", "ELRO")) %>% 
                  distinct()
npoint_name_pk <- table(point_name_pk$Admin_Unit_Code) %>% as.vector()

mxsite <- max(npoint_name_pk)
site_vec <- seq(1, mxsite,1)

point_names_pk <- as_tibble(matrix(as.character(NA), ncol = npk, nrow = mxsite)) 
colnames(point_names_pk) <- pk_list

for(j in 1:ncol(point_names_pk)){
  slop <- point_name_pk %>% 
    dplyr::filter(Admin_Unit_Code == pk_list[j]) %>% 
    dplyr::select(Point_Name) %>% 
    pull() %>% 
    sort()
  for(i in 1:length(slop)){
    point_names_pk[i,j] <- slop[i]
  }
}

sites_park_tib <- as_tibble(matrix(NA, ncol = 4, nrow = npk))
colnames(sites_park_tib) <- c("park", "nsite", "site_names", "site_n")
sites_park_tib <- sites_park_tib %>% 
  mutate_at(3, as.list) %>% 
  mutate_at(1, as.character) %>% 
  mutate_at(4, as.list)

for(i in 1:npk){
  a <- point_names_pk[,i] %>% drop_na() %>% as_tibble()
  a$site_n <- seq(1:nrow(a))
  sites_park_tib[i,4][[1]] <- list(a[,2])
  sites_park_tib[i,3][[1]] <- list(a[,1])
  sites_park_tib[i,2] <- nrow(a)
  sites_park_tib[i,1] <- pk_list[i]
  rm(a)
}

##### write file: data/src/sites_park_tib.rds ------
# write_rds(sites_park_tib, file = "data/src/sites_park_tib.rds")

for(ii in 1:npk) {
  st_name <- sites_park_tib %>% 
    filter(park == sites_park_tib$park[ii])
  sit_mer <- cbind(st_name$site_names[[1]],
                    st_name$site_n[[1]])
  colnames(sit_mer) <- c("Point_Name", "site_n")
  if(ii == 1) {
    sit_mer2 <- sit_mer
  } else {
    sit_mer2 <- rbind(sit_mer2, sit_mer)
  }
}
sit_mer2 <- as_tibble(sit_mer2)
##### write file: data/src/site_n_key.rds ------
# write_rds(sit_mer2, file = "data/out/site_n_key.rds")
visits1 <- visits %>% filter(Admin_Unit_Code %!in% c("ACAD", "SAIR", "ELRO"))
visits2 <- left_join(visits1, sit_mer2, by = "Point_Name")

## years (yet again) --------------------------------------------------------------------------------------------------------
years <- visits2 %>% 
  select(Year) %>% 
  distinct() %>% 
  arrange(Year) %>% 
  pull()

dify <- rep(NA, lenght(years)-1)
for(i in 2:lenght(years)){dify[i-1] = years[i] - years[i-1]}
if(sum(dify) != lenght(years)-1) stop("there are gap years")

nyears <- length(years)

yr_pk <- y1 %>% 
  dplyr::select(Admin_Unit_Code, Year, year_n, year_n_gap, year_min) %>%
  filter(Admin_Unit_Code %!in% c("ACAD", "SAIR", "ELRO")) %>% 
  distinct() %>% 
  arrange(Admin_Unit_Code, Year)

### site year - check if all sites were sampled all years -------------------------------------------------------------------------------------------
yrs_st <- as_tibble(matrix(as.numeric(NA), 
                            ncol = length(sit_mer2$Point_Name), 
                            nrow = length(y1$Year %>% unique() %>% sort()))) 
colnames(yrs_st) <- sit_mer2$Point_Name

for(jj in 1:ncol(yrs_st)){
  ylop <- y1 %>% 
    filter(Admin_Unit_Code %!in% c("ACAD", "SAIR", "ELRO")) %>% 
    dplyr::select(Point_Name, Year) %>% 
    distinct() %>% 
    dplyr::filter(Point_Name == sit_mer2$Point_Name[jj]) %>% 
    dplyr::select(Year) %>% 
    pull() %>% 
    sort()
  for(ii in 1:length(ylop)){
    yrs_st[ii,jj] <- ylop[ii] 
  }
}

yrs_st_long <- pivot_longer(yrs_st, everything(), names_to = "Point_Name", values_to = "Year")
yrs_st_long <- na.omit(yrs_st_long)
table(yrs_st_long$Point_Name)

# site year park WITHOUT and WITH gap
for(j in 1:nrow(sit_mer2)) {
  
  st_pk_nth2 <- sit_mer2[j,]
  
  nth_syp <- yrs_st_long %>% 
    filter(Point_Name == st_pk_nth2$Point_Name) %>% 
    mutate(year_n_site = seq(1, nrow(.), 1),
           year_n_gap_site = Year + 1 - min(Year))
  
  if(j == 1){ ## create the new file here inside the loop
    sps_syp_nth <- nth_syp
  } else {
    sps_syp_nth <- rbind(sps_syp_nth, nth_syp)
  }
}

## intervals --------------------------------------------------------------------------------------------------------
ninterval <- 10
interval <- seq(1, ninterval, 1)

## species --------------------------------------------------------------------------------------------------------
sps_list <- y1 %>% 
  filter(Admin_Unit_Code %!in% c('ACAD', 'SAIR', 'ELRO')) %>% 
  select(AOU_Code) %>% 
  distinct() %>% 
  arrange(AOU_Code) %>% 
  pull()

nsps <- length(sps_list)

sps_key <- y1 %>% 
  filter(Admin_Unit_Code %!in% c('ACAD', 'SAIR', 'ELRO')) %>% 
  select(AOU_Code) %>% 
  distinct() %>% 
  arrange(AOU_Code) %>%  
  mutate(spskey = seq(1, nrow(.), 1))

### species pool for each park ---------------------------------------------------
sps_pk <- y1 %>% 
  filter(Admin_Unit_Code %!in% c('ACAD', 'SAIR', 'ELRO')) %>% 
  dplyr::select(Admin_Unit_Code, AOU_Code) %>% 
  distinct() %>% 
  left_join(., sps_key, by = "AOU_Code") %>% 
  arrange(Admin_Unit_Code, AOU_Code) %>% 
  as_tibble()

(nsps_pk <- table(sps_pk$Admin_Unit_Code) %>% as.vector())

for(i in 1:lenght(nsps_pk)) {
  nth_sps <- sps_pk %>% 
    filter(Admin_Unit_Code == pk_list[i]) %>% 
    mutate(spskey_p = seq(1, nrow(.),1))
  
  if(i == 1){
    sps_pk_nth <- nth_sps
  } else {
  sps_pk_nth <- rbind(sps_pk_nth, nth_sps)
  }
}

sps_pk2 <- sps_pk_nth %>% 
    nest(sps = AOU_Code,
        spskey = spskey,
        spskey_p = spskey_p) 
sps_pk2$nsps <- nsps_pk

y1 <- y1 %>% 
  filter(Admin_Unit_Code %!in% c('ACAD', 'SAIR', 'ELRO'))%>% 
  left_join(., sps_pk_nth, by = c("Admin_Unit_Code", "AOU_Code"))

## create for each park all combinations
## species, park, year, site, interval

yrs_st_long2 <- yrs_st_long %>% 
  mutate(park = substr(Point_Name, 1, 4))

# all possible combinations - those are the y_dat objects (WITH zeros),
#   versus the y objects (detections only - only ONES)
## expand grid for species in a site in years that site was sampled
for(ii in 1:npk) { 
    if((lenght(unique(yrs_st_long2$park)) == nrow(sps_pk2)) != TRUE) {
        stop("  missing some park somewhere!!!")
    }
    # get site and year combs for each park
    st_yr_it <- yrs_st_long2 %>% 
      filter(park == pk_list[ii]) %>% 
      select(-park)

    # get species list for that same park
    sps_it <- pull(sps_pk2$sps[ii][[1]])

    pk_it <- expand_grid(st_yr_it, sps_it)

   if((length(sps_it) * nrow(st_yr_it) == nrow(pk_it)) == FALSE){
      stop(glue("{pk_list[ii]} has some problem in the dimentions of site and species and year"))
    }

    y_dat1 <- pk_it

    rm(pk_it)

    if(ii == 1) {
      y_dat <- y_dat1
    } else {
      y_dat <- rbind(y_dat, y_dat1)
    }
}

## merge with site numbers
y_dat2 <- y_dat %>% 
  mutate(park = substr(Point_Name, 1, 4),
          interval_n = 10) %>% 
  left_join(., sit_mer2, by = "Point_Name") 

## fill array with bird detections
# unique combination of all columns minus interval: long uniqueID
# i need to keep only the first interval when a sps was detected in an occasion
y2 <- y1 %>% 
        filter(Admin_Unit_Code %!in% c('ACAD', 'SAIR', 'ELRO'))%>% 
        mutate(cunID = paste(park, Point_Name, site_n, 
                              Year, year_min, year_n, year_n_gap,
                              AOU_Code, spskey, spskey_p, 
                              StartTime, EventDate,
                              sep = "_"))  %>%
        arrange(Interval_n)%>% 
        group_by(cunID) %>%
        arrange(Interval_n) %>% 
        slice(1) %>% 
        ungroup()

y2$cunID %>% lenght()
y2$cunID %>% unique() %>% lenght()
# individuals captured multiple times or not in the first interval
# 'unique' sps detection
dim(y1) ; dim(y2)

y2 <- y2 %>% 
    select(-cunID)

# y3 is y2 with less columns
y3 <- y2 %>% 
  select(park, Point_Name, site_n, 
          Year, year_min, year_n, year_n_gap,
          AOU_Code, spskey, spskey_p, 
          Interval_n, 
          Bird_Count, 
          StartTime, EventDate) 

# Then remove repeated detections in the same interval that are auditory or visual
y2$unID <- y3$unID <- seq(1,nrow(y2),1)
table(y3$unID == y2$unID)
# bird count is in here so if there is a 3 birds detected at interval 1 and 
#    3 at 2 there are still THE SAME
dupy2 <- y2[!duplicated(y2 %>% select(-unID, -Bird_Count)),] #%>% select(-unID)
dim(dupy2) ; dim(y2)
dupy3 <- y3[!duplicated(y3 %>% select(-unID, -Bird_Count)),]
dim(dupy3) ; dim(y3)

nrow(dupy3) ; nrow(dupy2)
find_why <- dupy2[which(dupy2$unID %!in% dupy3$unID),]
dim(find_why)
if((nrow(dupy2) == nrow(dupy3)) == FALSE) {stop("there are repeated detections in the same interval!")}
if((nrow(find_why) + nrow(dupy3) == nrow(dupy2)) == FALSE) {stop("dimention mismatch!!")}

## there can be some parks with DOUBLE occasion recording for a bird: one for visual, one for auditory
##  keep only one - remove ID method
## not the case here

# check if still have duplicates - lets see why!
y3_indx <- y3 %>% 
    select(Point_Name, Year, AOU_Code) %>% 
    distinct()
if((nrow(y3_indx) == nrow(y3 %>% select(-c(Interval_n, unID)) %>% distinct())) == FALSE) {stop("there are repeated detections in the same interval!")}
# are there duplicates?
y3 %>% select(-Interval_n) %>% distinct() %>% duplicated() %>% table() 

# dupy3 %>% filter(Point_Name == "ACAD3107", Year == 2018, AOU_Code =="RBNU")

## add removal sampling intervals ------------------------------------------------------------
# populate y_dat3 with info from y2 - add ot only detections, but zeros in both intervals and occasions
# y_dat3 is the dataset with all occasions and intervals that HAPPENED/EXIST - non-detections! true zeros
y3 <- y3 %>% 
    mutate(interval_n = 10)
y_dat3 <- splitstackshape::expandRows(y3, "interval_n") 
y_dat3$interval_n <- rep(seq(1,10,1), nrow(y3))
nrow(y_dat3)/nrow(y3) == 10

y_dat3$bird_detec <- as.numeric(NA)
for(ii in 1:nrow(y_dat3)){
   if(as.numeric(y_dat3$Interval_n[ii]) == y_dat3$interval_n[ii]) {y_dat3$bird_detec[ii] <- 1} else {
      if(as.numeric(y_dat3$Interval_n[ii]) > y_dat3$interval_n[ii]) {y_dat3$bird_detec[ii] <- 0} else {print(round(ii/nrow(y_dat3),2))}}
}

##### write file: data/out/y_dat3.rds ------
# write_rds(y_dat3, file = "data/out/y_dat3.rds")

# This has all the zeros for all intervals, but I'm still missing an occasion for a species that was not detected in year X in site Y, but was detected ther on year X-1 or X+1
# for each species, in a park, for the years the park was sampled
for(ii in 1:length(pk_list)) {
    pk_loop <- pk_list[ii]
    yrs_st_long2_loop <- yrs_st_long2 %>% 
      filter(park == pk_loop)

    sps_pk_loop <- sps_pk %>% 
      filter(Admin_Unit_Code == pk_loop) %>% 
      select(AOU_Code) %>% 
      pull()

    grid_loop <- expand_grid(yrs_st_long2_loop, sps_pk_loop)
    if(ii == 1) {
      spy_grid <-grid_loop} else {
      spy_grid <- rbind(spy_grid, grid_loop)
      }
    rm(grid_loop)
}
spy_grid <- spy_grid %>% 
    rename(AOU_Code = sps_pk_loop)

# check which ones already exist (1 at the occasion, and add only the zeros)
spy_grid_yesdetec <- y_dat3 %>% 
    select(Point_Name, Year, park, AOU_Code) %>% 
    distinct()
spy_grid_zero <- setdiff(spy_grid, spy_grid_yesdetec)
# remember that I need covariates for this!!!
spy_grid_yesdetec_cov <- spy_grid_zero %>% 
    left_join(., 
              y_dat3 %>% 
                select(-unID, -interval_n, -Interval_n, 
                                -Bird_Count, -bird_detec, -AOU_Code, -spskey, -spskey_p) %>% 
                distinct(),
              by = c("Point_Name", "Year", "park"))

dim(spy_grid_yesdetec_cov) ; dim(spy_grid_zero)
# add the missing columns and rename the y_dat3 column
# bird_detec is the detection for that interval, while detec_occ is if it was detected that occasion (site-year)
y_dat3 <- y_dat3 %>% 
    rename(detec_occ = Bird_Count)

# the NAs and zeros
spy_grid_yesdetec_cov2 <- spy_grid_yesdetec_cov %>% 
    mutate(unID = NA,
            bird_detec = 0, 
            detec_occ = 0,
            Admin_Unit_Code = park)
# add the codes for species and sps in park
spy_grid_yesdetec_cov2 <- left_join(
    spy_grid_yesdetec_cov2, 
    sps_pk_nth, 
    by = c("Admin_Unit_Code", "AOU_Code"))

# expand intervals for spy_grid_yesdetec_cov2
spy_grid_yesdetec_cov2$Interval_n <- 10
spy_grid_yesdetec_cov3 <- splitstackshape::expandRows(spy_grid_yesdetec_cov2, "Interval_n") 
spy_grid_yesdetec_cov3$interval_n <- rep(seq(1,10,1), nrow(spy_grid_yesdetec_cov2))
spy_grid_yesdetec_cov3 <- spy_grid_yesdetec_cov3 %>% 
    mutate(Interval_n = NA)
nrow(spy_grid_yesdetec_cov3)/nrow(spy_grid_yesdetec_cov2) == 10

# sort the columns in the same order as y_dat3, and rbind everything!
y_dat3 <- y_dat3 %>% 
    mutate(Admin_Unit_Code = park) 

colnames(spy_grid_yesdetec_cov3) %>% sort() == colnames(y_dat3) %>% sort()

spy_grid_yesdetec_cov4 <- spy_grid_yesdetec_cov3 %>% 
    relocate(colnames(y_dat3))

colnames(spy_grid_yesdetec_cov4)  == colnames(y_dat3) 

y_dat4 <- rbind(y_dat3,
                spy_grid_yesdetec_cov4)
## remember: I have only one one per row, all zeros before one, and all NA after one

yr_pk <- yr_pk %>% 
  mutate(park = Admin_Unit_Code)

# park and species - ops, hopefully last index!
pk_key <- cbind(pk_list, seq(1,length(pk_list), 1)) %>% 
  as_tibble() %>% 
  rename(park = pk_list, parkey = V2)

y_dat5 <- y_dat4 %>% 
  left_join(., pk_key, by = "park")

pk_key_sps <- y_dat5 %>% 
  select(AOU_Code, park, parkey) %>% 
  distinct() %>% 
  group_by(AOU_Code) %>% 
  mutate(parkey_s = row_number()) %>% 
  ungroup()

y_dat6 <- y_dat5 %>% 
  left_join(., pk_key_sps, by = c("AOU_Code", "park", "parkey"))

# get covariate data ----------------------------------------------------------------------------------
# index to have the same dimentions as the bird data
X <- y_dat6 %>% 
  select(park, site_n, Year, Point_Name, interval_n)

# no year so far in covariates - key to get the combinations of covariates in each site
site_key <- y_dat6 %>% 
    select(Point_Name, site_n, park) %>% 
    distinct()

## site ----------------------------------------------------------------------------------------------
#! it is OK if ACAD and SAIR do not have covs now,
#!   they are gonna be removed from the data in the next step (back2d_covs_scales_3)
site_covs <- read_rds(PATH_COVS_SITE) %>% 
                rename(Point_Name = bird_sit,
                        park = ParkUnit) %>%
                left_join(., site_key, by =  c("park", "Point_Name")) %>% 
                select(-c(siteSTA, siteSAPden, park))

X1 <- left_join(X, site_covs, by = c("Point_Name","site_n"))
dim(X1)
X1 %>% select(Point_Name,siteDEN) %>% distinct() %>% arrange(siteDEN) %>% view()

## park --------------------------------------------------------------------------------
park_covs <- read_rds(PATH_COVS_PARK) %>% 
                rename(park = ParkUnit) 

X2 <- left_join(X1, park_covs, by = c("park"))
dim(X2)

## county ---------------------------------------------------------------------------------
coun_covs <- read_rds(PATH_COVS_COUN) %>% 
                rename(park = ParkUnit)

X3 <- left_join(X2, coun_covs, by = "park")
dim(X3)

nrow(X1) == nrow(X2)
nrow(X2) == nrow(X3)

## diversity
div_covs_site <- read_rds(PATH_DIV_SITE_COVS) %>% 
                    rename(siteH_g = S_mean,
                            siteEh_g = J_mean)

X4 <- left_join(X3, div_covs_site, by = "Point_Name")
dim(X4)
nrow(X4) == nrow(X3)

div_covs_park <- read_rds(PATH_DIV_PARK_COVS) %>% 
                    rename(parkH_g = S_mean,
                            parkEh_g = J_mean)

X5 <- left_join(X4, div_covs_park, by = "park")
dim(X5)
nrow(X5) == nrow(X4)

## park area ------------------------------------------------------------------
park_size <- as_tibble(matrix(NA, nrow = length(unique(y_dat4$park)), ncol = 2))
colnames(park_size) <- c("park", "area")
park_size$park <- sort(unique(y_dat4$park))
  
for(i in 1:nrow(park_size)) {
  pb <- read_rds(file = glue("data/park_raster/{park_size[i,1]}_pb.rds"))
  park_size[i,2] <- raster::area(pb)   # square km
  print(pull(park_size[i,1]))
}

# write_rds(park_size, file = "data/park_size.rds")

X6 <- left_join(X5, park_size, by = "park")
dim(X6)
nrow(X6) == nrow(X5)

# get detection covariates! 
inte_key <- y1 %>% 
  filter(Admin_Unit_Code %!in% c("ACAD", "ELRO", "SAIR")) %>% 
  select(Interval_Length, Interval_n) %>% 
  rename(interval_n = Interval_n) %>% 
  mutate(interval_n = as.numeric(interval_n)) %>% 
  distinct()
y_dat7 <- left_join(y_dat6, inte_key, by = "interval_n")
y_dat8 <- y_dat7 %>% 
  mutate(StartTime2 = as.period(seconds(StartTime) + minutes(substr(Interval_Length,1,1) %>%
                                                                      as.numeric() %>% 
                                                                      as_hms()), unit = "hours") %>% 
                              as.numeric(),
          EventDate2 = yday(EventDate)) 

table(X6$park == y_dat8$park)
table(X6$site_n == y_dat8$site_n)
table(X6$Year == y_dat8$Year)
table(X6$Point_Name == y_dat8$Point_Name) 

X7 <- X6

X7$EventDate2 <- y_dat8$EventDate2 ; X7$StartTime2 <- y_dat8$StartTime2

# X6 <- X5 %>% 
#   mutate(date_jul = as.numeric(scale(EventDate2)),
#          time_jul = as.numeric(scale(StartTime2)),
#          siteBA_s = as.numeric(scale(siteBA)),
#          siteDEN_s = as.numeric(scale(siteDEN)),
#          parkBA_s = as.numeric(scale(parkBA)),
#          parkDEN_s = as.numeric(scale(parkDEN)),
#          counBA_s = as.numeric(scale(counBA)),
#          counDEN_s = as.numeric(scale(counDEN)),
#          area_s = as.numeric(scale(area))) 

##### write files  ------
write_rds(y_dat8, file = "data/y_dat8.rds")
write_rds(X7, file = "data/X.rds")
# write_rds(sps_pk_nth, file = "data/sps_pk_nth.rds")

cat(paste("\n\n Done \n\n\n"))
