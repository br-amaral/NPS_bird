# *********************************************************************************
# ----------------------------- 3_create_bird_data.R ------------------------------
# *********************************************************************************
# Code to create the y data file to run in the JAGS model 
#   can be run for 1, all or groups f species, in one or all parks
#
# Source ---------------------------------------------
#           - code/format_bird_data/2_format_data.R:
#
# Input ----------------------------------------------
#           - y1: table of ones and zeros for sps detections
#           - visits:

#           - from code/2_format_data.R:
#             -- y1: table of ones and zeros for sps detections
#             -- nsite_pk: number of sites sampled in each park
#             -- yrs_pk: number of years sampled in each park
#             -- ninterval: number of intervals of removal sampling
#             -- site_vec:  
#             -- site_pk:
#           - data/src/original/NETN_2020/BirdSpecies.csv: get species list
#           - data/NETN-forest/tree_ba_import.rds
#           - data/park_raster/{pk[i]}_pb.rds: raster of each park to get park size 
#             -- data/NETN-forest/tree_ba_import.rds
#             -- data/NETN-forest/tree_den_import.rds
#             -- data/NETN-forest/stand_import.rds
#             -- data/NETN-forest/tree_ba_tab_park.rds
#             -- data/NETN-forest/tree_den_tab_park.rds
#             -- data/NETN-forest/stand_struc_tab_park.rds
#             -- data/FIA/out/bas_area_tot_import.rds
#             -- data/FIA/out/tree_acre_tot_import.rds
#             -- data/FIA/out/stand_struc_import.rds
#
# Output ---------------------------------------------
#           - data/out/site_n_key.rds: park and site unique key
#           - data/src/sites_park_tib.rds: tibble with park, number of sites and site numbers and codes
#           - data/out/y_dat3.rds: first tibble with ALL occasions for sps, park, site, year and interval
#           - data/y_dat6.rds: birds data for each occasion, with park, species and site indexes
#           - data/X10.rds: environmental variables for all scales for each occasion, same dim() as y_dat6.rds
#           - data/sps_pk_nth.rds: species code in each park

#           - from code/2_format_data.R:
#             -- data/out/visits.rds : data from the visit files


## detach packages and clear workspace
if(!require(freshr)){install.packages("freshr")}
freshr::freshr()

# Load packages --------------------------------------
library(conflicted)
library(tidyverse)
library(glue)
library(hms)
library(lubridate)
# library(splitstackshape)

conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::filter)
# conflicts_prefer(scales::alpha)

# Make functions --------------------------------------
colanmes <- colnames
lenght <- length
`%!in%` <- Negate(`%in%`)

# Source code -----------------------------------------
## Create empty matrix with all parks, species, years, sites and intervals --------------------------
source("code/format_bird_data/format_data.R")

#
yog <- y1

# Import data -----------------------------------------
## file paths

#  Point_Name siteBA site_n park 
PATH_SITE_COVS <- "data/out/close_points_fcovs.rds"

PATH_TREE_BA_PARK <- "data/NETN-forest/tree_ba_tab_park.rds"
PATH_TREE_DEN_PARK <- "data/NETN-forest/tree_den_tab_park.rds"
PATH_TREE_STR_PARK <- "data/NETN-forest/stand_struc_tab_park.rds"

PATH_TREE_BA_COUN <- "data/FIA/out/bas_area_tot_import.rds"
PATH_TREE_DEN_COUN <- "data/FIA/out/tree_acre_tot_import.rds"
PATH_TREE_STR_COUN <- "data/FIA/out/stand_struc_import.rds"

## parks -------------------------------------------------------------------------------------------
pk_list <- visits %>% 
   select(Admin_Unit_Code) %>% 
   distinct() %>%  
   arrange(Admin_Unit_Code) %>% 
   pull()

npk <- length(pk_list)

## years -------------------------------------------------------------------------------------------
# year_n gives an ordinal number for each calendar year, while year_n_gap jumps years 
#  that have no sampling: year = 2007, 2009 ; year_n = 1, 2 ; year_n_gap = 1,3
yr_pk <- yr_pk %>% 
  arrange(Admin_Unit_Code,Year)

yr_pk_min <- yr_pk %>% 
  group_by(Admin_Unit_Code) %>% 
  mutate(year_min = min(Year),
         park = Admin_Unit_Code) %>% 
  ungroup() %>% 
  select(Admin_Unit_Code, park, year_min) %>% 
  distinct()

for(i in 1:lenght(nyr_pk)) {
  nth_year <- yr_pk %>% 
    filter(Admin_Unit_Code == pk[i]) %>% 
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

point_name_pk <- visits %>% dplyr::select(Admin_Unit_Code, Point_Name) %>% distinct()
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
write_rds(sites_park_tib, file = "data/src/sites_park_tib.rds")

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
write_rds(sit_mer2, file = "data/out/site_n_key.rds")

visits2 <- left_join(visits, sit_mer2, by = "Point_Name")

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
  distinct() %>% 
  arrange(Admin_Unit_Code, Year)

### site year - check if all sites were sampled all years -------------------------------------------------------------------------------------------
yrs_st <- as_tibble(matrix(as.numeric(NA), 
                           ncol = length(sit_mer2$Point_Name), 
                           nrow = length(y1$Year %>% unique() %>% sort()))) 
colnames(yrs_st) <- sit_mer2$Point_Name

for(jj in 1:ncol(yrs_st)){
  ylop <- y1 %>% dplyr::select(Point_Name, Year) %>% distinct() %>% 
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
  select(AOU_Code) %>% 
  distinct() %>% 
  arrange(AOU_Code) %>% 
  pull()

nsps <- length(sps_list)

sps_key <- y1 %>% 
  select(AOU_Code) %>% 
  distinct() %>% 
  arrange(AOU_Code) %>%  
  mutate(spskey = seq(1, nrow(.), 1))

### species pool for each park ---------------------------------------------------
sps_pk <- y1 %>% 
  dplyr::select(Admin_Unit_Code, AOU_Code) %>% 
  distinct() %>% 
  left_join(., sps_key, by = "AOU_Code") %>% 
  arrange(Admin_Unit_Code, AOU_Code) %>% 
  as_tibble()

(nsps_pk <- table(sps_pk$Admin_Unit_Code) %>% as.vector())

for(i in 1:lenght(nsps_pk)) {
  nth_sps <- sps_pk %>% 
    filter(Admin_Unit_Code == pk[i]) %>% 
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
# unique combination of all columns minus interval crazy uniqueID
y2 <- y1 %>% 
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
# bird count is in here so if there is a 3 birds detected at interval 1 and 3 at 2 there are still THE SAME
dupy2 <- y2[!duplicated(y2 %>% select(-unID, -Bird_Count)),] #%>% select(-unID)
dim(dupy2) ; dim(y2)
dupy3 <- y3[!duplicated(y3 %>% select(-unID, -Bird_Count)),]
dim(dupy3) ; dim(y3)

nrow(dupy3) ; nrow(dupy2)
find_why <- dupy2[which(dupy2$unID %!in% dupy3$unID),]
dim(find_why)
if((nrow(dupy2) == nrow(dupy3)) == FALSE) {stop("there are repeated detections in the same interval!")}
if((nrow(find_why) + nrow(dupy3) == nrow(dupy2)) == FALSE) {stop("dimention mismatch!!")}

## there can be some parks with DOUBLE occasion recording for a bird: one for visual, one for auditory :(
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
write_rds(y_dat3, file = "data/out/y_dat3.rds")

# This has all the zeros for all intervals, but I'm still missing an occasion for a species that was not detected in year X in site Y, but was detected ther on year X-1 or X+1
# for each species, in a park, for the years the park was sampled
for(ii in 1:length(pk)) {
   pk_loop <- pk[ii]
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
pk_key <- cbind(pk, seq(1,length(pk), 1)) %>% 
  as_tibble() %>% 
  rename(park = pk, parkey = V2)

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
X <- y_dat6 %>% 
  select(park, site_n, Year, Point_Name, interval_n, Year)

# no year so far in covariates
site_key <- y_dat6 %>% 
   select(Point_Name, site_n, park) %>% 
   distinct()

## site ------------
## no data for all years, using mean between years
close_points_f2 <- read_rds(PATH_SITE_COVS)

tree_ba_tab_site <- close_points_f2 %>% 
  mutate(Point_Name = bird_sit,
         siteBA = BA_m2haM) %>% 
  left_join(., site_key, by = c("Point_Name", "park")) %>%
  select(Point_Name, park, site_n, siteBA) 

X1 <- left_join(X, tree_ba_tab_site %>% select(-Point_Name), by = c("park", "site_n"))

tree_den_tab_site <- close_points_f2 %>% 
  mutate(Point_Name = bird_sit,
         siteDEN = treeden_haM) %>% 
  left_join(., site_key, by = c("Point_Name", "park")) %>% 
  select(Point_Name, park, site_n, siteDEN) 

X2 <- left_join(X1, tree_den_tab_site, by = c("park", "site_n", "Point_Name"))

stand_struc_tab_site <- close_points_f2 %>% 
  mutate(Point_Name = bird_sit) %>% 
  select(Point_Name,
         park,
         pctBA_poleM,
         pctBA_matureM,
         pctBA_largeM) %>%
  left_join(., site_key, by = c("Point_Name", "park")) 

X3 <- left_join(X2, stand_struc_tab_site, by = c("park", "site_n", "Point_Name"))

## park --------------------------------------------------------------------------------
tree_ba_tab_park <- read_rds(PATH_TREE_BA_PARK) %>% 
  na.omit() %>% 
  group_by(park) %>% 
  mutate(mean_total_BA = mean(mean_total_BA)) %>% 
  ungroup() %>% 
  select(-c(Year, mean_total_BA_SE)) %>% 
  distinct() %>% 
  rename(parkBA = mean_total_BA)

X4 <- left_join(X3, tree_ba_tab_park, by = c("park"))

tree_den_tab_park <- read_rds(PATH_TREE_DEN_PARK) %>% 
  na.omit() %>% 
  group_by(park) %>% 
  mutate(mean_total_den = mean(mean_total_den)) %>% 
  ungroup() %>% 
  select(-c(Year)) %>% 
  distinct() %>% 
  rename(parkDEN = mean_total_den)

X5 <- left_join(X4, tree_den_tab_park, by = c("park"))

stand_struc_tab_park <- read_rds(PATH_TREE_STR_PARK)

## county ---------------------------------------------------------------------------------
tree_ba_tab_coun <- read_rds(PATH_TREE_BA_COUN) %>% 
  select(-BAA_SE_mean) %>% 
  rename(counBA = BAA_mean)

X6 <- left_join(X5, tree_ba_tab_coun, by = c("park"))

tree_den_tab_coun <- read_rds(PATH_TREE_DEN_COUN) %>% 
  select(-TPA_SE_mean) %>% 
  rename(counDEN = TPA_mean)

X7 <- left_join(X6, tree_den_tab_coun, by = c("park"))

stand_struc_tab_coun <- read_rds(PATH_TREE_STR_COUN)

## park area ------------------------------------------------------------------
park_size <- as_tibble(matrix(NA, nrow = length(unique(y_dat4$park)), ncol = 2))
colnames(park_size) <- c("park", "area")
park_size$park <- sort(unique(y_dat4$park))
  
for(i in 1:length(pk)) {
  pb <- read_rds(file = glue("data/park_raster/{pk[i]}_pb.rds"))
  park_size[i,2] <- raster::area(pb)   # square km
}

if(length(park_size) > 1) {
  park_size$area <- park_size$area %>% scale() %>% as.numeric()
}

X8 <- left_join(X7, park_size, by = "park")

# get detection covariates! 
inte_key <- y1 %>% 
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

table(X8$park == y_dat8$park)
table(X8$site_n == y_dat8$site_n)
table(X8$Year == y_dat8$Year)
table(X8$Point_Name == y_dat8$Point_Name) 

X9 <- X8

X9$EventDate2 <- y_dat8$EventDate2 ; X9$StartTime2 <- y_dat8$StartTime2

X10 <- X9 %>% 
  mutate(date_jul = as.numeric(scale(EventDate2)),
         time_jul = as.numeric(scale(StartTime2)),
         siteBA_s = as.numeric(scale(siteBA)),
         siteDEN_s = as.numeric(scale(siteDEN)),
         parkBA_s = as.numeric(scale(parkBA)),
         parkDEN_s = as.numeric(scale(parkDEN)),
         counBA_s = as.numeric(scale(counBA)),
         counDEN_s = as.numeric(scale(counDEN)),
         area_s = as.numeric(scale(area))) 

##### write files  ------
write_rds(y_dat8, file = "data/y_dat8.rds")
write_rds(X10, file = "data/X10.rds")
write_rds(sps_pk_nth, file = "data/sps_pk_nth.rds")

rm(X)
rm(X1)
rm(X2)
rm(X3)
rm(X4)
rm(X5)
rm(X6)
rm(X7)
rm(X8)
rm(y_dat3)
rm(y_dat2)
rm(y_dat1)
rm(y_dat)
#rm(y1)
rm(y2)
rm(field_dat)
rm(field_dat0.5)
rm(field_dat0.75)
rm(field_dat1)
rm(field_dat1.5)
rm(field_dat1.75)
rm(field_dat2)
gc()

# 
# # merge objects
# tree_ba_tab_site <- tree_ba_tab_site %>% 
#   rename(siteBA = mean_tot_BA) %>% 
#   select(Point_Name, park, site_n, siteBA)
# 
# tree_ba_tab_park <- tree_ba_tab_park %>% 
#   rename(parkBA = mean_total_BA) %>% 
#   select(park, parkBA)
# 
# tree_ba_tab_coun <- tree_ba_tab_coun %>% 
#   rename(counBA = BAA_mean) %>% 
#   select(park, counBA)
# 
# tree_ba_mod <- tree_ba_tab_site %>% 
#   left_join(., tree_ba_tab_park, by = "park") %>% 
#   left_join(., tree_ba_tab_coun, by = "park")
# 
# cor(tree_ba_mod[,4:6])
# 
# tree_den_tab_site <- tree_den_tab_site %>% 
#   rename(siteDEN = mean_tot_den) %>% 
#   select(Point_Name, park, site_n, siteDEN)
# 
# tree_den_tab_park <- tree_den_tab_park %>% 
#   rename(parkDEN = mean_total_den) %>% 
#   select(park, parkDEN)
# 
# tree_den_tab_coun <- tree_den_tab_coun %>% 
#   rename(counDEN = TPA_mean) %>% 
#   select(park, counDEN)
# 
# tree_den_mod <- tree_den_tab_site %>% 
#   left_join(., tree_den_tab_park, by = "park") %>% 
#   left_join(., tree_den_tab_coun, by = "park")
# 
# cor(tree_den_mod[,4:6])
# 
# write_rds(tree_ba_mod, file = "data/out/tree_ba_mod.rds")
# write_rds(tree_den_mod, file = "data/out/tree_den_mod.rds")
# 
# # make sure all my covariates are the same dimentions as
# n_spsM <- y$sps %>% unique() %>% length()
# n_pkM <- y$park %>% unique() %>% length()
# n_bs <- 7
# n_as <- 3
# y <- y_dat3 %>% 
#   mutate(park = substr(Point_Name, 1, 4)) %>% 
#   left_join(., site_key, by = "Point_Name") %>% 
#   select(bird_detec, sps_it, site_n, year_s, interval_n)
# X
# 
# for(i in 1:n_spsM){	     # species i
#   for(r in 1:n_pkM){	  # park r
#     beta0[i,r]
#     alpha0[i,r]
#   }
#   
#   for(j in 1:n_bs){     # number of betas
#     beta[j,i]
#   }
#   
#   for(j in 1:n_as){     # number of betas
#     alpha[j,i]
#   }
# }
# 
# 
# Z[y[a,2],y[a,3],y[a,4]]
# # get park site coordinates
# 
# site_coords <- y1 %>% 
#   select(Admin_Unit_Code, Point_Name, Transect_CODE, Longitude, Latitude, site_n, BCR) %>% 
#   distinct()
# 
# write_rds(site_coords, "data/out/site_coords.rds")
# 
# 
# 
# 
# 
# 
# if (class(spslist)[1] == "tbl_df") {
#   yf <- y1 %>% 
#     filter(AOU_Code %in% pull(spslist)) 
# }
# 
# if (class(spslist)[1] == "character") {
#   yf <- y1 %>% 
#     filter(AOU_Code %in% spslist)
# }
# 
# if (park_ana != "all"){
#   yf <- yf %>% 
#     filter(Admin_Unit_Code %in% park_list) 
# }
# 
# # park
# (npk <- yf$Admin_Unit_Code %>% unique() %>% length())
# (pk <- sort(unique(yf$Admin_Unit_Code)))
# (park_list <- sort(unique(y1$Admin_Unit_Code)))
# 
# # site
# site_pk <- y1 %>% dplyr::select(Admin_Unit_Code, Point_Name) %>% distinct()
# (nsite_pk <- table(site_pk$Admin_Unit_Code) %>% as.vector())
# (nsite_pk <- nsite_pk[which(park_list %in% pk)])
# mxsite <- max(nsite_pk)
# site_vec <- seq(1, mxsite, 1)
# 
# # year
# years <- yf$Year %>% unique() %>% sort()
# years_s <- years - (min(y1$Year)-1)
# (nyr_s <- length(years_s))
# 
# yr_pk <- yf %>% dplyr::select(Admin_Unit_Code, Year) %>% distinct()
# (nyr_pk <- table(yr_pk$Admin_Unit_Code) %>% as.vector())
# 
# yrs_pk <- as_tibble(matrix(as.numeric(NA), 
#                            ncol = length(park_list), 
#                            nrow = length(y1$Year %>% unique() %>% sort()))) 
# colnames(yrs_pk) <- park_list
# 
# for(jj in 1:ncol(yrs_pk)){
#   ylop <- y1 %>% dplyr::select(Admin_Unit_Code, Year) %>% distinct() %>% 
#     dplyr::filter(Admin_Unit_Code == park_list[jj]) %>% 
#     dplyr::select(Year) %>% 
#     pull() %>% 
#     sort()
#   for(ii in 1:length(ylop)){
#     yrs_pk[ii,jj] <- ylop[ii] - (min(y1$Year)-1)
#   }
# }
# 
# # have only ones for detections
# sum(yf$Bird_Count)
# ifelse(yf$Bird_Count > 1, yf$Bird_Count <- 1, yf$Bird_Count <- yf$Bird_Count)
# sum(yf$Bird_Count) == length(yf$Bird_Count)
# 
# # ordinal day and time
# yf$EventDate2 <- scale(yday(yf$EventDate))
# yf$StartTime2 <- scale(as.period(seconds(yf$StartTime) + minutes(substr(yf$Interval_Length,1,1) %>%
#                                                                    as.numeric() %>% 
#                                                                    as_hms()), unit = "hours") %>% 
#                          as.numeric())
# 
# # years <- c(2001, 2004, 2006, 2008, 2011, 2013, 2016, 2019)   ## years with environmental data
# yf2 <- yf %>%  
#   #  filter(Year %in% years) %>% 
#   mutate(Interval = as.numeric(Interval))
# 
# # keep only first detection of the 10 intervals --------------------------------------------------
# yf3 <- yf2 %>% 
#   #filter(Bird_Count == 1) %>% 
#   mutate(Interval_n = as.numeric(Interval_n)) %>% 
#   group_by(AOU_Code, EventDate, site_n, Admin_Unit_Code) %>%
#   filter(Interval_n == min(Interval_n)) %>% 
#   slice(1) %>%   # takes the first occurrence if there is a tie
#   ungroup()
# 
# dim(yf3)
# # check
# sum(yf3$Bird_Count, na.rm = T)
# 
# ## if only one sps (format) -----------------
# if((as_tibble(spslist) %>% nrow()) == 1) {
#   # create 4 columns tibble
#   y2 <- matrix(ncol = 5,
#                nrow = sum(nyr_pk * nsite_pk * ninterval),
#                NA) %>% 
#     as_tibble()
#   
#   colnames(y2) <- c("y", "park", "year", "site", "interval")
#   
#   # parks
#   y2$park <- rep(pk, nyr_pk * nsite_pk * ninterval)
#   
#   # years
#   yr_tib <- vector()
#   yf3$year2 <- yf3$Year - (min(y1$Year)-1)
#   yf3 <- yf3 %>% select(Admin_Unit_Code, Year, year2, site_n, Interval_n, Bird_Count)
#   
#   for(ii in 1:npk){
#     yrlp <- yf3 %>% 
#       filter(Admin_Unit_Code == pk[ii]) %>% 
#       select(year2) %>% 
#       pull() %>% 
#       na.omit %>% 
#       as.vector()
#   
#     if(ii == 1){
#       yr_tib <- sort(rep(yrlp, nsite_pk[ii] * ninterval))
#     } 
#     if(ii > 1){
#       yr_tib <- c(yr_tib, sort(rep(yrlp, nsite_pk[ii] * ninterval)))
#     }
#   }
#   
#   y2$year <- yr_tib
#   
#   # sites
#   sit_tib <- vector()
#   
#   for(ii in 1:npk){
#     if(ii == 1){
#       sit_tib <- rep(seq(1:nsite_pk[ii]), 
#                      times = nyr_pk[ii],
#                      each = ninterval)
#     }
#     if(ii > 1){
#       sit_tib <- c(sit_tib, rep(seq(1:nsite_pk[ii]), 
#                                 times = nyr_pk[ii],
#                                 each = ninterval))
#     }
#   }
#   
#   y2$site <- sit_tib
#   
#   y2$interval <- rep(seq(1,10,1), length.out = nrow(y2))
#   
#   y2$y <- as.numeric(y2$y)
#   
#   # add counts (the ones) ------------------------------------------------------------------
#   for(aa in 1:nrow(yf3)){
#     
#     yl <- yf3[aa,]
#     yl
#    ( p <- which(pk == yl$Admin_Unit_Code, arr.ind = T))
#    ( j <- which(site_vec[1:nsite_pk[p]] == yl$site_n, arr.ind = T))       # site
# (    t <- yl$year2  )                         # year
# (    k <- as.numeric(yl$Interval_n)   )                                   # interval
#     
#     y2[which(y2$park == pk[p] &
#              y2$site == j &
#              y2$year == t &
#              y2$interval == k),1] <- 1
#   }
#   
#   aa <- aa + 1
#   
#   y2$y %>% sum(na.rm = T)
#   
#   
#   y2[which(y2$park == pk[p] &
#              y2$site == j ),]
#   
#   
#   # check
#   nrow(yf3); table(is.na(y2$y))
#   yf3 %>% select(Admin_Unit_Code, site_n, Year, Interval_n) %>% distinct() %>% dim()
#   
#   # add zeros before the ones --------------------------------------------------------
#   for(p in 1:dim(yyy3)[1]) {  
#     for(j in 1:nsite_pk[p]) {
#       for(t in 1:dim(yyy3)[3]) {
#         ind <- which.min(is.na(yyy3[p,j,t,])) %>% as.numeric()
#         if(ind > 1) {
#           yyy3[p,j,t,(1:(ind-1))] <- 0
#         }
#       }
#     }
#   }
#   
#   # check
#   table(yyy3) %>% sum(na.rm = T); table(is.na(yyy3))[1]
#   
#   # add all zeros if never detected --------------------------------------------------
#   ## create zeros for species, separate them from NAs and add them to a new data frame
#   SAMPLEDsites <- yf3 %>% 
#     dplyr::select(Point_Name, Year) %>%
#     distinct() 
#   
#   ALLsites_sps <- expand.grid(SAMPLEDsites %>% dplyr::select(Point_Name) %>% distinct() %>% pull(),
#                               SAMPLEDsites %>% dplyr::select(Year) %>% distinct() %>% pull()
#   )
#   
#   # select the zeros
#   table(yf3 %>% dplyr::select(Point_Name, Year) %>% distinct()) 
#   
#   ALLsites_sps <- ALLsites_sps %>%
#     rename(Point_Name = Var1,
#            Year = Var2) %>% 
#     as_tibble() 
#   
#   # Have to NA out sites with NA data
#   dim(ALLsites_sps) ; dim(SAMPLEDsites)
#   ZEROsites <- dplyr::setdiff(ALLsites_sps, SAMPLEDsites[,c(1,2)]) 
#   nrow(ALLsites_sps) - nrow(SAMPLEDsites) == nrow(ZEROsites)
#   
#   ZEROsites$point_data <- glue("{ZEROsites$Point_Name}_{ZEROsites$Year}")
#   SAMPLEDsites$point_data <- glue("{SAMPLEDsites$Point_Name}_{substring(SAMPLEDsites$Year,1,4)}")
#   table(ZEROsites$point_data %in% SAMPLEDsites$point_data) # how many occasions never occured
#   
#   NAsites <- as_tibble(cbind(point_data = ZEROsites$point_data,
#                              Point_Name = substring(ZEROsites$point_data,1,8),
#                              Year = as.numeric(substring(ZEROsites$point_data,10,13))))
#   
#   NAsites2 <- left_join(NAsites, site_pk[,2:3], by = "Point_Name") %>% 
#     mutate(Year = as.numeric(Year))
#   year_ind <- cbind(years, seq(1:length(years))) %>% as_tibble()
#   colnames(year_ind) <- c("Year", "year_ind")
#   NAsites2 <- left_join(NAsites2, year_ind, by = "Year") 
#   NAsites2 <- NAsites2 %>% 
#     mutate(Admin_Unit_Code = substr(Point_Name,1,4)) %>% 
#     dplyr::select(Admin_Unit_Code, site_n, year_ind)
#   pk_num <- cbind(pk, seq(1:npk)) %>%
#     as_tibble() %>% 
#     rename(Admin_Unit_Code = pk, AUC_num = V2)
#   NAsites2 <- left_join(NAsites2, pk_num, by = "Admin_Unit_Code") %>% 
#     mutate(AUC_num = as.numeric(AUC_num))
#   
#   for(p in 1:dim(yyy3)[1]) {
#     for(j in 1:nsite_pk[p]) {
#       for(t in 1:dim(yyy3)[3]) {
#         if(all(is.na(yyy3[p,j,t,])) ) {
#           yyy3[p,j,t,] <- 0
#         }
#       }
#     }
#   }
#   
#   # check
#   table(yyy3); table(is.na(yyy3))
#   
#   # but reinsert the all NA's for the NA sites
#   for(x in 1:nrow(NAsites2)) {
#     yyy3[NAsites2$AUC_num[x], NAsites2$site_n[x], NAsites2$year_ind[x], ] <- NA
#   }
#   
#   # check
#   table(yyy3); table(is.na(yyy3))
#   
#   # ordinal day and time
#   time <- array(0, 
#                 dim = c(npk,
#                         max(nsite_pk),
#                         length(years), 
#                         ninterval),
#                 dimnames = list(pk,
#                                 site_vec[1:max(nsite_pk)],
#                                 years,
#                                 seq(1,10,1)))
#   
#   day <- array(0, 
#                dim = c(npk,
#                        max(nsite_pk),
#                        length(years)),
#                dimnames = list(pk,
#                                site_vec[1:max(nsite_pk)],
#                                years))
#   for(p in 1:npk) {
#     for(j in 1:nsite_pk[p]) {
#       for(t in 1:dim(yyy3)[3]) {# nyr_pk[p]) {
#         dd <- yf3 %>% 
#           filter(Admin_Unit_Code == pk[p],
#                  Year == years[t],
#                  site_n == j) %>% 
#           dplyr::select(EventDate2) %>% 
#           distinct() %>% 
#           pull() %>% 
#           as.numeric()
#         
#         ifelse(length(dd) == 0, NA, day[p,j,t] <- dd)
#         
#         for(k in 1:ninterval) {
#           tt <- yf2 %>% 
#             filter(Admin_Unit_Code == pk[p],
#                    Year == years[t],
#                    site_n == j,
#                    Interval_n == k) %>% 
#             dplyr::select(StartTime2) %>% 
#             distinct() %>% 
#             pull() %>% 
#             as.numeric()
#           
#           ifelse(length(tt) == 0, NA, time[p,j,t,k] <- tt)
#           
#         }
#       }
#     }
#   }
# }
# 
# # filter for more than one sps tables (format) -----------------
# sps_f_names <- yf3 %>% 
#   dplyr::select(AOU_Code) %>% 
#   distinct()
# 
# n_sps <- sps_f_names %>% 
#   pull() %>% 
#   length()
# 
# ## if multiple species -------------------------------  
# if((as_tibble(spslist) %>% nrow()) > 1) {
#   
#   # create 4d array
#   yyy3 <- array(NA, 
#                 dim = c(n_sps,
#                         npk,
#                         max(nsite_pk),
#                         length(years), 
#                         ninterval
#                 ),
#                 dimnames = list(as.vector(sps_f_names %>% pull),
#                                 pk,
#                                 site_vec[1:max(nsite_pk)],
#                                 years,
#                                 seq(1,10,1)
#                 ))
#   
#   # add counts (the ones) ------------------------------------------------------------------
#   for(a in 1:nrow(yf3)){
#     
#     yl <- yf3[a,]
#     
#     i <- which(sps_f_names == yl$AOU_Code, arr.ind = T)
#     p <- which(pk == yl$Admin_Unit_Code, arr.ind = T)
#     j <- which(site_vec[1:nsite_pk[p]] == yl$site_n, arr.ind = T)       # site
#     t <- which(years == yl$Year, arr.ind = T)                           # year
#     k <- as.numeric(yl$Interval_n)                                      # interval
#     
#     yyy3[i,p,j,t,k] <- ifelse(sum(yyy3[i,p,j,t,k],
#                                 yl %>% 
#                                   dplyr::select(Bird_Count) %>% 
#                                   pull(), 
#                                 na.rm = T) > 0, 1, 0)
#   }
#   # check
#   table(yyy3); table(is.na(yyy3))[1]
#   
#   # add zeros before the ones --------------------------------------------------------
#   for(i in 1:dim(yyy3)[1]) {  
#     for(p in 1:dim(yyy3)[2]) {  
#       for(j in 1:nsite_pk[p]) {
#         for(t in 1:dim(yyy3)[4]) {
#           ind <- which.min(is.na(yyy3[i,p,j,t,])) %>% as.numeric()
#           if(ind > 1) {
#             yyy3[i,p,j,t,(1:(ind-1))] <- 0
#           }
#         }
#       }
#     }
#   }  
#   
#   # check
#   table(yyy3) %>% sum(na.rm = T); table(is.na(yyy3))[1]
#   
#   # add all zeros if never detected --------------------------------------------------
#   ## create zeros for species, separate them from NAs and add them to a new data frame
#   SAMPLEDsites <- yf3 %>% 
#     dplyr::select(Point_Name, Year) %>%
#     distinct() 
#   
#   ALLsites_sps <- expand.grid(SAMPLEDsites %>% dplyr::select(Point_Name) %>% distinct() %>% pull(),
#                               SAMPLEDsites %>% dplyr::select(Year) %>% distinct() %>% pull()
#   )
#   
#   # select the zeros
#   table(yf3 %>% dplyr::select(Point_Name, Year) %>% distinct()) 
#   
#   ALLsites_sps <- ALLsites_sps %>%
#     rename(Point_Name = Var1,
#            Year = Var2) %>% 
#     as_tibble() 
#   
#   # Have to NA out sites with NA data
#   dim(ALLsites_sps) ; dim(SAMPLEDsites)
#   ZEROsites <- dplyr::setdiff(ALLsites_sps, SAMPLEDsites[,c(1,2)]) 
#   nrow(ALLsites_sps) - nrow(SAMPLEDsites) == nrow(ZEROsites)
#   
#   ZEROsites$point_data <- glue("{ZEROsites$Point_Name}_{ZEROsites$Year}")
#   SAMPLEDsites$point_data <- glue("{SAMPLEDsites$Point_Name}_{substring(SAMPLEDsites$Year,1,4)}")
#   table(ZEROsites$point_data %in% SAMPLEDsites$point_data) # how many occasions never occured
#   
#   NAsites <- as_tibble(cbind(point_data = ZEROsites$point_data,
#                              Point_Name = substring(ZEROsites$point_data,1,8),
#                              Year = as.numeric(substring(ZEROsites$point_data,10,13))))
#   
#   NAsites2 <- left_join(NAsites, site_pk[,2:3], by = "Point_Name") %>% 
#     mutate(Year = as.numeric(Year))
#   year_ind <- cbind(years, seq(1:length(years))) %>% as_tibble()
#   colnames(year_ind) <- c("Year", "year_ind")
#   NAsites2 <- left_join(NAsites2, year_ind, by = "Year") 
#   NAsites2 <- NAsites2 %>% 
#     mutate(Admin_Unit_Code = substr(Point_Name,1,4)) %>% 
#     dplyr::select(Admin_Unit_Code, site_n, year_ind)
#   pk_num <- cbind(pk, seq(1:npk)) %>%
#     as_tibble() %>% 
#     rename(Admin_Unit_Code = pk, AUC_num = V2)
#   NAsites2 <- left_join(NAsites2, pk_num, by = "Admin_Unit_Code") %>% 
#     mutate(AUC_num = as.numeric(AUC_num))
#   
#   for(i in 1:dim(yyy3)[1]) {  
#     for(p in 1:dim(yyy3)[2]) {
#       for(j in 1:nsite_pk[p]) {
#         for(t in 1:dim(yyy3)[4]) {
#           if(all(is.na(yyy3[i,p,j,t,])) ) {
#             yyy3[i,p,j,t,] <- 0
#           }
#         }
#       }
#     }
#   }
#   
#   # check
#   table(yyy3) %>% sum(); table(is.na(yyy3))
#   
#   # but reinsert the all NA's for the NA sites
#   for(x in 1:nrow(NAsites2)) {
#     yyy3[,NAsites2$AUC_num[x], NAsites2$site_n[x], NAsites2$year_ind[x], ] <- NA
#   }
#   
#   # check
#   table(yyy3) %>% sum(); table(is.na(yyy3))
#   
#   # ordinal day and time
#   time <- array(0, 
#                 dim = c(npk,
#                         max(nsite_pk),
#                         length(years), 
#                         ninterval),
#                 dimnames = list(pk,
#                                 site_vec[1:max(nsite_pk)],
#                                 years,
#                                 seq(1,10,1)))
#   
#   day <- array(0, 
#                dim = c(npk,
#                        max(nsite_pk),
#                        length(years)),
#                dimnames = list(pk,
#                                site_vec[1:max(nsite_pk)],
#                                years))
#   for(p in 1:npk) {
#     for(j in 1:nsite_pk[p]) {
#       for(t in 1:dim(yyy3)[4]) {# nyr_pk[p]) {
#         dd <- yf3 %>% 
#           filter(Admin_Unit_Code == pk[p],
#                  Year == years[t],
#                  site_n == j) %>% 
#           dplyr::select(EventDate2) %>% 
#           distinct() %>% 
#           pull() %>% 
#           as.numeric()
#         
#         ifelse(length(dd) == 0, NA, day[p,j,t] <- dd)
#         
#         for(k in 1:ninterval) {
#           tt <- yf2 %>% 
#             filter(Admin_Unit_Code == pk[p],
#                    Year == years[t],
#                    site_n == j,
#                    Interval_n == k) %>% 
#             dplyr::select(StartTime2) %>% 
#             distinct() %>% 
#             pull() %>% 
#             as.numeric()
#           
#           ##### problem here that needs solving for the future ------------------------------- 
#           ifelse(length(tt) == 0, NA, time[p,j,t,k] <- tt[1] # [1] here is because there is a conflict here with different times for same interval
#                  )
#           
#         }
#       }
#     }
#   }
# }
# 
# # Get covariate values -----------------------------------------------
# # tree basal area (per acre)
# tree_ba_nps <- read_rds(file = "data/NETN-forest/tree_ba_import.rds") %>% 
#   select(Point_Name, park, site_n, Year, lon, lat, total_BA, SEtotal_BA) 
# 
# tree_ba_nps[,c(2,4)] %>% table()
# 
# bas_area_yr_fia <- read_rds(file = "data/FIA/out/bas_area_yr_import.rds")
# bas_area_tot_fia <- read_rds(file = "data/FIA/out/bas_area_tot_import.rds")
# 
# # tree abundance (tree per acre)
# tree_den_nps <- read_rds(file = "data/NETN-forest/tree_den_import.rds")
# 
# 
# # stand structure
# 
# 
# # shurb density
# shrub_nps <- read_rds(file = "data/NETN-forest/shrub_import.rds")
# 
# 
# # tree diversity - Shannon diversity and equitability
# div_fim_yr_fia <- read_rds(file = "data/FIA/out/div_fim_yr_import.rds")
# div_fim_tot_fia <- read_rds(file = "data/FIA/out/div_fim_tot_import.rds")
# 
# 
# # stand structure
# stand_str_nps <- read_rds(file = "data/NETN-forest/stand_import.rds")
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# # 11 parks, 7 years, 5 buffers (100, 250, 500, 750, 1000), 59 total sites (varies by park)
# # park level covs
# arr_area <- read_rds(file = glue("data/out/arr_area_park.rds"))
# 
# cov_indx <- which(sort(unique(y1$Admin_Unit_Code)) %in% park_list)
# 
# arr_area <- arr_area[c(cov_indx),,]
# 
# # last year of data for the environmental variables (recent_dat)
# # c(1,4,6) are the parks that are declining, standardize MABI, ACAD and MORR ONLY between themselves, ignore other parks data
# if(length(dim(arr_area)) == 2) {
#   
#   recent_dat <- dim(arr_area)[1]
#   
#   arr_area1 <- arr_area[recent_dat,] 
#   
#   arr_area2 <- arr_area1 %>% AHMbook::standardize()
#   
#   for_p <- arr_area2[c(1,5)]  ## i want only 100 and 1000 for now - add 2000!  
#   }
# 
# if(length(dim(arr_area)) > 2) {
#   
#   recent_dat <- dim(arr_area)[2]
#   
#   arr_area1 <- arr_area[,recent_dat,] 
#   
#   arr_area2 <- arr_area1 %>% AHMbook::standardize()
#   
#   for_p <- arr_area2[ ,c(1,5)]  ## i want only 100 and 1000 for now - add 2000!
#   }
# 
# ## site level covs ------------
# arr_area_site <- read_rds(file = glue("data/out/arr_area_site.rds"))
# 
# cov_indx <- which(sort(unique(y1$Admin_Unit_Code)) %in% park_list)
# 
# arr_area_site <- arr_area_site[c(cov_indx),,,]
# 
# if(length(dim(arr_area_site)) > 3) { ## multiple parks
#   
#   arr_area_site1 <- arr_area_site[,recent_dat,,] %>% AHMbook::standardize()
#   
#   for_s <- arr_area_site1
# 
# }
# 
# if(length(dim(arr_area_site)) == 3) {  ## one park
#   
#   arr_area_site1 <- arr_area_site[recent_dat,,] %>% AHMbook::standardize()
#   
#   for_s <- arr_area_site1
#   
# }
# 
# park_size <- NA
# 
# for(i in 1:length(pk)) {
#   pb <- read_rds(file = glue("data/park_raster/{pk[i]}/{pk[i]}_pb.rds"))
#   park_size[i] <- raster::area(pb)   # suqre km
# }
# 
# if(length(park_size) > 1) {
#   park_size <- park_size %>% scale() %>% as.numeric()
# }
# 
# pk_yrs <- yrs_pk - 2005
# 
# pk_yrs_le <- apply(pk_yrs,2,max, na.rm = T)
# 
# # ## occupancy BLISS model -----------------------------------------	
# # 
# # 
# # if(length(park_list) > 1)
# # str(jags.data <- list(y = yyy3,                    # bird detection array
# #                       pk_yrs_le = pk_yrs_le,       # number of years in each parks
# #                       pk_yrs = pk_yrs,             # which years in which parks
# #                       nsiteM = nsite_pk,           # number of sites in each park
# #                       npkM = dim(yyy3)[1],         # number of parks
# #                       nintervalM = dim(yyy3)[4], 	 # number of intervals       
# #                       day = day,                   # calendar day
# #                       time = time#,                # time of day
# #                       #park_size = park_size,      # park area
# #                       #for_s = for_s,              # forest cover in site
# #                       #for_p = for_p               # forest cover in park
# # ))	
# # 
# # if(length(park_list) == 1)
# #   str(jags.data <- list(y = yyy3,                    # bird detection array
# #                         pk_yrs_le = pk_yrs_le,       # number of years in each parks
# #                         pk_yrs = pk_yrs,             # which years in which parks
# #                         nsiteM = nsite_pk,           # number of sites in each park
# #                         nyrsM = dim(yyy3)[1],         # number of years
# #                         nintervalM = dim(yyy3)[3], 	 # number of intervals       
# #                         day = day,                   # calendar day
# #                         time = time#,                # time of day
# #                         #park_size = park_size,      # park area
# #                         #for_s = for_s,              # forest cover in site
# #                         #for_p = for_p               # forest cover in park
# #   ))
# # 
# # # Initial values
# # Zst <- apply(yyy3, c(1,2,3), max, na.rm = TRUE)
# # Zst[Zst == '-Inf'] <- 0         
# # 
# # inits <- function()list(Z = Zst#, beta0 = rnorm(10,0.6), beta1 = rnorm(10,0.6)
# #                         )
# # 
# # # Reverse jump (?) ----------------------------------------------------------
# # 
# # print(niterations)
# # 
# # 
# # cat("\n\n\n running jags \n\n\n\n")
# # 
# # ## initialize JAGS
# # jags_model <- rjags::jags.model(
# #   file = "models/mod_12_1p.txt",
# #   data = jags.data,
# #   inits = inits,
# #   n.chains = nchains,
# #   n.adapt = max(100, ceiling(.1 * niterations)),
# #   quiet = FALSE
# # )
# # 
# # cat("\n\n\n first done \n\n\n\n")
# # 
# # # burn-in
# # if (burnin > 0) {
# #   message(paste("burn-in:", burnin, "iterations"))
# #   rjags::jags.samples(
# #     jags_model,
# #     variable.names = c("alpha0"),
# #     n.iter = niterations,
# #     thin = 3
# #   )
# # }
# # 
# # write_rds(jags_model, 
# #           file = glue("data/model_res/M12.rds"))
# # 
# # cat("\n\n\n second done \n\n\n\n")
# # 
# # # posterior simulation
# # samples_jags <- coda.samples(
# #   #samples_jags <- rjags::jags.samples(
# #   jags_model,
# #   variable.names = c("mu.alpha0", 
# #                      "mu.alpha1",
# #                      "mu.alpha2", 
# #                      "mu.alpha3", 
# #                      "mu.beta0", 
# #                      "mu.beta1",
# #                      "scales_beta1"
# #                      
# #   ),
# #   n.iter = niterations,
# #   thin = 3
# # )
# # 
# # cat("\n\n\n third done \n\n\n\n")
# # 
# # write_rds(samples_jags, 
# #           file = glue("data/model_res/M07.rds"))
# # 
# # 
# # 
# # cat("\n\n\n DONE M06.R \n\n\n\n")
# # 
# # 
# # 
# # 
# # MCMCsummary(samples_jags,
# #             # params = 'alpha',
# #             round = 2)
# # 
# # MCMCtrace(samples_jags,
# #           params = c("mu.alpha0", 
# #                      "mu.alpha1",
# #                      "mu.alpha2", 
# #                      "mu.alpha3", 
# #                      "mu.beta0", 
# #                      "mu.beta1",
# #                      "scales_beta1"),
# #           ind = TRUE,
# #           pdf = FALSE)
# # 
# # MCMCplot(samples_jags,
# #          # params = 'beta',
# #          ref_ovl = TRUE)
# # 
# # # scale selection plots and objects:
# # 
# # sca_beta1 <- MCMCchains(samples_jags, params = 'scales_beta1')
# # selected_scales = rep(NA, 1)
# # for (i in 1:ncovs) {
# #   tb_mcmc_scales_i = table(sca_beta1)
# #   
# #   selected_scales[i] = as.integer(names(which.max(tb_mcmc_scales_i)))
# # }
# # 
# # sca_beta1
# # selected_scales
# # 
# # sca_beta1p <- as_tibble(sca_beta1) %>%
# #   mutate(new = 1)
# # sca_beta1p <- pivot_longer(sca_beta1p, -new, names_to = "site", values_to = "selected_scale") %>%
# #   select(site, selected_scale) %>%
# #   arrange(site)
# # 
# # # colors are sites
# # ggplot(aes(x = selected_scale, y = (..count..)/sum(..count..), fill = site), data = sca_beta1p) +
# #   geom_histogram(position = "stack", binwidth = 0.5) +
# #   theme_bw() +
# #   theme(legend.position = "none") +
# #   ylab("Frequency") + xlab("Selected scale")
# # 
# # ggplot(aes(x = selected_scale, fill = site), data = sca_beta1p) +
# #   theme_bw() +
# #   theme(legend.position = "none") +
# #   ylab("Frequency") + xlab("Selected scale") +
# #   geom_density(alpha = 0.08, color = "gray36") +
# #   scale_x_continuous(limits = c(0, 5))
