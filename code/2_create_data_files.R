# 3_create_bird_data.R
#
# Code to create the y data file to run in the JAGS model 
# can be run for 1, all or groups f species, in one or all parks
#
# source:   - code/1_ImportData.R
#           - code/2_format_data.R
#
# input:    - from code/2_format_data.R
#             -- y1: table of ones and zeros for sps detections
#             -- nsite_pk: number of sites sampled in each park
#             -- yrs_pk: number of years sampled in each park
#             -- ninterval: number of intervals of removal sampling
#             -- site_vec:  
#             -- site_pk:
#           - data/src/original/NETN_2020/BirdSpecies.csv: get species list
#
# output:   - data/out/site_n_key.rds

# .rs.restartR()
#detach()
#rm(list = ls(all.names = TRUE))

library(hms)
library(tidyverse)
# library(splitstackshape)

lenght <- length
colnmaes <- colnames

# Create empty matrix with all parks, species, years, sites and intervals --------------------------

source("code/2_format_data.R")

## parks -------------------------------------------------------------------------------------------
pk_list <- visits %>% 
  select(Admin_Unit_Code) %>% 
  distinct() %>% 
  arrange(Admin_Unit_Code) %>% 
  pull()

npk <- length(pk_list)

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

write_rds(sit_mer2, file = "data/out/site_n_key.rds")

visits2 <- left_join(visits, sit_mer2, by = "Point_Name")

## years --------------------------------------------------------------------------------------------------------
years <- visits2 %>% 
  select(Year) %>% 
  distinct() %>% 
  arrange(Year) %>% 
  pull()

years_s <- years - (min(years)-1)

years_s2 <- cbind(years, years_s)
colnames(years_s2) <- c("Year", "year_s")

nyears <- length(years_s)

yr_pk <- visits2 %>% dplyr::select(Admin_Unit_Code, Year) %>% distinct()
(nyr_pk <- table(yr_pk$Admin_Unit_Code) %>% as.vector())

yrs_pk <- as_tibble(matrix(as.numeric(NA), 
                           ncol = length(pk_list), 
                           nrow = length(y1$Year %>% unique() %>% sort()))) 
colnames(yrs_pk) <- pk_list

for(jj in 1:ncol(yrs_pk)){
  ylop <- y1 %>% dplyr::select(Admin_Unit_Code, Year) %>% distinct() %>% 
    dplyr::filter(Admin_Unit_Code == pk_list[jj]) %>% 
    dplyr::select(Year) %>% 
    pull() %>% 
    sort()
  for(ii in 1:length(ylop)){
    yrs_pk[ii,jj] <- ylop[ii] - (min(y1$Year)-1)
  }
}

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
    yrs_st[ii,jj] <- ylop[ii] - (min(y1$Year)-1)
  }
}

yrs_st_long <- pivot_longer(yrs_st, everything(), names_to = "Point_Name", values_to = "year_s")
yrs_st_long <- na.omit(yrs_st_long)
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

### species pool for each park ---------------------------------------------------
sps_pk <- y1 %>% dplyr::select(Admin_Unit_Code, AOU_Code) %>% distinct()
(nsps_pk <- table(sps_pk$Admin_Unit_Code) %>% as.vector())

sps_pk2 <- sps_pk %>% 
  nest(sps = AOU_Code)
sps_pk2$nsps <- nsps_pk

# add site numbers (site_n) and year standardized (year_s) to bird data
y2 <- y1 %>% 
  left_join(., sit_mer2, by = "Point_Name") %>% 
  left_join(., as_tibble(years_s2), by = "Year")

## create for each park all combinations
## species, park, year, site, interval

yrs_st_long2 <- yrs_st_long %>% 
  mutate(park = substr(Point_Name, 1, 4))

for(ii in 1:npk) {
   st_yr_it <- yrs_st_long2 %>% 
    filter(park == pk_list[ii]) %>% 
    select(-park)
   
   sps_it <- pull(sps_pk2$sps[ii][[1]])
   
   pk_it <- expand_grid(st_yr_it, sps_it)
  
   if((length(sps_it) * nrow(st_yr_it) == nrow(pk_it)) == FALSE){
     stop(pk_list[ii])
   }
   
   y_dat1 <- pk_it
   
   rm(pk_it)

   if(ii == 1) {
     y_dat <- y_dat1
   } else {
     y_dat <- rbind(y_dat, y_dat1)
   }
}

y_dat2 <- y_dat %>% 
  mutate(park = substr(Point_Name, 1, 4),
         interval_n = 10) %>% 
  left_join(., sit_mer2, by = "Point_Name") 

## add removal sampling intervals
y_dat3 <- splitstackshape::expandRows(y_dat2, "interval_n") 
y_dat3$interval_n <- rep(seq(1,10,1), nrow(y_dat2))

## fill array with bird detections
dim(y_dat3)
dim(y1)

y1 <- y1 %>% 
  mutate(year_s = (Year - (min(years) - 1)))

y2 <- y1 %>% 
  select(Point_Name, year_s, AOU_Code, Interval_n, Bird_Count) 

y2_indx <- y1 %>% 
  select(Point_Name, year_s, AOU_Code) %>% 
  distinct()

y_dat3$bird_detec <- as.numeric(NA)

options(warn=2)
for(ii in 1:nrow(y2_indx)){
  y_loop_inx <- y2_indx[ii,]
  
  y_loop <- y2 %>% 
    filter(Point_Name == y_loop_inx$Point_Name, 
           year_s == y_loop_inx$year_s,
           AOU_Code == y_loop_inx$AOU_Code)
  
  y_loop <- y_loop[!duplicated(y_loop[,c('Point_Name', 'year_s', 'AOU_Code', 'Interval_n')]),]
  
  ## single first occasion
  if(nrow(y_loop) == 1 & min(y_loop$Interval_n) == 1){
    # put a one there
    y_dat3[which(y_dat3$Point_Name == y_loop$Point_Name &
                 y_dat3$sps_it == y_loop$AOU_Code &
                 y_dat3$year_s == y_loop$year_s &
                 y_dat3$interval_n == y_loop$Interval_n), "bird_detec"] <- 1
    # no zeros before
  }
  
  ## single not first occasion
  if(nrow(y_loop) == 1 & min(y_loop$Interval_n) != 1){
    # put a one there
    y_dat3[which(y_dat3$Point_Name == y_loop$Point_Name &
                   y_dat3$sps_it == y_loop$AOU_Code &
                   y_dat3$year_s == y_loop$year_s &
                   y_dat3$interval_n == y_loop$Interval_n), "bird_detec"] <- 1
    # zeros before
    y_dat3[which(y_dat3$Point_Name == y_loop$Point_Name &
                   y_dat3$sps_it == y_loop$AOU_Code &
                   y_dat3$year_s == y_loop$year_s &
                   y_dat3$interval_n < y_loop$Interval_n),"bird_detec"] <- 0
  }
  
  if(nrow(y_loop) > 1){
  # ACAD3001     5      GCKI
    # put a one in the first, keep only first detect
    first_detec <- min(y_loop$Interval_n)
    
    y_loop2 <- y_loop %>% filter(Interval_n == first_detec) %>% 
      distinct()
    
    y_dat3[which(y_dat3$Point_Name == y_loop2$Point_Name &
                   y_dat3$sps_it == y_loop2$AOU_Code &
                   y_dat3$year_s == y_loop2$year_s &
                   y_dat3$interval_n == first_detec), "bird_detec"] <- 1
    
    # add zeros before
    if(first_detec > 1){
      y_dat3[which(y_dat3$Point_Name == y_loop2$Point_Name &
                     y_dat3$sps_it == y_loop2$AOU_Code &
                     y_dat3$year_s == y_loop2$year_s &
                     y_dat3$interval_n < first_detec),"bird_detec"] <- 0
    }
  }
}

options(warn=1)

write_rds(y_dat3, file = "data/out/y_dat3.rds")
## remember: I have only one one per row, all zeros before one, and all NA after one

# get covariate data ----------------------------------------------------------------------------------
## site -------------------------------
## no data for all years, chosing 2018 for now
tree_ba_tab_site <- read_rds(file = "data/NETN-forest/tree_ba_import.rds") %>% 
  select(Point_Name, park, site_n, Year, total_BA) #%>% 
  #filter(Year == 2018)
table(tree_ba_tab_site[,c(1,4)])

tree_ba_tab_site <- tree_ba_tab_site %>% 
  na.omit() %>% 
  group_by(Point_Name) %>% 
  mutate(mean_tot_BA = mean(total_BA)) %>% 
  ungroup() %>% 
  select(-total_BA) %>% 
  select(-Year) %>% 
  distinct()

tree_den_tab_site <- read_rds(file = "data/NETN-forest/tree_den_import.rds") %>% 
  mutate(park = substr(Point_Name, 1, 4)) %>% 
  select(Point_Name, park, site_n, Year, total_den) %>% 
  na.omit() %>% 
  group_by(Point_Name) %>% 
  mutate(mean_tot_den = mean(total_den)) %>% 
  ungroup() %>% 
  select(-total_den) %>% 
  select(-Year) %>% 
  distinct()

stand_struc_tab_site <- read_rds(file = "data/NETN-forest/stand_import.rds") %>% 
  mutate(park = substr(Point_Name, 1, 4)) %>% 
  select(Point_Name, park, site_n, Year, 
         Pct_Understory_Low, Pct_Understory_Mid, Pct_Understory_High) %>% 
  na.omit() %>% 
  group_by(Point_Name) %>% 
  mutate(mean_low = mean(Pct_Understory_Low),
         mean_mid = mean(Pct_Understory_Mid),
         mean_high = mean(Pct_Understory_High)) %>% 
  ungroup() %>% 
  select(-c(Year, Pct_Understory_Low, Pct_Understory_Mid, Pct_Understory_High)) %>% 
  distinct()

## park --------------------------------------------------------------------------------
tree_ba_tab_park <- read_rds(file = "data/NETN-forest/tree_ba_tab_park.rds") %>% 
  na.omit() %>% 
  group_by(park) %>% 
  mutate(mean_total_BA = mean(mean_total_BA)) %>% 
  ungroup() %>% 
  select(-c(Year, mean_total_BA_SE)) %>% 
  distinct()

tree_den_tab_park <- read_rds(file = "data/NETN-forest/tree_den_tab_park.rds") %>% 
  na.omit() %>% 
  group_by(park) %>% 
  mutate(mean_total_den = mean(mean_total_den)) %>% 
  ungroup() %>% 
  select(-c(Year)) %>% 
  distinct()

stand_struc_tab_park <- read_rds(file = "data/NETN-forest/stand_struc_tab_park.rds")

## county ---------------------------------------------------------------------------------
tree_ba_tab_coun <- read_rds(file = "data/FIA/out/bas_area_tot_import.rds")

tree_den_tab_coun <- read_rds(file = "data/FIA/out/tree_acre_tot_import.rds")

stand_struc_tab_coun <- read_rds(file = "data/FIA/out/stand_struc_import.rds")

# merge objects
tree_ba_tab_site <- tree_ba_tab_site %>% 
  rename(siteBA = mean_tot_BA) %>% 
  select(Point_Name, park, site_n, siteBA)

tree_ba_tab_park <- tree_ba_tab_park %>% 
  rename(parkBA = mean_total_BA) %>% 
  select(park, parkBA)

tree_ba_tab_coun <- tree_ba_tab_coun %>% 
  rename(counBA = BAA_mean) %>% 
  select(park, counBA)

tree_ba_mod <- tree_ba_tab_site %>% 
  left_join(., tree_ba_tab_park, by = "park") %>% 
  left_join(., tree_ba_tab_coun, by = "park")

cor(tree_ba_mod[,4:6])

tree_den_tab_site <- tree_den_tab_site %>% 
  rename(siteDEN = mean_tot_den) %>% 
  select(Point_Name, park, site_n, siteDEN)

tree_den_tab_park <- tree_den_tab_park %>% 
  rename(parkDEN = mean_total_den) %>% 
  select(park, parkDEN)

tree_den_tab_coun <- tree_den_tab_coun %>% 
  rename(counDEN = TPA_mean) %>% 
  select(park, counDEN)

tree_den_mod <- tree_den_tab_site %>% 
  left_join(., tree_den_tab_park, by = "park") %>% 
  left_join(., tree_den_tab_coun, by = "park")

cor(tree_den_mod[,4:6])

write_rds(tree_ba_mod, file = "data/out/tree_ba_mod.rds")
write_rds(tree_den_mod, file = "data/out/tree_den_mod.rds")


# get park site coordinates

site_coords <- y1 %>% 
  select(Admin_Unit_Code, Point_Name, Transect_CODE, Longitude, Latitude, site_n, BCR) %>% 
  distinct()

write_rds(site_coords, "data/out/site_coords.rds")






if (class(spslist)[1] == "tbl_df") {
  yf <- y1 %>% 
    filter(AOU_Code %in% pull(spslist)) 
}

if (class(spslist)[1] == "character") {
  yf <- y1 %>% 
    filter(AOU_Code %in% spslist)
}

if (park_ana != "all"){
  yf <- yf %>% 
    filter(Admin_Unit_Code %in% park_list) 
}

# park
(npk <- yf$Admin_Unit_Code %>% unique() %>% length())
(pk <- sort(unique(yf$Admin_Unit_Code)))
(park_list <- sort(unique(y1$Admin_Unit_Code)))

# site
site_pk <- y1 %>% dplyr::select(Admin_Unit_Code, Point_Name) %>% distinct()
(nsite_pk <- table(site_pk$Admin_Unit_Code) %>% as.vector())
(nsite_pk <- nsite_pk[which(park_list %in% pk)])
mxsite <- max(nsite_pk)
site_vec <- seq(1, mxsite, 1)

# year
years <- yf$Year %>% unique() %>% sort()
years_s <- years - (min(y1$Year)-1)
(nyr_s <- length(years_s))

yr_pk <- yf %>% dplyr::select(Admin_Unit_Code, Year) %>% distinct()
(nyr_pk <- table(yr_pk$Admin_Unit_Code) %>% as.vector())

yrs_pk <- as_tibble(matrix(as.numeric(NA), 
                           ncol = length(park_list), 
                           nrow = length(y1$Year %>% unique() %>% sort()))) 
colnames(yrs_pk) <- park_list

for(jj in 1:ncol(yrs_pk)){
  ylop <- y1 %>% dplyr::select(Admin_Unit_Code, Year) %>% distinct() %>% 
    dplyr::filter(Admin_Unit_Code == park_list[jj]) %>% 
    dplyr::select(Year) %>% 
    pull() %>% 
    sort()
  for(ii in 1:length(ylop)){
    yrs_pk[ii,jj] <- ylop[ii] - (min(y1$Year)-1)
  }
}

# have only ones for detections
sum(yf$Bird_Count)
ifelse(yf$Bird_Count > 1, yf$Bird_Count <- 1, yf$Bird_Count <- yf$Bird_Count)
sum(yf$Bird_Count) == length(yf$Bird_Count)

# ordinal day and time
yf$EventDate2 <- scale(yday(yf$EventDate))
yf$StartTime2 <- scale(as.period(seconds(yf$StartTime) + minutes(substr(yf$Interval_Length,1,1) %>%
                                                                   as.numeric() %>% 
                                                                   as_hms()), unit = "hours") %>% 
                         as.numeric())

# years <- c(2001, 2004, 2006, 2008, 2011, 2013, 2016, 2019)   ## years with environmental data
yf2 <- yf %>%  
  #  filter(Year %in% years) %>% 
  mutate(Interval = as.numeric(Interval))

# keep only first detection of the 10 intervals --------------------------------------------------
yf3 <- yf2 %>% 
  #filter(Bird_Count == 1) %>% 
  mutate(Interval_n = as.numeric(Interval_n)) %>% 
  group_by(AOU_Code, EventDate, site_n, Admin_Unit_Code) %>%
  filter(Interval_n == min(Interval_n)) %>% 
  slice(1) %>%   # takes the first occurrence if there is a tie
  ungroup()

dim(yf3)
# check
sum(yf3$Bird_Count, na.rm = T)

## if only one sps (format) -----------------
if((as_tibble(spslist) %>% nrow()) == 1) {
  # create 4 columns tibble
  y2 <- matrix(ncol = 5,
               nrow = sum(nyr_pk * nsite_pk * ninterval),
               NA) %>% 
    as_tibble()
  
  colnames(y2) <- c("y", "park", "year", "site", "interval")
  
  # parks
  y2$park <- rep(pk, nyr_pk * nsite_pk * ninterval)
  
  # years
  yr_tib <- vector()
  yf3$year2 <- yf3$Year - (min(y1$Year)-1)
  yf3 <- yf3 %>% select(Admin_Unit_Code, Year, year2, site_n, Interval_n, Bird_Count)
  
  for(ii in 1:npk){
    yrlp <- yf3 %>% 
      filter(Admin_Unit_Code == pk[ii]) %>% 
      select(year2) %>% 
      pull() %>% 
      na.omit %>% 
      as.vector()
  
    if(ii == 1){
      yr_tib <- sort(rep(yrlp, nsite_pk[ii] * ninterval))
    } 
    if(ii > 1){
      yr_tib <- c(yr_tib, sort(rep(yrlp, nsite_pk[ii] * ninterval)))
    }
  }
  
  y2$year <- yr_tib
  
  # sites
  sit_tib <- vector()
  
  for(ii in 1:npk){
    if(ii == 1){
      sit_tib <- rep(seq(1:nsite_pk[ii]), 
                     times = nyr_pk[ii],
                     each = ninterval)
    }
    if(ii > 1){
      sit_tib <- c(sit_tib, rep(seq(1:nsite_pk[ii]), 
                                times = nyr_pk[ii],
                                each = ninterval))
    }
  }
  
  y2$site <- sit_tib
  
  y2$interval <- rep(seq(1,10,1), length.out = nrow(y2))
  
  y2$y <- as.numeric(y2$y)
  
  # add counts (the ones) ------------------------------------------------------------------
  for(aa in 1:nrow(yf3)){
    
    yl <- yf3[aa,]
    yl
   ( p <- which(pk == yl$Admin_Unit_Code, arr.ind = T))
   ( j <- which(site_vec[1:nsite_pk[p]] == yl$site_n, arr.ind = T))       # site
(    t <- yl$year2  )                         # year
(    k <- as.numeric(yl$Interval_n)   )                                   # interval
    
    y2[which(y2$park == pk[p] &
             y2$site == j &
             y2$year == t &
             y2$interval == k),1] <- 1
  }
  
  aa <- aa + 1
  
  y2$y %>% sum(na.rm = T)
  
  
  y2[which(y2$park == pk[p] &
             y2$site == j ),]
  
  
  # check
  nrow(yf3); table(is.na(y2$y))
  yf3 %>% select(Admin_Unit_Code, site_n, Year, Interval_n) %>% distinct() %>% dim()
  
  # add zeros before the ones --------------------------------------------------------
  for(p in 1:dim(yyy3)[1]) {  
    for(j in 1:nsite_pk[p]) {
      for(t in 1:dim(yyy3)[3]) {
        ind <- which.min(is.na(yyy3[p,j,t,])) %>% as.numeric()
        if(ind > 1) {
          yyy3[p,j,t,(1:(ind-1))] <- 0
        }
      }
    }
  }
  
  # check
  table(yyy3) %>% sum(na.rm = T); table(is.na(yyy3))[1]
  
  # add all zeros if never detected --------------------------------------------------
  ## create zeros for species, separate them from NAs and add them to a new data frame
  SAMPLEDsites <- yf3 %>% 
    dplyr::select(Point_Name, Year) %>%
    distinct() 
  
  ALLsites_sps <- expand.grid(SAMPLEDsites %>% dplyr::select(Point_Name) %>% distinct() %>% pull(),
                              SAMPLEDsites %>% dplyr::select(Year) %>% distinct() %>% pull()
  )
  
  # select the zeros
  table(yf3 %>% dplyr::select(Point_Name, Year) %>% distinct()) 
  
  ALLsites_sps <- ALLsites_sps %>%
    rename(Point_Name = Var1,
           Year = Var2) %>% 
    as_tibble() 
  
  # Have to NA out sites with NA data
  dim(ALLsites_sps) ; dim(SAMPLEDsites)
  ZEROsites <- dplyr::setdiff(ALLsites_sps, SAMPLEDsites[,c(1,2)]) 
  nrow(ALLsites_sps) - nrow(SAMPLEDsites) == nrow(ZEROsites)
  
  ZEROsites$point_data <- glue("{ZEROsites$Point_Name}_{ZEROsites$Year}")
  SAMPLEDsites$point_data <- glue("{SAMPLEDsites$Point_Name}_{substring(SAMPLEDsites$Year,1,4)}")
  table(ZEROsites$point_data %in% SAMPLEDsites$point_data) # how many occasions never occured
  
  NAsites <- as_tibble(cbind(point_data = ZEROsites$point_data,
                             Point_Name = substring(ZEROsites$point_data,1,8),
                             Year = as.numeric(substring(ZEROsites$point_data,10,13))))
  
  NAsites2 <- left_join(NAsites, site_pk[,2:3], by = "Point_Name") %>% 
    mutate(Year = as.numeric(Year))
  year_ind <- cbind(years, seq(1:length(years))) %>% as_tibble()
  colnames(year_ind) <- c("Year", "year_ind")
  NAsites2 <- left_join(NAsites2, year_ind, by = "Year") 
  NAsites2 <- NAsites2 %>% 
    mutate(Admin_Unit_Code = substr(Point_Name,1,4)) %>% 
    dplyr::select(Admin_Unit_Code, site_n, year_ind)
  pk_num <- cbind(pk, seq(1:npk)) %>%
    as_tibble() %>% 
    rename(Admin_Unit_Code = pk, AUC_num = V2)
  NAsites2 <- left_join(NAsites2, pk_num, by = "Admin_Unit_Code") %>% 
    mutate(AUC_num = as.numeric(AUC_num))
  
  for(p in 1:dim(yyy3)[1]) {
    for(j in 1:nsite_pk[p]) {
      for(t in 1:dim(yyy3)[3]) {
        if(all(is.na(yyy3[p,j,t,])) ) {
          yyy3[p,j,t,] <- 0
        }
      }
    }
  }
  
  # check
  table(yyy3); table(is.na(yyy3))
  
  # but reinsert the all NA's for the NA sites
  for(x in 1:nrow(NAsites2)) {
    yyy3[NAsites2$AUC_num[x], NAsites2$site_n[x], NAsites2$year_ind[x], ] <- NA
  }
  
  # check
  table(yyy3); table(is.na(yyy3))
  
  # ordinal day and time
  time <- array(0, 
                dim = c(npk,
                        max(nsite_pk),
                        length(years), 
                        ninterval),
                dimnames = list(pk,
                                site_vec[1:max(nsite_pk)],
                                years,
                                seq(1,10,1)))
  
  day <- array(0, 
               dim = c(npk,
                       max(nsite_pk),
                       length(years)),
               dimnames = list(pk,
                               site_vec[1:max(nsite_pk)],
                               years))
  for(p in 1:npk) {
    for(j in 1:nsite_pk[p]) {
      for(t in 1:dim(yyy3)[3]) {# nyr_pk[p]) {
        dd <- yf3 %>% 
          filter(Admin_Unit_Code == pk[p],
                 Year == years[t],
                 site_n == j) %>% 
          dplyr::select(EventDate2) %>% 
          distinct() %>% 
          pull() %>% 
          as.numeric()
        
        ifelse(length(dd) == 0, NA, day[p,j,t] <- dd)
        
        for(k in 1:ninterval) {
          tt <- yf2 %>% 
            filter(Admin_Unit_Code == pk[p],
                   Year == years[t],
                   site_n == j,
                   Interval_n == k) %>% 
            dplyr::select(StartTime2) %>% 
            distinct() %>% 
            pull() %>% 
            as.numeric()
          
          ifelse(length(tt) == 0, NA, time[p,j,t,k] <- tt)
          
        }
      }
    }
  }
}

# filter for more than one sps tables (format) -----------------
sps_f_names <- yf3 %>% 
  dplyr::select(AOU_Code) %>% 
  distinct()

n_sps <- sps_f_names %>% 
  pull() %>% 
  length()

## if multiple species -------------------------------  
if((as_tibble(spslist) %>% nrow()) > 1) {
  
  # create 4d array
  yyy3 <- array(NA, 
                dim = c(n_sps,
                        npk,
                        max(nsite_pk),
                        length(years), 
                        ninterval
                ),
                dimnames = list(as.vector(sps_f_names %>% pull),
                                pk,
                                site_vec[1:max(nsite_pk)],
                                years,
                                seq(1,10,1)
                ))
  
  # add counts (the ones) ------------------------------------------------------------------
  for(a in 1:nrow(yf3)){
    
    yl <- yf3[a,]
    
    i <- which(sps_f_names == yl$AOU_Code, arr.ind = T)
    p <- which(pk == yl$Admin_Unit_Code, arr.ind = T)
    j <- which(site_vec[1:nsite_pk[p]] == yl$site_n, arr.ind = T)       # site
    t <- which(years == yl$Year, arr.ind = T)                           # year
    k <- as.numeric(yl$Interval_n)                                      # interval
    
    yyy3[i,p,j,t,k] <- ifelse(sum(yyy3[i,p,j,t,k],
                                yl %>% 
                                  dplyr::select(Bird_Count) %>% 
                                  pull(), 
                                na.rm = T) > 0, 1, 0)
  }
  # check
  table(yyy3); table(is.na(yyy3))[1]
  
  # add zeros before the ones --------------------------------------------------------
  for(i in 1:dim(yyy3)[1]) {  
    for(p in 1:dim(yyy3)[2]) {  
      for(j in 1:nsite_pk[p]) {
        for(t in 1:dim(yyy3)[4]) {
          ind <- which.min(is.na(yyy3[i,p,j,t,])) %>% as.numeric()
          if(ind > 1) {
            yyy3[i,p,j,t,(1:(ind-1))] <- 0
          }
        }
      }
    }
  }  
  
  # check
  table(yyy3) %>% sum(na.rm = T); table(is.na(yyy3))[1]
  
  # add all zeros if never detected --------------------------------------------------
  ## create zeros for species, separate them from NAs and add them to a new data frame
  SAMPLEDsites <- yf3 %>% 
    dplyr::select(Point_Name, Year) %>%
    distinct() 
  
  ALLsites_sps <- expand.grid(SAMPLEDsites %>% dplyr::select(Point_Name) %>% distinct() %>% pull(),
                              SAMPLEDsites %>% dplyr::select(Year) %>% distinct() %>% pull()
  )
  
  # select the zeros
  table(yf3 %>% dplyr::select(Point_Name, Year) %>% distinct()) 
  
  ALLsites_sps <- ALLsites_sps %>%
    rename(Point_Name = Var1,
           Year = Var2) %>% 
    as_tibble() 
  
  # Have to NA out sites with NA data
  dim(ALLsites_sps) ; dim(SAMPLEDsites)
  ZEROsites <- dplyr::setdiff(ALLsites_sps, SAMPLEDsites[,c(1,2)]) 
  nrow(ALLsites_sps) - nrow(SAMPLEDsites) == nrow(ZEROsites)
  
  ZEROsites$point_data <- glue("{ZEROsites$Point_Name}_{ZEROsites$Year}")
  SAMPLEDsites$point_data <- glue("{SAMPLEDsites$Point_Name}_{substring(SAMPLEDsites$Year,1,4)}")
  table(ZEROsites$point_data %in% SAMPLEDsites$point_data) # how many occasions never occured
  
  NAsites <- as_tibble(cbind(point_data = ZEROsites$point_data,
                             Point_Name = substring(ZEROsites$point_data,1,8),
                             Year = as.numeric(substring(ZEROsites$point_data,10,13))))
  
  NAsites2 <- left_join(NAsites, site_pk[,2:3], by = "Point_Name") %>% 
    mutate(Year = as.numeric(Year))
  year_ind <- cbind(years, seq(1:length(years))) %>% as_tibble()
  colnames(year_ind) <- c("Year", "year_ind")
  NAsites2 <- left_join(NAsites2, year_ind, by = "Year") 
  NAsites2 <- NAsites2 %>% 
    mutate(Admin_Unit_Code = substr(Point_Name,1,4)) %>% 
    dplyr::select(Admin_Unit_Code, site_n, year_ind)
  pk_num <- cbind(pk, seq(1:npk)) %>%
    as_tibble() %>% 
    rename(Admin_Unit_Code = pk, AUC_num = V2)
  NAsites2 <- left_join(NAsites2, pk_num, by = "Admin_Unit_Code") %>% 
    mutate(AUC_num = as.numeric(AUC_num))
  
  for(i in 1:dim(yyy3)[1]) {  
    for(p in 1:dim(yyy3)[2]) {
      for(j in 1:nsite_pk[p]) {
        for(t in 1:dim(yyy3)[4]) {
          if(all(is.na(yyy3[i,p,j,t,])) ) {
            yyy3[i,p,j,t,] <- 0
          }
        }
      }
    }
  }
  
  # check
  table(yyy3) %>% sum(); table(is.na(yyy3))
  
  # but reinsert the all NA's for the NA sites
  for(x in 1:nrow(NAsites2)) {
    yyy3[,NAsites2$AUC_num[x], NAsites2$site_n[x], NAsites2$year_ind[x], ] <- NA
  }
  
  # check
  table(yyy3) %>% sum(); table(is.na(yyy3))
  
  # ordinal day and time
  time <- array(0, 
                dim = c(npk,
                        max(nsite_pk),
                        length(years), 
                        ninterval),
                dimnames = list(pk,
                                site_vec[1:max(nsite_pk)],
                                years,
                                seq(1,10,1)))
  
  day <- array(0, 
               dim = c(npk,
                       max(nsite_pk),
                       length(years)),
               dimnames = list(pk,
                               site_vec[1:max(nsite_pk)],
                               years))
  for(p in 1:npk) {
    for(j in 1:nsite_pk[p]) {
      for(t in 1:dim(yyy3)[4]) {# nyr_pk[p]) {
        dd <- yf3 %>% 
          filter(Admin_Unit_Code == pk[p],
                 Year == years[t],
                 site_n == j) %>% 
          dplyr::select(EventDate2) %>% 
          distinct() %>% 
          pull() %>% 
          as.numeric()
        
        ifelse(length(dd) == 0, NA, day[p,j,t] <- dd)
        
        for(k in 1:ninterval) {
          tt <- yf2 %>% 
            filter(Admin_Unit_Code == pk[p],
                   Year == years[t],
                   site_n == j,
                   Interval_n == k) %>% 
            dplyr::select(StartTime2) %>% 
            distinct() %>% 
            pull() %>% 
            as.numeric()
          
          ##### problem here that needs solving for the future ------------------------------- 
          ifelse(length(tt) == 0, NA, time[p,j,t,k] <- tt[1] # [1] here is because there is a conflict here with different times for same interval
                 )
          
        }
      }
    }
  }
}

# Get covariate values -----------------------------------------------
# tree basal area (per acre)
tree_ba_nps <- read_rds(file = "data/NETN-forest/tree_ba_import.rds") %>% 
  select(Point_Name, park, site_n, Year, lon, lat, total_BA, SEtotal_BA) 

tree_ba_nps[,c(2,4)] %>% table()

bas_area_yr_fia <- read_rds(file = "data/FIA/out/bas_area_yr_import.rds")
bas_area_tot_fia <- read_rds(file = "data/FIA/out/bas_area_tot_import.rds")

# tree abundance (tree per acre)
tree_den_nps <- read_rds(file = "data/NETN-forest/tree_den_import.rds")


# stand structure


# shurb density
shrub_nps <- read_rds(file = "data/NETN-forest/shrub_import.rds")


# tree diversity - Shannon diversity and equitability
div_fim_yr_fia <- read_rds(file = "data/FIA/out/div_fim_yr_import.rds")
div_fim_tot_fia <- read_rds(file = "data/FIA/out/div_fim_tot_import.rds")


# stand structure
stand_str_nps <- read_rds(file = "data/NETN-forest/stand_import.rds")
















# 11 parks, 7 years, 5 buffers (100, 250, 500, 750, 1000), 59 total sites (varies by park)
# park level covs
arr_area <- read_rds(file = glue("data/out/arr_area_park.rds"))

cov_indx <- which(sort(unique(y1$Admin_Unit_Code)) %in% park_list)

arr_area <- arr_area[c(cov_indx),,]

# last year of data for the environmental variables (recent_dat)
# c(1,4,6) are the parks that are declining, standardize MABI, ACAD and MORR ONLY between themselves, ignore other parks data
if(length(dim(arr_area)) == 2) {
  
  recent_dat <- dim(arr_area)[1]
  
  arr_area1 <- arr_area[recent_dat,] 
  
  arr_area2 <- arr_area1 %>% AHMbook::standardize()
  
  for_p <- arr_area2[c(1,5)]  ## i want only 100 and 1000 for now - add 2000!  
  }

if(length(dim(arr_area)) > 2) {
  
  recent_dat <- dim(arr_area)[2]
  
  arr_area1 <- arr_area[,recent_dat,] 
  
  arr_area2 <- arr_area1 %>% AHMbook::standardize()
  
  for_p <- arr_area2[ ,c(1,5)]  ## i want only 100 and 1000 for now - add 2000!
  }

## site level covs ------------
arr_area_site <- read_rds(file = glue("data/out/arr_area_site.rds"))

cov_indx <- which(sort(unique(y1$Admin_Unit_Code)) %in% park_list)

arr_area_site <- arr_area_site[c(cov_indx),,,]

if(length(dim(arr_area_site)) > 3) { ## multiple parks
  
  arr_area_site1 <- arr_area_site[,recent_dat,,] %>% AHMbook::standardize()
  
  for_s <- arr_area_site1

}

if(length(dim(arr_area_site)) == 3) {  ## one park
  
  arr_area_site1 <- arr_area_site[recent_dat,,] %>% AHMbook::standardize()
  
  for_s <- arr_area_site1
  
}

park_size <- NA

for(i in 1:length(pk)) {
  pb <- read_rds(file = glue("data/park_raster/{pk[i]}/{pk[i]}_pb.rds"))
  park_size[i] <- raster::area(pb)   # suqre km
}

if(length(park_size) > 1) {
  park_size <- park_size %>% scale() %>% as.numeric()
}

pk_yrs <- yrs_pk - 2005

pk_yrs_le <- apply(pk_yrs,2,max, na.rm = T)

# ## occupancy BLISS model -----------------------------------------	
# 
# 
# if(length(park_list) > 1)
# str(jags.data <- list(y = yyy3,                    # bird detection array
#                       pk_yrs_le = pk_yrs_le,       # number of years in each parks
#                       pk_yrs = pk_yrs,             # which years in which parks
#                       nsiteM = nsite_pk,           # number of sites in each park
#                       npkM = dim(yyy3)[1],         # number of parks
#                       nintervalM = dim(yyy3)[4], 	 # number of intervals       
#                       day = day,                   # calendar day
#                       time = time#,                # time of day
#                       #park_size = park_size,      # park area
#                       #for_s = for_s,              # forest cover in site
#                       #for_p = for_p               # forest cover in park
# ))	
# 
# if(length(park_list) == 1)
#   str(jags.data <- list(y = yyy3,                    # bird detection array
#                         pk_yrs_le = pk_yrs_le,       # number of years in each parks
#                         pk_yrs = pk_yrs,             # which years in which parks
#                         nsiteM = nsite_pk,           # number of sites in each park
#                         nyrsM = dim(yyy3)[1],         # number of years
#                         nintervalM = dim(yyy3)[3], 	 # number of intervals       
#                         day = day,                   # calendar day
#                         time = time#,                # time of day
#                         #park_size = park_size,      # park area
#                         #for_s = for_s,              # forest cover in site
#                         #for_p = for_p               # forest cover in park
#   ))
# 
# # Initial values
# Zst <- apply(yyy3, c(1,2,3), max, na.rm = TRUE)
# Zst[Zst == '-Inf'] <- 0         
# 
# inits <- function()list(Z = Zst#, beta0 = rnorm(10,0.6), beta1 = rnorm(10,0.6)
#                         )
# 
# # Reverse jump (?) ----------------------------------------------------------
# 
# print(niterations)
# 
# 
# cat("\n\n\n running jags \n\n\n\n")
# 
# ## initialize JAGS
# jags_model <- rjags::jags.model(
#   file = "models/mod_12_1p.txt",
#   data = jags.data,
#   inits = inits,
#   n.chains = nchains,
#   n.adapt = max(100, ceiling(.1 * niterations)),
#   quiet = FALSE
# )
# 
# cat("\n\n\n first done \n\n\n\n")
# 
# # burn-in
# if (burnin > 0) {
#   message(paste("burn-in:", burnin, "iterations"))
#   rjags::jags.samples(
#     jags_model,
#     variable.names = c("alpha0"),
#     n.iter = niterations,
#     thin = 3
#   )
# }
# 
# write_rds(jags_model, 
#           file = glue("data/model_res/M12.rds"))
# 
# cat("\n\n\n second done \n\n\n\n")
# 
# # posterior simulation
# samples_jags <- coda.samples(
#   #samples_jags <- rjags::jags.samples(
#   jags_model,
#   variable.names = c("mu.alpha0", 
#                      "mu.alpha1",
#                      "mu.alpha2", 
#                      "mu.alpha3", 
#                      "mu.beta0", 
#                      "mu.beta1",
#                      "scales_beta1"
#                      
#   ),
#   n.iter = niterations,
#   thin = 3
# )
# 
# cat("\n\n\n third done \n\n\n\n")
# 
# write_rds(samples_jags, 
#           file = glue("data/model_res/M07.rds"))
# 
# 
# 
# cat("\n\n\n DONE M06.R \n\n\n\n")
# 
# 
# 
# 
# MCMCsummary(samples_jags,
#             # params = 'alpha',
#             round = 2)
# 
# MCMCtrace(samples_jags,
#           params = c("mu.alpha0", 
#                      "mu.alpha1",
#                      "mu.alpha2", 
#                      "mu.alpha3", 
#                      "mu.beta0", 
#                      "mu.beta1",
#                      "scales_beta1"),
#           ind = TRUE,
#           pdf = FALSE)
# 
# MCMCplot(samples_jags,
#          # params = 'beta',
#          ref_ovl = TRUE)
# 
# # scale selection plots and objects:
# 
# sca_beta1 <- MCMCchains(samples_jags, params = 'scales_beta1')
# selected_scales = rep(NA, 1)
# for (i in 1:ncovs) {
#   tb_mcmc_scales_i = table(sca_beta1)
#   
#   selected_scales[i] = as.integer(names(which.max(tb_mcmc_scales_i)))
# }
# 
# sca_beta1
# selected_scales
# 
# sca_beta1p <- as_tibble(sca_beta1) %>%
#   mutate(new = 1)
# sca_beta1p <- pivot_longer(sca_beta1p, -new, names_to = "site", values_to = "selected_scale") %>%
#   select(site, selected_scale) %>%
#   arrange(site)
# 
# # colors are sites
# ggplot(aes(x = selected_scale, y = (..count..)/sum(..count..), fill = site), data = sca_beta1p) +
#   geom_histogram(position = "stack", binwidth = 0.5) +
#   theme_bw() +
#   theme(legend.position = "none") +
#   ylab("Frequency") + xlab("Selected scale")
# 
# ggplot(aes(x = selected_scale, fill = site), data = sca_beta1p) +
#   theme_bw() +
#   theme(legend.position = "none") +
#   ylab("Frequency") + xlab("Selected scale") +
#   geom_density(alpha = 0.08, color = "gray36") +
#   scale_x_continuous(limits = c(0, 5))
