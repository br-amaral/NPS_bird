# format_bird_data/format_data
#                filtering visit and field data for only auditory, 50m distance band, and without missing info ('permanetly missing') in any columns we use, e.g. interval number
#                in - data/out/NETNtib.rds

# Load libraries -------------------------------------------------------------------------------------
library(stringr)
library(tidyverse)
library(lubridate)
library(glue)

# Set working directory -------------------------------------------------------------------------------------
# setwd("~/Documents/GitHub/NPS_birds/")
# Data paths -------------------------------------------------------------------------------------
NPS_DATA_PATH <- file.path("data/out/NETNtib.rds")

# Load data -------------------------------------------------------------------------------------
dat <- read_rds(file = NPS_DATA_PATH)

# Build functions --------------------------------------------------------------------------------------
colnmaes <- colnames

'%!in%' <- function(a,b) ! a %in% b

# Format data for modeling --------------------------------------------------------------------------------------
field_dat <- dat$field_data[1][[1]]
for(i in 2:nrow(dat)){
  field_dat <- rbind(field_dat, dat$field_data[i][[1]])
}
field_dat <- field_dat %>% 
                filter(Interval!= "Permanently missing",
                       Survey_Type == "Forest")

visits <- dat$visits[1][[1]]   
for(i in 2:nrow(dat)){
  visits <- rbind(visits, dat$visits[i][[1]])
}
visits <- visits %>%   
              dplyr::rename(observer_name = ObserverID) %>% 
              filter(Survey_Type == "Forest")

points <- dat$points[1][[1]] 
for(i in 2:nrow(dat)){
  points <- rbind(points, dat$points[i][[1]])
}
points <- points %>%   ## PT_DESC
              dplyr::select(-Transect_Name) %>% 
              filter(Survey_Type == "Forest")

visits2 <- left_join(visits, points, by = c("Admin_Unit_Code", "Point_Name", "Survey_Type"))
dim(visits)[1] == dim(visits2)[1]

field_dat0.5 <- left_join(field_dat %>% select(-Transect_CODE), visits2 %>% select(-Transect_CODE), 
                          by = c("Admin_Unit_Code", "Transect_Name", "Point_Name", "Survey_Type",
                                 "Visit", "EventDate", "Year" ))
dim(field_dat0.5)[1] == dim(field_dat)[1]

## filter only first visit and only distance 1
field_dat0.75 <- field_dat0.5 %>% 
  dplyr::filter(Distance_id == 1,
                Visit == 1) %>% 
  as_tibble()

# Remove NAs!
field_dat1 <- field_dat0.75 %>% 
  mutate(Interval = ifelse(Interval == "NR", NA, Interval)) %>% 
  dplyr::filter(!is.na(Year),
                !is.na(Point_Name),
                !is.na(EventDate),
                !is.na(Interval),
                !is.na(Distance),
                !is.na(Admin_Unit_Code)) 


## parks --------------------------------------------------------------------------------------
(npk <- field_dat1$Admin_Unit_Code %>% unique() %>% length())
pk <- field_dat1$Admin_Unit_Code %>% sort() %>% unique()
pk 

## years --------------------------------------------------------------------------------------
years <- field_dat1$Year %>% unique() %>% sort()
max(years)
years_s <- years - (years[1]-1)
(nyr_s <- max(years_s))

yr_pk <- field_dat1 %>% dplyr::select(Admin_Unit_Code, Year) %>% distinct()
nyr_pk <- table(yr_pk$Admin_Unit_Code) %>% as.vector()

yrs_pk <- as_tibble(matrix(as.numeric(NA), ncol = npk, nrow = nyr_s)) 
colnames(yrs_pk) <- pk

for(j in 1:ncol(yrs_pk)){
  ylop <- yr_pk %>% 
    dplyr::filter(Admin_Unit_Code == pk[j]) %>% 
    dplyr::select(Year) %>% 
    pull() %>% 
    sort()
  for(i in 1:length(ylop)){
    yrs_pk[i,j] <- ylop[i]
  }
}

## sites --------------------------------------------------------------------------------------
(nsite <- length(visits$Point_Name %>% unique()))
sites <- visits$Point_Name %>% unique() %>% sort()

site_pk <- field_dat1 %>% dplyr::select(Admin_Unit_Code, Point_Name) %>% distinct()
nsite_pk <- table(site_pk$Admin_Unit_Code) %>% as.vector()
write_csv(as.data.frame(nsite_pk), file = "data/nsite_pk.csv")

mxsite <- max(nsite_pk)
site_vec <- seq(1, mxsite,1)

sites_pk <- as_tibble(matrix(as.character(NA), ncol = npk, nrow = mxsite)) 
colnames(sites_pk) <- pk

for(j in 1:ncol(sites_pk)){
  slop <- site_pk %>% 
    dplyr::filter(Admin_Unit_Code == pk[j]) %>% 
    dplyr::select(Point_Name) %>% 
    pull() %>% 
    sort()
  for(i in 1:length(slop)){
    sites_pk[i,j] <- slop[i]
  }
}

sites_park_tib <- as_tibble(matrix(NA, ncol = 3, nrow = npk))
colnames(sites_park_tib) <- c("park", "nsite", "site_names")
sites_park_tib <- sites_park_tib %>% 
  mutate_at(3, as.list) %>% 
  mutate_at(1, as.character)

for(i in 1:npk){
  a <- sites_pk[,i] %>% drop_na() %>% as_tibble()
  a$site_n <- seq(1:nrow(a))
  sites_park_tib[i,3][[1]] <- list(a)
  sites_park_tib[i,2] <- sites_pk[,i] %>% drop_na() %>% nrow()
  sites_park_tib[i,1] <- pk[i]
  rm(a)
}

# add numbers for site for each park
for(i in 1:npk) {
  if(i == 1) {
    site_n_vec <- sites_park_tib[i,3] %>% pull() 
    site_n_vec <- site_n_vec[[1]]
    colnames(site_n_vec) <- c("Point_Name", "site_n")
  } else {
    join_site <-  sites_park_tib[i,3] %>% pull() 
    join_site <- join_site[[1]]
    colnames(join_site) <- c("Point_Name", "site_n")
    site_n_vec <- rbind(site_n_vec, join_site)
  }
}

site_pk <- site_n_vec %>% 
  mutate(park = substr(Point_Name, 1, 4),
         Admin_Unit_Code = park)

field_dat1.5 <- left_join(field_dat1, site_pk, by = c("Admin_Unit_Code", "Point_Name"))

## intervals --------------------------------------------------------------------------------------
field_dat1.75 <- field_dat1.5 %>% 
  mutate(Interval = ifelse(Interval == "NR", NA, Interval)) %>% 
  dplyr::filter(!is.na(Year),
                !is.na(Point_Name),
                !is.na(EventDate),
                !is.na(Interval))

ninterval <- field_dat1.75$Interval %>% unique() %>% length()
interval_tab <- cbind(Interval = field_dat1.75$Interval %>% unique() %>% sort(),
                      Interval_n = seq(1,10,1)) %>% as_tibble()

field_dat2 <- left_join(field_dat1.75, interval_tab, by = "Interval") %>% 
                  rename(Interval_Length = Interval)

# change intervals for only 5 (group 1 ans 2, and so on)
# field_dat2 <- field_dat1.75 %>% 
#   mutate(Interval = ifelse(Interval == 0 | Interval == 1, 1,
#                            ifelse(Interval == 2 | Interval == 3, 2,
#                                   ifelse(Interval == 4 | Interval == 5, 3,
#                                          ifelse(Interval == 6 | Interval == 7, 4,
#                                                 ifelse(Interval == 8 | Interval == 9, 5, Interval))))))

## distances --------------------------------------------------------------------------------------
ndist <- field_dat2$Distance_id %>% unique() %>% length()

dist <- field_dat2$Distance %>% factor(levels = c( "< 50 Meters", "> 50 Meters")) %>% as.numeric() %>% -1

## species --------------------------------------------------------------------------------------
(nsps <- field_dat2$AOU_Code %>% unique() %>% length())
sps <- field_dat2$AOU_Code %>% sort() %>% unique()

sp_pk <- field_dat2 %>% dplyr::select(Admin_Unit_Code, AOU_Code) %>% distinct()
nsp_pk <- table(sp_pk$Admin_Unit_Code) %>% as.vector()

mxsp <- max(nsp_pk)

sps_pk <- as_tibble(matrix(as.character(NA), ncol = npk, nrow = mxsp)) 
colnames(sps_pk) <- pk

for(j in 1:ncol(sps_pk)){
  slop <- sp_pk %>% 
    dplyr::filter(Admin_Unit_Code == pk[j]) %>% 
    dplyr::select(AOU_Code) %>% 
    pull() %>% 
    sort()
  for(i in 1:length(slop)){
    sps_pk[i,j] <- slop[i]
  }
}

# add 0 and 1 for distance
y1 <- field_dat2 %>% 
  as_tibble() %>% 
  cbind(.,dist)  %>% 
  filter(IdentificationMethod == "Audio")
  
  #%>% 
#mutate(Interval = as.numeric(Interval)) %>% 
#left_join(., intervals2, by = "Interval") %>% 
#dplyr::filter(!Interval == "NR") %>%    # remove obs with no interval info
#dplyr::select(-Interval) %>% 
#mutate(int_s = as.numeric(int_s))

#  select visit one
table(y1$Visit)
table(y1$IdentificationMethod)
table(y1$Distance_id)

# write_rds(visits2, file = "data/out/visits.rds")
