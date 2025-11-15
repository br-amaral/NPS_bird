#? *********************************************************************************
#? ----------------------------  back2d_covs_scales_3  -----------------------------
#? *********************************************************************************
# Code to run model to estimate the effect of different environmental
#   covariates on bird occupancy in several national parks and on three
#   different spatial scales. Code here filters and format the data for  
#   single-species models
#! Input ----------------------------------------------
#           - data/y_dat8.rds: tibble with bird data (2_create_data_files.R)
#           - data/X.rds: tibble with covariate data (2_create_data_files.R)
#           - data/out/nsite_pk.rds: vector with number of sites in each park
#           - data/src/key_park.rds: vector of all parks being analyzed
#
#! Output ---------------------------------------------
#           - data/model_res/jags_res_{sps}_{park}_run{run_number}.rds: file with result of jags model
#  freshr::freshr()
 #   test <- FALSE ; step_numb <- 1; sps_loop <- "BHVI"

# Load packages --------------------------------------
library(conflicted)
library(tidyverse)
library(glue)
library(jagsUI)
library(rjags)
#library(MCMCvis)
library(AHMbook)
library(fs)
library(here)
library(MCMCvis)
#library(BayesPostEst)

conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::filter)
conflicts_prefer(scales::alpha)
 
#if("sps_loop" %in% ls() == FALSE){stop("No species selected #38")}

# Make functions --------------------------------------
colanmes <- colnames
lenght <- length
`%!in%` <- Negate(`%in%`)
sum_na <- function(df){ # sum fuction to ignore NAs, but keep NA if all entries are NA
  if (all(is.na(df))){
    suma <- NA
  }  
  else {    
    suma <- sum(df, na.rm = T)
  }
  return(suma)
}

# Get the script name and time that it started running --
script_name <- "back2d_covs_scales_2min_spscovs.R"

cat("\n", "\n", "\n", 
    'Current script:', script_name, 
    "\n", "\n", "\n", "\n")
system_time1 <- Sys.time()
(date_step1 <- glue("{substr(system_time1, 1,4)}_{substr(system_time1, 6,7)}_{substr(system_time1, 9,10)}"))
date_step1 <- as.character(date_step1)

# Import data -----------------------------------------
## file paths
YDAT_PATH <- "data/y_dat8.rds"
XDAT_PATH <- "data/X.rds"
SITE_PK_PATH <- "data/out/nsite_pk.rds" #! TODO: where is nsite_pk.rds created?
PARK_PATH <- "data/key_park.rds"

## read files
y_dat4 <- read_rds(file = YDAT_PATH)

## stats
y_sta <- y_dat4 %>% 
          filter(AOU_Code %in% c("BAWW", "BHVI", "BLBW", "BRCR", "BTBW", "BTNW", "DOWO", "HAWO", 
                                 "HETH", "OVEN", "REVI", "SCTA", "VEER", "WBNU", "WOTH", "YBSA"))

y_sta %>% filter(bird_detec > 0) %>%  mutate(uniqueid = glue("{Point_Name}_{EventDate}")) %>% select(uniqueid, AOU_Code) %>%  distinct() %>% select(AOU_Code) %>% table()  %>% sum() 

y_sta %>% filter(bird_detec > 0) %>%  mutate(uniqueid = glue("{Point_Name}_{EventDate}")) %>% select(uniqueid, AOU_Code) %>%  distinct() %>% select(AOU_Code) %>% table() 

y_sta %>% filter(bird_detec > 0) %>%  mutate(uniqueid = glue("{Point_Name}_{EventDate}")) %>% select(uniqueid, AOU_Code) %>%  distinct() %>% select(AOU_Code) %>% table()   %>% sort()

y_sta %>% filter(Interval_n == 1) %>%  select(park) %>% table() %>% sort()

y_sta %>% filter(Interval_n == 1) %>%  select(Year, AOU_Code) %>% table() 


y_sta %>% 
  filter(bird_detec > 0) %>% 
   mutate(uniqueid = glue("{Point_Name}_{EventDate}")) %>% 
   select(uniqueid, AOU_Code, Year) %>%  
   distinct() %>% 
   select(Year, AOU_Code) %>% 
   distinct() %>% 
   select(AOU_Code) %>% 
   table() %>% 
   sort() # %>% mean()

y_sta %>% 
  filter(bird_detec > 0) %>% 
   mutate(uniqueid = glue("{Point_Name}_{EventDate}")) %>% 
   select(uniqueid, AOU_Code, Year) %>%  
   distinct() %>% 
   select(Year, AOU_Code) %>% 
   distinct() %>% 
   table()  %>% 
   colSums()

y_sta %>% 
  filter(bird_detec > 0) %>% 
   mutate(uniqueid = glue("{Point_Name}_{EventDate}")) %>% 
   select(uniqueid, AOU_Code, park) %>%  
   distinct() %>% 
   select(park, AOU_Code) %>% 
   distinct() %>% 
   table()  %>% 
   colSums()

y_sta %>% 
   mutate(uniqueid = glue("{park}_{Year}")) %>% 
   select(uniqueid, park, Year) %>%  
   distinct() %>% 
   select(Year, park) %>% 
   distinct() %>% 
   table()  %>% 
   colSums()

y_sta %>% 
   mutate(uniqueid = glue("{Point_Name}_{Year}")) %>% 
   select(uniqueid, Point_Name, Year, park) %>%  
   distinct() %>% 
   select(Point_Name, park) %>% 
   distinct() %>% 
   table()  %>% 
   colSums()

y_dat4  %>% select(AOU_Code, park) %>% distinct() %>% table() %>% colSums()

y_sta  %>% select(AOU_Code, park) %>% distinct() %>% table() %>% colSums()

#########
X10 <- read_rds(file = XDAT_PATH)

nsite_pk <- read_rds(SITE_PK_PATH)
pk <- read_rds(PARK_PATH) %>%
  dplyr::select(parks) %>%
  pull() %>%
  sort()

rem_pks <- as_tibble(cbind(nsite_pk,pk)) %>% 
              filter(pk %!in% c("ACAD","ELRO","SAIR"))

nsite_pk <- as.numeric(rem_pks$nsite_pk)
pk <- as.vector(rem_pks$pk)

# Filter for species and park ---------------------------------------
## 1 sps several parks
y_dat5 <- y_dat4
# _n means that the 1 is the first occasion for that sps, year, loc, etc, not the first calendar one

y_dat5 <- y_dat5 %>%
  mutate( parkey = as.numeric(parkey),
          sps_it = AOU_Code)

nrow(X10) == nrow(y_dat5)

y_dat5$unique_index <- seq(1,nrow(y_dat5),1)

X10$unique_index <- seq(1,nrow(y_dat5),1)

if(setequal(y_dat5$unique_index, X10$unique_index) != "TRUE") stop("ah wrong indexing!!!! #82")

y_dat6 <- y_dat5 %>% 
  dplyr::filter(sps_it %in% sps_loop,
                park %in% pk
  )

if(length(sps_loop) == 1){
  print(glue("analazing one species: {sps_loop}"))
  } else {
  print('analazing a community: {sps_loop}')
}

y_dat6 <- y_dat6 %>% filter(bird_detec > 0) 

X10 <- X10 %>% 
    dplyr::filter(unique_index %in% y_dat6$unique_index)

nrow(X10) == nrow(y_dat6)
glu1 <- paste(shQuote(sort(unique(y_dat6$sps_it))), collapse=", ")
spsglue <- glue("the species are {glu1}, and parks are")
parkglue <- paste(shQuote(sort(unique(y_dat6$park))), collapse=", ")
print(paste(spsglue,parkglue))

## add a step here to fix parkey
parkey_right <- y_dat6 %>% 
  dplyr::select(Admin_Unit_Code, parkey) %>% 
  arrange(parkey) %>% 
  distinct() %>% 
  mutate(parkey = seq(1, nrow(.)))

y_dat6 <- y_dat6 %>% 
  dplyr::select(-parkey) %>% 
  left_join(., parkey_right, by = "Admin_Unit_Code")%>%
          rename(time_jul = StartTime2,
                 date_jul = EventDate2)

y <- y_dat6 %>% 
  dplyr::select(bird_detec, parkey, site_n, year_n, interval_n, Year) 

#! STOP AND CHECK ---------------------------------------------------------
# check detections that I have for each occasion (5 intervals)
y_test <- y %>% 
            mutate(unique_occ = glue("{parkey}_{site_n}_{year_n}")) %>%
            select(unique_occ, bird_detec) %>%
            mutate(bird_detec_na = ifelse(is.na(bird_detec), 2, bird_detec)) %>% 
            select(unique_occ, bird_detec_na) %>%
            table() 

rowSums(y_test) %>% unique() # has to be always 10
colSums(y_test)
sum(y_test) == nrow(y)
sum(y_test[,1]) == sum(y$bird_detec, na.rm = T)
sum(y_dat6$bird_detec, na.rm = T) == sum(y_test[,1])

# check detections by:
## park
table(y_dat6 %>% select(park, bird_detec))
table(y %>% select(parkey, bird_detec))
setequal(table(y_dat6 %>% select(park, bird_detec)),
         table(y %>% select(parkey, bird_detec)))

## park and site and year 
table(y_dat6 %>% select(parkey, site_n, year_n, bird_detec) %>% 
                 mutate(uni_comb = glue("{parkey}_{site_n}_{year_n}")) %>% 
                              select(uni_comb, bird_detec))

table(y %>% select(parkey, site_n, year_n, bird_detec) %>% 
            mutate(uni_comb = glue("{parkey}_{site_n}_{year_n}")) %>% 
            select(uni_comb, bird_detec))

setequal(y_dat6 %>% select(parkey, site_n, year_n, bird_detec) %>% 
                    mutate(uni_comb = glue("{parkey}_{site_n}_{year_n}")) %>% 
                    select(uni_comb, bird_detec),
         y %>% select(parkey, site_n, year_n, bird_detec) %>% 
               mutate(uni_comb = glue("{parkey}_{site_n}_{year_n}")) %>% 
               select(uni_comb, bird_detec))

#? get covariates ----------------------------------------------------------------
X <- X10 %>% 
        dplyr::select(-unique_index) %>% 
        rename(date_jul = EventDate2,
               time_jul = StartTime2,
               Admin_Unit_Code = park) %>% 
        mutate(siteDEN_s =   standardize(treeden_ha_site),
               parkDEN_s =   standardize(treeden_ha_park),
               counDEN_s =   standardize(treeden_ha_coun),
               siteBAcon_s = standardize(BA_m2ha_Conifer_site), 
               parkBAcon_s = standardize(BA_m2ha_Conifer_park),
               counBAcon_s = standardize(BA_m2ha_Conifer_coun),
               siteBAlar_s = standardize(BA_m2ha_large_site), 
               parkBAlar_s = standardize(BA_m2ha_large_park),
               counBAlar_s = standardize(BA_m2ha_large_coun),
               siteSHR_s =   standardize(shrub_avg_cov_site),
               parkSHR_s =   standardize(shrub_avg_cov_park),
               counSHR_s =   standardize(shrub_cov_coun),
               siteBA_s =    standardize(BA_m2ha_site),
               parkBA_s =    standardize(BA_m2ha_park), 
               counBA_s =    standardize(BA_m2ha_coun),
               area_s =      standardize(area),
               date_jul_s =  standardize(date_jul),
               time_jul_s =  standardize(time_jul))

# save mean and sd for covariates to unstransform predictions
X_unstd <- X %>% 
               # mean
        mutate(siteDEN_mean =   mean(treeden_ha_site, na.rm = T),
               parkDEN_mean =   mean(treeden_ha_park, na.rm = T),
               counDEN_mean =   mean(treeden_ha_coun, na.rm = T),
               siteBAcon_mean = mean(BA_m2ha_Conifer_site, na.rm = T), 
               parkBAcon_mean = mean(BA_m2ha_Conifer_park, na.rm = T),
               counBAcon_mean = mean(BA_m2ha_Conifer_coun, na.rm = T),
               siteBAlar_mean = mean(BA_m2ha_large_site, na.rm = T), 
               parkBAlar_mean = mean(BA_m2ha_large_park, na.rm = T),
               counBAlar_mean = mean(BA_m2ha_large_coun, na.rm = T),
               siteSHR_mean =   mean(shrub_avg_cov_site, na.rm = T),
               parkSHR_mean =   mean(shrub_avg_cov_park, na.rm = T),
               counSHR_mean =   mean(shrub_cov_coun, na.rm = T),
               siteBA_mean =    mean(BA_m2ha_site, na.rm = T),
               parkBA_mean =    mean(BA_m2ha_park, na.rm = T), 
               counBA_mean =    mean(BA_m2ha_coun, na.rm = T),
               area_mean =      mean(area, na.rm = T),
               date_jul_mean =  mean(date_jul, na.rm = T),
               time_jul_mean =  mean(time_jul, na.rm = T),
               # sd
               siteDEN_sd =   sd(treeden_ha_site, na.rm = T),
               parkDEN_sd =   sd(treeden_ha_park, na.rm = T),
               counDEN_sd =   sd(treeden_ha_coun, na.rm = T),
               siteBAcon_sd = sd(BA_m2ha_Conifer_site, na.rm = T), 
               parkBAcon_sd = sd(BA_m2ha_Conifer_park, na.rm = T),
               counBAcon_sd = sd(BA_m2ha_Conifer_coun, na.rm = T),
               siteBAlar_sd = sd(BA_m2ha_large_site, na.rm = T), 
               parkBAlar_sd = sd(BA_m2ha_large_park, na.rm = T),
               counBAlar_sd = sd(BA_m2ha_large_coun, na.rm = T),
               siteSHR_sd =   sd(shrub_avg_cov_site, na.rm = T),
               parkSHR_sd =   sd(shrub_avg_cov_park, na.rm = T),
               counSHR_sd =   sd(shrub_cov_coun, na.rm = T),
               siteBA_sd =    sd(BA_m2ha_site, na.rm = T),
               parkBA_sd =    sd(BA_m2ha_park, na.rm = T), 
               counBA_sd =    sd(BA_m2ha_coun, na.rm = T),
               area_sd =      sd(area, na.rm = T),
               date_jul_sd =  sd(date_jul, na.rm = T),
               time_jul_sd =  sd(time_jul),
               # min
               siteDEN_min =   min(treeden_ha_site, na.rm = T),
               parkDEN_min =   min(treeden_ha_park, na.rm = T),
               counDEN_min =   min(treeden_ha_coun, na.rm = T),
               siteBAcon_min = min(BA_m2ha_Conifer_site, na.rm = T), 
               parkBAcon_min = min(BA_m2ha_Conifer_park, na.rm = T),
               counBAcon_min = min(BA_m2ha_Conifer_coun, na.rm = T),
               siteBAlar_min = min(BA_m2ha_large_site, na.rm = T), 
               parkBAlar_min = min(BA_m2ha_large_park, na.rm = T),
               counBAlar_min = min(BA_m2ha_large_coun, na.rm = T),
               siteSHR_min =   min(shrub_avg_cov_site, na.rm = T),
               parkSHR_min =   min(shrub_avg_cov_park, na.rm = T),
               counSHR_min =   min(shrub_cov_coun, na.rm = T),
               siteBA_min =    min(BA_m2ha_site, na.rm = T),
               parkBA_min =    min(BA_m2ha_park, na.rm = T), 
               counBA_min =    min(BA_m2ha_coun, na.rm = T),
               area_min =      min(area, na.rm = T),
               date_jul_min =  min(date_jul, na.rm = T),
               time_jul_min =  min(time_jul, na.rm = T),
               # max
               siteDEN_max =   max(treeden_ha_site, na.rm = T),
               parkDEN_max =   max(treeden_ha_park, na.rm = T),
               counDEN_max =   max(treeden_ha_coun, na.rm = T),
               siteBAcon_max = max(BA_m2ha_Conifer_site, na.rm = T), 
               parkBAcon_max = max(BA_m2ha_Conifer_park, na.rm = T),
               counBAcon_max = max(BA_m2ha_Conifer_coun, na.rm = T),
               siteBAlar_max = max(BA_m2ha_large_site, na.rm = T), 
               parkBAlar_max = max(BA_m2ha_large_park, na.rm = T),
               counBAlar_max = max(BA_m2ha_large_coun, na.rm = T),
               siteSHR_max =   max(shrub_avg_cov_site, na.rm = T),
               parkSHR_max =   max(shrub_avg_cov_park, na.rm = T),
               counSHR_max =   max(shrub_cov_coun, na.rm = T),
               siteBA_max =    max(BA_m2ha_site, na.rm = T),
               parkBA_max =    max(BA_m2ha_park, na.rm = T), 
               counBA_max =    max(BA_m2ha_coun, na.rm = T),
               area_max =      max(area, na.rm = T),
               date_jul_max =  max(date_jul, na.rm = T),
               time_jul_max =  max(time_jul)) 
               
X_sites <- X_unstd %>% 
              select(Admin_Unit_Code, Point_Name, 
                      ends_with("_s"))  %>% 
              distinct()

X_vals <- X_unstd  %>% 
        select(ends_with("_mean"), ends_with("_sd"), ends_with("_min"), ends_with("_max"))  %>% 
        distinct()

write_rds(X_sites, file = glue("data/out/X_sites_{sps_loop}.rds"))

write_rds(X_vals, file = glue("data/out/X_vals_{sps_loop}.rds"))
