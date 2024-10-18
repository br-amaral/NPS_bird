#? *********************************************************************************
#? ----------------------------  back2d_covs_scales_3  -----------------------------
#? *********************************************************************************
# Code to run model to estimate the effect of different environmental
#   covariates on bird occupancy in several national parks and on three
#   different spatial scales
#! Input ----------------------------------------------
#           - data/y_dat8.rds: tibble with bird data (2_create_data_files.R)
#           - data/X.rds: tibble with covariate data (2_create_data_files.R)
#           - data/out/nsite_pk.rds: vector with number of sites in each park
#           - data/src/key_park.rds: vector of all parks being analyzed
#
#! Output ---------------------------------------------
#           - data/model_res/jags_res_{sps}_{park}_run{run_number}.rds: file with result of jags model

# detach packages and clear workspace
#if(!require(freshr)){install.packages("freshr")}
#freshr::freshr()

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
script_name <- "back2d_covs_scales_2min_spscovs_yr.R"

cat("\n", "\n", "\n", 
    'Current script:', script_name, 
    "\n", "\n", "\n", "\n")
system_time1 <- Sys.time()
(date_out <- glue("{substr(system_time1, 1,4)}_{substr(system_time1, 6,7)}_{substr(system_time1, 9,10)}"))

# Import data -----------------------------------------
## file paths
YDAT_PATH <- "data/y_dat8_yr.rds"
XDAT_PATH <- "data/X_yr.rds"
SITE_PK_PATH <- "data/out/nsite_pk.rds"
PARK_PATH <- "data/src/key_park.rds"

## read files
y_dat4 <- read_rds(file = YDAT_PATH)

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

y_dat5_cov <- left_join(y_dat5, X10, by = c("Year", "Point_Name", "park", "site_n", "interval_n"))

y_dat6 <- y_dat5_cov %>% 
  dplyr::filter(sps_it %in% sps_loop,
                park %in% pk
  )

if(length(sps_loop) == 1){
  print(glue("analazing one species: {sps_loop}"))
  } else {
  print('analazing a community: {sps_loop}')
}

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
  left_join(., parkey_right, by = "Admin_Unit_Code")

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

rowSums(y_test) %>% unique()
colSums(y_test)
sum(y_test) == nrow(y)
sum(y_test[,2]) == sum(y$bird_detec, na.rm = T)
sum(y_dat6$bird_detec, na.rm = T) == sum(y_test[,2])

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
  dplyr::select(Point_Name, Year,
          siteDENYR, siteBAYR,
          siteH_g, siteEh_g,
          siteBA_poleYR, siteBA_matureYR, siteBA_largeYR,
          siteSHRUdenYR,
          siteCANOdenYR, siteDEBRdenYR,
          parkDEN, parkBA, 
          parkH_g, parkEh_g,
          parkBA_pole, parkBA_mature, parkBA_large,   
          parkSHRUden, 
          parkCANyr, parkDEByr,
          counDEN, counBA, 
          counH_g, counEh_g, ## https://rdrr.io/cran/rFIA/man/diversity.html
          counPER_pole, counPER_matu, counPER_late,
          counSHRUden,
          Can_cov, Dwn_Dbr,
          area,
          EventDate2, StartTime2) %>% 
  rename(date_jul = EventDate2,
          time_jul = StartTime2) %>% 
  mutate( siteBA_s = standardize(siteBAYR),
          siteDEN_s = standardize(siteDENYR),
          siteH_g = standardize(siteH_g),
          siteEh_g = standardize(siteEh_g),
          siteBA_pole_s = standardize(siteBA_poleYR),
          siteBA_mature_s = standardize(siteBA_matureYR),
          siteBA_large_s = standardize(siteBA_largeYR),
          siteSHRUden_s = standardize(siteSHRUdenYR),
          siteCANcov_s = standardize(siteCANOdenYR),
          siteDEBden_s = standardize(siteDEBRdenYR),
          parkDEN_s = standardize(parkDEN),
          parkBA_s = standardize(parkBA),
          parkH_g = standardize(parkH_g), 
          parkEh_g = standardize(parkEh_g),
          parkBA_pole_s = standardize(parkBA_pole),
          parkBA_mature_s = standardize(parkBA_mature),
          parkBA_large_s = standardize(parkBA_large),
          parkSHRUden_s = standardize(parkSHRUden),
          parkCANcov_s = standardize(parkCANyr),
          parkDEBden_s = standardize(parkDEByr),
          counDEN_s = standardize(counDEN),
          counBA_s = standardize(counBA),
          counH_g = standardize(counH_g),
          counEh_g = standardize(counEh_g),
          counPER_pole_s = standardize(counPER_pole),
          counPER_matu_s = standardize(counPER_matu),
          counPER_late_s = standardize(counPER_late),
          counSHRUper_s = standardize(counSHRUden),
          counCANcov_s = standardize(Can_cov),
          counDEBden_s = standardize(Dwn_Dbr),
          area_s = standardize(area),
          date_jul_s = standardize(date_jul),
          time_jul_s = standardize(time_jul))

#! TODO: for now, im putting zeros in the occasions that have no environmental data (mean)
table(is.na(X))
X[is.na(X)] <- 0

# occupancy variables - separate them in covs in all scales per tibble
## tree basal area
X1 <- X %>% 
  dplyr::select(siteBA_s, parkBA_s, counBA_s) %>% 
  mutate(siteBA_s = ifelse(is.na(siteBA_s) == TRUE, 0, siteBA_s),
         parkBA_s = ifelse(is.na(parkBA_s) == TRUE, 0, parkBA_s),
         counBA_s = ifelse(is.na(counBA_s) == TRUE, 0, counBA_s))

## tree density
X2 <- X %>% 
  dplyr::select(siteDEN_s, parkDEN_s, counDEN_s)%>% 
  mutate(siteDEN_s = ifelse(is.na(siteDEN_s) == TRUE, 0, siteDEN_s),
         parkDEN_s = ifelse(is.na(parkDEN_s) == TRUE, 0, parkDEN_s),
         counDEN_s = ifelse(is.na(counDEN_s) == TRUE, 0, counDEN_s))

## Shrub density and percentage
X3 <- X %>% 
  dplyr::select(siteSHRUden_s, parkSHRUden_s, counSHRUper_s)%>% 
  mutate(siteSHRUden_s = ifelse(is.na(siteSHRUden_s) == TRUE, 0, siteSHRUden_s),
         parkSHRUden_s = ifelse(is.na(parkSHRUden_s) == TRUE, 0, parkSHRUden_s),
         counSHRUper_s = ifelse(is.na(counSHRUper_s) == TRUE, 0, counSHRUper_s))
  
## Forets diversity
X4 <- X %>% 
  dplyr::select(siteH_g, siteEh_g,
                parkH_g, parkEh_g,
                counH_g, counEh_g)%>% 
  mutate(siteH_g = ifelse(is.na(siteH_g) == TRUE, 0, siteH_g),
         parkH_g = ifelse(is.na(parkH_g) == TRUE, 0, parkH_g),
         counH_g = ifelse(is.na(counH_g) == TRUE, 0, counH_g),
         siteEh_g = ifelse(is.na(siteEh_g) == TRUE, 0, siteEh_g),
         parkEh_g = ifelse(is.na(parkEh_g) == TRUE, 0, parkEh_g),
         counEh_g = ifelse(is.na(counEh_g) == TRUE, 0, counEh_g))

## Basal area large
X5l <- X %>% 
  dplyr::select(siteBA_large_s, parkBA_large_s, counPER_late_s)%>% 
  mutate(siteBA_large_s = ifelse(is.na(siteBA_large_s) == TRUE, 0, siteBA_large_s),
         parkBA_large_s = ifelse(is.na(parkBA_large_s) == TRUE, 0, parkBA_large_s),
         counPER_late_s = ifelse(is.na(counPER_late_s) == TRUE, 0, counPER_late_s))

## Basal area mature
X5m <- X %>% 
  dplyr::select(siteBA_mature_s, parkBA_mature_s, counPER_matu_s)%>% 
  mutate(siteBA_mature_s = ifelse(is.na(siteBA_mature_s) == TRUE, 0, siteBA_mature_s),
         parkBA_mature_s = ifelse(is.na(parkBA_mature_s) == TRUE, 0, parkBA_mature_s),
         counPER_matu_s = ifelse(is.na(counPER_matu_s) == TRUE, 0, counPER_matu_s))

## Basal area pole
X5p <- X %>% 
  dplyr::select(siteBA_pole_s, parkBA_pole_s, counPER_pole_s)%>% 
  mutate(siteBA_pole_s = ifelse(is.na(siteBA_pole_s) == TRUE, 0, siteBA_pole_s),
         parkBA_pole_s = ifelse(is.na(parkBA_pole_s) == TRUE, 0, parkBA_pole_s),
         counPER_pole_s = ifelse(is.na(counPER_pole_s) == TRUE, 0, counPER_pole_s))

## Canopy cover
X6 <- X %>% 
  dplyr::select(siteCANcov_s, parkCANcov_s, counCANcov_s) %>% 
  mutate(siteCANcov_s = ifelse(is.na(siteCANcov_s) == TRUE, 0, siteCANcov_s),
         parkCANcov_s = ifelse(is.na(parkCANcov_s) == TRUE, 0, parkCANcov_s),
         counCANcov_s = ifelse(is.na(counCANcov_s) == TRUE, 0, counCANcov_s))

## Coarse wood debris
X7 <- X %>% 
  dplyr::select(siteDEBden_s, parkDEBden_s, counDEBden_s)%>% 
  mutate(siteDEBden_s = ifelse(is.na(siteDEBden_s) == TRUE, 0, siteDEBden_s),
         parkDEBden_s = ifelse(is.na(parkDEBden_s) == TRUE, 0, parkDEBden_s),
         counDEBden_s = ifelse(is.na(counDEBden_s) == TRUE, 0, counDEBden_s))

## park size
Xp <- X %>% 
  dplyr::select(area_s) %>% 
  pull() %>% 
  as.numeric()

# detection variables
Xa <- X %>% 
  dplyr::select(time_jul_s)

Xb <- X %>% 
  dplyr::select(date_jul_s)

#! STOP AND CHECK ---------------------------------------------------------
## site, park, county and covs values

## site, interval, year and detec covs

# put everything together, arrange, and split!

y_all <- cbind(y, X1, X2, X3, X4, X5p, X5m, X5l, Xa, Xb, Xp) %>% 
  as_tibble() %>% 
  arrange(parkey, site_n, year_n, interval_n)  %>% 
# and GROUPING THE INTERVALS IN FIVES  
  mutate(interval_2 = ifelse(interval_n %in% c(1,2), 1, 
                                ifelse(interval_n %in% c(3,4), 2, 
                                    ifelse(interval_n %in% c(5,6), 3, 
                                        ifelse(interval_n %in% c(7,8), 4, 
                                            5)))))
# group the 10 intervals on fives
y_all2 <- y_all  %>% 
        group_by(parkey, site_n, year_n, interval_2
                 ) %>%
        mutate(bird_detec2 = ifelse(sum_na(bird_detec) > 0, 1, 0), 
               Year2 = mean(Year), 
               siteBA_s2 = mean(siteBA_s), 
               parkBA_s2 = mean(parkBA_s), 
               counBA_s2 = mean(counBA_s), 
               siteDEN_s2 = mean(siteDEN_s), 
               parkDEN_s2 = mean(parkDEN_s), 
               counDEN_s2 = mean(counDEN_s),  
               siteSHRUden_s2 = mean(siteSHRUden_s), 
               parkSHRUden_s2 = mean(parkSHRUden_s), 
               counSHRUper_s2 = mean(counSHRUper_s),
               siteBA_pole_s2 = mean(siteBA_pole_s), 
               parkBA_pole_s2 = mean(parkBA_pole_s), 
               counPER_pole_s2 = mean(counPER_pole_s),
               siteBA_mature_s2 = mean(siteBA_mature_s),
               parkBA_mature_s2 = mean(parkBA_mature_s),
               counPER_matu_s2 = mean(counPER_matu_s),
               siteBA_large_s2 = mean(siteBA_large_s), 
               parkBA_large_s2 = mean(parkBA_large_s), 
               counPER_late_s2 = mean(counPER_late_s),
               siteH_g2 = mean(siteH_g),
               parkH_g2 = mean(parkH_g),
               counH_g2 = mean(counH_g),
               siteEh_g2 = mean(siteEh_g),
               parkEh_g2 = mean(parkEh_g),
               counEh_g2 = mean(counEh_g),
               time_jul_s2 = mean(time_jul_s),
               date_jul_s2 = mean(date_jul_s),
               area_s2 = mean(Xp)) %>% 
        ungroup()

table(y_all2$Year == y_all2$Year2)
table(y_all2$siteBA_s == y_all2$siteBA_s2)
table(y_all2$parkBA_s == y_all2$parkBA_s2)
table(y_all2$counBA_s == y_all2$counBA_s2)
table(y_all2$siteDEN_s == y_all2$siteDEN_s2)
table(y_all2$parkDEN_s == y_all2$parkDEN_s2)
table(y_all2$counDEN_s == y_all2$counDEN_s2)
table(y_all2$siteSHRUden_s == y_all2$siteSHRUden_s2)
table(y_all2$parkSHRUden_s == y_all2$parkSHRUden_s2)
table(y_all2$counSHRUper_s == y_all2$counSHRUper_s2)
table(y_all2$date_jul_s2 == y_all2$date_jul_s)
table(y_all2$area_s2 == y_all2$Xp)

# FALSES
table(y_all2$time_jul_s2 == y_all2$time_jul_s)
table(y_all2$bird_detec2 == y_all2$bird_detec)
sum(y$bird_detec, na.rm = T)
sum(y_all$bird_detec, na.rm = T)
sum(y_all2$bird_detec, na.rm = T)

y_all3 <- y_all2 %>% 
                select(bird_detec2, parkey, site_n, year_n, Year2, interval_2,
                       time_jul_s2, date_jul_s2, area_s2,
                       siteBA_s2, parkBA_s2, counBA_s2, 
                       siteDEN_s2, parkDEN_s2, counDEN_s2,  
                       siteSHRUden_s2, parkSHRUden_s2, counSHRUper_s2,
                       siteBA_pole_s2, parkBA_pole_s2, counPER_pole_s2,
                       siteBA_mature_s2, parkBA_mature_s2, counPER_matu_s2,
                       siteBA_large_s2, parkBA_large_s2, counPER_late_s2,
                       siteH_g2, parkH_g2, counH_g2,
                       siteEh_g2, parkEh_g2, counEh_g2) %>% 
                rename(bird_detec      = bird_detec2, 
                       Year            = Year2,
                       interval_n      = interval_2,
                       siteBA_s        = siteBA_s2, 
                       parkBA_s        = parkBA_s2, 
                       counBA_s        = counBA_s2, 
                       siteDEN_s       = siteDEN_s2, 
                       parkDEN_s       = parkDEN_s2, 
                       counDEN_s       = counDEN_s2,  
                       siteSHRUden_s   = siteSHRUden_s2, 
                       parkSHRUden_s   = parkSHRUden_s2, 
                       counSHRUper_s   = counSHRUper_s2,
                       siteBA_pole_s   = siteBA_pole_s2, 
                       parkBA_pole_s   = parkBA_pole_s2, 
                       counPER_pole_s  = counPER_pole_s2,
                       siteBA_mature_s = siteBA_mature_s2,
                       parkBA_mature_s = parkBA_mature_s2,
                       counPER_matu_s  = counPER_matu_s2,
                       siteBA_large_s  = siteBA_large_s2, 
                       parkBA_large_s  = parkBA_large_s2, 
                       counPER_late_s  = counPER_late_s2,
                       siteH_g         = siteH_g2,
                       parkH_g         = parkH_g2,
                       counH_g         = counH_g2,
                       siteEh_g        = siteEh_g2,
                       parkEh_g        = parkEh_g2,
                       counEh_g        = counEh_g2,
                       time_jul_s      = time_jul_s2,
                       date_jul_s      = date_jul_s2,
                       area_s          = area_s2)
dim(y_all3)
dim(y_all2)

cols_covs <- c()
if(BA  == 1) {cols_covs <- c(cols_covs, 10, 11, 12)}
if(DEN == 1) {cols_covs <- c(cols_covs, 13, 14, 15)}
if(SHR == 1) {cols_covs <- c(cols_covs, 16, 17, 18)}
if(DIV == 1) {cols_covs <- c(cols_covs, 28, 29, 30)} # or 31, 32, 33
if(EAR == 1) {cols_covs <- c(cols_covs, 19, 20, 21)}
if(MID == 1) {cols_covs <- c(cols_covs, 22, 23, 24)}
if(LAT == 1) {cols_covs <- c(cols_covs, 25, 26, 27)}
(c(BA, DEN, SHR, DIV, EAR, MID,LAT) == cov_key %>% as.numeric())

y_all3.2 <- cbind(y_all3[,c(1:9, cols_covs)]) %>% as_tibble()
dim(y_all3)
dim(y_all3.2)
colnames(y_all3.2)

rm(list = c("y", "X1", "X2", "X3", "X4", "X5p", "X5m", "X5l", "Xa", "Xb", "Xp"))

y_all4 <- y_all3.2  %>% 
                distinct()

#! getting HALF of the rows because now I have 5 removal sampling intervals, not 10 
nrow(y_all4) == 1/2*(nrow(y_all3.2))

if(BA  == 1) {X1 <- y_all4 %>% select(siteBA_s, parkBA_s, counBA_s) %>% as.matrix()}
if(DEN == 1) {X2 <- y_all4 %>% select(siteDEN_s, parkDEN_s, counDEN_s) %>% as.matrix()}
if(SHR == 1) {X3 <- y_all4 %>% select(siteSHRUden_s, parkSHRUden_s, counSHRUper_s) %>% as.matrix()}
if(DIV == 1) {X4 <- y_all4 %>% select(siteH_g, parkH_g, counH_g) %>% as.matrix()}
if(EAR == 1) {X51 <- y_all4 %>% select(siteBA_pole_s, parkBA_pole_s, counPER_pole_s) %>% as.matrix()}
if(MID == 1) {X52 <- y_all4 %>% select(siteBA_mature_s, parkBA_mature_s, counPER_matu_s) %>% as.matrix()}
if(LAT == 1) {X53 <- y_all4 %>% select(siteBA_large_s, parkBA_large_s, counPER_late_s) %>% as.matrix()}

Xa <- y_all4 %>% select(time_jul_s)
Xb <- y_all4 %>% select(date_jul_s)
Xp <- y_all4 %>% select(area_s) %>% pull()

y <- y_all4 %>% select(bird_detec, parkey, site_n, year_n, 
                         interval_n, Year) # interval_n is now interval2

## trick for coding = only interval one for starting values
y2 <- y %>% 
  dplyr::filter(interval_n == 1)

#colnames(y) <- c("bird_detec", "parkey", "sitekey", "yearkey", "intervalkey",# "year_site",
#                  "Year")

# initial values
Zst <- y %>% 
  dplyr::select(bird_detec, parkey, site_n, year_n, interval_n) %>% 
  group_by(parkey, site_n, year_n) %>% 
  mutate(z = ifelse(sum(bird_detec, na.rm = T) == 0, 0, 1)) %>% 
  ungroup() %>% 
  dplyr::filter(interval_n == 1) 

site_vec <- seq(1,max(nsite_pk),1)
(npk <- length(unique(y$parkey)))
(pk <- sort(unique(y$parkey)))
years <- y %>% 
  dplyr::select(Year) %>% 
  distinct() %>% 
  arrange() %>% 
  pull() %>% 
  sort()
years
ninterval <- 5

Zst2 <- 
  array(NA, 
        dim = c(npk,
                max(nsite_pk),
                length(years)
        ),
        dimnames = list(pk,
                        site_vec,
                        years
        ))

for(a in 1:nrow(Zst)){
  #   a <- which(Zst$parkey == 10 & Zst$site_n == 1 & Zst$year_s == 1, arr.ind = T)
  zl <- Zst[a,]
  
  r <- as.numeric(zl$parkey)
  j <- zl$site_n 
  t <- zl$year_n

  Zst2[r,j,t] <- as.numeric(zl$z)
  
}

y <- data.matrix(y)
y2 <- data.matrix(y2)
y_ind <- sort(rep(seq(1, nrow(y2),1),ninterval))
nrow(y)
nrow(y2)*ninterval
length(Xp)
dim(Xa)
dim(Xb)

# number of alphas and betas
(cov_key2 <- ifelse(cov_key == 1 , 1, 0))
(n_bs <- sum(cov_key2) + 1)
n_as <- 3
if(length(sps_loop) > 1) { sps_name <- "commu"} else {sps_name <- sps_loop}
if(length((unique(y[,2]))) == 1) { park_name <- unique(y[,2])} else {park_name <- "parks"}

# remove covariates that are not in the sps analysis (set to zero)
# bigX <- cbind(X1, X2, X3, X4, X51, X52, X53)
# rem_covs <- which(cov_key2 == 0, arr.ind = T)[,2]
# if(1 %in% rem_covs){bigX[  ,1:3]  <- NA ; X1[,] <- NA}
# if(2 %in% rem_covs){bigX[  ,4:6]  <- NA ; X2[,] <- NA}
# if(3 %in% rem_covs){bigX[  ,7:9]  <- NA ; X3[,] <- NA}
# if(4 %in% rem_covs){bigX[ ,10:12] <- NA ; X4[,] <- NA}
# if(5 %in% rem_covs){bigX[ ,13:15] <- NA ; X51[,] <- NA}
# if(6 %in% rem_covs){bigX[ ,16:18] <- NA ; X52[,] <- NA}
# if(7 %in% rem_covs){bigX[ ,19:21] <- NA ; X53[,] <- NA}
# Define the object names
object_names <- c("X1", "X2", "X3", "X4", "X51", "X52", "X53")

# Check for existence and filter the names
existing_objects <- object_names[sapply(object_names, exists)]
cov_Xs <- rbind(colnames(cov_key), object_names)
cov_Xs[2,which(cov_key2>0)] == existing_objects

# Create the list with existing objects
object_list <- mget(existing_objects)

# model
jags.data <- c(
  list(
    y = y,
    y2 = y2,
    n_bs = n_bs,
    n_as = n_as,
    nrowy = nrow(y),
    nrowy2 = nrow(y2),
    Xp = Xp,
    Xa = as.matrix(Xa),
    Xb = as.matrix(Xb),
    n_yrM = length(unique(y[, 4])),
    n_pkM = length(unique(y[, 2]))
    # y_ind = y_ind
  ),
  object_list
)

# Print the structure of jags.data to verify
str(jags.data)

non_numeric_elements <- sapply(jags.data, function(x) !is.numeric(x))
non_numeric_elements <- names(jags.data)[non_numeric_elements]
print(non_numeric_elements)

#! jags.data structure:
# y: detection matrix
# y2: first detection matrix
# n_bs: number of betas
# n_as: number of alphas
# n_sca_b: number of scales of beta
# nrowy: number of total rows (all detections)
# nrowy2: number of rows of first detections 
# bigX: all environmental covs in the three scales
# Xp: park size
# Xa: detection time
# Xb: detection day of the year
# n_yrM: number of years 
# n_pkM: number of parks 
write_rds(jags.data, file = glue("data/ana_file/{date_out}_data_{sps_name}_{park_name}.rds"))

# source("code/check_data.R") 

inits <- function() {
    list(
        Z = Zst2,
        # mu_beta0 = rnorm(1, 0.5), # check this!!!!!
        beta = rnorm(n_bs, 0.5),
        mu.alpha0 = rnorm(1, 0.5),
        alpha = rnorm(n_as, 0.5)
    )
}

paste('\n ************************************* \n \n \n Running JAGS for:', '\n',
      '  Parks =', park_name, '\n',
      '  Species =', sps_name, '\n',
      '  Iterations =', niterations, '\n',
      '  Burn-in =', nburnin, '\n',
      '  Data size =', nrow(y), '\n',
      '  Started running on =', Sys.time(),  '\n \n \n',
      '**************************************
      ') %>% cat()
scales_beta <- glue("scales_beta{seq(1,n_bs-1,1)}")
params <- c("beta0", "beta", "alpha0", "alpha", 
            scales_beta,
            "mu.beta0", "tau.beta0", "mu.alpha0", "tau.alpha0") %>% # Z, psi
          as.character()

# Define the model file and the output file name
model_file <- glue("models/mod_1_vector_spscov_{sps_loop}.txt")
mod_name <- glue("data/ana_file/{date_out}_mod_{sps_name}_{park_name}.txt") %>% as.character()

# Read the content of the model file
mod_content <- readLines(model_file)

# Combine the content into a single string
mod_string <- paste(mod_content, collapse = "\n")

# Write the content to the output file
writeLines(mod_string, mod_name)

## initialize JAGS
cat("\n\n\n running first jags \n\n\n\n")

jags_model <- rjags::jags.model(
  file = model_file,
  data = jags.data,
  inits = inits, 
  n.chains = nchains,
  n.adapt = max(100, ceiling(.1 * niterations)),
  quiet = FALSE
)

cat("\n\n\n first done, running second \n\n\n\n") 

# burn-in
if (nburnin > 0) {
  message(paste("burn-in:", nburnin, "iterations"))
  rjags::jags.samples(
    jags_model,
    variable.names = params,
    n.iter = niterations,
    thin = nthin,
    quiet = FALSE
  )
}

# write_rds(jags_model,
#           file = glue("data/model_res/M12.rds"))

cat("\n\n\n second done, running third \n\n\n\n")

# posterior simulation
samples_jags <- coda.samples(
  jags_model,
  variable.names = params,
  n.iter = niterations,
  thin = nthin,
  quiet = FALSE
)

cat("\n\n\n third done!!! \n\n\n\n")
fil_nam <- sps_loop

file_name <- glue("{date_out}_{fil_nam}_{park_name}_{niterations}its_2min_spscov_")

file_name2 <- paste0(file_name, 'run',
                      length(list.files(path = file.path(getwd(),"data/model_res/"),
                                        pattern = file_name,
                                        full.names = FALSE)) + 1)

folder_path <- "data/model_res"

if (!file.exists(folder_path)) {
  dir.create(folder_path, recursive = TRUE)
}

write_rds(samples_jags,
          file = glue('data/model_res/{file_name2}.rds')
          )

system_time2 <- Sys.time()
if(as.numeric(system_time2 - system_time1) < 60) {
  time_it_took <- round(difftime(system_time2, system_time1, units = c("mins")),2)
  unit_time <- "mins"}
if(as.numeric(system_time2 - system_time1) >= 60 & 
      as.numeric(system_time2 - system_time1) <= 1440) {
  time_it_took <- round(difftime(system_time2, system_time1, units = c("hours")),2)
  unit_time <- "hours"}
if(as.numeric(system_time2 - system_time1) > 1440) {
  time_it_took <- round(difftime(system_time2, system_time1, units = c("days")),2)
  unit_time <- "days"}

# Get covariate names
covs_equal_to_1 <- cov_key2 == 1
# Get column names
covs_names <- colnames(cov_key2)[covs_equal_to_1]
covs_names2 <- paste(covs_names, collapse = "_")

# Print info in slurm.out file
paste('\n ************************************** \n \n \n ---------------- DONE ----------------', '\n\n',
      'Output File Name = ', glue('{file_name2}.rds'), '\n', 
      'Script = ', script_name, '\n', 
      'Parks =', park_name, '\n',
      'Species =', sps_name, '\n',
      'Covariates =', covs_names2, '\n',
      'Iterations =', niterations, '\n',
      'Run number =', str_split(file_name2, 'run', simplify = TRUE)[2], '\n',
      'Started running on =', system_time1, '\n',
      'Stopped running on =', system_time2, '\n',
      'Time it took =', time_it_took , unit_time,  '\n \n \n',
      '**************************************  \n') %>% 
      cat()


meta_name <- file(glue("data/ana_file/{date_out}_metadata_{sps_name}_{park_name}.txt"))
writeLines(paste(

                'Results File Name = ', glue('{file_name2}.rds'), '\n', 
                'Data File Name = ', glue("data/ana_file/{date_out}_data_{sps_name}_{park_name}.rds"), '\n', 
                'Script = ', script_name, '\n',
                'Model file =', glue("{mod_name}"), '\n',
                'Species =', sps_name, '\n',
                'Parks =', park_name, '\n',
                'Covariates =', covs_names2, '\n',
                'Iterations =', niterations, '\n',
                'Chains =', nchains, '\n',
                'Burn-in =', nburnin, '\n',
                'Thinning =', nthin, '\n',
                'Run number =', str_split(file_name2, 'run', simplify = TRUE)[2], '\n',
                'Started running on =', system_time1, '\n',
                'Stopped running on =', system_time2, '\n',
                'Time it took =', time_it_took , unit_time), 

          meta_name)

close(meta_name)


# code to check the data and initial values
# r <- 10   # what is the deal with 7 versus 10? (they both have the same values and 10 does not work)
# j <- 1
# t <- 1
# Zst %>% filter(parkey == r, site_n == j, year_s == t)
# y_dat6 %>% filter(parkey == r, site_n == j, year_s == t)
# Zst2[r,j,t]
# # row 73 has the values in the loop

# #################################################################################
# MCMCsummary(samples_jags,
#              #params = params[c(2,4,5,7)],
#              round = 2) 

# MCMCtrace(samples_jags,
#           params = params[c(10,8,2,4,5,7)],
#           ind = TRUE,
#           pdf = FALSE)

# par(mfrow = c(1,1))
# MCMCplot(samples_jags,
#          #params = params[c(2,4,5,7)],
#          ref_ovl = TRUE)

