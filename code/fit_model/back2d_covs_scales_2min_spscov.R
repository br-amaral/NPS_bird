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
        dplyr::select(-unique_index) %>% 
        rename(date_jul = EventDate2,
               time_jul = StartTime2,
               Admin_Unit_Code = park) %>% 
        mutate(siteDEN_s =   standardize(treeden_ha_site),
               parkDEN_s =   standardize(treeden_ha_park),
               counDEN_s =   standardize(treeden_ha_coun),
               siteBAcon_s = standardize(BA_m2ha_perc_con_site), 
               parkBAcon_s = standardize(BA_m2ha_perc_con_park),
               counBAcon_s = standardize(BA_m2ha_perc_con_coun),
               siteBAlar_s = standardize(BA_m2ha_perc_large_site), 
               parkBAlar_s = standardize(BA_m2ha_perc_large_park),
               counBAlar_s = standardize(BA_m2ha_perc_large_coun),
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
               siteBAcon_mean = mean(BA_m2ha_perc_con_site, na.rm = T), 
               parkBAcon_mean = mean(BA_m2ha_perc_con_park, na.rm = T),
               counBAcon_mean = mean(BA_m2ha_perc_con_coun, na.rm = T),
               siteBAlar_mean = mean(BA_m2ha_perc_large_site, na.rm = T), 
               parkBAlar_mean = mean(BA_m2ha_perc_large_park, na.rm = T),
               counBAlar_mean = mean(BA_m2ha_perc_large_coun, na.rm = T),
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
               siteBAcon_sd = sd(BA_m2ha_perc_con_site, na.rm = T), 
               parkBAcon_sd = sd(BA_m2ha_perc_con_park, na.rm = T),
               counBAcon_sd = sd(BA_m2ha_perc_con_coun, na.rm = T),
               siteBAlar_sd = sd(BA_m2ha_perc_large_site, na.rm = T), 
               parkBAlar_sd = sd(BA_m2ha_perc_large_park, na.rm = T),
               counBAlar_sd = sd(BA_m2ha_perc_large_coun, na.rm = T),
               siteSHR_sd =   sd(shrub_avg_cov_site, na.rm = T),
               parkSHR_sd =   sd(shrub_avg_cov_park, na.rm = T),
               counSHR_sd =   sd(shrub_cov_coun, na.rm = T),
               siteBA_sd =    sd(BA_m2ha_site, na.rm = T),
               parkBA_sd =    sd(BA_m2ha_park, na.rm = T), 
               counBA_sd =    sd(BA_m2ha_coun, na.rm = T),
               area_sd =      sd(area, na.rm = T),
               date_jul_sd =  sd(date_jul, na.rm = T),
               time_jul_sd =  sd(time_jul)) 
               
X_sites <- X_unstd %>% 
              select(Admin_Unit_Code, Point_Name, 
                      ends_with("_s"))  %>% 
              distinct()

X_vals <- X_unstd  %>% 
        select(ends_with("_mean"), ends_with("_sd"))  %>% 
        distinct()

write_rds(X_sites, file = glue("data/out/X_sites_{sps_loop}.rds"))

write_rds(X_vals, file = glue("data/out/X_vals_{sps_loop}.rds"))

# Summary table for unique Point_Names have NAs for site variables
# the ones with no shrub are expected, since that data is sparse
# the one with no data for the 5 covariates are the ones with no neighbors
s_columns <- names(X)[grepl("^site", names(X))] %>% sort()
unique_na_summary <- X %>% 
  select(Point_Name, all_of(s_columns)) %>% 
  group_by(Point_Name) %>% 
  summarise(
    across(all_of(s_columns), ~any(is.na(.x))),
    .groups = 'drop'
  ) %>% 
  filter(if_any(all_of(s_columns), ~.x == TRUE)) %>% 
  arrange(Point_Name) %>% 
  pivot_longer(cols = all_of(s_columns), 
               names_to = "column", 
               values_to = "has_na") %>% 
  filter(has_na == TRUE) %>% 
  select(-has_na) %>% 
  add_count(Point_Name, name = "frequency") %>%  # Add frequency column
  arrange(desc(frequency), Point_Name) 
unique_na_summary %>% print(n = nrow(unique_na_summary))

rem_points <- unique_na_summary$Point_Name %>% unique()

cols_y <- colnames(y_dat6)
cols_x <- colnames(X)

X_y <- y_dat6 %>% 
          left_join(., X, by = c("Point_Name", "Admin_Unit_Code", "site_n", "Year", "interval_n", "date_jul", "time_jul"))

## remove rows without forest data
X_y2 <- X_y %>%
          filter(Point_Name %!in% rem_points)
nrow(X_y)/10
nrow(X_y2)/10

y <- X_y2 %>% 
  select(all_of(cols_y))

X <-  X_y2 %>% 
  select(all_of(cols_x))

nrow(X) ; nrow(y)
# occupancy variables - separate them in covs in all scales per tibble
## tree density
X1 <- X %>% 
  dplyr::select(siteDEN_s, parkDEN_s, counDEN_s)

## conifer basal area
X2 <- X %>% 
  dplyr::select(siteBAcon_s, parkBAcon_s, counBAcon_s)

## large tree basal area percentage  
X3 <- X %>% 
  dplyr::select(siteBAlar_s, parkBAlar_s, counBAlar_s)

## shrub cover
X4 <- X %>% 
  dplyr::select(siteSHR_s, parkSHR_s, counSHR_s)

## total basal area
X5 <- X %>% 
  dplyr::select(siteBA_s, parkBA_s, counBA_s)

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

y_all <- cbind(y, X1, X2, X3, X4, X5, Xa, Xb, Xp) %>% 
  as_tibble() %>% 
  rename(area_s = Xp) %>% 
  arrange(parkey, site_n, year_n, interval_n)  %>% 
# and GROUPING THE INTERVALS IN FIVES  
  mutate(interval_2 = ifelse(interval_n %in% c(1,2), 1, 
                                ifelse(interval_n %in% c(3,4), 2, 
                                    ifelse(interval_n %in% c(5,6), 3, 
                                        ifelse(interval_n %in% c(7,8), 4, 
                                            5)))))
# group the 10 intervals on fives
y_all2 <- y_all  %>% 
        group_by(parkey, site_n, year_n, interval_2) %>%
        mutate(bird_detec2 = ifelse(sum_na(bird_detec) > 0, 1, 0),
               time_jul_s2 = mean(time_jul_s)) %>% 
        ungroup()

# FALSES
table(y_all2$time_jul_s2 == y_all2$time_jul_s)
table(y_all2$bird_detec2 == y_all2$bird_detec)
sum(y$bird_detec, na.rm = T)
sum(y_all$bird_detec, na.rm = T)
sum(y_all2$bird_detec, na.rm = T)

y_all3 <- y_all2 %>% 
                select(bird_detec2, parkey, site_n, year_n, Year, interval_2,
                       time_jul_s2, date_jul_s, area_s,
                       siteDEN_s, parkDEN_s, counDEN_s,
                       siteBAcon_s, parkBAcon_s, counBAcon_s,
                       siteBAlar_s, parkBAlar_s, counBAlar_s,
                       siteSHR_s, parkSHR_s, counSHR_s,
                       siteBA_s, parkBA_s, counBA_s) %>% 
                rename(bird_detec      = bird_detec2, 
                       interval_n      = interval_2,
                       time_jul_s      = time_jul_s2) %>% 
                distinct()

#! getting HALF of the rows because now I have 5 removal sampling intervals, not 10 
nrow(y_all2) == 2*(nrow(y_all3))

# detection variables
Xa <- y_all3 %>% 
  dplyr::select(time_jul_s)

Xb <- y_all3 %>% 
  dplyr::select(date_jul_s)

y <- y_all3 %>% select(bird_detec, parkey, site_n, year_n, 
                         interval_n, Year) # interval_n is now interval2

## trick for coding = only interval one for starting values
y2 <- y %>% 
  dplyr::filter(interval_n == 1)

# occupancy variables - separate them in covs in all scales per tibble
y3 <- y_all3 %>% 
  dplyr::filter(interval_n == 1)

## tree density
X1 <- y3 %>% 
  dplyr::select(siteDEN_s, parkDEN_s, counDEN_s)

## conifer basal area
X2 <- y3 %>% 
  dplyr::select(siteBAcon_s, parkBAcon_s, counBAcon_s)

## large tree basal area percentage  
X3 <- y3 %>% 
  dplyr::select(siteBAlar_s, parkBAlar_s, counBAlar_s)

## shrub cover
X4 <- y3 %>% 
  dplyr::select(siteSHR_s, parkSHR_s, counSHR_s)

## total basal area
X5 <- y3 %>% 
  dplyr::select(siteBA_s, parkBA_s, counBA_s)

## park size
Xp <- y3 %>% 
  dplyr::select(area_s) %>% 
  pull() %>% 
  as.numeric()

#colnames(y) <- c("bird_detec", "parkey", "sitekey", "yearkey", "intervalkey",# "year_site",
#                  "Year")

# initial values
Zst <- y_all3 %>% 
  dplyr::select(bird_detec, parkey, site_n, year_n, interval_n) %>% 
  group_by(parkey, site_n, year_n) %>% 
  mutate(z = ifelse(sum(bird_detec, na.rm = T) == 0, 0, 1)) %>% 
  ungroup() %>% 
  dplyr::filter(interval_n == 1) 

 nsite_pk_filt <- y_all3$site_n  # rem_pks %>% 
#                     filter(pk %in% pull(parkey_right %>% select(Admin_Unit_Code))) %>% 
#                     pull(nsite_pk) %>% 
#                     as.numeric()

site_vec <- seq(1,max(nsite_pk_filt),1)
(npk <- length(unique(y3$parkey)))
(pk <- sort(unique(y3$parkey)))
years <- y3 %>% 
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
                max(nsite_pk_filt),
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

# save initial values for post hoc analysis
z <- list(Zst = Zst,
          Zst2 = Zst2)
          
if(test == FALSE){
  write_rds(z, file = glue("data/ana_file/{sps_loop}_step{step_numb}_Z_{date_step1}.rds"))
}

y <- data.matrix(y)
y2 <- data.matrix(y2)
y_ind <- sort(rep(seq(1, nrow(y2),1),ninterval))

nrow(y)
nrow(y2)*ninterval
dim(Xa)
dim(Xb)

nrow(y2)
length(Xp)
dim(X1)
# number of alphas and betas
n_bs <- 6
n_beta_int <- n_bs - 1
n_as <- 3
if(length(sps_loop) > 1) { sps_loop <- "commu"} else {sps_loop <- sps_loop}
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

# model
jags_data <- list(
    y = y,
    y2 = y2,
    n_bs = n_bs,
    n_beta_int = n_beta_int,
    n_as = n_as,
    nrowy = nrow(y),
    nrowy2 = nrow(y2),
    Xp = Xp,
    Xa = as.matrix(Xa),
    Xb = as.matrix(Xb),
    X1 = as.matrix(X1),
    X2 = as.matrix(X2),
    X3 = as.matrix(X3),
    X4 = as.matrix(X4),
    X5 = as.matrix(X5),
    n_yrM = length(unique(y[, 4])),
    n_pkM = length(unique(y[, 2]))
)

# Print the structure of jags_data to verify
str(jags_data)

non_numeric_elements <- sapply(jags_data, function(x) !is.numeric(x))
non_numeric_elements <- names(jags_data)[non_numeric_elements]
print(non_numeric_elements)

#! jags_data structure:
# y: detection matrix
# y2: first detection matrix
# n_bs: number of betas
# n_as: number of alphas
# nrowy: number of total rows (all detections)
# nrowy2: number of rows of first detections 
# X1-5: all environmental covs in the three scales
# Xp: park size
# Xa: detection time
# Xb: detection day of the year
# n_yrM: number of years 
# n_pkM: number of parks 


if(test == FALSE){
  write_rds(jags_data, file = glue("data/ana_file/{sps_loop}_step{step_numb}_jagsdata_{date_step1}.rds"))
}

# Define the model file and the output file name (on run_step1_step2.R)
if(model_file == "models/mod_all_covs2.txt") {
  mod_name <- glue("data/ana_file/{sps_loop}_step{step_numb}_model_int_{date_step1}.txt") %>% as.character()} else {
  mod_name <- glue("data/ana_file/{sps_loop}_step{step_numb}_model_{date_step1}.txt") %>% as.character()}

# source("code/check_data.R") 
if(model_file == "models/mod_all_covs2.txt") {
  inits <- function() {
      list(
          Z = Zst2,
          beta_int = rnorm(n_beta_int, 0.5),
          beta = rnorm(n_bs, 0.5),
          mu.alpha0 = rnorm(1, 0.5),
          alpha = rnorm(n_as, 0.5)
      )
  } } else {
      inits <- function() {
      list(
          Z = Zst2,
          # mu_beta0 = rnorm(1, 0.5), # check this!!!!!
          beta = rnorm(n_bs, 0.5),
          mu.alpha0 = rnorm(1, 0.5),
          alpha = rnorm(n_as, 0.5)
      )
  } }

if(test == TRUE){
  nchains <- 1
  niterations <- 6
  nburnin <- 1
  nthin <- 1
  nadapt_min <- 1
  print("test with 5 iterations")
}

paste('\n ************************************* \n \n \n   Running JAGS for:', '\n',
      '  Parks =', park_name, '\n',
      '  Species =', sps_loop, '\n',
      '  Iterations =', niterations, '\n',
      '  Burn-in =', nburnin, '\n',
      '  Data size =', nrow(y), '\n',
      '  Started running on =', Sys.time(),  '\n \n \n',
      '**************************************
      ') %>% cat()

if(model_file == "models/mod_all_covs2.txt") {
    scales_beta <- glue("scales_beta{seq(1,n_bs-1,1)}")

    params <- c("beta0", "beta", "beta_int", "alpha0", "alpha", 
                scales_beta,
                "mu.beta0", "tau.beta0", "mu.alpha0", "tau.alpha0") %>% # Z, psi
              as.character()
  } else {
    scales_beta <- glue("scales_beta{seq(1,n_bs-1,1)}")
    params <- c("beta0", "beta", "alpha0", "alpha", 
                scales_beta,
                "mu.beta0", "tau.beta0", "mu.alpha0", "tau.alpha0") %>% # Z, psi
              as.character()}

# Read the content of the model file
mod_content <- readLines(model_file)

# Combine the content into a single string
mod_string <- paste(mod_content, collapse = "\n")

# Write the content to the output file
if(test == FALSE){writeLines(mod_string, mod_name)}

## initialize JAGS

cat("\n\n\n running first jags \n\n\n\n")

jags_model <- rjags::jags.model(
  file = model_file,
  data = jags_data,
  inits = inits, 
  n.chains = nchains,
  n.adapt = max(nadapt_min, ceiling(.1 * niterations)),
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
    quiet = FALSE,
    parallel = TRUE,
    n.cores = nchains
  )
}

cat("\n\n\n second done, running third \n\n\n\n")

# posterior simulation
samples_jags <- coda.samples(
  jags_model,
  variable.names = params,
  n.iter = niterations,
  thin = nthin,
  quiet = FALSE,
  parallel = TRUE,
  n.cores = nchains
)

cat("\n\n\n third done!!! \n\n\n\n")
file_name <- glue("{sps_loop}_step{step_numb}_output_{date_step1}")

file_name2 <- paste0(file_name, 'run',
                      length(list.files(path = file.path(getwd(),"data/model_res/"),
                                        pattern = file_name,
                                        full.names = FALSE)) + 1)

if(model_file == "models/mod_all_covs2.txt") {
  file_name2 <- paste0(file_name, 'run',
                      length(list.files(path = file.path(getwd(),"data/model_res/"),
                                        pattern = glue("{file_name}_int"),
                                        full.names = FALSE)) + 1)
                                        }

folder_path <- "data/model_res"

if (!file.exists(folder_path)) {
  dir.create(folder_path, recursive = TRUE)
}

if(test == FALSE) {
  write_rds(samples_jags,
            file = glue('data/model_res/{file_name2}.rds')
            )
}

if(niterations > 10000) {
  write_rds(samples_jags,
            file = glue('data/model_res/{file_name2}.rds')
            )
}

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


# Print info in slurm.out file
paste('\n ************************************** \n \n \n ---------------- DONE ----------------', '\n\n',
      'Output File Name = ', glue('{file_name2}.rds'), '\n', 
      'Script = ', script_name, '\n', 
      'Parks =', park_name, '\n',
      'Species =', sps_loop, '\n',
      'Iterations =', niterations, '\n',
      'Run number =', str_split(file_name2, 'run', simplify = TRUE)[2], '\n',
      'Started running on =', system_time1, '\n',
      'Stopped running on =', system_time2, '\n',
      'Time it took =', time_it_took , unit_time,  '\n \n \n',
      '**************************************  \n') %>% 
      cat()


meta_name <- file(glue("data/ana_file/{sps_loop}_step{step_numb}_metadata_{date_step1}.txt"))
if(test == FALSE){
    writeLines(paste(

                  'Species =', sps_loop, '\n',
                  'Step =', step_numb, '\n',
                  'Date =', date_step1, '\n',

                  'Metadata File Name =', meta_name, '\n', 
                  'Results File Name =', glue('{file_name2}.rds'), '\n', 
                  'Model File Name =', glue("{mod_name}"), '\n',
                  'Data File Name =', glue("data/ana_file/{date_step1}_data_{sps_loop}_{park_name}.rds"), '\n', 
                  'Z File Name =', glue("data/ana_file/{date_step1}_data_{sps_loop}_Z.rds"), '\n', 

                  'Script =', script_name, '\n',
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
}

if(test == TRUE){
  cat(glue(" \n \n \n Test for {sps_loop} and step {step_numb} done!  \n \n \n"))
}

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

