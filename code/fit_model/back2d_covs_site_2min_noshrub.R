#? *********************************************************************************
#? ----------------------------  back2d_covs_scales_3  -----------------------------
#? *********************************************************************************
# Code to run model to estimate the effect of different environmental
#   covariates on bird occupancy in several national parks and on three
#   different spatial scales
#
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
script_name <- "back2d_covs_site_2min_noshrub.R"

cat("\n", "\n", "\n", 
    'Current script:', script_name, 
    "\n", "\n", "\n", "\n")
system_time1 <- Sys.time()
(date_out <- glue("{substr(system_time1, 1,4)}_{substr(system_time1, 6,7)}_{substr(system_time1, 9,10)}"))

# Import data -----------------------------------------
## file paths
YDAT_PATH <- "data/y_dat8.rds"
XDAT_PATH <- "data/X_1000.rds"
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

y_dat5$unique_index <- seq(1,nrow(y_dat5),1)

X10$unique_index <- seq(1,nrow(y_dat5),1)

if(setequal(y_dat5$unique_index, X10$unique_index) != "TRUE") stop("ah wrong indexing!!!! #82")

y_dat6 <- y_dat5 %>% 
  dplyr::filter(sps_it == sps_loop,
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
  left_join(., parkey_right, by = "Admin_Unit_Code")

y <- y_dat6 %>% 
  dplyr::select(bird_detec, parkey, site_n, year_n, interval_n, Year) 

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

# check by park

# park and site

# park and year

# park and site and year

# site, park, county and covs values

# site, interval, year and detec covs



#? get covariates ----------------------------------------------------------------
X <- X10 %>% 
  dplyr::select(Point_Name,
          siteDEN, siteBA,
          siteH_g, siteEh_g,
          siteBA_pole, siteBA_mature, siteBA_large,
          siteSHRUden,
          area,
          EventDate2, StartTime2) %>% 
  rename(date_jul = EventDate2,
          time_jul = StartTime2) %>% 
  mutate( siteBA_s = standardize(siteBA),
          siteDEN_s = standardize(siteDEN),
          siteH_g = standardize(siteH_g),
          siteEh_g = standardize(siteEh_g),
          siteBA_pole_s = standardize(siteBA_pole),
          siteBA_mature_s = standardize(siteBA_mature),
          siteBA_large_s = standardize(siteBA_large),
          siteSHRUden_s = standardize(siteSHRUden),
          area_s = standardize(area),
          date_jul_s = standardize(date_jul),
          time_jul_s = standardize(time_jul))

#! TODO: for now, im putting zeros in the occasions that have no environmental data (mean)
table(is.na(X))
X[is.na(X)] <- 0

# occupancy variables - separate them in covs in all scales per tibble
## tree basal area
X1 <- X %>% 
  dplyr::select(siteBA_s)

## tree density
X2 <- X %>% 
  dplyr::select(siteDEN_s)
  
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

# put everything together, arrange, and split!

y_all <- cbind(y, X1, X2, Xa, Xb, Xp) %>% 
  as_tibble() %>% 
  arrange(parkey, site_n, year_n, interval_n)  %>% 
  mutate(interval_2 = ifelse(interval_n %in% c(1,2), 1, 
                                ifelse(interval_n %in% c(3,4), 2, 
                                    ifelse(interval_n %in% c(5,6), 3, 
                                        ifelse(interval_n %in% c(7,8), 4, 
                                            5)))))

y_all2 <- y_all  %>% 
        group_by(parkey, site_n, year_n, interval_2) %>%
        mutate(bird_detec2 = ifelse(sum_na(bird_detec) > 0, 1, 0), 
               Year2 = mean(Year), 
               siteBA_s2 = mean(siteBA_s), 
               siteDEN_s2 = mean(siteDEN_s), 
               time_jul_s2 = mean(time_jul_s),
               date_jul_s2 = mean(date_jul_s),
               area_s2 = mean(Xp)) %>% 
        ungroup()

table(y_all2$Year == y_all2$Year2)
table(y_all2$siteBA_s == y_all2$siteBA_s2)
table(y_all2$siteDEN_s == y_all2$siteDEN_s2)
table(y_all2$date_jul_s2 == y_all2$date_jul_s)
table(y_all2$area_s2 == y_all2$Xp)

# FALSES
table(y_all2$time_jul_s2 == y_all2$time_jul_s)
table(y_all2$bird_detec2 == y_all2$bird_detec)

y_all3 <- y_all2 %>% 
                select(bird_detec2, parkey, site_n, year_n, Year2,
                       interval_2,
                       siteBA_s2,   
                       siteDEN_s2, 
                       time_jul_s2, date_jul_s2, area_s2) 
dim(y_all3)
dim(y_all2)

rm(list = c("y", "X1", "X2", "X3","Xa", "Xb", "Xp"))

y_all4 <- y_all3 %>% 
                rename(bird_detec = bird_detec2, 
                       Year = Year2,
                       interval_n = interval_2,
                       siteBA_s = siteBA_s2, 
                       siteDEN_s = siteDEN_s2, 
                       time_jul_s = time_jul_s2, 
                       date_jul_s = date_jul_s2, 
                       area_s = area_s2) %>% 
                distinct()

dim(y_all4)

nrow(y_all4) == 1/2*(nrow(y_all3))

X1 <- y_all4 %>% select(siteBA_s)
X2 <- y_all4 %>% select(siteDEN_s)

Xa <- y_all4 %>% select(time_jul_s)
Xb <- y_all4 %>% select(date_jul_s)
Xp <- y_all4 %>% select(area_s) %>% pull()

y <- y_all4 %>% select(bird_detec, parkey, site_n, year_n, 
                         interval_n, Year) # interval_n is now interval2

## trick for coding = only interval one
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
  pull()
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
dim(X1)
dim(X2)
nrow(y_all3)/2
length(Xp)
dim(Xa)
dim(Xb)

# number of alphas and betas
n_bs <- 3
n_as <- 3

if(length(sps_loop) > 1) { sps_name <- "commu"} else {sps_name <- sps_loop}
if(length((unique(y[,2]))) == 1) { park_name <- unique(y[,2])} else {park_name <- "parks"}

# model
str(jags.data <- list(y = y,
                      y2 = y2,
                      n_bs = n_bs,
                      n_as = n_as,
                      nrowy = nrow(y),
                      nrowy2 = nrow(y2),
                      X1 = X1,
                      X2 = X2,
                      Xp = Xp,
                      Xa = Xa,
                      Xb = Xb,
                      n_yrM = length((unique(y[,4]))),
                      n_pkM = length((unique(y[,2])))
                      #y_ind = y_ind
))
#! jags.data structure:
# y: detection matrix
# y2: first detection matrix
# n_bs: number of betas
# n_as: number of alphas
# nrowy: number of total rows (all detections)
# nrowy2: number of rows of first detections 
# X1: tree basal area
# X2: tree density
# X3: shrub density
# Xp: park size
# Xa: detection time
# Xb: detection day of the year
# n_yrM: number of years 
# n_pkM: number of parks 
write_rds(jags.data, file = glue("data/ana_file/{date_out}_data_{sps_name}_{park_name}_site_noshrub.rds"))

# source("code/check_data.R") 

inits <- function() {
    list(
        Z = Zst2,
        # mu_beta0 = rnorm(1, 0.5),
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

params <- c("beta0","beta", "alpha0", "alpha", 
            "mu.beta0", "tau.beta0", "mu.alpha0", "tau.alpha0") # Z, psi


# Define the model file and the output file name
model_file <- "models/mod_1_vector1spsparks_simple_3covs_a0s_site_noshrub.txt"
mod_name <- glue("data/ana_file/{date_out}_mod_{sps_name}_{park_name}_site_noshrub.txt") %>% as.character()

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
fil_nam <- sps_loop2

file_name <- glue("{date_out}_{fil_nam}_{park_name}_{niterations}its_2min_site_noshrub_")

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

# Print info in slurm.out file
paste('\n ************************************** \n \n \n ---------------- DONE ----------------', '\n\n',
      'Output File Name = ', glue('{file_name2}.rds'), '\n', 
      'Script = ', script_name, '\n', 
      'Parks =', park_name, '\n',
      'Species =', sps_name, '\n',
      'Iterations =', niterations, '\n',
      'Run number =', str_split(file_name2, 'run', simplify = TRUE)[2], '\n',
      'Started running on =', system_time1, '\n',
      'Stopped running on =', system_time2, '\n',
      'Time it took =', time_it_took , unit_time,  '\n \n \n',
      '**************************************  \n') %>% 
      cat()


meta_name <- file(glue("data/ana_file/{date_out}_metadata_{sps_name}_{park_name}.txt"))
writeLines(paste(

                ' Results File Name = ', glue('{file_name2}.rds'), '\n', 
                'Data File Name = ', glue("data/ana_file/{date_out}_data_{sps_name}_{park_name}.rds"), '\n', 
                'Script = ', script_name, '\n',
                'Model file =', glue("{mod_name}"), '\n',
                'Species =', sps_name, '\n',
                'Parks =', park_name, '\n',
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

