# *********************************************************************************
# ----------------------------  back2d_covs_scales_3  -----------------------------
# *********************************************************************************
# Code to run model to estimate the effect of different environmental
#   covariates on bird occupancy in several national parks and on three
#   different spatial scales
#
# Input ----------------------------------------------
#           - data/y_dat8.rds: tibble with bird data
#           - data/X10.rds: tibble with covariate data
#           - data/out/nsite_pk.rds: vector with number of sites in each park
#           - data/src/key_park.rds: vector of all parks being analyzed
#
# Output ---------------------------------------------
#           - data/model_res/jags_res_{sps}_{park}_run{run_number}.rds: file with result of jags model

# detach packages and clear workspace
# if(!require(freshr)){install.packages("freshr")}
# freshr::freshr()

script_name <- 'back2d_covs_scales_3.R'

cat(paste('\n ************************************** \n \n \n Running scrip', script_name, '\n \n \n',
      '**************************************
      '))

system_time1 <- Sys.time()

# Load packages --------------------------------------
library(conflicted)
library(tidyverse)
library(glue)
library(jagsUI)
library(tidyverse)
library(rjags)
library(MCMCvis)
library(AHMbook)

conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::filter)
# conflicts_prefer(scales::alpha)

#if("sps_loop" %in% ls() == FALSE){stop("No species selected #38")}

# Make functions --------------------------------------
colanmes <- colnames
lenght <- length
`%!in%` <- Negate(`%in%`)

# Import data -----------------------------------------
## file paths
YDAT_PATH <- "data/y_dat8.rds"
XDAT_PATH <- "data/X.rds"
SITE_PK_PATH <- "data/out/nsite_pk.rds"
PARK_PATH <- "data/src/key_park.rds"

## read files
y_dat4 <- read_rds(file = YDAT_PATH)
X10 <- read_rds(file = XDAT_PATH)
nsite_pk <- read_rds(SITE_PK_PATH)
pk <- read_rds(PARK_PATH) %>%
  select(parks) %>%
  pull() %>%
  sort()

pk <- pk[-1]
pk <- pk[-7]

nsite_pk <- nsite_pk[-1]
nsite_pk <- nsite_pk[-7]

# Filter for species and park ---------------------------------------
## 1 sps several parks
y_dat5 <- y_dat4
# _n means that the 1 is the first occasion for that sps, year, loc, etc, not the first calendar one

y_dat5 <- y_dat5 %>%
  mutate(parkey = as.numeric(parkey),
          sps_it = AOU_Code)

nrow(X10) == nrow(y_dat5)

y_dat5$unique_index <- seq(1,nrow(y_dat5),1)

X10$unique_index <- seq(1,nrow(y_dat5),1)

if(setequal(y_dat5$unique_index, X10$unique_index) != "TRUE") stop("ah wrong indexing!!!! #82")

y_dat6 <- y_dat5 %>% 
  filter(sps_it == sps_loop,
          park %in% pk
  )

if(length(sps_loop) == 1){
  print(glue("analazing one species {sps_loop}"))
  } else {
  print('analazing a community: {sps_loop}')
}

X10 <- X10 %>% 
   filter(unique_index %in% y_dat6$unique_index)

nrow(X10) == nrow(y_dat6)
glu1 <- paste(shQuote(sort(unique(y_dat6$sps_it))), collapse=", ")
spsglue <- glue("the species are {glu1}, and parks are")
parkglue <- paste(shQuote(sort(unique(y_dat6$park))), collapse=", ")
print(paste(spsglue,parkglue))

## add a step here to fix parkey
parkey_right <- y_dat6 %>% 
  select(Admin_Unit_Code, parkey) %>% 
  arrange(parkey) %>% 
  distinct() %>% 
  mutate(parkey = seq(1, nrow(.)))

y_dat6 <- y_dat6 %>% 
  select(-parkey) %>% 
  left_join(., parkey_right, by = "Admin_Unit_Code")

y <- y_dat6 %>% 
  select(bird_detec, parkey, site_n, year_n, interval_n, Year) %>% 
  arrange(parkey, site_n, year_n, interval_n)

##
# colnames(y)

## trick for coding = only interval one
y2 <- y %>% 
  filter(interval_n == 1)

X <- X10 %>% 
  select(Point_Name,
          siteDEN, siteBA,
          siteH_g, siteEh_g,
          siteBA_pole, siteBA_mature, siteBA_large,
          siteSHRUden,
          parkDEN, parkBA, 
          parkH_g, parkEh_g,
          parkBA_pole, parkBA_mature, parkBA_large,   
          parkSHRUden, 
          counDEN, counBA, 
          counH_g, counEh_g, ## https://rdrr.io/cran/rFIA/man/diversity.html
          counPER_pole, counPER_matu, counPER_late,
          counSHRUden,
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
          parkDEN_s = standardize(parkDEN),
          parkBA_s = standardize(parkBA),
          parkH_g = standardize(parkH_g), 
          parkEh_g = standardize(parkEh_g),
          parkBA_pole_s = standardize(parkBA_pole),
          parkBA_mature_s = standardize(parkBA_mature),
          parkBA_large_s = standardize(parkBA_large),
          parkSHRUden_s = standardize(parkSHRUden),
          counDEN_s = standardize(counDEN),
          counBA_s = standardize(counBA),
          counH_g = standardize(counH_g),
          counEh_g = standardize(counEh_g),
          counPER_pole_s = standardize(counPER_pole),
          counPER_matu_s = standardize(counPER_matu),
          counPER_late_s = standardize(counPER_late),
          counSHRUper_s = standardize(counSHRUden),
          area_s = standardize(area),
          date_jul_s = standardize(date_jul),
          time_jul_s = standardize(time_jul))

#! TODO: for now, im putting zeros in the occasions that have no environmental data (mean)
X[is.na(X)] <- 0

# occupancy variables - separate them in covs in all scales per tibble
## tree basal area
X1 <- X %>% 
  select(siteBA_s, parkBA_s, counBA_s)

## tree density
X2 <- X %>% 
  select(siteDEN_s, parkDEN_s, counDEN_s)

## Forets diversity
X3 <- X %>% 
  select(siteH_g, siteEh_g,
          parkH_g, parkEh_g,
          counH_g, counEh_g)

## Shrub density and percentage
X4 <- X %>% 
  select(siteSHRUden_s, parkSHRUden_s, counSHRUper_s)

## Basal area large
X5l <- X %>% 
  select(siteBA_large_s, parkBA_large_s, counPER_late_s)

## Basal area mature
X5m <- X %>% 
  select(siteBA_mature_s, parkBA_mature_s, counPER_matu_s)

## Basal area pole
X5p <- X %>% 
  select(siteBA_pole_s, parkBA_pole_s, counPER_pole_s)

if(for_stage == "late") {
  X5 <- X5l
  } else {
    if(for_stage == "mature") {
      X5 <- X5m
      } else {
        if(for_stage == "mature") { X5 <- X5p} else {stop("wrong stage row #220")}}}

## park size
Xp <- X %>% 
  select(area_s) %>% 
  pull() %>% 
  as.numeric()

# detection variables
Xa <- X %>% 
  select(time_jul)

Xb <- X %>% 
  select(date_jul)

# initial values
Zst <- y %>% 
  select(bird_detec, parkey, site_n, year_n, interval_n) %>% 
  group_by(parkey, site_n, year_n) %>% 
  mutate(z = ifelse(sum(bird_detec, na.rm = T) == 0, 0, 1)) %>% 
  ungroup() %>% 
  filter(interval_n == 1) 

site_vec <- seq(1,max(nsite_pk),1)
(npk <- length(unique(y$parkey)))
(pk <- sort(unique(y$parkey)))
years <- y %>% 
  select(Year) %>% 
  distinct() %>% 
  arrange() %>% 
  pull()
ninterval <- 10

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

colnames(y) <- c("bird_detec", "parkey", "sitekey", "yearkey", "intervalkey",#"year_site",
"Year")

y <- data.matrix(y)
y2 <- data.matrix(y2)
y_ind <- sort(rep(seq(1, nrow(y2),1),10))
nrow(y_dat6)
nrow(y)
nrow(y2)*ninterval
nrow(X)
dim(X1)
dim(X2)
dim(X3)
dim(X4)
dim(X5)
length(Xp)
dim(Xa)
dim(Xb)

# number of alphas and betas
n_bs <- 6
n_as <- 3

# model
str(jags.data <- list(y = y,
                      y2 = y2,
                      n_bs = n_bs,
                      n_as = n_as,
                      nrowy = nrow(y),
                      nrowy2 = nrow(y2),
                      X1 = X1,
                      X2 = X2,
                      X3 = X3,
                      X4 = X4,
                      X5 = X5,
                      Xp = Xp,
                      Xa = Xa,
                      Xb = Xb,
                      n_yrM = length((unique(y[,4]))),
                      n_pkM = length((unique(y[,2])))
                      #y_ind = y_ind
))
inits <- function()list(Z = Zst2
#, beta0 = rnorm(10,0.6), beta1 = rnorm(10,0.6)
)

niterations <- 3
burnin <- 1
nchains <- 1

if(length(sps_loop) > 1) { sps_name <- "commu"} else {sps_name <- sps_loop}
if(length((unique(y[,2]))) == 1) { park_name <- unique(y[,2])} else {park_name <- "parks"}

paste('\n ************************************* \n \n \n Running JAGS for:', '\n',
      '  Parks =', park_name, '\n',
      '  Species =', sps_name, '\n',
      '  Iterations =', niterations, '\n',
      '  Data size =', nrow(y), '\n',
      '  Started running on =', Sys.time(),  '\n \n \n',
      '**************************************
      ') %>% cat()

cat("\n\n\n running first jags \n\n\n\n")
params <- c("beta0","beta", "alpha0", "alpha", 
            "scales_beta1", "scales_beta2", "scales_beta3", "scales_beta4", "scales_beta5",
            "mu.beta0", "tau.beta0", "mu.alpha0", "tau.alpha0") # Z, psi

if(yearbo == "yes") { model_file <- "models/mod_1_vector1spsparks_simple_MOREcovs_scales.txt"}
if(yearbo == "no") { model_file <- "models/mod_1_vector1spsparks_simple_MOREcovs_scalesnoyear.txt"}

## initialize JAGS
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
if (burnin > 0) {
  message(paste("burn-in:", burnin, "iterations"))
  rjags::jags.samples(
    jags_model,
    variable.names = params,
    n.iter = niterations,
    thin = 5
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
  thin = 5
)

cat("\n\n\n third done!!! \n\n\n\n")
fil_nam <- master_tab  %>% 
  filter(sps_list == sps_loop) %>% 
  pull(res_name)  

file_name <- glue("jags_res_{fil_nam}_{park_name}_")

file_name2 <- paste0(file_name, 'run',
                      length(list.files(path = file.path(getwd(),"data/model_res/"),
                                        pattern = file_name,
                                        full.names = FALSE)) + 1)

write_rds(samples_jags,
          file = glue('data/model_res/{file_name2}.rds')
          #file = glue("data/model_res/jags_res_{sps_loop}2dnoA1bo.rds")
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




# code to check the data and initial values
# r <- 10   # what is the deal with 7 versus 10? (they both have the same values and 10 does not work)
# j <- 1
# t <- 1
# Zst %>% filter(parkey == r, site_n == j, year_s == t)
# y_dat6 %>% filter(parkey == r, site_n == j, year_s == t)
# Zst2[r,j,t]
# # row 73 has the values in the loop

#################################################################################
MCMCsummary(samples_jags,
             #params = params[c(2,4,5,7)],
             round = 2) 

MCMCtrace(samples_jags,
          params = params[c(10,8,2,4,5,7)],
          ind = TRUE,
          pdf = FALSE)

par(mfrow = c(1,1))
MCMCplot(samples_jags,
         #params = params[c(2,4,5,7)],
         ref_ovl = TRUE)


