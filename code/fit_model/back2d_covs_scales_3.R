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
if(!require(freshr)){install.packages("freshr")}
freshr::freshr()

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
XDAT_PATH <- "data/X10.rds"
SITE_PK_PATH <- "data/out/nsite_pk.rds"
PARK_PATH <- "data/src/key_park.rds"

sps_list <- sps_loop <- "RBWO"
yearbo <- "no"

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

if(setequal(y_dat5$unique_index, X10$unique_index) != "TRUE") 
   stop("ah wrong indexing!!!! #82")

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
  select(siteBA_s, siteDEN_s, 
         parkBA_s, parkDEN_s, 
         counBA_s, counDEN_s, 
         area_s, 
         date_jul,time_jul)

table(is.na(X$siteBA_s))
table(is.na(X$siteDEN_s))
table(is.na(X$parkBA_s))
table(is.na(X$parkDEN_s))
table(is.na(X$counBA_s))
table(is.na(X$counDEN_s))
table(is.na(X$date_jul))
table(is.na(X$time_jul))

# occupancy variables
## tree basal area
X1 <- X %>% 
  select(siteBA_s, parkBA_s, counBA_s)

## tree density
X2 <- X %>% 
  select(siteDEN_s, parkDEN_s, counDEN_s)

# detection variables
Xa <- X %>% 
  select(date_jul, time_jul)

# initial values
Zst <- y %>% 
  select(bird_detec, parkey, site_n, year_n, interval_n) %>% 
  group_by(parkey, site_n, year_n) %>% 
  mutate(z = ifelse(sum(bird_detec, na.rm = T) == 0, 0, 1)) %>% 
  ungroup() %>% 
  filter(interval_n == 1) 

site_vec <- seq(1,max(nsite_pk),1)
(npk <- length(unique(y_dat6$parkey)))
(pk <- sort(unique(y_dat6$parkey)))
years <- y_dat6 %>% 
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

colnames(y) <- c("bird_detec", "parkey", "sitekey", "yearkey", "intervalkey","year_site","Year")

y <- data.matrix(y)
y2 <- data.matrix(y2)
y_ind <- sort(rep(seq(1, nrow(y2),1),10))
nrow(y_dat6)
nrow(y)
nrow(y2)*ninterval
nrow(X)

# tree basal area - data frame with 3 columns for each of the three scales
X1 <- X %>% 
  select(siteBA_s, parkBA_s, counBA_s) %>% 
  mutate(siteBA_s = as.numeric(siteBA_s), 
         parkBA_s = as.numeric(parkBA_s), 
         counBA_s = as.numeric(counBA_s))

# tree density - data frame with 3 columns for each of the three scales
X2 <- X %>% 
  select(siteDEN_s, parkDEN_s, counDEN_s) %>% 
  mutate(siteDEN_s = as.numeric(siteDEN_s), 
         parkDEN_s = as.numeric(parkDEN_s), 
         counDEN_s = as.numeric(counDEN_s))

# park size - vector
X3 <- X %>% 
  select(area_s) %>% 
  pull() %>% 
  as.numeric()

Xa <- X %>% 
  select(date_jul) %>% 
  pull() %>% 
  as.numeric()

Xb <- X %>% 
  select(time_jul) %>% 
  pull() %>% 
  as.numeric()

nrow(X)
dim(X1)
dim(X2)
length(X3)

# number of alphas and betas
n_bs <- 3
n_as <- 3

#! TODO: for now, im putting zeros in the occasions that have no environmental data
 for(i in 1:nrow(X1)) {
    for(j in 1:3) {
      if(is.na(X1[i,j])) {
        X1[i,j] <- 0

      if(is.na(X2[i,j])) {
        X2[i,j] <- 0
      }
    }
       
    if(is.na(X3[i])) {
      X3[i] <- 0
    }
  }
 }

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
                      Xa = Xa,
                      Xb = Xb,
                      n_yrM = length((unique(y[,4]))),
                      n_pkM = length((unique(y[,2])))
                      #y_ind = y_ind
))

inits <- function()list(Z = Zst2
#, beta0 = rnorm(10,0.6), beta1 = rnorm(10,0.6)
)

niterations <- 20000
burnin <- 10000
nchains <- 5

if(length(sps_loop) > 1) { sps_name <- "commu"} else {sps_name <- sps_loop}
if(length((unique(y[,2]))) == 1) { park_name <- unique(y[,2])} else {park_name <- "parks"}

paste('\n ************************************* \n \n \n Running JAGS for:', '\n',
      'Parks =', park_name, '\n',
      'Species =', sps_name, '\n',
      'Iterations =', niterations, '\n',
      'Data size =', nrow(y), '\n',
      'Started running on =', Sys.time(),  '\n \n \n',
      '**************************************
      ') %>% cat()

cat("\n\n\n running first jags \n\n\n\n")
params <- c("beta0","beta", "alpha0", "alpha", "scales_beta1", "scales_beta2",
            "mu.beta0", "tau.beta0", "mu.alpha0", "tau.alpha0") # Z, psi

if(yearbo == "yes") { model_file <- "models/mod_1_vector1spsparks_simple_covs_scales.txt"}
if(yearbo == "no") { model_file <- "models/mod_1_vector1spsparks_simple_covs_scalesnoyear.txt"}

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

file_name <- glue("jags_res_{sps_name}_{park_name}_nei_")

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


