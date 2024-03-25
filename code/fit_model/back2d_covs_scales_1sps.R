# *********************************************************************************
# -------------------------------   Amazing Title   -------------------------------
# *********************************************************************************
# Code to ...
#
#
# Input ----------------------------------------------
#           - data/y_dat4.rds: tibble with bird data
#           - data/X10.rds: tibble with covariate data
#           - data/out/nsite_pk.rds: vector with number of sites in each park
#           - data/src/key_park.rds: vector of all parks being analyzed
#           - :
#
# Output ---------------------------------------------
#           - :
#           - :


# detach packages and clear workspace
if(!require(freshr)){install.packages("freshr")}
freshr::freshr()

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

# Make functions --------------------------------------
colanmes <- colnames
lenght <- length
`%!in%` <- Negate(`%in%`)

# Import data -----------------------------------------
## file paths
YDAT_PATH <- "data/y_dat6.rds"
XDAT_PATH <- "data/X10.rds"
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

# load("data/datJDnov2023.RData")

# Filter for species and park ---------------------------------------
sps_UNfilt_len <- length(sort(unique(y_dat4$sps_it)))
sps_filt_list <- c("GCFL", "AMGO", "DOWO", "NOCA", "SCTA", "SOSP", "GRCA", "RBWO", "COYE", "WOTH", "RWBL",
                   "WBNU", "BTNW", "EAWP", "BCCH", "BLJA", "TUTI", "AMRO", "REVI", "OVEN", "BTBW", "YBSA", 
                   "BOBO", "YRWA", "PIWA", "CEDW", "CHSP", "NOFL", "HAWO", "BRCR", "RBGR", "DEJU", "AMCR", 
                   "BAOR", "RBNU", "BHVI", "GCKI", "EATO", "FISP", "HETH", "VEER", "MODO", "BLBW")

y_dat4$unique_index <- seq(1,nrow(y_dat4),1)

X10$unique_index <- seq(1,nrow(y_dat4),1)

if(setequal(y_dat4$unique_index, X10$unique_index) != "TRUE") 
  stop("ah wrong indexing!!!!")

if(exists("sps_filt_list")) {
  y_dat4 <- y_dat4 %>% 
    filter(sps_it %in% sps_filt_list)
  
  X10 <- X10 %>% 
    filter(unique_index %in% y_dat4$unique_index)
  
}

nrow(X10) == nrow(y_dat4)

## What am I analyzing?
glue1 <- glue("the species are ")
glue2 <- glue(", and parks are")
if(sps_UNfilt_len == lenght(sps_filt_list)) {spsglue <- "All species"
} else {
  spsglue <- paste(shQuote(sort(unique(y_dat4$sps_it))), collapse=", ")
  spsglue <- str_replace_all(spsglue, "'", "")
  } 
parkglue <- paste(shQuote(sort(unique(y_dat4$park))), collapse=", ")
parkglue <- str_replace_all(parkglue, "'", "")
paste(glue1, spsglue, glue2, parkglue)  

# get species key and key_p for s subset of species
if(max(y_dat4$spskey) > length(unique(y_dat4$spskey))) {
  spskey_key <- cbind(sort(unique(y_dat4$sps_it)), seq(1, length(unique(y_dat4$spskey)))) %>% 
    as_tibble()
  colnames(spskey_key) <- c("sps_it", "spskey")
  sps_pk_key <- y_dat4 %>% 
    dplyr::select(parkey, sps_it) %>% 
    arrange(sps_it) %>% 
    distinct() %>% 
   group_by(parkey) %>% 
    mutate(spskey_p = seq(1,n(),1))
  
  y_dat4 <- y_dat4 %>% 
    select(-spskey) %>% 
    select(-spskey_p) %>% 
    left_join(., spskey_key, by = "sps_it") %>% 
    left_join(., sps_pk_key, by = c("sps_it", "parkey")) %>% 
    mutate(spskey = spskey,
           spskey_p = spskey_p)
  
} else { 
  print("All good!!!")}

# _n means that the 1 is the first occasion for that sps, year, loc, etc, not the first calendar one
y <- y_dat4 %>%
  mutate(parkey = as.numeric(parkey)) %>% 
  select(bird_detec, parkey, site_n, year_s, interval_n, #year_n,
         yr_st, Year, spskey, spskey_p, sps_it) %>% 
  arrange(parkey, site_n, yr_st, interval_n)
 
## trick for coding only interval one
y2 <- y %>% 
  filter(interval_n == 1)

# initial values
Zst <- y %>% 
  select(bird_detec, parkey, site_n, year_s, interval_n, spskey) %>% 
  group_by(parkey, site_n, year_s,spskey) %>% 
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
n_spsM <- length(unique(y$sps_it))
spskey <- unique(y$spskey)

Zst2 <- 
  array(NA, 
        dim = c(n_spsM,
                npk,
                max(nsite_pk),
                length(years)
        ),
        dimnames = list(spskey,
                        pk,
                        site_vec,
                        years
        ))

for(a in 1:nrow(Zst)){
  #   a <- which(Zst$parkey == 10 & Zst$site_n == 1 & Zst$year_s == 1, arr.ind = T)
  zl <- Zst[a,]
  
  i <- zl$spskey
  r <- as.numeric(zl$parkey)
  j <- zl$site_n 
  t <- zl$year_s

  Zst2[i,r,j,t] <- as.numeric(zl$z)
  
}

colnames(y) <- c("bird_detec", "parkey", "sitekey", "yearkey", 
                 "intervalkey", "year_site", "Year", 
                 "spskey", "spskey_p", "sps_it")

y <- data.matrix(y)
y2 <- data.matrix(y2)
y_ind <- sort(rep(seq(1, nrow(y2),1),10))
nrow(y)
nrow(y2)*ninterval

# Occupancy variables
# tree basal area - data frame with 3 columns for each of the three scales
X1 <- X10 %>% 
  select(siteBA_s, parkBA_s, counBA_s) %>% 
  mutate(siteBA_s = as.numeric(siteBA_s), 
         parkBA_s = as.numeric(parkBA_s), 
         counBA_s = as.numeric(counBA_s))

# tree density - data frame with 3 columns for each of the three scales
X2 <- X10 %>% 
  select(siteDEN_s, parkDEN_s, counDEN_s) %>% 
  mutate(siteDEN_s = as.numeric(siteDEN_s), 
         parkDEN_s = as.numeric(parkDEN_s), 
         counDEN_s = as.numeric(counDEN_s))

# park size - vector
X3 <- X10 %>% 
  select(area_s) %>% 
  pull() %>% 
  as.numeric()

# Date
Xa <- X10 %>% 
  select(date_jul) %>% 
  pull() %>% 
  as.numeric()

# Time
Xb <- X10 %>% 
  select(time_jul) %>% 
  pull() %>% 
  as.numeric()

nrow(X10)
dim(X1)
dim(X2)
length(X3)
length(Xa)
length(Xb)
dim(y)

# number of alphas and betas
n_bs <- 3
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
                      Xa = Xa,
                      Xb = Xb,
                      n_yrM = length((unique(y[,4]))),
                      n_pkM = length((unique(y[,2]))),
                      n_spsM = n_spsM
))

inits <- function()list(Z = Zst2#, beta0 = rnorm(10,0.6), beta1 = rnorm(10,0.6)
)

niterations <- 50
burnin <- 10
nchains <- 1
print(niterations)

cat("\n\n\n running jags \n\n\n\n")
params <- c("beta0","beta", "alpha0", "alpha", "scales_beta1", "scales_beta2",
            "mu.beta0", "tau.beta0", "mu.alpha0", "tau.alpha0") # Z, psi

## initialize JAGS
jags_model <- rjags::jags.model(
  file = "models/mod_1_vector_community_parks_simple_covs_scales.txt",
  data = jags.data,
  inits = inits, 
  n.chains = nchains,
  n.adapt = max(100, ceiling(.1 * niterations)),
  quiet = FALSE
)

cat("\n\n\n first done \n\n\n\n") 

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

cat("\n\n\n second done \n\n\n\n")

# posterior simulation
samples_jags <- coda.samples(
  jags_model,
  variable.names = params,
  n.iter = niterations,
  thin = 5
)

cat("\n\n\n third done \n\n\n\n")

# code to check the data and initial values
r <- 10   # what is the deal with 7 versus 10? (they both have the same values and 10 does not work)
j <- 1
t <- 1
Zst %>% filter(parkey == r, site_n == j, year_s == t)
y %>% filter(parkey == r, site_n == j, year_s == t)
Zst2[r,j,t]
# row 73 has the values in the loop

#################################################################################################################

MCMCsummary(samples_jags,
            params = params[c(2,4,5,7)],
            round = 2) 

MCMCtrace(samples_jags,
          params = params[c(2,4,5,7)],
          ind = TRUE,
          pdf = FALSE)

par(mfrow = c(1,1))
MCMCplot(samples_jags,
         params = params[c(2,4,5,7)],
         ref_ovl = TRUE)
