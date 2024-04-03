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

# Print script file name
#context <- rstudioapi::getSourceEditorContext()
#cat("\n", "\n", "\n", "Current script: ", basename(context$path), "\n", "\n", "\n", "\n")

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
X9 <- read_rds(file = XDAT_PATH)
nsite_pk <- read_rds(SITE_PK_PATH)
pk <- read_rds(PARK_PATH) %>%
  select(parks) %>%
  pull() %>%
  sort()

# load("data/datJDnov2023.RData")

# Filter for species and park ---------------------------------------
sps_UNfilt_len <- length(sort(unique(y_dat4$sps_it)))
sps_filt_list <- c(#"GCFL", "AMGO", "DOWO", "NOCA", "SCTA", "SOSP", "GRCA", "RBWO", "COYE", "WOTH", "RWBL",
                   #"WBNU", "BTNW", "EAWP", "BCCH", "BLJA", "TUTI", "AMRO", "REVI", "OVEN", "BTBW", "YBSA", 
                   #"BOBO", "YRWA", "PIWA", "CEDW", "CHSP", "NOFL", "HAWO", "BRCR", "RBGR", "DEJU", "AMCR", 
                   #"BAOR", "RBNU", "BHVI", "GCKI", "EATO", "FISP", "HETH", 
                   "VEER", "MODO", "BLBW")

y_dat4$unique_index <- seq(1,nrow(y_dat4),1)

X10 <- X9

X10$unique_index <- seq(1,nrow(y_dat4),1)

if(setequal(y_dat4$unique_index, X10$unique_index) != "TRUE") 
   stop("ah wrong indexing!!!!")

y_dat6 <- y_dat4

y_dat6 <- y_dat6 %>%
  mutate(parkey = as.numeric(parkey))

nrow(X10) == nrow(y_dat6)

y_dat6 <- y_dat6 %>%
   filter(sps_it %in% sps_filt_list)

X10 <- X10 %>%  # nolint: object_name_linter.
   filter(unique_index %in% y_dat6$unique_index)

nrow(X10) == nrow(y_dat6)

if(setequal(y_dat6$unique_index, X10$unique_index) != "TRUE") 
   stop("ah wrong indexing!!!!")

## What am I analyzing?
glue1 <- glue("the species are ")
glue2 <- glue(", and parks are")
if(sps_UNfilt_len == lenght(sps_filt_list)) {spsglue <- "All species"
} else {
   spsglue <- paste(shQuote(sort(unique(y_dat6$sps_it))), collapse=", ")
   spsglue <- str_replace_all(spsglue, "'", "")
} 
parkglue <- paste(shQuote(sort(unique(y_dat6$park))), collapse=", ")
parkglue <- str_replace_all(parkglue, "'", "")
cat("\n", "\n", glue1, spsglue, glue2, parkglue, "\n", "\n", "\n")  

## add a step here to fix parkey

parkey_right <- y_dat6 %>% 
   select(Admin_Unit_Code) %>% 
   arrange(Admin_Unit_Code) %>% 
   distinct() %>% 
   mutate(parkey = seq(1, nrow(.)))

y_dat6 <- y_dat6 %>% 
   select(-parkey) %>% 
   left_join(., parkey_right, by = "Admin_Unit_Code")

spskey_right <- y_dat6 %>% 
   select(sps_it) %>% 
   arrange(sps_it) %>% 
   distinct() %>% 
   mutate(spskey = seq(1, nrow(.)))

y_dat6 <- y_dat6 %>% 
   select(-spskey) %>% 
   left_join(., spskey_right, by = "sps_it")

yearkey_right <- y_dat6 %>% 
   select(Year) %>% 
   arrange(Year) %>% 
   distinct() %>% 
   mutate(yearkey = seq(1, nrow(.)))

y_dat6 <- y_dat6 %>% 
   left_join(., yearkey_right, by = "Year")

# sub-indexes
# get species key and key_p for s subset of species - what is the number of each species in each park
if(length(unique(y_dat4$sps_it)) > length(unique(y_dat6$sps_it))) {
   # which species number in each park 
   sps_pk_key <- y_dat6 %>% 
      dplyr::select(parkey, sps_it) %>% 
      arrange(sps_it) %>% 
      distinct() %>% 
      group_by(parkey) %>% 
      mutate(spskey_p = seq(1,n(),1)) %>%
      ungroup()
   
   y_dat6 <- y_dat6 %>% 
      select(-spskey_p) %>% 
      left_join(., sps_pk_key, by = c("sps_it", "parkey"))
   
} else { 
   print("All good!!!")}

# what is the number of my park for each species
parkey_s <- y_dat6 %>% 
   dplyr::select(parkey, sps_it) %>% 
   arrange(parkey) %>% 
   distinct() %>% 
   group_by(sps_it) %>% 
   mutate(parkey_s = seq(1,n(),1))

y_dat6 <- y_dat6 %>% 
   left_join(., parkey_s, by = c("sps_it", "parkey"))

# what is the number of years for each park
y_dat6 <- y_dat6 %>% 
   mutate(yearkey_p = Year - year_min + 1)

# view(y_dat6 %>% select(Admin_Unit_Code, Year, year_min, yearkey_p) %>% distinct())
# select the columns that I want
y <- y_dat6 %>%
  select(bird_detec, 
         sps_it, spskey, spskey_p,
         Admin_Unit_Code, parkey, parkey_s,
         Point_Name, site_n, 
         Year, year_min, yearkey, yearkey_p,
         interval_n, 
         unique_index) %>% 
  arrange(parkey, site_n, Year, interval_n)

## keys that I need
colnames(y)

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
  select(bird_detec, spskey, parkey, site_n, yearkey, interval_n) %>% 
  group_by(spskey, parkey, site_n, yearkey) %>% 
  mutate(z = ifelse(sum(bird_detec, na.rm = T) == 0, 0, 1)) %>% 
  ungroup() %>% 
  filter(interval_n == 1) 

site_vec <- seq(1,max(nsite_pk),1)
(npk <- length(unique(y$parkey)))
(pk <- sort(unique(y$parkey)))
(years <- y %>% 
      select(Year) %>% 
      distinct() %>% 
      pull() %>% 
      sort())
ninterval <- 10
n_spsM <- length(unique(y$sps_it))
spskey <- sort(unique(y$spskey))

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
  
  s <- zl$spskey
  r <- as.numeric(zl$parkey)
  j <- zl$site_n 
  t <- zl$yearkey

  Zst2[s,r,j,t] <- as.numeric(zl$z)
  
}

sum(Zst2, na.rm = T) == sum(Zst$z)

col_namesy <- as_tibble(cbind(colnames(y), seq(1,ncol(y),1)))

y <- data.matrix(y)
y2 <- data.matrix(y2)
# y_ind <- sort(rep(seq(1, nrow(y2),1),10))
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
nrow(X) == nrow(y)

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
                      #y_ind = y_ind
))

inits <- function()list(Z = Zst2#, beta0 = rnorm(10,0.6), beta1 = rnorm(10,0.6)
)

niterations <- 10
burnin <- 5
nchains <- 1
print(niterations)

params <- c("beta0","beta", "alpha0", "alpha", "scales_beta1", "scales_beta2",
            "mu.beta0", "tau.beta0", "mu.alpha0", "tau.alpha0") # Z, psi

col_namesy

cat(glue("\n\n\n running jags with {niterations} iterations (first) \n\n\n\n"))

## initialize JAGS
jags_model <- rjags::jags.model(
  file = "models/mod_1_vector_community_parks_simple_covs_scales_JD_detec.txt",
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
# r <- 10   # what is the deal with 7 versus 10? (they both have the same values and 10 does not work)
# j <- 1
# t <- 1
# Zst %>% filter(parkey == r, site_n == j, year_s == t)
# y_dat6 %>% filter(parkey == r, site_n == j, year_s == t)
# Zst2[r,j,t]
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
