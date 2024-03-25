# ********************************************
# -----------   occupancy_noscale   ----------
# ********************************************
# Script to run occupancy model for one species in one park for one covariate 
# PL stands for park level
#
# input:    - :
#           - :
# output:   - data/out/samples_jags_BLISS_multisps_covs{yf2$Admin_Unit_Code[p]}_{niterations}.rds: jags model result

# .rs.restartR()
#detach()
rm(list = ls(all.names = TRUE))

cat("\n\n\n occupancy_1sp_pks_1covPL.R \n\n\n\n")

# get data
#source("~/Documents/GitHub/NPS_birds/code/2_format_data.R")
#setwd("~/Documents/GitHub/NPS_birds/")

###### CHOSE/CHANGE
ncovs <- 1
for_int <- "T"

niterations <- 250
burnin <- 50
nchains <- 3

niterations <- 10000
burnin <- 5000
nchains <- 9

source("code/2_format_data.R")

library(rjags)
library(hms)
#library(egg) #ggarrange
#library(conflicted)
library(tidyverse)
#library(MCMCvis)
library(coda)
#conflicts_prefer(dplyr::select)
#conflicts_prefer(dplyr::filter)

### choose multidimensional array dimensions
## filter data: 1 park, all years, all sps, all sites, all intervals, all buffers

y1$AOU_Code %>% table() %>% sort()

spslist <- "OVEN"

yf <- y1 %>% 
  filter(AOU_Code == spslist
  ) 

# have only ones for detections
sum(yf$Bird_Count)
ifelse(yf$Bird_Count > 1, yf$Bird_Count <- 1, yf$Bird_Count <- yf$Bird_Count)
sum(yf$Bird_Count)

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

years <- yf2 %>% select(Year) %>% distinct() %>% arrange(Year) %>% pull()

# keep only first detection of the 10 intervals --------------------------------------------------
yf2 %>% dim()

yf3 <- yf2 %>% 
  filter(Bird_Count == 1) %>% 
  mutate(Interval_n = as.numeric(Interval_n)) %>% 
  group_by(AOU_Code, EventDate, site_n) %>%
  filter(Interval_n == min(Interval_n)) %>% 
  slice(1) %>%   # takes the first occurrence if there is a tie
  ungroup()

dim(yf3)
# check
sum(yf3$Bird_Count, na.rm = T)

# create 4d array
yyy3 <- array(NA, 
              dim = c(npk,
                max(nsite_pk),
                length(years), 
                ninterval
              ),
              dimnames = list(pk,
                site_vec[1:max(nsite_pk)],
                years,
                seq(1,10,1)
              ))

# add counts (the ones) ------------------------------------------------------------------
for(a in 1:nrow(yf3)){

  yl <- yf3[a,]
  
  p <- which(pk == yl$Admin_Unit_Code, arr.ind = T)
  j <- which(site_vec[1:nsite_pk[p]] == yl$site_n, arr.ind = T)       # site
  t <- which(years == yl$Year, arr.ind = T)                           # year
  k <- as.numeric(yl$Interval_n)                                      # interval
  
  yyy3[p,j,t,k] <- ifelse(sum(yyy3[p,j,t,k],
                              yl %>% 
                                select(Bird_Count) %>% 
                                pull(), 
                              na.rm = T) > 0, 1, 0)
}
# check
table(yyy3); table(is.na(yyy3))[1]

# add zeros before the ones --------------------------------------------------------
for(p in 1:dim(yyy3)[1]) {  
  for(j in 1:dim(yyy3)[2]) {
    for(t in 1:dim(yyy3)[3]) {
      ind <- which.min(is.na(yyy3[p,j,t,])) %>% as.numeric()
      if(ind > 1) {
        yyy3[p,j,t,(1:(ind-1))] <- 0
      }
    }
  }
}

# check
table(yyy3) %>% sum(); table(is.na(yyy3))[1]

# add all zeros if never detected --------------------------------------------------
## create zeros for species, separate them from NAs and add them to a new data frame
SAMPLEDsites <- yf3 %>% 
  select(Point_Name, Year) %>%
  distinct() 

ALLsites_sps <- expand.grid(SAMPLEDsites %>% select(Point_Name) %>% distinct() %>% pull(),
                            SAMPLEDsites %>% select(Year) %>% distinct() %>% pull()
)

# select the zeros
table(yf3 %>% select(Point_Name, Year) %>% distinct()) 

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
  select(Admin_Unit_Code, site_n, year_ind)
pk_num <- cbind(pk, seq(1:npk)) %>%
  as_tibble() %>% 
  rename(Admin_Unit_Code = pk, AUC_num = V2)
NAsites2 <- left_join(NAsites2, pk_num, by = "Admin_Unit_Code") %>% 
  mutate(AUC_num = as.numeric(AUC_num))

for(p in 1:dim(yyy3)[1]) {
  for(j in 1:dim(yyy3)[2]) {
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
  yyy3[NAsites2$AUC_num[x], NAsites2$site_n[x],NAsites2$year_ind[x], ] <- NA
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
    for(t in 1:nyr_pk[p]) {
      dd <- yf3 %>% 
        filter(Admin_Unit_Code == pk[p],
               Year == years[t],
               site_n == j) %>% 
        select(EventDate2) %>% 
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
          select(StartTime2) %>% 
          distinct() %>% 
          pull() %>% 
          as.numeric()
        
        ifelse(length(tt) == 0, NA, time[p,j,t,k] <- tt)
        
      }
    }
  }
}

# Get covariate values -----------------------------------------------
# 11 parks, 7 years, 5 buffers (100, 250, 500, 750, 1000), 59 total sites (varies by park)
# forest area
arr_area <- read_rds(file = glue("data/out/arr_area_park.rds"))

# last year of data for the environmental variables (recent_dat)
# c(1,4,6) are the parks that are declining, standardize MABI, ACAD and MORR ONLY between themselves, ignore other parks data
recent_dat <- dim(arr_area)[2]

arr_area1 <- arr_area[,recent_dat,] 

arr_area2 <- arr_area1 %>% AHMbook::standardize()

x1 <- arr_area2
x1[8,1] <- 0
## occupancy BLISS model -----------------------------------------	
sink("models/mod_occ_1sp_pks_1covPL.txt")	
cat("	
model {	
  # Community priors (with hyperparameters) for species-specific (i) parameters	

  for(pp in 1:npkM){	# park pp
    #for(tt in 1:nyrM){ # year tt
      beta0[pp] ~ dnorm(mu.beta0, tau.beta0)      # Abundance intercepts for each species and year	
    #}  	

    beta1[pp] ~ dnorm(mu.beta1, tau.beta1)            # Abundance slope for each species	

    alpha0[pp] ~ dnorm(mu.alpha0, tau.alpha0)         # Detection intercepts  for each species	
    alpha1[pp] ~ dnorm(mu.alpha1, tau.alpha1)         # Detection slope for each species	
    alpha2[pp] ~ dnorm(mu.alpha2, tau.alpha2)         # Detection slope for each species	
    alpha3[pp] ~ dnorm(mu.alpha3, tau.alpha3)         # Detection slope for each species	

  }	

  # Hyperpriors for community hyperparameters - hey put this in loops.	
  # Ecological process	
  mu.beta0 ~ dnorm(0, 0.3676)	
  tau.beta0 <- pow(sd.beta0, -2)	
  sd.beta0 ~ dgamma(0.3676,0.3676) 

  mu.beta1 ~ dnorm(0, 0.3676)	
  tau.beta1 <- pow(sd.beta1, -2)	
  sd.beta1 ~ dgamma(0.01,0.01) 

  # Sampling process	
  mu.alpha0 ~ dnorm(0, 0.3676)	
  tau.alpha0 <- pow(sd.alpha0, -2)	
  sd.alpha0 ~ dgamma(0.01,0.01) 

  mu.alpha1 ~ dnorm(0, 0.3676)	
  tau.alpha1 <- pow(sd.alpha0, -2)	
  sd.alpha1 ~ dgamma(0.01,0.01) 
   	
  mu.alpha2 ~ dnorm(0, 0.3676)	
  tau.alpha2 <- pow(sd.alpha0, -2)	
  sd.alpha2 ~ dgamma(0.01,0.01) 
   	
  mu.alpha3 ~ dnorm(0, 0.3676)	
  tau.alpha3 <- pow(sd.alpha0, -2)	
  sd.alpha3 ~ dgamma(0.01,0.01) 

  # indicator variable for scales	
  for (pp in 1:npkM){  # site j	
    scales_beta1[pp] ~ dcat(c(0.2, 0.2, 0.2, 0.2, 0.2))	
  }

  # Likelihood	
  for(pp in 1:npkM){         # species i	
    for(jj in 1:nsiteM){       # site j	
      for(tt in 1:nyrM){       # year t	
        # Occupancy	
        Z[pp,jj] ~ dbern(psi[pp,jj])	
  
        # Covariate effects on the ecological process	
        logit(psi[pp,jj]) <- beta0[pp] + 	
                           beta1[pp] * x1[pp, scales_beta1[pp]] 
  
        # Observation model for replicated counts	
        for(kk in 1:nintervalM){  # interval k
       
          logit(p[pp,jj,tt,kk]) <- alpha0[pp] + 
                                   alpha1[pp] * day[pp,jj,tt] + 
                                   alpha2[pp] * (day[pp,jj,tt]^2) + 
                                   alpha3[pp] * time[pp,jj,tt,kk]
       
          y[pp,jj,tt,kk] ~ dbern(p[pp,jj,tt,kk] * Z[pp,jj])
        }
      }	
    }	
  }	

  # input covariates
  for(pp in 1:npkM){                 # park kk	
    #x1[pp, scales_beta1[pp]] ~ dnorm(mu_x1, tau_x1)

    for(jj in 1:nsiteM){             # site jj	
      for(tt in 1:nyrM){             # year tt	    
        day[pp,jj,tt] ~ dnorm(mu_day, tau_day)
        
        for(kk in 1:nintervalM){   # interval kk
             time[pp,jj,tt,kk] ~ dnorm(mu_time, tau_time)

        }
      }
    }  
  }
  mu_day ~ dnorm(0, 0.01)
  sigma_day ~ dunif(0, 10)
  tau_day <- pow(sigma_day, -2)

  mu_time ~ dnorm(0, 0.01)
  sigma_time ~ dunif(0, 10)
  tau_time <- pow(sigma_time, -2)

  # mu_x1 ~ dnorm(0, 0.01)
  # sigma_x1 ~ dunif(0, 10)
  # tau_x1 <- pow(sigma_x1, -2)
}	
",fill = TRUE)	
sink()	


str(jags.data <- list(y = yyy3,                    # bird detection array
                      npkM = dim(yyy3)[1],       # number of sites
                      nsiteM = dim(yyy3)[2],       # number of sites
                      nyrM = dim(yyy3)[3],         # number of years
                      nintervalM = dim(yyy3)[4], 	 # number of intervals       
                      day = day,                   # calendar day
                      time = time,                 # time of day
                      x1 = x1
))	

# Initial values
Zst <- apply(yyy3, c(1,2,3), max, na.rm = TRUE)
Zst[Zst == '-Inf'] <- 0         

inits <- function()list(Z = Zst)

# Reverse jump ----------------------------------------------------------

print(niterations)

cat("\n\n\n running jags \n\n\n\n")

# # initialize JAGS
jags_model <- rjags::jags.model(
  file = "models/mod_occ_1sp_pks_1covPL.txt",
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
    variable.names = c("alpha0"),
    n.iter = niterations,
    thin = 3
  )
}

write_rds(jags_model, 
          file = glue("data/model_res/jags_model_occ_1sp_pks_1covPL{yf2$Admin_Unit_Code[p]}_{niterations}its_run{length(list.files(path = file.path(getwd(),'/data/out'),pattern = glue('samples_jags'), full.names = FALSE)) + 1}.rds"))

cat("\n\n\n second done \n\n\n\n")

# posterior simulation
samples_jags <- coda.samples(
  #samples_jags <- rjags::jags.samples(
  jags_model,
  variable.names = c("mu.alpha0", "sd.alpha0",
                     "mu.alpha1", "sd.alpha1",
                     "mu.alpha2", "sd.alpha2",
                     "mu.alpha3", "sd.alpha3",
                     "mu.beta0", "sd.beta0",
                     "mu.beta1", "sd.beta1",
                     "mu.beta2", "sd.beta2",
                     "mu.beta3", "sd.beta3"#,
                     #"alpha0", "alpha1", "alpha2", "alpha3",
                     #"beta0", "beta1",
                     #"Z","psi", "p"
                     #"scales_beta1"
  ),
  n.iter = niterations,
  thin = 3
)

cat("\n\n\n third done \n\n\n\n")

write_rds(samples_jags, 
          file = glue("data/model_res/samples_jags_occ_1sp_pks_1covPL{yf2$Admin_Unit_Code[p]}_{niterations}its_run{length(list.files(path = file.path(getwd(),'/data/out'),pattern = glue('samples_jags'), full.names = FALSE)) + 1}.rds"))


cat("\n\n\n DONE    occupancy_1sp_pks_1covPL.R \n\n\n\n")



