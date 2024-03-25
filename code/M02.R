# ********************************************
# -----------   occupancy_noscale   ----------
# ********************************************
# 1 park, all scales by site, all sps
#
# input:    - :
#           - :
# output:   - data/out/samples_jags_BLISS_multisps_covs{yf2$Admin_Unit_Code[p]}_{niterations}.rds: jags model result

# .rs.restartR()
#detach()
rm(list = ls(all.names = TRUE))

cat("\n\n\n occupancy_BLISS_multisps_multicovs.R \n\n\n\n")

# get data
#source("~/Documents/GitHub/NPS_birds/code/2_format_data.R")
#setwd("~/Documents/GitHub/NPS_birds/")

###### CHOSE/CHANGE
ncovs <- 4
for_int <- "T"

niterations <- 5000
burnin <- 1000
nchains <- 3

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

spslist <- read_rds("data/src/guilds.rds") %>% 
  as_tibble()

spslist2 <- spslist %>% 
  filter(Response_Guild == "InteriorForestObligate")    

yf <- y1 %>% 
  filter(Admin_Unit_Code == park_name,
         AOU_Code %in% spslist2$AOU_Code       ## get only forest obligates
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
              dim = c( 
                (unique(na.omit(sps_pk[p]))) %>% nrow(),
                nsite_pk[p],
                length(years), 
                ninterval
                #npk
              ),
              dimnames = list( 
                (unique(na.omit(sps_pk[p]))) %>% arrange() %>% pull(), 
                site_vec[1:nsite_pk[p]],
                years,
                seq(1,10,1)
                #pk
              ))

# add counts (the ones) ------------------------------------------------------------------
for(a in 1:nrow(yf3)){
  
  yl <- yf3[a,]
  
  #  p <- which(pk == yl$Admin_Unit_Code, arr.ind = T)                # park
  i <- which((unique(na.omit(sps_pk[4]))) %>% arrange() %>% 
               pull() == yl$AOU_Code, arr.ind = T)                    # species
  j <- which(site_vec[1:nsite_pk[p]] == yl$site_n, arr.ind = T)       # site
  t <- which(years == yl$Year, arr.ind = T)                           # year
  k <- as.numeric(yl$Interval_n)                                      # interval
  
  yyy3[i,j,t,k] <- ifelse(sum(yyy3[i,j,t,k],
                              yl %>% 
                                select(Bird_Count) %>% 
                                pull(), 
                              na.rm = T) > 0, 1, 0)
}
# check
table(yyy3); table(is.na(yyy3))[2]

# add zeros before the ones --------------------------------------------------------
for(i in 1:dim(yyy3)[1]) {
  for(j in 1:dim(yyy3)[2]) {
    for(t in 1:dim(yyy3)[3]) {
      ind <- which.min(is.na(yyy3[i,j,t,])) %>% as.numeric()
      if(ind > 1) {
        yyy3[i,j,t,(1:(ind-1))] <- 0
      }
    }
  }
}
# check
table(yyy3); table(is.na(yyy3))[2]

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
ZEROsites <- setdiff(ALLsites_sps, SAMPLEDsites) 
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
  select(site_n, year_ind)

for(i in 1:dim(yyy3)[1]) {
  for(j in 1:dim(yyy3)[2]) {
    for(t in 1:dim(yyy3)[3]) {
      if(all(is.na(yyy3[i,j,t,])) ) {
        yyy3[i,j,t,] <- 0
      }
    }
  }
}

# check
table(yyy3); table(is.na(yyy3))[2]

# but reinsert the all NA's for the NA sites
for(x in 1:nrow(NAsites2)) {
  yyy3[ ,NAsites2$site_n[x],NAsites2$year_ind[x], ] <- NA
}

# check
table(yyy3); table(is.na(yyy3))[2]

# ordinal day and time
time <- array(0, 
              dim = c(nsite_pk[p], length(years), ninterval),
              dimnames = list(site_vec[1:nsite_pk[p]], years, seq(1,10,1)))

day <- matrix(0,
              nrow = nsite_pk[p], 
              ncol = length(years)) 

for(j in 1:nsite_pk[p]) {
  for(t in 1:nyr_pk[p]) {
    dd <- yf3 %>% 
      filter(Year == years[t],
             site_n == j) %>% 
      select(EventDate2) %>% 
      distinct() %>% 
      pull() %>% 
      as.numeric()
    
    ifelse(length(dd) == 0, NA, day[j,t] <- dd)
    
    for(k in 1:ninterval) {
      tt <- yf2 %>% 
        filter(Year == years[t],
               site_n == j,
               Interval_n == k) %>% 
        select(StartTime2) %>% 
        distinct() %>% 
        pull() %>% 
        as.numeric()
      
      ifelse(length(tt) == 0, NA, time[j,t,k] <- tt)
      
    }
  }
}

# Get covariate values -----------------------------------------------
# 11 parks, 7 years, 5 buffers (100, 250, 500, 750, 1000), 59 total sites (varies by park)
# forest area
arr_area_site <- read_rds(file = glue("data/out/arr_area_site.rds"))

# cluster index
arr_clus_site <- read_rds(file = glue("data/out/arr_clu_site.rds"))

# number of core areas
arr_core_site <- read_rds(file = glue("data/out/arr_core_site.rds"))

# last year of data for the environmental variables (recent_dat)
# c(1,4,6) are the parks that are declining, standardize MABI, ACAD and MORR ONLY between themselves, ignore other parks data
recent_dat <- dim(arr_area_site)[2]

arr_area_site1 <- arr_area_site[,recent_dat,,] 
arr_core_site1 <- arr_core_site[,recent_dat,,] 
arr_clus_site1 <- arr_clus_site[,recent_dat,,]

arr_area_site2 <- arr_area_site1[c(1,4,6),,] %>% AHMbook::standardize()
arr_core_site2 <- arr_core_site1[c(1,4,6),,] %>% AHMbook::standardize()
arr_clus_site2 <- arr_clus_site1[c(1,4,6),,]

arr_area_site1[c(1,4,6),,] <- arr_area_site2 
arr_core_site1[c(1,4,6),,] <- arr_core_site2  

# each x has five buffer scale
# forest area
x1s1 <- arr_area_site1[p,1,1:nsite_pk[p]] %>% as.vector()
x1s2 <- arr_area_site1[p,2,1:nsite_pk[p]] %>% as.vector()
x1s3 <- arr_area_site1[p,3,1:nsite_pk[p]] %>% as.vector()
x1s4 <- arr_area_site1[p,4,1:nsite_pk[p]] %>% as.vector()
x1s5 <- arr_area_site1[p,5,1:nsite_pk[p]] %>% as.vector()

x1 <- cbind(x1s1, x1s2, x1s3, x1s4, x1s5) 
x1[is.na(x1[,])] <- 0

## occupancy BLISS model -----------------------------------------	
sink("models/mod_occ_BLISS_multisps_covs.txt")	
cat("	
model {	
  # Community priors (with hyperparameters) for species-specific (i) parameters	

  for(i in 1:nspecM){	# species i
    #for(t in 1:nyrM){ # year y  ### remove year!!!
      beta0[i] ~ dnorm(mu.beta0, tau.beta0)          # Abundance intercepts for each species and year	
    #}  	

    beta1[i] ~ dnorm(mu.beta1, tau.beta1)            # Abundance slope for each species	

    alpha0[i] ~ dnorm(mu.alpha0, tau.alpha0)         # Detection intercepts  for each species	
    alpha1[i] ~ dnorm(mu.alpha1, tau.alpha1)         # Detection slope for each species	
    alpha2[i] ~ dnorm(mu.alpha2, tau.alpha2)         # Detection slope for each species	
    alpha3[i] ~ dnorm(mu.alpha3, tau.alpha3)         # Detection slope for each species	

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
  scales_beta1 ~ dcat(c(0.2, 0.2, 0.2, 0.2, 0.2))	
  
  # Likelihood	
  for(i in 1:nspecM){         # species i	
    for(j in 1:nsiteM){       # site j	
      for(t in 1:nyrM){       # year t	
        # Occupancy	
        Z[i,j,t] ~ dbern(psi[i,j,t])	
  
        # Covariate effects on the ecological process	
        logit(psi[i,j,t]) <- beta0[i] + 	
                             beta1[i] * x1[j, scales_beta1] # that would be same scale accross sites at once
                          
        # Observation model for replicated counts	
        for(k in 1:nintervalM){  # interval k
       
          logit(p[i,j,t,k]) <- alpha0[i] + alpha1[i] * day[j,t] + alpha2[i] * (day[j,t]^2) + alpha3[i] * time[j,t,k]
       
          y[i,j,t,k] ~ dbern(p[i,j,t,k] * Z[i,j,t])
        }
      }	
    }	
  }	
}	
",fill = TRUE)	
sink()	

str(jags.data <- list(y = yyy3,                    # bird detection array
                      nspecM = dim(yyy3)[1],       # number of species (M for model code)
                      nsiteM = dim(yyy3)[2],       # number of sites
                      nyrM = dim(yyy3)[3],         # number of years
                      nintervalM = dim(yyy3)[4], 	 # number of intervals       
                      day = day,                   # calendar day
                      time = time,                 # time of day
                      x1 = x1   	                 # forest cover 
))	

# Initial values
Zst <- apply(yyy3, c(1,2,3), max, na.rm = TRUE)
Zst[Zst == '-Inf'] <- 0         

inits <- function()list(Z = Zst)

# Reverse jump (?) ----------------------------------------------------------

print(niterations)

cat("\n\n\n running jags \n\n\n\n")

## initialize JAGS
jags_model <- rjags::jags.model(
  file = "models/mod_occ_BLISS_multisps_covs.txt",
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
          file = glue("data/model_res/jags_model2_BLISS_multisps_covs{yf2$Admin_Unit_Code[p]}_{niterations}its_run{length(list.files(path = file.path(getwd(),'/data/out'),pattern = glue('samples_jags'), full.names = FALSE)) + 1}.rds"))

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
                     #"alpha0", "alpha1", "alpha2", "alpha3",
                     #"beta0", "beta1",
                     #"Z","psi", "p"
                     "scales_beta1"
  ),
  n.iter = niterations,
  thin = 3
)

cat("\n\n\n third done \n\n\n\n")

write_rds(samples_jags, 
          file = glue("data/model_res/samples_jags3_BLISS_multisps_covs{yf2$Admin_Unit_Code[p]}_{niterations}its_run{length(list.files(path = file.path(getwd(),'/data/out'),pattern = glue('samples_jags'), full.names = FALSE)) + 1}.rds"))



cat("\n\n\n DONE occupancy_BLISS_multisps_multicovs.R \n\n\n\n")


library(MCMCvis)

MCMCsummary(samples_jags,
            # params = 'alpha',
            round = 2)

MCMCtrace(samples_jags,
          params = c("mu.alpha0", 
                       "mu.alpha1",
                       "mu.alpha2", 
                       "mu.alpha3",
                       "mu.beta0",
                       "mu.beta1"),
          ind = TRUE,
          pdf = FALSE)

MCMCplot(samples_jags,
         # params = 'beta',
         ref_ovl = TRUE)

# scale selection plots and objects:

sca_beta1 <- MCMCchains(samples_jags, params = 'scales_beta1')
selected_scales = rep(NA, 1)
for (i in 1:ncovs) {
  tb_mcmc_scales_i = table(sca_beta1)

  selected_scales[i] = as.integer(names(which.max(tb_mcmc_scales_i)))
}

sca_beta1
selected_scales

sca_beta1p <- as_tibble(sca_beta1) %>%
  mutate(new = 1)
sca_beta1p <- pivot_longer(sca_beta1p, -new, names_to = "site", values_to = "selected_scale") %>%
  select(site, selected_scale) %>%
  arrange(site)

# colors are sites
ggplot(aes(x = selected_scale, y = (..count..)/sum(..count..), fill = site), data = sca_beta1p) +
  geom_histogram(position = "stack", binwidth = 0.5) +
  theme_bw() +
  theme(legend.position = "none") +
  ylab("Frequency") + xlab("Selected scale")

ggplot(aes(x = selected_scale, fill = site), data = sca_beta1p) +
  theme_bw() +
  theme(legend.position = "none") +
  ylab("Frequency") + xlab("Selected scale") +
  geom_density(alpha = 0.08, color = "gray36") +
  scale_x_continuous(limits = c(0, 5))
