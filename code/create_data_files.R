# ********************************************
# -----------   occupancy_noscale   ----------
# ********************************************
# all parks, 1 scale by park, 1 sps
#
# input:    - :
#           - :
# output:   - data/out/samples_jags_BLISS_multisps_covs{yf2$Admin_Unit_Code[p]}_{niterations}.rds: jags model result

# .rs.restartR()
#detach()
#rm(list = ls(all.names = TRUE))

library(rjags)
library(hms)
#library(egg) #ggarrange
#library(conflicted)
library(tidyverse)
#library(MCMCvis)
library(coda)
#conflicts_prefer(dplyr::select)
#conflicts_prefer(dplyr::filter)


# get data
#source("~/Documents/GitHub/NPS_birds/code/2_format_data.R")
#setwd("~/Documents/GitHub/NPS_birds/")

###### CHOSE/CHANGE
ncovs <- 1
# spslist <- "WBNU"
#park_list <- "MABI"
#for_int <- "F"          # forest interior birds

niterations <- 50
burnin <- 10
nchains <- 3

source("code/2_format_data.R")

spslist <- read_csv("data/src/original/NETN_2020/BirdSpecies.csv") %>% 
  select(AOU_Code)
### choose multidimensional array dimensions
## filter data: 1 park, all years, all sps, all sites, all intervals, all buffers

y1 %>% dplyr::select(AOU_Code) %>% table() %>% sort()

pk <- sort(unique(y1$Admin_Unit_Code))
park_list <- pk#[1:9]
#nsite_pk <- nsite_pk[c(2:7, 9:11)]

# get park site coordinates

site_coords <- y1 %>% 
  select(Admin_Unit_Code, Point_Name, Transect_CODE, Longitude, Latitude, site_n, BCR) %>% 
  distinct()

write_rds(site_coords, "site_coords.rds")

yf <- y1 %>% 
  filter(AOU_Code %in% pull(spslist),
         Admin_Unit_Code %in% park_list) 

pk <- sort(unique(yf$Admin_Unit_Code))
npk <- length(pk)
park_list <- pk#[c(2:7, 9:11)]

cov_indx <- which(sort(unique(y1$Admin_Unit_Code)) %in% park_list)

nsite_pk <- nsite_pk[which(sort(unique(y1$Admin_Unit_Code)) %in% park_list)]

yrs_pk <- yrs_pk[,which(sort(unique(y1$Admin_Unit_Code)) %in% park_list)]
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

years <- yf2 %>% dplyr::select(Year) %>% distinct() %>% arrange(Year) %>% pull()

# keep only first detection of the 10 intervals --------------------------------------------------
yf2 %>% dim()

yf3 <- yf2 %>% 
  filter(Bird_Count == 1) %>% 
  mutate(Interval_n = as.numeric(Interval_n)) %>% 
  group_by(AOU_Code, EventDate, site_n, Admin_Unit_Code) %>%
  filter(Interval_n == min(Interval_n)) %>% 
  slice(1) %>%   # takes the first occurrence if there is a tie
  ungroup()

dim(yf3)
# check
sum(yf3$Bird_Count, na.rm = T)

# filter for only one sps tables (format) -----------------
if((as_tibble(spslist) %>% nrow()) == 1) {
  
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
                                  dplyr::select(Bird_Count) %>% 
                                  pull(), 
                                na.rm = T) > 0, 1, 0)
  }
  # check
  table(yyy3); table(is.na(yyy3))[1]
  
  # add zeros before the ones --------------------------------------------------------
  for(p in 1:dim(yyy3)[1]) {  
    for(j in 1:nsite_pk[p]) {
      for(t in 1:dim(yyy3)[3]) {
        ind <- which.min(is.na(yyy3[p,j,t,])) %>% as.numeric()
        if(ind > 1) {
          yyy3[p,j,t,(1:(ind-1))] <- 0
        }
      }
    }
  }
  
  # check
  table(yyy3) %>% sum(na.rm = T); table(is.na(yyy3))[1]
  
  # add all zeros if never detected --------------------------------------------------
  ## create zeros for species, separate them from NAs and add them to a new data frame
  SAMPLEDsites <- yf3 %>% 
    dplyr::select(Point_Name, Year) %>%
    distinct() 
  
  ALLsites_sps <- expand.grid(SAMPLEDsites %>% dplyr::select(Point_Name) %>% distinct() %>% pull(),
                              SAMPLEDsites %>% dplyr::select(Year) %>% distinct() %>% pull()
  )
  
  # select the zeros
  table(yf3 %>% dplyr::select(Point_Name, Year) %>% distinct()) 
  
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
    dplyr::select(Admin_Unit_Code, site_n, year_ind)
  pk_num <- cbind(pk, seq(1:npk)) %>%
    as_tibble() %>% 
    rename(Admin_Unit_Code = pk, AUC_num = V2)
  NAsites2 <- left_join(NAsites2, pk_num, by = "Admin_Unit_Code") %>% 
    mutate(AUC_num = as.numeric(AUC_num))
  
  for(p in 1:dim(yyy3)[1]) {
    for(j in 1:nsite_pk[p]) {
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
    yyy3[NAsites2$AUC_num[x], NAsites2$site_n[x], NAsites2$year_ind[x], ] <- NA
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
      for(t in 1:dim(yyy3)[3]) {# nyr_pk[p]) {
        dd <- yf3 %>% 
          filter(Admin_Unit_Code == pk[p],
                 Year == years[t],
                 site_n == j) %>% 
          dplyr::select(EventDate2) %>% 
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
            dplyr::select(StartTime2) %>% 
            distinct() %>% 
            pull() %>% 
            as.numeric()
          
          ifelse(length(tt) == 0, NA, time[p,j,t,k] <- tt)
          
        }
      }
    }
  }
}

# filter for more than one sps tables (format) -----------------
sps_f_names <- yf3 %>% 
  dplyr::select(AOU_Code) %>% 
  distinct()

n_sps <- sps_f_names %>% 
  pull() %>% 
  length()
  
if((as_tibble(spslist) %>% nrow()) > 1) {
  
  # create 4d array
  yyy3 <- array(NA, 
                dim = c(n_sps,
                        npk,
                        max(nsite_pk),
                        length(years), 
                        ninterval
                ),
                dimnames = list(as.vector(sps_f_names %>% pull),
                                pk,
                                site_vec[1:max(nsite_pk)],
                                years,
                                seq(1,10,1)
                ))
  
  # add counts (the ones) ------------------------------------------------------------------
  for(a in 1:nrow(yf3)){
    
    yl <- yf3[a,]
    
    i <- which(sps_f_names == yl$AOU_Code, arr.ind = T)
    p <- which(pk == yl$Admin_Unit_Code, arr.ind = T)
    j <- which(site_vec[1:nsite_pk[p]] == yl$site_n, arr.ind = T)       # site
    t <- which(years == yl$Year, arr.ind = T)                           # year
    k <- as.numeric(yl$Interval_n)                                      # interval
    
    yyy3[i,p,j,t,k] <- ifelse(sum(yyy3[i,p,j,t,k],
                                yl %>% 
                                  dplyr::select(Bird_Count) %>% 
                                  pull(), 
                                na.rm = T) > 0, 1, 0)
  }
  # check
  table(yyy3); table(is.na(yyy3))[1]
  
  # add zeros before the ones --------------------------------------------------------
  for(i in 1:dim(yyy3)[1]) {  
    for(p in 1:dim(yyy3)[2]) {  
      for(j in 1:nsite_pk[p]) {
        for(t in 1:dim(yyy3)[4]) {
          ind <- which.min(is.na(yyy3[i,p,j,t,])) %>% as.numeric()
          if(ind > 1) {
            yyy3[i,p,j,t,(1:(ind-1))] <- 0
          }
        }
      }
    }
  }  
  
  # check
  table(yyy3) %>% sum(na.rm = T); table(is.na(yyy3))[1]
  
  # add all zeros if never detected --------------------------------------------------
  ## create zeros for species, separate them from NAs and add them to a new data frame
  SAMPLEDsites <- yf3 %>% 
    dplyr::select(Point_Name, Year) %>%
    distinct() 
  
  ALLsites_sps <- expand.grid(SAMPLEDsites %>% dplyr::select(Point_Name) %>% distinct() %>% pull(),
                              SAMPLEDsites %>% dplyr::select(Year) %>% distinct() %>% pull()
  )
  
  # select the zeros
  table(yf3 %>% dplyr::select(Point_Name, Year) %>% distinct()) 
  
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
    dplyr::select(Admin_Unit_Code, site_n, year_ind)
  pk_num <- cbind(pk, seq(1:npk)) %>%
    as_tibble() %>% 
    rename(Admin_Unit_Code = pk, AUC_num = V2)
  NAsites2 <- left_join(NAsites2, pk_num, by = "Admin_Unit_Code") %>% 
    mutate(AUC_num = as.numeric(AUC_num))
  
  for(i in 1:dim(yyy3)[1]) {  
    for(p in 1:dim(yyy3)[2]) {
      for(j in 1:nsite_pk[p]) {
        for(t in 1:dim(yyy3)[4]) {
          if(all(is.na(yyy3[i,p,j,t,])) ) {
            yyy3[i,p,j,t,] <- 0
          }
        }
      }
    }
  }
  
  # check
  table(yyy3) %>% sum(); table(is.na(yyy3))
  
  # but reinsert the all NA's for the NA sites
  for(x in 1:nrow(NAsites2)) {
    yyy3[,NAsites2$AUC_num[x], NAsites2$site_n[x], NAsites2$year_ind[x], ] <- NA
  }
  
  # check
  table(yyy3) %>% sum(); table(is.na(yyy3))
  
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
      for(t in 1:dim(yyy3)[4]) {# nyr_pk[p]) {
        dd <- yf3 %>% 
          filter(Admin_Unit_Code == pk[p],
                 Year == years[t],
                 site_n == j) %>% 
          dplyr::select(EventDate2) %>% 
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
            dplyr::select(StartTime2) %>% 
            distinct() %>% 
            pull() %>% 
            as.numeric()
          
          ##### problem here that needs solving for the future ------------------------------- 
          ifelse(length(tt) == 0, NA, time[p,j,t,k] <- tt[1] # [1] here is because there is a conflict here with different times for same interval
                 )
          
        }
      }
    }
  }
}

# Get covariate values -----------------------------------------------
# 11 parks, 7 years, 5 buffers (100, 250, 500, 750, 1000), 59 total sites (varies by park)
# park level covs
arr_area <- read_rds(file = glue("data/out/arr_area_park.rds"))

cov_indx <- which(sort(unique(y1$Admin_Unit_Code)) %in% park_list)

arr_area <- arr_area[c(cov_indx),,]

# last year of data for the environmental variables (recent_dat)
# c(1,4,6) are the parks that are declining, standardize MABI, ACAD and MORR ONLY between themselves, ignore other parks data
if(length(dim(arr_area)) == 2) {
  
  recent_dat <- dim(arr_area)[1]
  
  arr_area1 <- arr_area[recent_dat,] 
  
  arr_area2 <- arr_area1 %>% AHMbook::standardize()
  
  for_p <- arr_area2[c(1,5)]  ## i want only 100 and 1000 for now - add 2000!  
  }

if(length(dim(arr_area)) > 2) {
  
  recent_dat <- dim(arr_area)[2]
  
  arr_area1 <- arr_area[,recent_dat,] 
  
  arr_area2 <- arr_area1 %>% AHMbook::standardize()
  
  for_p <- arr_area2[ ,c(1,5)]  ## i want only 100 and 1000 for now - add 2000!
  }

## site level covs ------------
arr_area_site <- read_rds(file = glue("data/out/arr_area_site.rds"))

cov_indx <- which(sort(unique(y1$Admin_Unit_Code)) %in% park_list)

arr_area_site <- arr_area_site[c(cov_indx),,,]

if(length(dim(arr_area_site)) > 3) { ## multiple parks
  
  arr_area_site1 <- arr_area_site[,recent_dat,,] %>% AHMbook::standardize()
  
  for_s <- arr_area_site1

}

if(length(dim(arr_area_site)) == 3) {  ## one park
  
  arr_area_site1 <- arr_area_site[recent_dat,,] %>% AHMbook::standardize()
  
  for_s <- arr_area_site1
  
}

park_size <- NA

for(i in 1:length(pk)) {
  pb <- read_rds(file = glue("data/park_raster/{pk[i]}/{pk[i]}_pb.rds"))
  park_size[i] <- raster::area(pb)   # suqre km
}

if(length(park_size) > 1) {
  park_size <- park_size %>% scale() %>% as.numeric()
}

pk_yrs <- yrs_pk - 2005

pk_yrs_le <- apply(pk_yrs,2,max, na.rm = T)

# ## occupancy BLISS model -----------------------------------------	
# 
# 
# if(length(park_list) > 1)
# str(jags.data <- list(y = yyy3,                    # bird detection array
#                       pk_yrs_le = pk_yrs_le,       # number of years in each parks
#                       pk_yrs = pk_yrs,             # which years in which parks
#                       nsiteM = nsite_pk,           # number of sites in each park
#                       npkM = dim(yyy3)[1],         # number of parks
#                       nintervalM = dim(yyy3)[4], 	 # number of intervals       
#                       day = day,                   # calendar day
#                       time = time#,                # time of day
#                       #park_size = park_size,      # park area
#                       #for_s = for_s,              # forest cover in site
#                       #for_p = for_p               # forest cover in park
# ))	
# 
# if(length(park_list) == 1)
#   str(jags.data <- list(y = yyy3,                    # bird detection array
#                         pk_yrs_le = pk_yrs_le,       # number of years in each parks
#                         pk_yrs = pk_yrs,             # which years in which parks
#                         nsiteM = nsite_pk,           # number of sites in each park
#                         nyrsM = dim(yyy3)[1],         # number of years
#                         nintervalM = dim(yyy3)[3], 	 # number of intervals       
#                         day = day,                   # calendar day
#                         time = time#,                # time of day
#                         #park_size = park_size,      # park area
#                         #for_s = for_s,              # forest cover in site
#                         #for_p = for_p               # forest cover in park
#   ))
# 
# # Initial values
# Zst <- apply(yyy3, c(1,2,3), max, na.rm = TRUE)
# Zst[Zst == '-Inf'] <- 0         
# 
# inits <- function()list(Z = Zst#, beta0 = rnorm(10,0.6), beta1 = rnorm(10,0.6)
#                         )
# 
# # Reverse jump (?) ----------------------------------------------------------
# 
# print(niterations)
# 
# 
# cat("\n\n\n running jags \n\n\n\n")
# 
# ## initialize JAGS
# jags_model <- rjags::jags.model(
#   file = "models/mod_12_1p.txt",
#   data = jags.data,
#   inits = inits,
#   n.chains = nchains,
#   n.adapt = max(100, ceiling(.1 * niterations)),
#   quiet = FALSE
# )
# 
# cat("\n\n\n first done \n\n\n\n")
# 
# # burn-in
# if (burnin > 0) {
#   message(paste("burn-in:", burnin, "iterations"))
#   rjags::jags.samples(
#     jags_model,
#     variable.names = c("alpha0"),
#     n.iter = niterations,
#     thin = 3
#   )
# }
# 
# write_rds(jags_model, 
#           file = glue("data/model_res/M12.rds"))
# 
# cat("\n\n\n second done \n\n\n\n")
# 
# # posterior simulation
# samples_jags <- coda.samples(
#   #samples_jags <- rjags::jags.samples(
#   jags_model,
#   variable.names = c("mu.alpha0", 
#                      "mu.alpha1",
#                      "mu.alpha2", 
#                      "mu.alpha3", 
#                      "mu.beta0", 
#                      "mu.beta1",
#                      "scales_beta1"
#                      
#   ),
#   n.iter = niterations,
#   thin = 3
# )
# 
# cat("\n\n\n third done \n\n\n\n")
# 
# write_rds(samples_jags, 
#           file = glue("data/model_res/M07.rds"))
# 
# 
# 
# cat("\n\n\n DONE M06.R \n\n\n\n")
# 
# 
# 
# 
# MCMCsummary(samples_jags,
#             # params = 'alpha',
#             round = 2)
# 
# MCMCtrace(samples_jags,
#           params = c("mu.alpha0", 
#                      "mu.alpha1",
#                      "mu.alpha2", 
#                      "mu.alpha3", 
#                      "mu.beta0", 
#                      "mu.beta1",
#                      "scales_beta1"),
#           ind = TRUE,
#           pdf = FALSE)
# 
# MCMCplot(samples_jags,
#          # params = 'beta',
#          ref_ovl = TRUE)
# 
# # scale selection plots and objects:
# 
# sca_beta1 <- MCMCchains(samples_jags, params = 'scales_beta1')
# selected_scales = rep(NA, 1)
# for (i in 1:ncovs) {
#   tb_mcmc_scales_i = table(sca_beta1)
#   
#   selected_scales[i] = as.integer(names(which.max(tb_mcmc_scales_i)))
# }
# 
# sca_beta1
# selected_scales
# 
# sca_beta1p <- as_tibble(sca_beta1) %>%
#   mutate(new = 1)
# sca_beta1p <- pivot_longer(sca_beta1p, -new, names_to = "site", values_to = "selected_scale") %>%
#   select(site, selected_scale) %>%
#   arrange(site)
# 
# # colors are sites
# ggplot(aes(x = selected_scale, y = (..count..)/sum(..count..), fill = site), data = sca_beta1p) +
#   geom_histogram(position = "stack", binwidth = 0.5) +
#   theme_bw() +
#   theme(legend.position = "none") +
#   ylab("Frequency") + xlab("Selected scale")
# 
# ggplot(aes(x = selected_scale, fill = site), data = sca_beta1p) +
#   theme_bw() +
#   theme(legend.position = "none") +
#   ylab("Frequency") + xlab("Selected scale") +
#   geom_density(alpha = 0.08, color = "gray36") +
#   scale_x_continuous(limits = c(0, 5))
