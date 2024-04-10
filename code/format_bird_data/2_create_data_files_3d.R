# *********************************************************************************
# ---------------------------- 2_create_data_files_3d -----------------------------
# *********************************************************************************
# Code to ...
#
#
# Input ----------------------------------------------
#           - :
#           - :
#
# Output ----------------------------------------------
#           - :
#           - :

# detach packages and clear workspace
if(!require(freshr)){install.packages("freshr")}
freshr::freshr()

# Load packages ---------------------------------------
library(conflicted)
library(tidyverse)
library(rjags)
library(glue)

conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::filter)
# conflicts_prefer(scales::alpha)

# Make functions --------------------------------------
colanmes <- colnames
lenght <- length
`%!in%` <- Negate(`%in%`)

# Import data -----------------------------------------
## file paths
Y_DATA_PATH <- "data/y_dat8.rds"

## read files
y_dat6 <- read_rds(file = Y_DATA_PATH)

pkey <- y_dat6 %>% 
  select(park, parkey) %>% 
  distinct() %>% 
  arrange()

X11 <- read_rds(file = "data/X10.rds") %>% 
  left_join(., pkey, by = "park")

pk <- read_rds("data/src/key_park.rds") %>% 
  select(parks) %>% 
  pull() %>% 
  sort()

sps_pk_nth <- read_rds(file = "data/sps_pk_nth.rds")

'%!in%' <- function(x,y)!('%in%'(x,y))

## for one species and one park ------------------------------------------------------------------------------------------
y_dat4 <- y_dat6
X10 <- X11

nrow(X10) == nrow(y_dat4)

years <- y_dat4 %>% 
  select(Year) %>% 
  distinct() %>% 
  arrange() %>% 
  pull()

X10$sps_it <- y_dat4$sps_it

X10$spskey <- y_dat4$spskey

y_dat4 <- y_dat4 %>% 
  filter(#parkey == 1,      #change###################################################
  spskey == 4)            #change###################################################

X10 <- X10 %>% 
  filter(#park == "ACAD",   #change###################################################
         spskey == 4)     #change###################################################

dim(X10)
dim(y_dat4)

nsite_pk <- read_rds("data/out/nsite_pk.rds")[1]  #change############################
sps_pk_nth <- read_rds(file = "data/sps_pk_nth.rds") %>% 
  filter(Admin_Unit_Code == "ACAD")    #change#######################################

# make sure all my covariates are the same dimensions as
(n_spsM <- y_dat4$sps_it %>% unique() %>% length())
(n_pkM <- y_dat4$park %>% unique() %>% length())
(nyears <- length(years))

n_bs <- 7 #change####################################################################
n_as <- 3

table(is.na(X10$siteBA_s))
table(is.na(X10$siteDEN_s))
table(is.na(X10$parkBA_s))
table(is.na(X10$parkDEN_s))
table(is.na(X10$counBA_s))
table(is.na(X10$counDEN_s))
table(is.na(X10$date_jul))
table(is.na(X10$time_jul))

ninterval <- 10

site_vec <- seq(1,max(nsite_pk),1)

(npk <- length(unique(y_dat4$parkey)))

# detections
yyy3 <- 
   array(NA, 
        dim = c(max(nsite_pk),
                length(years), 
                ninterval
                ),
              dimnames = list(site_vec,
                              years,
                              seq(1,10,1)
              ))

for(a in 1:nrow(y_dat4)){
   
   yl <- y_dat4[a,]
   
   j <- yl$site_n 
   t <- yl$year_n
   k <- yl$interval_n
   
   yyy3[j,t,k] <- yl$bird_detec
   
}
# check
(yyy3 %>% sum(na.rm = T)) == (y_dat4$bird_detec %>% sum(na.rm = T))

# covariates
xxx1 <- xxx2 <- xxx3 <- xxx4 <- xxx5 <- xxx6 <- 
  array(0, 
        dim = c(max(nsite_pk),
                length(years)
        ),
        dimnames = list(site_vec,
                        years
        ))

xxx7 <- xxx8 <-
  array(0, 
        dim = c(max(nsite_pk),
                length(years), 
                ninterval
        ),
        dimnames = list(site_vec,
                        years,
                        seq(1,10,1)
        ))

for(a in 1:nrow(X10)){
  
  Xl <- X10[a,]
  
  jj <- Xl$site_n 
  tt <- Xl$year_n

  xxx1[jj,tt] <- Xl$siteBA_s
  xxx2[jj,tt] <- Xl$siteDEN_s
  xxx3[jj,tt] <- Xl$parkBA_s
  xxx4[jj,tt] <- Xl$parkDEN_s
  xxx5[jj,tt] <- Xl$counBA_s
  xxx6[jj,tt] <- Xl$counDEN_s

}

for(b in 1:nrow(X10)){
  
  Xl <- X10[b,]
  
  jj <- Xl$site_n 
  tt <- Xl$year_n
  kk <- Xl$interval_n
  
  xxx7[jj,tt,kk] <- Xl$date_jul
  xxx8[jj,tt,kk] <- Xl$time_jul
}

# check 
round((X10$siteBA_s %>% sum(na.rm = T)),2) - round((xxx1 %>% sum(na.rm = T)),3)*10 < 0.0001
round((X10$siteDEN_s %>% sum(na.rm = T)),5) - round((xxx2 %>% sum(na.rm = T)),5)*10 < 0.0001
round((X10$parkBA_s %>% sum(na.rm = T)),5) - round((xxx3 %>% sum(na.rm = T)),5)*10 < 0.0001
round((X10$parkDEN_s %>% sum(na.rm = T)),5) - round((xxx4 %>% sum(na.rm = T)),5)*10 < 0.0001
round((X10$counBA_s %>% sum(na.rm = T)),5) - round((xxx5 %>% sum(na.rm = T)),5)*10 < 0.0001
round((X10$counDEN_s %>% sum(na.rm = T)),5) - round((xxx6 %>% sum(na.rm = T)),5)*10 < 0.0001
round((X10$date_jul %>% sum(na.rm = T)),5) == round((xxx7 %>% sum(na.rm = T)),5)
round((X10$time_jul %>% sum(na.rm = T)),5) == round((xxx8 %>% sum(na.rm = T)),5)

dim(xs1 <- simplify2array(list(xxx1, xxx3, xxx5)))
# right now, covariates are the same for every year, so:
             xs1 <- xs1[,11,]   # change in the future ################

xs2 <- simplify2array(list(xxx2, xxx4, xxx6))
# right now, covariates are the same for every year, so:
             xs2 <- xs2[,11,]   # change in the future ################

# initial values
Zst <- apply(yyy3, c(1, 2), function(x) ifelse(all(is.na(x)), NA, max(x, na.rm = TRUE)))

str(jags.data <- list(y = yyy3,                    # bird detection array
                      xs2 = xs2,
                      xxx7 = xxx7,
                      xxx8 = xxx8,
                      n_siteM = dim(yyy3)[1],
                      n_yrsM = dim(yyy3)[2],
                      n_intM = dim(yyy3)[3]
))


inits <- function()list(Z = Zst,#, beta0 = rnorm(10,0.6), 
                        beta2 = rnorm(0,0.6))

niterations <- 4000
burnin <- 1000
nchains <- 3
print(niterations)

cat("\n\n\n running jags \n\n\n\n")
params <- c("beta0", "beta2", 
            "alpha0", "alpha1", "alpha2", "alpha3", 
            "mu.beta0",  
            "scales_beta2")

## initialize JAGS
jags_model <- rjags::jags.model(
  file = "models/mod_12_late_rdc9_scales2.txt",
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
    thin = 3
  )
}

# write_rds(jags_model,
#           file = glue("data/model_res/M12.rds"))

cat("\n\n\n second done \n\n\n\n")

# posterior simulation
samples_jags <- coda.samples(
  #samples_jags <- rjags::jags.samples(
  jags_model,
  variable.names = params,
  n.iter = niterations,
  thin = 3
)

cat("\n\n\n third done \n\n\n\n")

# write_rds(samples_jags,
#           file = glue("data/model_res/M07.rds"))

library(MCMCvis)
MCMCsummary(samples_jags,
            # params = 'alpha',
            round = 2)

MCMCtrace(samples_jags,
          params = params,
          ind = TRUE,
          pdf = FALSE)

MCMCplot(samples_jags,
         # params = 'beta',
         ref_ovl = TRUE)

# scale selection plots and objects:
sca_beta1 <- MCMCchains(samples_jags, params = 'scales_beta2')
selected_scales = rep(NA, 1)
for (i in 1:n_bs) {
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

## for several species and one park ------------------------------------------------------------------------------------------
y_dat4 <- y_dat6
X10 <- X11

nsite_pk <- read_rds("data/out/nsite_pk.rds")[1]  #change############################
sps_pk_nth <- read_rds(file = "data/sps_pk_nth.rds")

nrow(X10) == nrow(y_dat6)

years <- y_dat4 %>% 
  select(Year) %>% 
  distinct() %>% 
  arrange() %>% 
  pull()

X10$sps_it <- y_dat4$sps_it

X10$spskey <- y_dat4$spskey

y_select_sps <- y_dat4 %>% filter(bird_detec>0, park == "ACAD") #change############################
sps_tab <- table(y_select_sps$spskey, y_select_sps$site_n) 

sps_sel <- c(7, 12, 14, 18, 26, 27, 63, 43, 40, 125, 134, 112) %>% sort() #change############################

y_dat4 <- y_dat4 %>% 
  filter(parkey == 1,
         spskey %in% sps_sel)

X10 <- X10 %>% 
  filter(park == "ACAD",
         spskey %in% sps_sel)
dim(X10)
dim(y_dat4)

sps_pk_nth <- read_rds(file = "data/sps_pk_nth.rds") %>% 
  filter(Admin_Unit_Code == "ACAD") #change############################

# make sure all my covariates are the same dimentions as
(n_spsM <- y_dat4$sps_it %>% unique() %>% length())
n_spsM == length(sps_sel)
sps_list <- y_dat4$sps_it %>% unique() %>% sort()

(n_pkM <- y_dat4$park %>% unique() %>% length())

(nyears <- length(years))

n_bs <- 3 #change############################
n_as <- 3 #change############################

table(is.na(X10$siteBA_s))
table(is.na(X10$siteDEN_s))
table(is.na(X10$parkBA_s))
table(is.na(X10$parkDEN_s))
table(is.na(X10$counBA_s))
table(is.na(X10$counDEN_s))
table(is.na(X10$date_jul))
table(is.na(X10$time_jul))

nsite_pk <- read_rds("data/out/nsite_pk.rds")[1] #change############################

ninterval <- 10

site_vec <- seq(1,max(nsite_pk),1)

# detections
yyy3 <- 
  array(NA, 
        dim = c(n_spsM,
                max(nsite_pk),
                length(years), 
                ninterval
        ),
        dimnames = list(sps_list,
                        site_vec,
                        years,
                        seq(1,10,1)
        ))

for(a in 1:nrow(y_dat4)){
  
  yl <- y_dat4[a,]
  
  i <- which(sps_list == yl$sps_it, arr.ind = T)
  j <- yl$site_n 
  t <- yl$year_n
  k <- yl$interval_n
  
  yyy3[i,j,t,k] <- yl$bird_detec
  
}

# check
yyy3 %>% sum(na.rm = T)
y_dat4$bird_detec %>% sum(na.rm = T)

# covariates
xxx1 <- xxx2 <- xxx3 <- xxx4 <- xxx5 <- xxx6 <- 
  array(0, 
        dim = c(n_spsM,
                max(nsite_pk),
                length(years)
        ),
        dimnames = list(sps_list,
                        site_vec,
                        years
        ))

xxx7 <- xxx8 <-  array(0, 
                       dim = c(n_spsM,
                               max(nsite_pk),
                               length(years), 
                               ninterval
                       ),
                       dimnames = list(sps_list,
                                       site_vec,
                                       years,
                                       seq(1,10,1)
                       ))

for(a in 1:nrow(X10)){
  
  Xl <- X10[a,]
  
  jj <- Xl$site_n 
  tt <- Xl$year_n

  xxx1[,jj,tt] <- Xl$siteBA_s
  xxx2[,jj,tt] <- Xl$siteDEN_s
  xxx3[,jj,tt] <- Xl$parkBA_s
  xxx4[,jj,tt] <- Xl$parkDEN_s
  xxx5[,jj,tt] <- Xl$counBA_s
  xxx6[,jj,tt] <- Xl$counDEN_s

}

for(b in 1:nrow(X10)){
  
  Xl <- X10[b,]
  
  jj <- Xl$site_n 
  tt <- Xl$year_n
  kk <- Xl$interval_n
  
  xxx7[,jj,tt,kk] <- Xl$date_jul
  xxx8[,jj,tt,kk] <- Xl$time_jul
}

xs1 <- simplify2array(list(xxx1, xxx3, xxx5))
xs2 <- simplify2array(list(xxx2, xxx4, xxx6))

# initial values
Zst <- apply(yyy3, c(1, 2, 3), function(x) ifelse(all(is.na(x)), NA, max(x, na.rm = TRUE)))

str(jags.data <- list(y = yyy3,                    # bird detection array
                      xs1 = xs1,
                      xs2 = xs2,
                      xxx7 = xxx7,
                      xxx8 = xxx8,
                      n_spsM = dim(yyy3)[1],
                      n_siteM = dim(yyy3)[2],
                      n_yrsM = dim(yyy3)[3],
                      n_intM = dim(yyy3)[4]
))

inits <- function()list(Z = Zst#, beta0 = rnorm(10,0.6), beta1 = rnorm(10,0.6)
)

niterations <- 20
burnin <- 5
nchains <- 2
print(niterations)

cat("\n\n\n running jags \n\n\n\n")
params <- c("beta0", "beta2",
            "alpha0", "alpha1", "alpha2", "alpha3", 
            "mu.beta0",  
            "mu.alpha0",
            "scales_beta2")

# save workspace for running model in the HPCC
# save.image(file = "/Volumes/home-207/bamaral/NPS_birds/data_14sps_ACAD.RData")

## initialize JAGS
jags_model <- rjags::jags.model(
  file = "models/mod_12_late_rdc9_scales3.txt",
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
    thin = 3
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
  thin = 3
)

cat("\n\n\n third done \n\n\n\n")

# write_rds(samples_jags,
#           file = glue("data/model_res/M07.rds"))

# y %>% as_tibble() %>% 
#    filter(
#       #bird_detec == 1,
#       #spskey == 1,
#       parkey == 1,
#       site_n == 1,
#       #year_s == 1,
#       #interval_n == 1,
#       year_n == 1,
#       spskey_p == 11
#    )
# 

# p = spskey_p, site_n, year_n, interval_n

library(MCMCvis)

MCMCsummary(samples_jags,
            params = params,
            round = 2)

MCMCtrace(samples_jags,
          params = params,
          ind = TRUE,
          pdf = FALSE)

par(mfrow = c(1,1))
MCMCplot(samples_jags,
         params = params,
         ref_ovl = TRUE)

# scale selection plots and objects:

sca_beta1 <- MCMCchains(jags_model, params = 'scales_beta2')
ncovs <- 1
selected_scales = rep(NA, 1)
for (i in 1:ncovs) {
  tb_mcmc_scales_i = table(sca_beta1)
  
  selected_scales[i] = as.integer(names(which.max(tb_mcmc_scales_i)))
}

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
  scale_x_continuous(limits = c(0, 6))

## for one species and several parks ------------------------------------------------------------------------------------------
y_dat4 <- y_dat6
X10 <- X11

nsite_pk <- read_rds("data/out/nsite_pk.rds")  
sps_pk_nth <- read_rds(file = "data/sps_pk_nth.rds")

nrow(X10) == nrow(y_dat6)

years <- y_dat4 %>% 
  select(Year) %>% 
  distinct() %>% 
  arrange() %>% 
  pull() %>% 
  sort()

X10$sps_it <- y_dat4$spskey_p

X10$spskey <- y_dat4$spskey

X10$year_n <- y_dat4$Year - (min(X10$Year) + 1)

y_select_sps <- y_dat4 %>% filter(bird_detec>0)
sps_tab <- table(y_select_sps$spskey, y_select_sps$Admin_Unit_Code) 

sps_sel <- c(4) %>% sort() #change############################

y_dat4 <- y_dat4 %>% 
  filter(spskey %in% sps_sel)

X10 <- X10 %>% 
  filter(spskey %in% sps_sel)

dim(X10)
dim(y_dat4)

# make sure all my covariates are the same dimentions as
(n_spsM <- y_dat4$spskey %>% unique() %>% length())
n_spsM == length(sps_sel)
sps_list <- y_dat4$spskey %>% unique() %>% sort()

(n_pkM <- y_dat4$park %>% unique() %>% length())
(pk <- y_dat4$parkey %>% unique() %>% as.numeric() %>% sort())
pk_seq <- seq(1,11,1)
(mis_pk <- pk_seq[which(pk_seq %!in% pk, arr.ind = T)])

if(length(mis_pk) != 0) {
  
  if(length(mis_pk) == 1) {
  
    for(i in 1:nrow(X10)){
      if(as.numeric(X10$parkey[i]) >= mis_pk) {
        X10$parkey[i] <- as.character(as.numeric(X10$parkey[i]) - 1)
      }
    }
    
    for(i in 1:nrow(y_dat4)){
      if(as.numeric(y_dat4$parkey[i]) >= mis_pk) {
        y_dat4$parkey[i] <- as.character(as.numeric(y_dat4$parkey[i]) - 1)
      }
    }
    
  } else { stop (" several missing parks!!!")}
}

(nyears <- length(years))

n_bs <- 4 #change############################
n_as <- 3 #change############################

table(is.na(X10$siteBA_s))
table(is.na(X10$siteDEN_s))
table(is.na(X10$parkBA_s))
table(is.na(X10$parkDEN_s))
table(is.na(X10$counBA_s))
table(is.na(X10$counDEN_s))
table(is.na(X10$date_jul))
table(is.na(X10$time_jul))

ninterval <- 10

site_vec <- seq(1,max(nsite_pk),1)

npk <- length(pk)

yyy3 <- 
  array(NA, 
        dim = c(npk,
                max(nsite_pk),
                length(years), 
                ninterval
        ),
        dimnames = list(pk,
                        site_vec,
                        years,
                        seq(1,10,1)
        ))

for(a in 1:nrow(y_dat4)){
  
  yl <- y_dat4[a,]
  
  r <- as.numeric(yl$parkey)
  j <- yl$site_n 
  t <- yl$year_n
  k <- yl$interval_n
  
  yyy3[r,j,t,k] <- yl$bird_detec
  
}

# check
yyy3 %>% sum(na.rm = T)
y_dat4$bird_detec %>% sum(na.rm = T)

# covariates 
xxx1 <- xxx2 <- xxx3 <- xxx4 <- xxx5 <- xxx6 <- 
  array(0, 
        dim = c(npk,
                max(nsite_pk),
                length(years)
        ),
        dimnames = list(pk,
                        site_vec,
                        years
        ))

xxx7 <- xxx8 <-
  array(0, 
        dim = c(npk,
                max(nsite_pk),
                length(years), 
                ninterval
        ),
        dimnames = list(pk,
                        site_vec,
                        years,
                        seq(1,10,1)
        ))

for(a in 1:nrow(X10)){
  
  Xl <- X10[a,]
  
  rr <- as.numeric(Xl$parkey)
  jj <- Xl$site_n 
  tt <- Xl$year_n

  xxx1[rr,jj,tt] <- Xl$siteBA_s
  xxx2[rr,jj,tt] <- Xl$siteDEN_s
  xxx3[rr,jj,tt] <- Xl$parkBA_s
  xxx4[rr,jj,tt] <- Xl$parkDEN_s
  xxx5[rr,jj,tt] <- Xl$counBA_s
  xxx6[rr,jj,tt] <- Xl$counDEN_s

}

for(b in 1:nrow(X10)){
  
  Xl <- X10[b,]
  
  rr <- as.numeric(Xl$parkey)
  jj <- Xl$site_n 
  tt <- Xl$year_n
  kk <- Xl$interval_n
  
  xxx7[rr,jj,tt,kk] <- Xl$date_jul
  xxx8[rr,jj,tt,kk] <- Xl$time_jul
}

# check 
round((X10$siteBA_s %>% sum(na.rm = T)),2) - round((xxx1 %>% sum(na.rm = T)),3)*10 < 0.0001
round((X10$siteDEN_s %>% sum(na.rm = T)),5) - round((xxx2 %>% sum(na.rm = T)),5)*10 < 0.0001
round((X10$parkBA_s %>% sum(na.rm = T)),5) - round((xxx3 %>% sum(na.rm = T)),5)*10 < 0.0001
round((X10$parkDEN_s %>% sum(na.rm = T)),5) - round((xxx4 %>% sum(na.rm = T)),5)*10 < 0.0001
round((X10$counBA_s %>% sum(na.rm = T)),5) - round((xxx5 %>% sum(na.rm = T)),5)*10 < 0.0001
round((X10$counDEN_s %>% sum(na.rm = T)),5) - round((xxx6 %>% sum(na.rm = T)),5)*10 < 0.0001
round((X10$date_jul %>% sum(na.rm = T)),5) == round((xxx7 %>% sum(na.rm = T)),5)
round((X10$time_jul %>% sum(na.rm = T)),5) == round((xxx8 %>% sum(na.rm = T)),5)

xs1 <- simplify2array(list(xxx1, xxx3, xxx5))
xs2 <- simplify2array(list(xxx2, xxx4, xxx6))

# select only 2018
xs1 <- xs1[,,13,]
xs2 <- xs2[,,13,]

park_size <- NA

(pk2 <- y_dat4$park %>% unique() %>% sort())

for(i in 1:length(pk)) {
  pb <- read_rds(file = glue("data/park_raster/{pk2[i]}_pb.rds"))
  park_size[i] <- raster::area(pb)   # square km
}

if(length(park_size) > 1) {
  park_size <- park_size %>% scale() %>% as.numeric()
}
# check
yyy3 %>% sum(na.rm = T)
y_dat4$bird_detec %>% sum(na.rm = T)

# initial values
Zst <- apply(yyy3, c(1, 2, 3), function(x) ifelse(all(is.na(x)), NA, max(x, na.rm = TRUE)))

str(jags.data <- list(y = yyy3,    # bird detection array
                      xs1 = xs1,                
                      xs2 = xs2,
                      park_size = park_size,
                      xxx7 = xxx7,
                      xxx8 = xxx8,
                      n_pkM = dim(yyy3)[1],
                      n_siteM = dim(yyy3)[2],
                      n_yrsM = dim(yyy3)[3],
                      n_intM = dim(yyy3)[4]
))

inits <- function()list(Z = Zst#, beta0 = rnorm(10,0.6), beta1 = rnorm(10,0.6)
)

niterations <- 50
burnin <- 10
nchains <- 1
print(niterations)

cat("\n\n\n running jags \n\n\n\n")
params <- c("beta0", "beta1", "beta2", "beta3",
            "alpha0", "alpha1", "alpha2", "alpha3", 
            "mu.beta0",  
            "mu.alpha0",
            "scales_beta1","scales_beta2",
            "Z")

# save workspace for running model in the HPCC
#save.image(file = "/Volumes/home-207/bamaral/NPS_birds/data_14sps_ACAD.RData")

## initialize JAGS
jags_model <- rjags::jags.model(
  file = 
  "models/mod_1_vector1spsparks_simple_covs_scales3D.txt",
  #"models/mod_12_late_rdc9_scales4.txt",
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

# write_rds(samples_jags,
#           file = glue("data/model_res/1spsmultipark_scales4_2.rds"))

# y %>% as_tibble() %>% 
#    filter(
#       #bird_detec == 1,
#       #spskey == 1,
#       parkey == 1,
#       site_n == 1,
#       #year_s == 1,
#       #interval_n == 1,
#       year_n == 1,
#       spskey_p == 11
#    )
# 

# p = spskey_p, site_n, year_n, interval_n

# samples_jags <- read_rds(file = glue("data/model_res/1spsmultipark_scales4.rds"))

library(MCMCvis)

MCMCsummary(samples_jags2,
            params = c("mu.beta0",  "beta2",
                       "mu.alpha0", "alpha1", "alpha2", "alpha3",
                       "scales_beta2"),
            round = 2)

MCMCtrace(samples_jags2,
          params = c("mu.beta0",  "beta2", 
                     "mu.alpha0", "alpha1", "alpha2", "alpha3",
                     "scales_beta2"),
          ind = TRUE,
          pdf = FALSE)

par(mfrow = c(1,1))
MCMCplot(samples_jags2,
         params = c("mu.beta0",  "beta2", 
                    "mu.alpha0", "alpha1", "alpha2", "alpha3",
                    "scales_beta2"),
         ref_ovl = TRUE)

# scale selection plots and objects:

sca_beta1 <- MCMCchains(samples_jags, params = 'scales_beta2')
ncovs <- 1
selected_scales = rep(NA, 1)
for (i in 1:ncovs) {
  tb_mcmc_scales_i = table(sca_beta1)
  
  selected_scales[i] = as.integer(names(which.max(tb_mcmc_scales_i)))
}

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
  scale_x_continuous(limits = c(0, 6))

## for several species and several parks ------------------------------------------------------------------------------------------
y_dat4 <- y_dat6
X10 <- X11

nsite_pk <- read_rds("data/out/nsite_pk.rds")  
sps_pk_nth <- read_rds(file = "data/sps_pk_nth.rds")

nrow(X10) == nrow(y_dat6)

years <- y_dat4 %>% 
  select(Year) %>% 
  distinct() %>% 
  arrange() %>% 
  pull()

X10$sps_it <- y_dat4$sps_it

X10$spskey <- y_dat4$spskey

y_select_sps <- y_dat4 %>% filter(bird_detec>0)
sps_tab <- table(y_select_sps$spskey, y_select_sps$Admin_Unit_Code) 

sps_sel <- c(7, 12, 14, 18, 26, 27, 63, 43, 40, 125, 134, 112) %>% sort() #change#########################

y_dat4 <- y_dat4 %>% 
  filter(spskey %in% sps_sel)

X10 <- X10 %>% 
  filter(spskey %in% sps_sel)

dim(X10)
dim(y_dat4)

## change the spskey numbers to aqccount for the subset of species that was removed
y_dat4 <- y_dat4 %>% 
  select(-spskey_p) %>% 
  select(-spskey)
X10 <- X10 %>% 
  select(-spskey) %>% 
  mutate(Admin_Unit_Code = park)

spscodeskey <- y_dat4 %>% 
  select(Admin_Unit_Code, sps_it) %>% 
  arrange(Admin_Unit_Code, sps_it) %>% 
  distinct() %>% 
  group_by(Admin_Unit_Code) %>% 
  mutate(spskey_p = row_number()) %>% 
  ungroup()

spscodeskey_unique <- spscodeskey %>% 
  select(sps_it) %>% 
  distinct() %>% 
  arrange(sps_it) %>% 
  mutate(spskey = row_number()) 

spscodeskey <- spscodeskey %>% 
  left_join(spscodeskey_unique, by = "sps_it")

y_dat4 <- y_dat4 %>% 
  left_join(spscodeskey, by = c("sps_it", "Admin_Unit_Code"))
X10 <- X10 %>% 
  left_join(spscodeskey, by = c("sps_it", "Admin_Unit_Code"))

dim(X10)
dim(y_dat4)

# make sure all my covariates are the same dimentions as
(n_spsM <- y_dat4$sps_it %>% unique() %>% length())
n_spsM == length(sps_sel)
(sps_list <- y_dat4$sps_it %>% unique() %>% sort())

(n_pkM <- y_dat4$park %>% unique() %>% length())
(pk <- y_dat4$parkey %>% unique() %>% as.numeric() %>% sort())
pk_seq <- seq(1,11,1)
(mis_pk <- pk_seq[which(pk_seq %!in% pk, arr.ind = T)])

if(length(mis_pk) != 0) {
  
  if(length(mis_pk) == 1) {
    
    for(i in 1:nrow(X10)){
      if(as.numeric(X10$parkey[i]) >= mis_pk) {
        X10$parkey[i] <- as.character(as.numeric(X10$parkey[i]) - 1)
      }
    }
    
    for(i in 1:nrow(y_dat4)){
      if(as.numeric(y_dat4$parkey[i]) >= mis_pk) {
        y_dat4$parkey[i] <- as.character(as.numeric(y_dat4$parkey[i]) - 1)
      }
    }
    
  } else { stop (" several missing parks!!!")}
}

(nyears <- length(years))

n_bs <- 4 #change############################
n_as <- 3 #change############################

table(is.na(X10$siteBA_s))
table(is.na(X10$siteDEN_s))
table(is.na(X10$parkBA_s))
table(is.na(X10$parkDEN_s))
table(is.na(X10$counBA_s))
table(is.na(X10$counDEN_s))
table(is.na(X10$date_jul))
table(is.na(X10$time_jul))

ninterval <- 10

site_vec <- seq(1,max(nsite_pk),1)

npk <- length(pk)

yyy3 <- 
  array(NA, 
        dim = c(npk,
                n_spsM,
                max(nsite_pk),
                length(years), 
                ninterval
        ),
        dimnames = list(pk,
                        sps_list,
                        site_vec,
                        years,
                        seq(1,10,1)
        ))

for(a in 1:nrow(y_dat4)){
  
  yl <- y_dat4[a,]
  
  r <- as.numeric(yl$parkey)
  i <- yl$spskey
  j <- yl$site_n 
  t <- yl$year_n
  k <- yl$interval_n
  
  yyy3[r,i,j,t,k] <- yl$bird_detec
  
}

# check
yyy3 %>% sum(na.rm = T)
y_dat4$bird_detec %>% sum(na.rm = T)

# covariates 
xxx1 <- xxx2 <- xxx3 <- xxx4 <- xxx5 <- xxx6 <- 
  array(0, 
        dim = c(npk,
                n_spsM,
                max(nsite_pk),
                length(years)
        ),
        dimnames = list(pk,
                        sps_list,
                        site_vec,
                        years
        ))

xxx7 <- xxx8 <-
  array(0, 
        dim = c(npk,
                n_spsM,
                max(nsite_pk),
                length(years), 
                ninterval
        ),
        dimnames = list(pk,
                        sps_list,
                        site_vec,
                        years,
                        seq(1,10,1)
        ))

for(a in 1:nrow(X10)){
  
  Xl <- X10[a,]
  
  rr <- as.numeric(Xl$parkey)
  ii <- Xl$spskey
  jj <- Xl$site_n 
  tt <- Xl$year_n
  
  xxx1[rr,ii,jj,tt] <- Xl$siteBA_s
  xxx2[rr,ii,jj,tt] <- Xl$siteDEN_s
  xxx3[rr,ii,jj,tt] <- Xl$parkBA_s
  xxx4[rr,ii,jj,tt] <- Xl$parkDEN_s
  xxx5[rr,ii,jj,tt] <- Xl$counBA_s
  xxx6[rr,ii,jj,tt] <- Xl$counDEN_s
  
}

for(b in 1:nrow(X10)){
  
  Xl <- X10[b,]
  
  rr <- as.numeric(Xl$parkey)
  ii <- Xl$spskey
  jj <- Xl$site_n 
  tt <- Xl$year_n
  kk <- Xl$interval_n
  
  xxx7[rr,ii,jj,tt,kk] <- Xl$date_jul
  xxx8[rr,ii,jj,tt,kk] <- Xl$time_jul
}

# check 
round((X10$siteBA_s %>% sum(na.rm = T)),2) - round((xxx1 %>% sum(na.rm = T)),3)*10 < 0.0001
round((X10$siteDEN_s %>% sum(na.rm = T)),5) - round((xxx2 %>% sum(na.rm = T)),5)*10 < 0.0001
round((X10$parkBA_s %>% sum(na.rm = T)),5) - round((xxx3 %>% sum(na.rm = T)),5)*10 < 0.0001
round((X10$parkDEN_s %>% sum(na.rm = T)),5) - round((xxx4 %>% sum(na.rm = T)),5)*10 < 0.0001
round((X10$counBA_s %>% sum(na.rm = T)),5) - round((xxx5 %>% sum(na.rm = T)),5)*10 < 0.0001
round((X10$counDEN_s %>% sum(na.rm = T)),5) - round((xxx6 %>% sum(na.rm = T)),5)*10 < 0.0001
round((X10$date_jul %>% sum(na.rm = T)),5) == round((xxx7 %>% sum(na.rm = T)),5)
round((X10$time_jul %>% sum(na.rm = T)),5) == round((xxx8 %>% sum(na.rm = T)),5)

xs1 <- simplify2array(list(xxx1, xxx3, xxx5))
xs2 <- simplify2array(list(xxx2, xxx4, xxx6))

park_size <- NA

(pk2 <- y_dat4$park %>% unique() %>% sort())

for(i in 1:length(pk)) {
  pb <- read_rds(file = glue("data/park_raster/{pk2[i]}_pb.rds"))
  park_size[i] <- raster::area(pb)   # square km
}

if(length(park_size) > 1) {
  park_size <- park_size %>% scale() %>% as.numeric()
}

# check once again
yyy3 %>% sum(na.rm = T)
y_dat4$bird_detec %>% sum(na.rm = T)

# initial values
Zst <- apply(yyy3, c(1, 2, 3, 4), function(x) ifelse(all(is.na(x)), NA, max(x, na.rm = TRUE)))

str(jags.data <- list(y = yyy3,                    # bird detection array
                      xs2 = xs2,
                      xxx7 = xxx7,
                      xxx8 = xxx8,
                      n_pkM = dim(yyy3)[1],
                      n_spsM = dim(yyy3)[2],
                      n_siteM = dim(yyy3)[3],
                      n_yrsM = dim(yyy3)[4],
                      n_intM = dim(yyy3)[5]))

inits <- function()list(Z = Zst#, beta0 = rnorm(10,0.6), beta1 = rnorm(10,0.6)
)

niterations <- 5
burnin <- 1
nchains <- 1
print(niterations)

cat("\n\n\n running jags \n\n\n\n")
params <- c("beta0", "beta1", "beta2",
            "alpha0", "alpha1", "alpha2", "alpha3", 
            "mu.beta0",  
            "mu.alpha0",
            "scales_beta2")

# save workspace for running model in the HPCC
# save.image(file = "/Volumes/home-207/bamaral/NPS_birds/data_14sps_parks.RData")

## initialize JAGS
jags_model <- rjags::jags.model(
  file = "models/mod_12_late_rdc9_scales5.txt",
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

# write_rds(samples_jags,
#           file = glue("data/model_res/1spsmultipark_scales4.rds"))

# y %>% as_tibble() %>% 
#    filter(
#       #bird_detec == 1,
#       #spskey == 1,
#       parkey == 1,
#       site_n == 1,
#       #year_s == 1,
#       #interval_n == 1,
#       year_n == 1,
#       spskey_p == 11
#    )
# 

# p = spskey_p, site_n, year_n, interval_n

library(MCMCvis)

MCMCsummary(samples_jags,
            params = params,
            round = 2)

MCMCtrace(samples_jags,
          params = params,
          ind = TRUE,
          pdf = FALSE)

par(mfrow = c(1,1))
MCMCplot(samples_jags,
         params = params,
         ref_ovl = TRUE)

# scale selection plots and objects:

sca_beta1 <- MCMCchains(samples_jags, params = 'scales_beta2')
ncovs <- 1
selected_scales = rep(NA, 1)
for (i in 1:ncovs) {
  tb_mcmc_scales_i = table(sca_beta1)
  
  selected_scales[i] = as.integer(names(which.max(tb_mcmc_scales_i)))
}

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
  scale_x_continuous(limits = c(0, 6))

