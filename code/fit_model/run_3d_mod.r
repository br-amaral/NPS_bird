
if(!require(freshr)){install.packages("freshr")}
freshr::freshr()

print(paste0("run_3d_mod.R"))

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

pk <- read_rds("data/src/key_park.rds") %>% 
  select(parks) %>% 
  pull() %>% 
  sort()

sps_pk_nth <- read_rds(file = "data/sps_pk_nth.rds")
## read files
y_dat6 <- read_rds(file = Y_DATA_PATH)

pkey <- y_dat6 %>% 
  select(park, parkey) %>% 
  distinct() %>% 
  arrange()

X11 <- read_rds(file = "data/X10.rds") %>% 
  left_join(., pkey, by = "park")

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

(pk2 <- tolower(y_dat4$park %>% unique() %>% sort()))

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

niterations <- 10000
burnin <- 5000
nchains <- 5
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

(sps_loop <- y_dat4$AOU_Code %>% unique() )

write_rds(samples_jags,
          file = glue("data/model_res/jags_res_{sps_loop}3d.rds"))
