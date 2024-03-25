# MODEL mod_12_alphas0c_betas_gammas.txt ---------------------------------------------------------
spslist <- "WBNU"

source("code/format_bird_data/2_create_data_files.R")

for_s <- for_s[,c(1,5),] 
for_s[is.na(for_s)] <- 0

str(jags.data <- list(y = yyy3,                    # bird detection array
                      n_siteM = nsite_pk,          # number of sites in each park
                      n_yrsM = pk_yrs_le,          # number of years
                      n_intM = dim(yyy3)[4],     	 # number of intervals    
                      n_pkM = length(pk_yrs_le),   # number of parks
                      day = day,                   # calendar day
                      time = time,                 # time of the day
                      for_s = for_s,                # forest cover at the site level
                      park_size = park_size
))

dim(yyy3)
length(nsite_pk)
length(pk_yrs_le)
dim(day)
dim(time)
dim(for_s)
length(park_size)

# Initial values
Zst <- matrix(1, ncol = max(nsite_pk), nrow = length(park_list))

for(i in 1:nrow(Zst)) {
  if(nsite_pk[i] < max(nsite_pk)){
    Zst[i, (nsite_pk[i]+1):max(nsite_pk)]  <- NA
    print(i)
  }
}

for(i in 1:nrow(Zst)) {
  for(j in 1:nsite_pk[i]) {
    if(sum(yyy3[i,j,,], na.rm = T) == 0){
      Zst[i, j] <- 0
      print(i)
    }
  }
}

# JWD: you also need to supply initial values for W[r], since this is a latent 
#      binary parameter. JAGS will try and give it initial values, but it doesn't do
#      a good job, as it doesn't force W[r] to be 1 if any of the Z[r, ] are 1 
#      (which is necessary for the model to work).
# JWD: below, I create Wst, the starting values for W, which takes value 1 if the 
#      species exists at the park, and value 0 if it does not.
Wst <- apply(Zst, 1, max, na.rm = TRUE)
Wst
# JWD: Take a look at the Wst. It's all 1s (aka this species occurs at every park). 
#      The model will probably have a difficult time trying to estimate
#      parameters associated with the park level effects, since there is no variability 
#      (i.e., at the park level, the species occurs everywhere). Let's fit the model below
#      and see what happens.

inits <- function()list(Z = Zst, W = Wst#, beta0 = rnorm(10,0.6), beta1 = rnorm(10,0.6)
)

niterations <- 10000
burnin <- 2000
nchains <- 3
print(niterations)

cat("\n\n\n running jags \n\n\n\n")

## initialize JAGS
jags_model <- rjags::jags.model(
  file = "models/mod_12_alphas0c_betas_gammas.txt",
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


cat("\n\n\n second done \n\n\n\n")

# posterior simulation
samples_jags <- coda.samples(
  #samples_jags <- rjags::jags.samples(
  jags_model,
  variable.names = c("mu.alpha0",
                     "mu.alpha1",
                     "mu.alpha2",
                     "mu.alpha3",
                     "gamma0", 
                     "gamma1",
                     "beta0",
                     "mu.beta0",
                     "beta1",
                     "beta2", 
                     'alpha1', 
                     'alpha2', 
                     'alpha3'),
  n.iter = niterations,
  thin = 3
)

write_rds(samples_jags, 
          file = glue("data/model_res/mod_12_alphas0c_betas_{spslist}.rds"))

cat("\n\n\n third done \n\n\n\n")

MCMCsummary(samples_jags,
            # params = 'alpha',
            round = 2)
# JWD: alright, looks like the model converges! Notice that gamma0 is very large 
#      (plogis(2.54) = 0.92), indicating the species is essentially present at 
#      all parks (which we know to be the case given the data), and the effect 
#      of park size has a very large credible interval that spans 0. This makes 
#      sense: if the species is present at all the parks, it will be difficult
#      for the model to precisely estimate how different park-level covariates
#      relate to occupancy probability. Regardless... this is a great sign that this
#      works and it should extend nicely to a multi-species model! There will also
#      likely be much more heterogeneity for different species across the 8 parks
#      that will make any park-level effects a bit easier to estimate.

MCMCtrace(samples_jags,
          ind = TRUE,
          pdf = FALSE)

par(mfrow = c(1,2))
MCMCplot(samples_jags,
         params = c("mu.alpha0",
                    "mu.alpha1",
                    "mu.alpha2",
                    "mu.alpha3",
                    "mu.gamma0", 
                    "mu.gamma1",
                    "mu.beta0",
                    "mu.beta1"),
         ref_ovl = TRUE,
         main = glue("{spslist}"))

MCMCplot(samples_jags,
         params = c("beta0",
                    'alpha1', 
                    'alpha2', 
                    'alpha3'),
         ref_ovl = TRUE,
         main = glue("{spslist}"))

# MODEL mod_12_alphas0c_betasScales_gammas.txt  -----------------------------
# for_s <- for_s[,c(1,5),] 
# for_s[is.na(for_s)] <- 0

str(jags.data <- list(y = yyy3,                    # bird detection array
                      n_siteM = nsite_pk,          # number of sites in each park
                      n_yrsM = pk_yrs_le,          # number of years
                      n_intM = dim(yyy3)[4],     	 # number of intervals    
                      n_pkM = length(pk_yrs_le),   # number of parks
                      day = day,                   # calendar day
                      time = time,                 # time of the day
                      for_s = for_s,               # forest cover at the site level
                      park_size = park_size
))

# Initial values
Zst <- matrix(1, ncol = max(nsite_pk), nrow = length(park_list))

for(i in 1:nrow(Zst)) {
  if(nsite_pk[i] < max(nsite_pk)){
    Zst[i, (nsite_pk[i]+1):max(nsite_pk)]  <- NA
    print(i)
  }
}

for(i in 1:nrow(Zst)) {
  for(j in 1:nsite_pk[i]) {
    if(sum(yyy3[i,j,,], na.rm = T) == 0){
      Zst[i, j] <- 0
      print(i)
    }
  }
}

Wst <- apply(Zst, 1, max, na.rm = TRUE)
Wst

inits <- function()list(Z = Zst, W = Wst#, beta0 = rnorm(10,0.6), beta1 = rnorm(10,0.6)
)

niterations <- 10000
burnin <- 2000
nchains <- 3
print(niterations)

cat("\n\n\n running jags \n\n\n\n")

## initialize JAGS
jags_model <- rjags::jags.model(
  file = "models/mod_12_alphas0c_betasScales_gammas.txt",
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


cat("\n\n\n second done \n\n\n\n")

# posterior simulation
samples_jags <- coda.samples(
  #samples_jags <- rjags::jags.samples(
  jags_model,
  variable.names = c("mu.alpha0",
                     "mu.alpha1",
                     "mu.alpha2",
                     "mu.alpha3",
                     "gamma0", 
                     "gamma1",
                     "mu.beta0",
                     "beta1",
                     'alpha1', 
                     'alpha2', 
                     'alpha3',
                     'scales_beta1'),
  n.iter = niterations,
  thin = 3
)

write_rds(samples_jags, 
          file = glue("data/model_res/mod_12_alphas0c_betasScales_gammas_{spslist}.rds"))

cat("\n\n\n third done \n\n\n\n")

MCMCsummary(samples_jags,
            # params = 'alpha',
            round = 2)

MCMCtrace(samples_jags,
          ind = TRUE,
          pdf = FALSE)

par(mfrow = c(1,2))
MCMCplot(samples_jags,
         params = c("mu.alpha0",
                    "mu.alpha1",
                    "mu.alpha2",
                    "mu.alpha3",
                    "gamma0", 
                    "gamma1",
                    "mu.beta0",
                    'scales_beta1'),
         ref_ovl = TRUE,
         main = glue("{spslist}"))

MCMCplot(samples_jags,
         params = c("beta0",
                    'alpha1', 
                    'alpha2', 
                    'alpha3'),
         ref_ovl = TRUE,
         main = glue("{spslist}"))

# scale selection plots and objects:
sca_beta1 <- MCMCchains(samples_jags, params = 'scales_beta1')
selected_scales = rep(NA, 1)
for (i in 1:ncovs) {
  tb_mcmc_scales_i = table(sca_beta1)
  
  selected_scales[i] = as.integer(names(which.max(tb_mcmc_scales_i)))
}

sca_beta1 %>% table()
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

# MODEL mod_12_alphas0c_betasScales_gammas_multi.txt  -----------------------------

rm(list = ls(all.names = TRUE))

spslist_g <- read_rds("data/src/guilds.rds") %>% 
  as_tibble()

spslist2 <- spslist_g %>% 
  filter(Response_Guild == "InteriorForestObligate") 

spslist <- spslist2 %>% dplyr::select(AOU_Code)

source("code/create_data_files.R")

for_s <- for_s[,c(1,3,5),] 
for_s[is.na(for_s)] <- 0

str(jags.data <- list(y = yyy3,                    # bird detection array
                      n_spsM = dim(yyy3)[1],
                      n_siteM = nsite_pk,          # number of sites in each park
                      n_yrsM = pk_yrs_le,          # number of years
                      n_intM = dim(yyy3)[5],     	 # number of intervals    
                      n_pkM = length(pk_yrs_le),   # number of parks
                      day = day,                   # calendar day
                      time = time,                 # time of the day
                      for_s = for_s,                # forest cover at the site level
                      park_size = park_size
))

dim(yyy3)
length(nsite_pk)
length(pk_yrs_le)
dim(day)
dim(time)
dim(for_s)
length(park_size)

# Initial values
if((as_tibble(spslist) %>% nrow()) == 1) {
  Zst <- matrix(1, ncol = max(nsite_pk), nrow = length(park_list))
  
  for(i in 1:nrow(Zst)) {
    if(nsite_pk[i] < max(nsite_pk)){
      Zst[i, (nsite_pk[i]+1):max(nsite_pk)]  <- NA
      print(i)
    }
  }
  
  for(i in 1:nrow(Zst)) {
    for(j in 1:nsite_pk[i]) {
      if(sum(yyy3[i,j,,], na.rm = T) == 0){
        Zst[i, j] <- 0
        print(i)
      }
    }
  }
}

if((as_tibble(spslist) %>% nrow()) > 1) {
  Zst <- array(1, 
               dim = c(n_sps,
                       length(park_list),
                       max(nsite_pk)),
               dimnames = list(sps_f_names %>% pull(),
                               park_list,
                               site_vec[1:max(nsite_pk)]))
  
  for(r in 1:dim(Zst)[2]) {
    if(nsite_pk[r] < max(nsite_pk)){
      Zst[, r, (nsite_pk[r]+1):max(nsite_pk)]  <- NA
      print(r)
    }
  }
  
  for(r in 1:dim(Zst)[2]) {
    for(j in 1:nsite_pk[r]) {
      if(sum(yyy3[,r,j,,], na.rm = T) == 0){
        Zst[,r,j] <- 0
        print(r)
      }
    }
  }

}
# JWD: you also need to supply initial values for W[r], since this is a latent 
#      binary parameter. JAGS will try and give it initial values, but it doesn't do
#      a good job, as it doesn't force W[r] to be 1 if any of the Z[r, ] are 1 
#      (which is necessary for the model to work).
# JWD: below, I create Wst, the starting values for W, which takes value 1 if the 
#      species exists at the park, and value 0 if it does not.
Wst <- apply(Zst, c(1,2), max, na.rm = TRUE)
Wst
# JWD: Take a look at the Wst. It's all 1s (aka this species occurs at every park). 
#      The model will probably have a difficult time trying to estimate
#      parameters associated with the park level effects, since there is no variability 
#      (i.e., at the park level, the species occurs everywhere). Let's fit the model below
#      and see what happens.

inits <- function()list(Z = Zst, W = Wst#, beta0 = rnorm(10,0.6), beta1 = rnorm(10,0.6)
)

niterations <- 500
burnin <- 50
nchains <- 3
print(niterations)

cat("\n\n\n running jags \n\n\n\n")

## initialize JAGS
jags_model <- rjags::jags.model(
  file = "models/mod_12_alphas0c_betasScales_gammas_sps.txt",
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


cat("\n\n\n second done \n\n\n\n")

# posterior simulation
samples_jags <- coda.samples(
  #samples_jags <- rjags::jags.samples(
  jags_model,
  variable.names = c("mu.alpha0",
                     "mu.alpha1",
                     "mu.alpha2",
                     "mu.alpha3",
                     "gamma0", 
                     "gamma1",
                     "mu.beta0",
                     "beta1",
                     'alpha1', 
                     'alpha2', 
                     'alpha3',
                     'scales_beta1'),
  n.iter = niterations,
  thin = 3
)

write_rds(samples_jags, 
          file = glue("data/model_res/mod_12_alphas0c_betasScales_gammas_sps.rds"))

cat("\n\n\n third done \n\n\n\n")
library(MCMCvis)
MCMCsummary(samples_jags,
            # params = 'alpha',
            round = 2)

MCMCtrace(samples_jags,
          ind = TRUE,
          pdf = FALSE)

par(mfrow = c(1,2))
MCMCplot(samples_jags,
         params = c("mu.alpha0",
                    "mu.alpha1",
                    "mu.alpha2",
                    "mu.alpha3",
                    "gamma0", 
                    "gamma1",
                    "mu.beta0",
                    'scales_beta1'),
         ref_ovl = TRUE,
         main = glue("{spslist}"))

MCMCplot(samples_jags,
         params = c("beta0",
                    'alpha1', 
                    'alpha2', 
                    'alpha3'),
         ref_ovl = TRUE,
         main = glue("{spslist}"))

# scale selection plots and objects:
sca_beta1 <- MCMCchains(samples_jags, params = 'scales_beta1')
selected_scales = rep(NA, 1)
for (i in 1:ncovs) {
  tb_mcmc_scales_i = table(sca_beta1)
  
  selected_scales[i] = as.integer(names(which.max(tb_mcmc_scales_i)))
}

sca_beta1 %>% table()
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