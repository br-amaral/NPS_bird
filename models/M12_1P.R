
y <- yyy3[1,,,]

## occupancy BLISS model -----------------------------------------	
str(jags.data <- list(y = y,                    # bird detection array
                      n_yrsM = dim(y)[2],         # number of parks
                      nsiteM = dim(y)[1],       # number of sites
                      nintervalM = dim(y)[3]#, 	 # number of intervals       
                      #day = day,                   # calendar day
                      #time = time#,                 # time of day
                      #park_size = park_size,       # park area
                      #for_s = for_s,               # forest cover in site
                      #for_p = for_p                # forest cover in park
))	

# Initial values
Zst <- apply(y, c(1,2), max, na.rm = TRUE)
Zst[Zst == '-Inf'] <- 0         

inits <- function()list(Z = Zst#, beta0 = rnorm(10,0.6), beta1 = rnorm(10,0.6)
)

# Reverse jump (?) ----------------------------------------------------------

print(niterations)

cat("\n\n\n running jags \n\n\n\n")

## initialize JAGS
jags_model <- rjags::jags.model(
  file = "models/mod_12_1p.txt",
  data = jags.data,
  inits = inits,
  n.chains = nchains,
  n.adapt = max(100, ceiling(.1 * niterations)),
  quiet = FALSE
)

if (burnin > 0) {
  message(paste("burn-in:", burnin, "iterations"))
  rjags::jags.samples(
    jags_model,
    variable.names = c("alpha0"),
    n.iter = niterations,
    thin = 3
  )
}

samples_jags <- coda.samples(
  #samples_jags <- rjags::jags.samples(
  jags_model,
  variable.names = c("mu.beta0", 
                     "alpha0"
  ),
  n.iter = niterations,
  thin = 3
)

library(MCMCvis)
MCMCtrace(samples_jags,
          ind = TRUE,
          pdf = FALSE)

########################### several betas ------------------
for_s2 <- for_s[1,,] %>% t()

Zst <- apply(y, c(1,2), max, na.rm = TRUE)
Zst[Zst == '-Inf'] <- 0
Zst <- Zst[1:nsite_pk[1], 1:nyr_pk[1]]

inits <- function()list(Z = Zst#, beta0 = rnorm(10,0.6), beta1 = rnorm(10,0.6)
)

str(jags.data <- list(y = y,                    # bird detection array
                      n_yrsM = nyr_pk[1],         # number of parks
                      nsiteM = nsite_pk[1],       # number of sites
                      nintervalM = dim(y)[3], 	 # number of intervals       
                      #day = day,                   # calendar day
                      #time = time#,                 # time of day
                      #park_size = park_size,       # park area
                      for_s = for_s2#,               # forest cover in site
                      #for_p = for_p                # forest cover in park
))	

## initialize JAGS
jags_model <- rjags::jags.model(
  file = "models/mod_12_1p_betas.txt",
  data = jags.data,
  inits = inits,
  n.chains = nchains,
  n.adapt = max(100, ceiling(.1 * niterations)),
  quiet = FALSE
)

if (burnin > 0) {
  message(paste("burn-in:", burnin, "iterations"))
  rjags::jags.samples(
    jags_model,
    variable.names = c("alpha0"),
    n.iter = niterations,
    thin = 3
  )
}

samples_jags <- coda.samples(
  #samples_jags <- rjags::jags.samples(
  jags_model,
  variable.names = c("mu.beta0", 
                     "alpha0"
  ),
  n.iter = niterations,
  thin = 3
)

library(MCMCvis)
MCMCtrace(samples_jags,
          ind = TRUE,
          pdf = FALSE)

##################### add alphas --------------------------------------------
for_s2 <- for_s[1,,] %>% t()

day2 <- day[1,1:nsite_pk[1], 1:nyr_pk[1]]
time2 <- time[1,1:nsite_pk[1], 1:nyr_pk[1],]

Zst <- apply(y, c(1,2), max, na.rm = TRUE)
Zst[Zst == '-Inf'] <- 0
Zst <- Zst[1:nsite_pk[1], 1:nyr_pk[1]]

inits <- function()list(Z = Zst#, beta0 = rnorm(10,0.6), beta1 = rnorm(10,0.6)
)

str(jags.data <- list(y = y,                    # bird detection array
                      n_yrsM = nyr_pk[1],         # number of parks
                      nsiteM = nsite_pk[1],       # number of sites
                      nintervalM = dim(y)[3], 	 # number of intervals       
                      day = day2,                   # calendar day
                      time = time2,                 # time of day
                      #park_size = park_size,       # park area
                      for_s = for_s2#,               # forest cover in site
                      #for_p = for_p                # forest cover in park
))	

## initialize JAGS
jags_model <- rjags::jags.model(
  file = "models/mod_12_1p_betas_alphas.txt",
  data = jags.data,
  inits = inits,
  n.chains = nchains,
  n.adapt = max(100, ceiling(.1 * niterations)),
  quiet = FALSE
)

jags_model <- rjags::jags.model(
  file = "models/mod_12_1p_betas_alphas_nobeta0time.txt",
  data = jags.data,
  inits = inits,
  n.chains = nchains,
  n.adapt = max(100, ceiling(.1 * niterations)),
  quiet = FALSE
)


if (burnin > 0) {
  message(paste("burn-in:", burnin, "iterations"))
  rjags::jags.samples(
    jags_model,
    variable.names = c("alpha0"),
    n.iter = niterations,
    thin = 3
  )
}

samples_jags <- coda.samples(
  #samples_jags <- rjags::jags.samples(
  jags_model,
  variable.names = c(#"mu.beta0", 
                     "beta1",
                     "beta2",
                     "alpha0",
                     "alpha1",
                     "alpha2",
                     "alpha3"
  ),
  n.iter = niterations,
  thin = 3
)

library(MCMCvis)
MCMCtrace(samples_jags,
          ind = TRUE,
          pdf = FALSE)

################# adding gammas --------------------------------
for_p2 <- for_p[1,]  %>% as.vector()

str(jags.data <- list(y = y,                    # bird detection array
                      n_yrsM = nyr_pk[1],         # number of parks
                      nsiteM = nsite_pk[1],       # number of sites
                      nintervalM = dim(y)[3], 	 # number of intervals       
                      day = day2,                   # calendar day
                      time = time2,                 # time of day
                      park_size = park_size[1],       # park area
                      for_s = for_s2,               # forest cover in site
                      for_p = for_p2                # forest cover in park
))	

## initialize JAGS
jags_model <- rjags::jags.model(
  file = "models/mod_12_1p_betas_alphas_gammas.txt",
  data = jags.data,
  inits = inits,
  n.chains = nchains,
  n.adapt = max(100, ceiling(.1 * niterations)),
  quiet = FALSE
)

if (burnin > 0) {
  message(paste("burn-in:", burnin, "iterations"))
  rjags::jags.samples(
    jags_model,
    variable.names = c("alpha0"),
    n.iter = niterations,
    thin = 3
  )
}

samples_jags <- coda.samples(
  #samples_jags <- rjags::jags.samples(
  jags_model,
  variable.names = c("mu.beta0", 
                     "beta1",
                     "beta2",
                     "alpha0",
                     "alpha1",
                     "alpha2",
                     "alpha3",
                     "gamma0",
                     "gamma1"
  ),
  n.iter = niterations,
  thin = 3
)

library(MCMCvis)
MCMCtrace(samples_jags,
          ind = TRUE,
          pdf = FALSE)


