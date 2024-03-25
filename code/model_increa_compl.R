# Beginning parallel processing using 3 cores. Console output will be suppressed.
# Error in checkForRemoteErrors(val) : 
#   one node produced an error: Error in node beta1[68]
# Slicer stuck at value with infinite density

## try to solve this error - remove beta 1 vriable and see if model works
# fit the model
# Bundle and summarize data set
str(win.data <- list(y = yyy, ninterval = dim(yyy)[4], nsite = dim(yyy)[2],
                     nspec = dim(yyy)[1], nyr_s = dim(yyy)[3], #npk = dim(yyy)[1])
                     day = day, time = time #x1 = fore
))

# Specify model in BUGS language
sink("mod_tro_0.txt")
cat("
model {

  # Community priors (with hyperparameters) for species-specific parameters
  # priors
  for(i in 1:nspec){
    for(t in 1:nyr_s){ 
      beta0[i,t] ~ dnorm(mu.beta0, tau.beta0)      # Abundance intercepts
    }  

    alpha0[i] ~ dnorm(mu.alpha0, tau.alpha0)         # Detection intercepts
    # alpha1[i] ~ dnorm(mu.alpha1, tau.alpha1)         # Detection slope
    # alpha2[i] ~ dnorm(mu.alpha2, tau.alpha2)         # Detection slope
    # alpha3[i] ~ dnorm(mu.alpha3, tau.alpha3)         # Detection slope

    #beta1[i] ~ dnorm(mu.beta1, tau.beta1)            # Detection slope
    #beta2[i] ~ dnorm(mu.beta2, tau.beta2)            # Detection slope

    # r1[i] ~ dunif(0, 50)
    # r2[i] ~ dunif(0, 50)
   }

  # Hyperpriors for community hyperparameters - hey put this in loops.
  # abundance model
  mu.beta0 ~ dnorm(0, 1)
  tau.beta0 <- pow(sd.beta0, -2)
  sd.beta0 ~ dgamma(0.01,0.01) #dunif(0, 10)

  # mu.beta1 ~ dnorm(0, 0.1)
  # tau.beta1 <- pow(sd.beta1, -2)
  # sd.beta1 ~ dgamma(0.01,0.01) #dunif(0, 10)

  # mu.beta2 ~ dnorm(0, 0.1)
  # tau.beta2 <- pow(sd.beta2, -2)
  # sd.beta2 ~ dunif(0, 10)

  # detection model
  mu.alpha0 ~ dnorm(0, 0.1)
  tau.alpha0 <- pow(sd.alpha0, -2)
  sd.alpha0 ~ dgamma(0.01,0.01) #dunif(0, 10)

  # mu.alpha1 ~ dnorm(0, 0.1)
  # tau.alpha1 <- pow(sd.alpha0, -2)
  # sd.alpha1 ~ dgamma(0.01,0.01) #dunif(0, 10)

  # mu.alpha2 ~ dnorm(0, 0.1)
  # tau.alpha2 <- pow(sd.alpha0, -2)
  # sd.alpha2 ~ dgamma(0.01,0.01) #dunif(0, 10)
  # 
  # mu.alpha3 ~ dnorm(0, 0.1)
  # tau.alpha3 <- pow(sd.alpha0, -2)
  # sd.alpha3 ~ dgamma(0.01,0.01) #dunif(0, 10)

  # likelihood
  for(i in 1:nspec){
    for (j in 1:nsite){
      for(t in 1:nyr_s){
      #  for(p in 1:npk){
          # Ecological model for true abundance (process model)
          #N[i,j,t] ~ dnegbin(Q1[i,j,t], r1[i]) 
          #Q1[i,j,t] <- r1[i] / (r1[i] + lambda[i,j,t])
          N[i,j,t] ~ dpois(lambda[i,j,t]) 
          log(lambda[i,j,t]) <- beta0[i,t] #+ beta1[i] * x1[t] #+ beta2[i] * x2[i] 
    
          # Observation model for replicated counts
          logit(p[i,j,t]) <- alpha0[i] #+ alpha1[i] * day[j,t] + alpha2[i] * (day[j,t]^2) + alpha3[i] * time[j,t]
          
          for(k in 1:ninterval){  # intervals
             # Poisson parameter 
             #y[i,j,t,k] ~ dnegbin(Q2[i,j,t,k], r2[i])  ### CHECK THIISSSS!!!!
             #Q2[i,j,t,k] <- r2[i] / (r2[i] + pi[i,j,t,k])
             y[i,j,t,k] ~ dpois(pi[i,j,t,k])
             pi[i,j,t,k] <- (p[i,j,t] * (1 - p[i,j,t])^(k-1)) * lambda[i,j,t]
          # check HOW WELL poisson FITs FITS THE DATA; maybe just poisson is ok.... Poisson gamma mixture – if mixingis your issue!
          }
      # number of each species
      #Nsp[i,t] <- sum(N[i,,t]) # ?? why error
        }

      }
    }
  #}
}
",fill = TRUE)
sink()

# Initial values
some.more <- 5          # May have to play with this until JAGS is happy
Nst <- apply(yyy, c(1,2,3), max, na.rm = TRUE) + some.more
Nst[Nst == '-Inf'] <- 20          # May have to play with this, too
Nst <- Nst
inits <- function()list(N = Nst#,
                        # mu.alpha0 = -2.1,
                        # mu.beta0 = 2.4#, mu.beta1 = -0.02, mu.beta2 = 0.01
)

# OR: use inits at earlier solutions (greatly speeds up convergence)
# pm <- out01$mean     # Pull out posterior means from earlier run
# inits <- function() list(a = ast, N = Nst, alpha0 = rnorm(nspec), beta0 = rnorm(nspec), alpha = matrix(rnorm(n = nspec*3), ncol = 3), beta = matrix(rnorm(n = nspec*3), ncol = 3), mu.beta0 = pm$mu.beta0, sd.beta0 = pm$sd.beta0, mu.beta = pm$mu.beta, sd.beta = pm$sd.beta, mu.alpha0 = pm$mu.alpha0, sd.alpha0 = pm$sd.alpha0, mu.alpha = pm$mu.alpha, sd.alpha = pm$sd.alpha )

# Parameters monitored
params <- c("mu.alpha0", "sd.alpha0",
            "mu.beta0", "sd.beta0", 
            #"mu.beta1", "sd.beta1", 
            "Nsp",
            "lambda", 
            "alpha0",
            "beta0",#, "beta1", "beta2"
            "N"
)

# MCMC settings
ni <- 30000   ;   nt <- 4   ;   nb <- 10000   ;   nc <- 3 

# Call JAGS from R (BRT XXX min), check convergence and summarize posteriors
out0 <- jags(win.data, inits, params, "mod_tro_0.txt", n.chains = nc,
             # n.thin = nt, n.iter = ni, n.burnin = nb, parallel = FALSE)
             n.thin = nt, n.iter = ni, n.burnin = nb, parallel = TRUE)

print(out0, 2)

plot(out0)

write_rds(out0, file = "data/out/out0.rds")

# fit the model
# Bundle and summarize data set
str(win.data <- list(y = yyy, ninterval = dim(yyy)[4], nsite = dim(yyy)[2],
                     nspec = dim(yyy)[1], nyr_s = dim(yyy)[3], #npk = dim(yyy)[1])
                     day = day, time = time #x1 = fore
                     ))

# Specify model in BUGS language
sink("mod_tro_1.txt")
cat("
model {

  # Community priors (with hyperparameters) for species-specific parameters
  # priors
  for(i in 1:nspec){
      for(t in 1:nyr_s){ 
        beta0[i,t] ~ dnorm(mu.beta0, tau.beta0)      # Abundance intercepts
      }  

    alpha0[i] ~ dnorm(mu.alpha0, tau.alpha0)         # Detection intercepts
    alpha1[i] ~ dnorm(mu.alpha1, tau.alpha1)         # Detection slope
    alpha2[i] ~ dnorm(mu.alpha2, tau.alpha2)         # Detection slope
    alpha3[i] ~ dnorm(mu.alpha3, tau.alpha3)         # Detection slope

    #beta1[i] ~ dnorm(mu.beta1, tau.beta1)            # Detection slope
    beta2[i] ~ dnorm(mu.beta2, tau.beta2)            # Detection slope

    r1[i] ~ dunif(0, 50)
    r2[i] ~ dunif(0, 50)
   }

  # Hyperpriors for community hyperparameters - hey put this in loops.
  # abundance model
  mu.beta0 ~ dnorm(0, 0.1)
  tau.beta0 <- pow(sd.beta0, -2)
  sd.beta0 ~ dgamma(0.01,0.01) #dunif(0, 10)

  # mu.beta1 ~ dnorm(0, 0.1)
  # tau.beta1 <- pow(sd.beta1, -2)
  # sd.beta1 ~ dgamma(0.01,0.01) #dunif(0, 10)

  mu.beta2 ~ dnorm(0, 0.1)
  tau.beta2 <- pow(sd.beta2, -2)
  sd.beta2 ~ dunif(0, 10)

  # detection model
  mu.alpha0 ~ dnorm(0, 0.1)
  tau.alpha0 <- pow(sd.alpha0, -2)
  sd.alpha0 ~ dgamma(0.01,0.01) #dunif(0, 10)

  mu.alpha1 ~ dnorm(0, 0.1)
  tau.alpha1 <- pow(sd.alpha0, -2)
  sd.alpha1 ~ dgamma(0.01,0.01) #dunif(0, 10)

  mu.alpha2 ~ dnorm(0, 0.1)
  tau.alpha2 <- pow(sd.alpha0, -2)
  sd.alpha2 ~ dgamma(0.01,0.01) #dunif(0, 10)

  mu.alpha3 ~ dnorm(0, 0.1)
  tau.alpha3 <- pow(sd.alpha0, -2)
  sd.alpha3 ~ dgamma(0.01,0.01) #dunif(0, 10)

  # likelihood
  for(i in 1:nspec){
    for (j in 1:nsite){
      for(t in 1:nyr_s){
      #  for(p in 1:npk){
          # Ecological model for true abundance (process model)
          #N[i,j,t] ~ dnegbin(Q1[i,j,t], r1[i]) 
          #Q1[i,j,t] <- r1[i] / (r1[i] + lambda[i,j,t])
          N[i,j,t] ~ dpois(lambda[i,j,t]) 
          log(lambda[i,j,t]) <- beta0[i,t] #+ beta1[i] * x1[t] #+ beta2[i] * x2[i] 
    
          # Observation model for replicated counts
          logit(p[i,j,t]) <- alpha0[i] + alpha1[i] * day[j,t] + alpha2[i] * (day[j,t]^2) + alpha3[i] * time[j,t]
          
          for(k in 1:ninterval){  # intervals
             # Poisson parameter 
             #y[i,j,t,k] ~ dnegbin(Q2[i,j,t,k], r2[i])  ### CHECK THIISSSS!!!!
             #Q2[i,j,t,k] <- r2[i] / (r2[i] + pi[i,j,t,k])
             y[i,j,t,k] ~ dpois(pi[i,j,t,k])
             pi[i,j,t,k] <- (p[i,j,t] * (1 - p[i,j,t])^(k-1)) * lambda[i,j,t]
          # check HOW WELL poisson FITs FITS THE DATA; maybe just poisson is ok.... Poisson gamma mixture – if mixingis your issue!
          }
      # number of each species
      #Nsp[i,t] <- sum(N[i,,t]) # ?? why error
        }

      }
    }
  #}
}
",fill = TRUE)
sink()

# Initial values
some.more <- 5          # May have to play with this until JAGS is happy
Nst <- apply(yyy, c(1,2,3), max, na.rm = TRUE) + some.more
Nst[Nst == '-Inf'] <- 20          # May have to play with this, too
Nst <- Nst
inits <- function()list(N = Nst,
                        mu.alpha0 = -2.1, mu.alpha1 = 0.02, mu.alpha2 = 0.02, mu.alpha3 = 0.02,
                        mu.beta0 = 2.4#, mu.beta1 = -0.02, mu.beta2 = 0.01
                        )

# OR: use inits at earlier solutions (greatly speeds up convergence)
# pm <- out11$mean     # Pull out posterior means from earlier run
# inits <- function() list(a = ast, N = Nst, alpha0 = rnorm(nspec), beta0 = rnorm(nspec), alpha = matrix(rnorm(n = nspec*3), ncol = 3), beta = matrix(rnorm(n = nspec*3), ncol = 3), mu.beta0 = pm$mu.beta0, sd.beta0 = pm$sd.beta0, mu.beta = pm$mu.beta, sd.beta = pm$sd.beta, mu.alpha0 = pm$mu.alpha0, sd.alpha0 = pm$sd.alpha0, mu.alpha = pm$mu.alpha, sd.alpha = pm$sd.alpha )

# Parameters monitored
params <- c("mu.alpha0", "sd.alpha0",
            "mu.alpha1", "sd.alpha1", 
            "mu.alpha2", "sd.alpha2",
            "mu.alpha3", "sd.alpha3", 
            "mu.beta0", "sd.beta0", 
            #"mu.beta1", "sd.beta1", 
            "mu.beta2", "sd.beta2",
            "Nsp",
            "lambda", 
            "alpha0", "alpha1", "alpha2", "alpha3",
            "beta0",#, "beta1", "beta2"
            "N"
)

# MCMC settings
ni <- 4000   ;   nt <- 3   ;   nb <- 1000   ;   nc <- 3 

# Call JAGS from R (BRT XXX min), check convergence and summarize posteriors
out1 <- jags(win.data, inits, params, "mod_tro_1.txt", n.chains = nc,
             # n.thin = nt, n.iter = ni, n.burnin = nb, parallel = FALSE)
             n.thin = nt, n.iter = ni, n.burnin = nb, parallel = TRUE)

print(out1, 2)

plot(out1)

write_rds(out1, file = "data/out/out1.rds")

## Try beta 1 again ----------------------------------------------------------------------------------
arr_area_site <- read_rds(file = "data/out/arr_area_site.rds")

# 1 park, 8 years, 1 buffer, 25 sites 
# data frame with 25 rows and 8 columns
x1 <- arr_area_site[4, ,1, ] %>% 
  t()
x1 <- x1[1:25,1]
x1 <- scale(x1)

# fit the model
# Bundle and summarize data set
str(win.data <- list(y = yyy, ninterval = dim(yyy)[4], nsite = dim(yyy)[2],
                     nspec = dim(yyy)[1], nyr_s = dim(yyy)[3], #npk = dim(yyy)[1])
                     day = day, time = time, x1 = x1
                    ))

# Specify model in BUGS language
sink("mod_tro_2.txt")
cat("
model {

  # Community priors (with hyperparameters) for species-specific parameters
  # priors
  for(i in 1:nspec){
      for(t in 1:nyr_s){ 
        beta0[i,t] ~ dnorm(mu.beta0, tau.beta0)      # Abundance intercepts
      }  

    alpha0[i] ~ dnorm(mu.alpha0, tau.alpha0)         # Detection intercepts
    alpha1[i] ~ dnorm(mu.alpha1, tau.alpha1)         # Detection slope
    alpha2[i] ~ dnorm(mu.alpha2, tau.alpha2)         # Detection slope
    alpha3[i] ~ dnorm(mu.alpha3, tau.alpha3)         # Detection slope

    beta1[i] ~ dnorm(mu.beta1, tau.beta1)            # Detection slope
    #beta2[i] ~ dnorm(mu.beta2, tau.beta2)            # Detection slope

    r1[i] ~ dunif(0, 50)
    r2[i] ~ dunif(0, 50)
   }

  # Hyperpriors for community hyperparameters - hey put this in loops.
  # abundance model
  mu.beta0 ~ dnorm(0, 0.1)
  tau.beta0 <- pow(sd.beta0, -2)
  sd.beta0 ~ dgamma(0.01,0.01) #dunif(0, 10)

  mu.beta1 ~ dnorm(0, 0.1)
  tau.beta1 <- pow(sd.beta1, -2)
  sd.beta1 ~ dgamma(0.01,0.01) #dunif(0, 10)

  # mu.beta2 ~ dnorm(0, 0.1)
  # tau.beta2 <- pow(sd.beta2, -2)
  # sd.beta2 ~ dunif(0, 10)

  # detection model
  mu.alpha0 ~ dnorm(0, 0.1)
  tau.alpha0 <- pow(sd.alpha0, -2)
  sd.alpha0 ~ dgamma(0.01,0.01) #dunif(0, 10)

  mu.alpha1 ~ dnorm(0, 0.1)
  tau.alpha1 <- pow(sd.alpha0, -2)
  sd.alpha1 ~ dgamma(0.01,0.01) #dunif(0, 10)

  mu.alpha2 ~ dnorm(0, 0.1)
  tau.alpha2 <- pow(sd.alpha0, -2)
  sd.alpha2 ~ dgamma(0.01,0.01) #dunif(0, 10)

  mu.alpha3 ~ dnorm(0, 0.1)
  tau.alpha3 <- pow(sd.alpha0, -2)
  sd.alpha3 ~ dgamma(0.01,0.01) #dunif(0, 10)

  # likelihood
  for(i in 1:nspec){
    for (j in 1:nsite){
      for(t in 1:nyr_s){
      #  for(p in 1:npk){
          # Ecological model for true abundance (process model)
          #N[i,j,t] ~ dnegbin(Q1[i,j,t], r1[i]) 
          #Q1[i,j,t] <- r1[i] / (r1[i] + lambda[i,j,t])
          N[i,j,t] ~ dpois(lambda[i,j,t]) 
          log(lambda[i,j,t]) <- beta0[i,t] + beta1[i] * x1[t] #+ beta2[i] * x2[i] 
    
          # Observation model for replicated counts
          logit(p[i,j,t]) <- alpha0[i] + alpha1[i] * day[j,t] + alpha2[i] * (day[j,t]^2) + alpha3[i] * time[j,t]
          
          for(k in 1:ninterval){  # intervals
             # Poisson parameter 
             #y[i,j,t,k] ~ dnegbin(Q2[i,j,t,k], r2[i])  ### CHECK THIISSSS!!!!
             #Q2[i,j,t,k] <- r2[i] / (r2[i] + pi[i,j,t,k])
             y[i,j,t,k] ~ dpois(pi[i,j,t,k])
             pi[i,j,t,k] <- (p[i,j,t] * (1 - p[i,j,t])^(k-1)) * lambda[i,j,t]
          # check HOW WELL poisson FITs FITS THE DATA; maybe just poisson is ok.... Poisson gamma mixture – if mixingis your issue!
          }
      # number of each species
      #Nsp[i,t] <- sum(N[i,,t]) # ?? why error
        }

      }
    }
  #}
}
",fill = TRUE)
sink()

# Initial values
some.more <- 5          # May have to play with this until JAGS is happy
Nst <- apply(yyy, c(1,2,3), max, na.rm = TRUE) + some.more
Nst[Nst == '-Inf'] <- 20          # May have to play with this, too
Nst <- Nst
inits <- function()list(N = Nst, 
                        mu.alpha0 = -2.1, mu.alpha1 = 0.02, mu.alpha2 = 0.02, mu.alpha3 = 0.02,
                        mu.beta0 = 2.4, mu.beta1 = -0.02#, mu.beta2 = 0.01
)

# OR: use inits at earlier solutions (greatly speeds up convergence)
# pm <- out11$mean     # Pull out posterior means from earlier run
# inits <- function() list(a = ast, N = Nst, alpha0 = rnorm(nspec), beta0 = rnorm(nspec), alpha = matrix(rnorm(n = nspec*3), ncol = 3), beta = matrix(rnorm(n = nspec*3), ncol = 3), mu.beta0 = pm$mu.beta0, sd.beta0 = pm$sd.beta0, mu.beta = pm$mu.beta, sd.beta = pm$sd.beta, mu.alpha0 = pm$mu.alpha0, sd.alpha0 = pm$sd.alpha0, mu.alpha = pm$mu.alpha, sd.alpha = pm$sd.alpha )

# Parameters monitored
params <- c("mu.alpha0", "sd.alpha0",
            "mu.alpha1", "sd.alpha1", 
            "mu.alpha2", "sd.alpha2",
            "mu.alpha3", "sd.alpha3", 
            "mu.beta0", "sd.beta0", 
            #"mu.beta1", "sd.beta1", 
            "mu.beta2", "sd.beta2",
            "Nsp","N",
            "lambda", 
            "alpha0", "alpha1", "alpha2", "alpha3",
            "beta0", "beta1"#, "beta2"
)

# hw 3 or 4

# MCMC settings
ni <- 10000   ;   nt <- 3   ;   nb <- 1000   ;   nc <- 3 

# Call JAGS from R (BRT XXX min), check convergence and summarize posteriors
out2 <- jags(win.data, inits, params, "mod_tro_1.txt", n.chains = nc,
             # n.thin = nt, n.iter = ni, n.burnin = nb, parallel = FALSE)
             n.thin = nt, n.iter = ni, n.burnin = nb, parallel = TRUE)

print(out2, 2)

plot(out2)

write_rds(out2, file = "data/out/out2.rds")
