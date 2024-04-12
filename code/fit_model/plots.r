library(tidyverse)
library(MCMCvis)
library(rjags)

samples_jags <- read_rds("data/model_res/jags_res_SOSP2d.rds")

MCMCsummary(samples_jags,
            round = 2)

MCMCsummary(samples_jags,
            params = c("mu.beta0","beta1", "beta2","beta3",
                       "mu.alpha0","alpha1","alpha2","alpha3",
                      "scales_beta1","scales_beta2"),
            round = 2)

MCMCsummary(samples_jags,
            params = c("mu.beta0","beta",
                       "mu.alpha0","alpha",
                      "scales_beta1","scales_beta2"),
                        round = 2)
## trace ----------------------
MCMCtrace(samples_jags,
           #params = params[c(2,4,5,7)],
           ind = TRUE,
           pdf = TRUE)

MCMCtrace(samples_jags,
           params = c("mu.beta0","beta1", "beta2","beta3",
                    "mu.alpha0","alpha1","alpha2","alpha3",
                    "scales_beta1","scales_beta2"),
           ind = TRUE,
           pdf = TRUE)

MCMCtrace(samples_jags,
           params = c("mu.beta0","beta",
                    "mu.alpha0","alpha",
                    "scales_beta1","scales_beta2"),
           ind = TRUE,
           pdf = TRUE)

# par estimates ----------------------------------
par(mfrow = c(1,1))
MCMCplot(samples_jags,
          #params = c("mu.beta0","beta1", "beta2","beta3",
              #          "mu.alpha0","alpha1","alpha2","alpha3",
               #         "scales_beta1","scales_beta2"),
          ref_ovl = TRUE)
MCMCplot(samples_jags,
         params = c("mu.beta0","beta", 
                    "mu.alpha0","alpha",
                    "scales_beta1","scales_beta2"),
         ref_ovl = TRUE)

MCMCplot(samples_jags,
         params = c("mu.beta0","beta1", "beta2","beta3",
                    "mu.alpha0","alpha1","alpha2","alpha3",
                    "scales_beta1","scales_beta2"),
         ref_ovl = TRUE)
         
#TODO 
summary(samples_jags)
