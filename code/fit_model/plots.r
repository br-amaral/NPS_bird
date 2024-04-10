library(tidyverse)
library(MCMCvis)

samples_jags <- read_rds("data/model_res/jags_res_AMGO.rds")

MCMCsummary(samples_jags,
             params = c("mu.beta0","beta1", "beta2","beta3",
                        "mu.alpha0","alpha1","alpha2","alpha3",
                        "scales_beta1","scales_beta2"),
                        round = 2)
 MCMCtrace(samples_jags,
           #params = params[c(2,4,5,7)],
           ind = TRUE,
           pdf = FALSE)

 par(mfrow = c(1,1))
 MCMCplot(samples_jags,
          #params = params[c(2,4,5,7)],
          ref_ovl = TRUE)
