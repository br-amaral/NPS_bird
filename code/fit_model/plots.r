# *********************************************************************************
# -------------------------------   Amazing Title   -------------------------------
# *********************************************************************************
# Code to ...
#
#
# Source ---------------------------------------------
#           - :
#           - :
#
# Input ----------------------------------------------
#           - :
#           - :
#
# Output ----------------------------------------------
#           - :
#           - :
#
# detach packages and clear workspace
if(!require(freshr)){install.packages('freshr')}
freshr::freshr()
#
# Load packages ---------------------------------------
library(conflicted)
library(tidyverse)
library(glue)
library(MCMCvis)
library(rjags)
#
conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::filter)
# conflicts_prefer(scales::alpha)
#
# Make functions --------------------------------------
colanmes <- colnames
lenght <- length
`%!in%` <- Negate(`%in%`)
#
#! Source code -----------------------------------------
#
#! Import data -----------------------------------------
## file paths and read files

sps <- "BRCR"

samples_jags <- read_rds(glue("data/model_res/jags_res_{sps}2d.rds"))

samples_jags <- read_rds(file = glue("data/model_res/jags_res_WBNU2dnoA.rds"))

#! Summary --------------------------------------------
MCMCsummary(samples_jags,
            round = 2)

#MCMCsummary(samples_jags,
#            params = c("mu.beta0","beta1", "beta2","beta3",
#                       "mu.alpha0","alpha1","alpha2","alpha3",
#                      "scales_beta1","scales_beta2"),
#            round = 2)

MCMCsummary(samples_jags,
            params = c("mu.beta0","beta",
                       "mu.alpha0","alpha",
                      "scales_beta1","scales_beta2"),
                        round = 2)
##! traceplots ----------------------
#MCMCtrace(samples_jags,
#           #params = params[c(2,4,5,7)],
#           ind = TRUE,
#           pdf = TRUE)

#MCMCtrace(samples_jags,
#           params = c("mu.beta0","beta1", "beta2","beta3",
#                    "mu.alpha0","alpha1","alpha2","alpha3",
#                    "scales_beta1","scales_beta2"),
#           ind = TRUE,
#           pdf = TRUE)

MCMCtrace(samples_jags,
          params = c("mu.beta0","beta",
                   "mu.alpha0","alpha",
                   "scales_beta1","scales_beta2"),
          ind = TRUE,
          pdf = TRUE,
          filename = glue("data/model_res/{sps}_2b_trace_{nrow(samples_jags[[1]])}its"),
          exact = TRUE,
          Rhat = TRUE,
          n.eff = TRUE)

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
         params = "beta0",
         ref_ovl = TRUE)

#MCMCplot(samples_jags,
#         params = c("mu.beta0","beta1", "beta2","beta3",
#                    "mu.alpha0","alpha1","alpha2","alpha3",
#                    "scales_beta1","scales_beta2"),
#         ref_ovl = TRUE)
         
#TODO:
summary(samples_jags)

#NOTE:
