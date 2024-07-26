# *********************************************************************************
# -------------------------------   Plot model output   ---------------------------
# *********************************************************************************
# Code to make plots
#
hg <- httpgd::hgd()
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

#! Import data -----------------------------------------
## file paths and read files

#sps <- "AMGO"
# sps_list1 <- c("GCFL", "AMGO", "DOWO", "NOCA", "SCTA", "SOSP", "GRCA", "RBWO", "COYE", "WOTH", "RWBL",
#                 "WBNU", "BTNW", "EAWP", "BCCH", "BLJA", "TUTI", "AMRO", "REVI", "OVEN", "BTBW", "YBSA", 
#                 "BOBO", "YRWA", "PIWA", "CEDW", "CHSP", "NOFL", "HAWO", "BRCR", "RBGR", "DEJU", "AMCR", 
#                 "BAOR", "RBNU", "BHVI", "GCKI", "EATO", "FISP", "HETH", "VEER", "MODO", "BLBW")
# yearbo1 <- c('yes', 'no')
# for_stage1 <- c('late', 'mature', 'pole')
# master_tab <- expand_grid(sps_list1, yearbo1, for_stage1) %>% 
#                 mutate(res_name = glue("{sps_list1}_b0{yearbo1}_{for_stage1}"))
# colnames(master_tab) <- c("sps_list", "yearbo", "for_stage", "res_name")
# mod_loop <- 10
# samples_jags <- read_rds(file = glue("data/model_res/jags_res_{master_tab[mod_loop,4]}_parks_10000itsrun1.rds"))

samples_jags <- read_rds(glue("data/model_res/jags_res_AMGO_b0yes_parks_30000its_LESSHRrun3.rds"))

#! Summary --------------------------------------------
MCMCsummary(samples_jags,
            round = 2)

MCMCsummary(samples_jags,
            params = c("mu.beta0","beta",
                        "mu.alpha0","alpha",
                        "scales_beta1","scales_beta2"),
                        round = 2)
##! traceplots ----------------------
#print(glue("jags_res_{master_tab[mod_loop,4]}_parks_10000itsrun1"))

MCMCtrace(samples_jags,
            params = c("mu.alpha0", "mu.beta0",
                        "beta","alpha",
                     "scales_beta1","scales_beta2"),
            ind = TRUE,
            pdf = FALSE,
            #filename = glue("figures/preliminary/jags_res_GCFL_b0yes_parks_20000its_LESSHRrun1"),
            exact = TRUE,
            Rhat = TRUE,
            n.eff = TRUE) 

MCMCtrace(samples_jags,
            params = c("alpha0", "beta0"),
            ind = TRUE,
            pdf = FALSE,
            #filename = glue("figures/preliminary/trace2_jags_res_COYE_b0yes_parks_10000its_LESSrun1"),
            exact = TRUE,
            Rhat = TRUE,
            n.eff = TRUE)

# par estimates ----------------------------------
par(mfrow = c(1,1))

MCMCplot(samples_jags,
         params = c("mu.beta0","beta", 
                     "mu.alpha0","alpha",
                     "scales_beta1","scales_beta2"),
         ref_ovl = TRUE)

MCMCplot(samples_jags,
         params = c("alpha0","beta0"),
         ref_ovl = TRUE)

MCMCplot(samples_jags,
         params = c("mu.beta0","beta",
                    "mu.alpha0","alpha"),
         ref_ovl = TRUE)
#MCMCplot(samples_jags,
#         params = c("mu.beta0","beta1", "beta2","beta3",
#                    "mu.alpha0","alpha1","alpha2","alpha3",
#                    "scales_beta1","scales_beta2"),
#         ref_ovl = TRUE)
         
#TODO:
summary(samples_jags)

#NOTE:
