#? *********************************************************************************
#? ---------------------------   multiple_single_sps.R   ---------------------------
#? *********************************************************************************
#
#! Code to ...
#
#! Source ---------------------------------------------
#           - code/fit_model/back2d_covs_scales_2min.R:
#!              - Input:
#                   - data/y_dat8.rds: tibble with bird data
#                   - data/X.rds: tibble with covariate data
#                   - data/out/nsite_pk.rds: vector with number of sites in each park
#                   - data/src/key_park.rds: vector of all parks being analyzed:
#!              - Output: 
#                   - data/model_res/jags_res_{sps}_{park}_run{run_number}.rds: file with result of jags model
#
#! Input ----------------------------------------------
#           - :
#           - :
#
#! Output ----------------------------------------------
#           - :
#           - :

# detach packages and clear workspace
freshr::freshr()

# Print script file name
context <- "run_step1_step2.R" #rstudioapi::getSourceEditorContext()
cat("\n", "\n", "\n", 
    'Current script:', context, #basename(context[[2]]), 
    "\n", "\n", "\n", "\n")

#! Package library and versions -------------------------
#  Created a library repo?
#  (  )yes  (  )no
#  renv::init()

# Load an existing library?
#  renv::status()
#  renv::restore()

# Installed new packages?
#  renv::snapshot()

test <- FALSE

#! Load packages ---------------------------------------
#library(conflicted)
library(tidyverse)
library(glue)

#! Make functions --------------------------------------
colanmes <- colnames
lenght <- length
`%!in%` <- Negate(`%in%`)

#! Source code and Import data -----------------------------------------
## file paths

## read files
# import file, create model names, and save it!
#! MCMC settings ---------------------------------------------------
niterations <- 30000
nburnin <- 15000
nchains <- 8
nthin <- 5
if(test == TRUE){nadapt_min <- 1} else {nadapt_min <- 500}

# b_sps <- c("BHVI", "BRCR", "BTBW", "HETH", "OVEN", 
#                                 "VEER", "REVI", "WBNU", "SCTA", "WOTH",
#                                 "DOWO", "HAWO", "BLBW", "YBSA", "BCCH", "BAWW", "BTNW")

# master_tab <- as_tibble(cbind(b_sps, rep(1, length(b_sps))))
# colnames(master_tab) <- c("AOU_Code","step")
master_tab <- read_csv("data/mod_key2.csv")

for (key_ite in 1:nrow(master_tab)){
    # key_ite <- 1
    tib_loop <- master_tab[key_ite, ]

    (sps_loop <- tib_loop$AOU_Code)
    (step_numb <- tib_loop$step)
   
    cat(glue("\n \n Is it a test? {test} \n \n \n "))

    if(tib_loop$step == 2){
        # get scales for step 2
        sca_file <- read_rds(glue("data/model_res/{tib_loop$select}.rds"))
        scales_loop <- as.numeric(sca_file %>% filter(overlap0 == "no") %>% pull(sca_sel))
        date_step1 <- substr(tib_loop$result, 19, 28)
        cov_key2 <- sca_file %>% filter(overlap0 == "no") %>% pull(betas)
        source("code/fit_model/step2_analysis.R")

    } else {
        cat("Before sourcing - objects in environment:\n")
        print(ls())
        source("code/fit_model/back2d_covs_scales_2min_spscov.R")
        # After sourcing
        cat("After sourcing - objects in environment:\n")
        print(ls())
    
    }

}

cat(paste('\n ************************************** \n \n \n 
        ---------------- all DONE Lol ----------------', 
        '\n\n \n ************************************** \n'))
