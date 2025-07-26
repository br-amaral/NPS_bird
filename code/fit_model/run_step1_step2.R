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

test <- TRUE

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

niterations <- 30000
nburnin <- 15000
nchains <- 8
nthin <- 5

b_sps <- c("BHVI", "BRCR", "BTBW", "HETH", "OVEN", 
                                "VEER", "REVI", "WBNU", "SCTA", "WOTH",
                                "DOWO", "HAWO", "BLBW", "YBSA", "BCCH", "BAWW", "BTNW")

master_tab <- as_tibble(cbind(b_sps, rep(1, length(b_sps))))
colnames(master_tab) <- c("AOU_Code","step")

for (key_ite in 1:nrow(master_tab)){
    # key_ite <- 1
    tib_loop <- master_tab[key_ite, ]

    (sps_loop <- tib_loop$AOU_Code)
    (step_numb <- tib_loop$step)
   
    cat(glue("\n \n Is it a test? {test} \n \n \n "))

    if(tib_loop$step == 2){
        # get scales for step 2
        scales_loop <- as.numeric(unlist(strsplit(tib_loop$scales2, split = "")))
        date_step1 <- tib_loop$date_step1

        source("/code/fit_model/step2_analysis.R")

    } else {
        cat("Before sourcing - objects in environment:\n")
        print(ls())
        source("/code/fit_model/back2d_covs_scales_2min_spscov.R")
        # After sourcing
        cat("After sourcing - objects in environment:\n")
        print(ls())
    
    }

}

cat(paste('\n ************************************** \n \n \n 
        ---------------- all DONE Lol ----------------', 
        '\n\n \n ************************************** \n'))
