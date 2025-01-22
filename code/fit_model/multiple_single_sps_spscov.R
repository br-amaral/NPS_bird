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

# Print script file name
context <- "multiple_single_sps_spscovs.R" #rstudioapi::getSourceEditorContext()
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
#options(repos = c(CRAN = "https://cloud.r-project.org/"))
#update.packages("freshr")
# detach packages and clear workspace
#if(!require(freshr)){install.packages('freshr')}
 freshr::freshr()

#! Load packages ---------------------------------------
#library(conflicted)
library(tidyverse)
library(glue)

#conflicts_prefer(dplyr::select)
#conflicts_prefer(dplyr::filter)
# conflicts_prefer(scales::alpha)

#! Make functions --------------------------------------
colanmes <- colnames
lenght <- length
`%!in%` <- Negate(`%in%`)

#! MCMC settings ---------------------------------------
niterations <- 20000
nburnin <- 10000
nchains <- 5
nthin <- 5

# niterations <- 10 ; nburnin <- 5 ; nchains <- 1 ; nthin <- 1

#! Source code and Import data -----------------------------------------
## file paths

## read files
master_tab <- read_rds("data/out/sps_covs.rds") %>% 
    filter(AOU_Code %!in% c("BLBW", "PIWA", "BTNW"))

for (i in 1:nrow(master_tab)){
    (sps_loop <- master_tab$AOU_Code[i])
    BA  <- master_tab$BA[i]
    DEN <- master_tab$DEN[i]   
    SHR <- master_tab$SHR[i]   
    DIV <- master_tab$DIV[i]   
    EAR <- master_tab$EAR[i]   
    MID <- master_tab$MID[i]   
    LAT <- master_tab$LAT[i]
    CAN <- master_tab$CAN[i]
    DEB <- master_tab$DEB[i]
    
    cov_key <- master_tab[i,2:ncol(master_tab)]
    print(sps_loop)
    print(cov_key)
    # Print object name if the value is greater than zero
    if (BA == 1)  print("BA")
    if (DEN == 1) print("DEN")
    if (SHR == 1) print("SHR")
    if (DIV == 1) print("DIV")
    if (EAR == 1) print("EAR")
    if (MID == 1) print("MID")
    if (LAT == 1) print("LAT")
    if (CAN == 1) print("CAN")
    if (DEB == 1) print("DEB")
    source("code/fit_model/back2d_covs_scales_2min_spscov.R")
    #source("code/fit_model/get_z.R")

}

cat(paste('\n ************************************** \n \n \n 
        ---------------- all DONE Lol ----------------', 
        '\n\n \n ************************************** \n'))
