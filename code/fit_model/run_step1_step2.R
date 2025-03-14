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
master_tab <- read_csv("code/fit_model/model_sps_key.csv")  %>% 
                mutate(#mod_name = ascharacter(mod_name),
                       mod_name = glue("mod_{AOU_Code}_{BA}{DEN}{SHR}{DIV}{EAR}{MID}{LAT}_step{step}_sca_{scales2}"))

for (key_ite in 10:nrow(master_tab)){
    # key_ite <- 1
    tib_loop <- master_tab[key_ite, ]

    (sps_loop <- tib_loop$AOU_Code)
    (step_numb <- tib_loop$step)
    mod_name_loop <- glue("models/{tib_loop$mod_name}.txt")

    #! MCMC settings ---------------------------------------------------
    niterations <- tib_loop$niterations
    nburnin <- tib_loop$nburnin
    nchains <- tib_loop$nchains
    nthin <- tib_loop$nthin
    if(test == TRUE){nadapt_min <- 1} else {nadapt_min <- 100}
    # niterations <- 10 ; nburnin <- 5 ; nchains <- 1 ; nthin <- 1
    
    #! Get species and covariates --------------------------------------
    BA  <- tib_loop$BA
    DEN <- tib_loop$DEN
    SHR <- tib_loop$SHR
    DIV <- tib_loop$DIV
    EAR <- tib_loop$EAR
    MID <- tib_loop$MID
    LAT <- tib_loop$LAT
    CAN <- tib_loop$CAN
    DEB <- tib_loop$DEB
    
    cov_key <- tib_loop[ ,2:10]
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

    cat(glue("\n \n Is it a test? {test} \n \n \n "))

    if(tib_loop$step == 2){
        # get scales for step 2
        scales_loop <- as.numeric(unlist(strsplit(tib_loop$scales2, split = "")))
        date_step1 <- tib_loop$date_step1

        source("code/fit_model/step2_analysis.R")

    } else {
        source("code/fit_model/back2d_covs_scales_2min_spscov.R")
    }

}

cat(paste('\n ************************************** \n \n \n 
        ---------------- all DONE Lol ----------------', 
        '\n\n \n ************************************** \n'))
