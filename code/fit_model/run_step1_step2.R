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

#  setwd("/Volumes/zipkinlab/bamaral/NPS_bird_copy/")
freshr::freshr()

# hg <- httpgd::hgd()
# httpgd::hgd_browse()

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
interaction <- FALSE
step_number_define <- 1
if(substr(getwd(), 1, 3) == "/Us") {direc <- "local"} else {direc <- "hpc"}

#! Load packages ---------------------------------------
#library(conflicted)
library(tidyverse)
library(glue)

#! Make functions --------------------------------------
colanmes <- colnames
lenght <- length
`%!in%` <- Negate(`%in%`)

#! MCMC settings ---------------------------------------------------
niterations <- 120000
nburnin <- 60000
nchains <- 6
nthin <- 6
nadapt_min <- 20000

#! Source code and Import data -----------------------------------------
## read files
if(interaction == T){model_file <- "models/mod_all_covs2.txt"}
if(interaction == F){model_file <- "models/mod_all_covs.txt"}

if(direc == "local"){
    master_tab <- read_csv("/Users/bamaral/Library/CloudStorage/OneDrive-MichiganStateUniversity/GitHubOne/NPS_bird_copy/code/fit_model/mod_key.csv") %>%
            #filter(run == "yes") %>% 
            filter(step %in% c(step_number_define)) %>% 
            distinct()

    model_file <- glue("/Users/bamaral/Library/CloudStorage/OneDrive-MichiganStateUniversity/GitHubOne//NPS_bird_copy/{model_file}")

    } else {master_tab <- read_csv("code/fit_model/mod_key.csv") %>%
            #filter(run == "yes") %>% 
            filter(step %in% c(step_number_define)) %>% 
            distinct()
    }

paste('\n ************************************* \n \n \n   Running Models:', '\n',
      '  Test?', test, '\n',
      '  Interaction?', interaction, '\n',
      '  Step =', step_number_define, '\n',
      '  Number of sps =', nrow(master_tab), '\n',
      '  Total iterations =', nburnin + niterations, '\n',
      '  Started running on =', Sys.time(),  '\n \n \n',
      '**************************************') %>% cat()

for(key_ite in 1:nrow(master_tab)){
    # key_ite <- 1
    nburnin <- 30000

    tib_loop <- master_tab[key_ite, ]

    sps_loop <- tib_loop$AOU_Code
    step_numb <- tib_loop$step

    cat(glue("\n 
              \n
              \n 
              The species is {sps_loop}  \n
              Analysis on step {step_numb}  \n
              Interaction term? {interaction} \n
              Is it a test? {test} \n 
              Good luck Houston! 
              \n
              \n 
              \n "))

    if(tib_loop$step == 2){
        print("step 2 selected scales")
        # step 2 selected scales no or yes interaction
        sca_file <- read_rds(glue("data/model_res/{tib_loop$select}.rds"))
        scales_loop <- as.numeric(sca_file %>% filter(overlap0 == "no") %>% pull(sca_sel)) # filter(overlap0 == "no") %>%
        date_step1 <- substr(tib_loop$result, 19, 28)
        cov_key2 <- sca_file %>% 
                        #filter(overlap0 == "no") %>% 
                        pull(betas)
        
        # if(tib_loop$all_sca == F){ 
            if(interaction == FALSE){     
                print("step 2 selected scales no interaction")                       
                if(direc == "hpc"){
                    source("code/fit_model/step2_analysis.R")
                        } else {
                            source("/Users/bamaral/Library/CloudStorage/OneDrive-MichiganStateUniversity/GitHubOne/NPS_bird_copy/code/fit_model/step2_analysis.R")
                        }
                    }
            if(interaction == TRUE){  
                print("step 2 selected scales with interaction")           
                if(direc == "hpc"){
                    source("code/fit_model/step2_analysis_interaction.R")
                        } else {
                            source("/Users/bamaral/Library/CloudStorage/OneDrive-MichiganStateUniversity/GitHubOne/NPS_bird_copy/code/fit_model/step2_analysis_interaction.R")
                        }
                }  
            # } else {
            #     print("step 1 all scales with interaction")
            #     # step 1 all scales with interaction
            #         if(direc == "local"){
            #                 source("/Users/bamaral/Documents/GitHub/NPS_bird_copy/code/fit_model/back2d_covs_scales_2min_spscov_interact.R")
            #             } else {
            #                 source("code/fit_model/back2d_covs_scales_2min_spscov_interact.R")
            #                 }
            # }
        } else { 
            # step 1
                print("step 1 selected scales no interaction")
                # step 1 selected scales no interaction
                    if(direc == "local"){
                            source("/Users/bamaral/Documents/GitHub/NPS_bird_copy/code/fit_model/back2d_covs_scales_2min_spscov.R")
                        } else {
                            source("code/fit_model/back2d_covs_scales_2min_spscov.R")
                            }
                    # source("/Users/bamaral/Documents/GitHub/NPS_bird_copy/code/fit_model/x_min_max.r")
                    # source("code/fit_model/x_min_max.r")
            
        }

}

cat(paste('\n ********************************************** \n \n \n 
              ---------------- all DONE Lol ----------------', 
     '\n\n \n ********************************************** \n'))
