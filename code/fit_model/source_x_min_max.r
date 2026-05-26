#? *********************************************************************************
#? ----------------------------  back2d_covs_scales_3  -----------------------------
#? *********************************************************************************
# Code to run model to estimate the effect of different environmental
#   covariates on bird occupancy in several national parks and on three
#   different spatial scales. Code here filters and format the data for  
#   single-species models
#! Input ----------------------------------------------
#           - data/y_dat8.rds: tibble with bird data (2_create_data_files.R)
#           - data/X.rds: tibble with covariate data (2_create_data_files.R)
#           - data/out/nsite_pk.rds: vector with number of sites in each park
#           - data/src/key_park.rds: vector of all parks being analyzed
#
#! Output ---------------------------------------------
#           - data/model_res/jags_res_{sps}_{park}_run{run_number}.rds: file with result of jags model
#  freshr::freshr()
 #   test <- FALSE ; step_numb <- 1; sps_loop <- "BHVI"

# Load packages --------------------------------------
library(conflicted)
library(tidyverse)
library(glue)
library(jagsUI)
library(rjags)
#library(MCMCvis)
library(AHMbook)
library(fs)
library(here)
library(MCMCvis)
#library(BayesPostEst)

conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::filter)
conflicts_prefer(scales::alpha)

test <- FALSE 
interaction <- FALSE
step_number_define <- 2
if(substr(getwd(), 1, 3) == "/Us") {direc <- "local"} else {direc <- "hpc"}

#! Source code and Import data -----------------------------------------
## read files
if(interaction == T){model_file <- "models/mod_all_covs2.txt"}
if(interaction == F){model_file <- "models/mod_all_covs.txt"}

if(direc == "local"){
    master_tab <- read_csv("/Users/bamaral/Library/CloudStorage/OneDrive-MichiganStateUniversity/GitHubOne/NPS_bird_copy/code/fit_model/mod_key.csv") %>%
            #filter(run == "yes") %>% 
            filter(step %in% c(step_number_define)) %>% 
            distinct()

    } else {master_tab <- read_csv("code/fit_model/mod_key.csv") %>%
            #filter(run == "yes") %>% 
            filter(step %in% c(step_number_define)) %>% 
            distinct()
    }


for(key_ite in 1:nrow(master_tab)){
    # key_ite <- 1

    tib_loop <- master_tab[key_ite, ]

    sps_loop <- tib_loop$AOU_Code
    step_numb <- tib_loop$step

    cat(glue("\n 
              \n
              \n 
              The species is {sps_loop}  \n 
              \n
              \n 
              \n "))
    if(direc == "local"){
        source("/Users/bamaral/Documents/GitHub/NPS_bird_copy/code/fit_model/x_min_max.r")
        } else {source("code/fit_model/x_min_max.r")}
    
}

cat(paste('\n ********************************************** \n \n \n 
              ---------------- all DONE Lol ----------------', 
     '\n\n \n ********************************************** \n'))
