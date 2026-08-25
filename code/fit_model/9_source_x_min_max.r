#? *********************************************************************************
#? ----------------------------  9_source_x_min_max.R  -----------------------------
#? *********************************************************************************
# Code to source x_min_max.R for all species and parks
#! Input ----------------------------------------------
#           - models/mod_all_covs.txt : model files with all covariates for step 1
#           - code/fit_model/mod_key.csv: table ith the path to all model results
#
#! Output ---------------------------------------------

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
