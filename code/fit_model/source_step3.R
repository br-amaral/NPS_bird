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
freshr::freshr()
#
# Load packages ---------------------------------------
library(conflicted)
library(tidyverse)
library(glue)
library(readxl)
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
# Source code -----------------------------------------
#
# Import data -----------------------------------------
## file paths
MOD_RES_PATH <- "results_tracker.xlsx"
## read files
mod_specs <- read_excel(MOD_RES_PATH, sheet = "key") %>%
                filter(Step == 1) %>% 
                filter(SPS %in% c("HETH", "OVEN", "VEER", "BTBW")) %>% 
                select(OUTPUT_FILE, SPS)

nchains <- 8
niterations <- 30000
nburnin <- 20000
nthin <- 5

for(jj in 1:nrow(mod_specs)){

    if(mod_specs$OUTPUT_FILE[jj] == "BTBW"){nchains <- 8;niterations <- 50000; nburnin <- 30000;nthin <- 5}else{nchains <- 8;niterations <- 30000; nburnin <- 20000;nthin <- 5}

    file_name <- mod_specs$OUTPUT_FILE[jj]
    source("code/fit_model/step2_analysis.R")

}

