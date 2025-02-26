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
                select(OUTPUT_FILE)

nchains <- 8
niterations <- 30000
nburnin <- 20000
nthin <- 5

for(jj in 1:now(mod_specs)){

    file_name <- mod_specs$OUTPUT_FILE[jj]
    source("code/fit_model/step2analysis.R")
    
}

