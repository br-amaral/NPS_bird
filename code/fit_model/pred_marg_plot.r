#? *********************************************************************************
#? ------------------------------   pred_marg_plot.r   -----------------------------
#? *********************************************************************************
#
#! Code to ...
#
#! Source ---------------------------------------------
#           - :
#           - :
#
#! Input ----------------------------------------------
#           - data/out/coefs_sps_sca.rds : table with all the beta coefficient estimates with their scales
#           - data/model_res/{sps}_step2_output_20{xx}_{xx}_{xx}run{x}.rds :
#
#! Output ----------------------------------------------
#           - :
#           - :
#
# detach packages and clear workspace
#  setwd("/Volumes/zipkinlab/bamaral/NPS_bird_copy/")
freshr::freshr()

#! Load packages ---------------------------------------
library(tidyverse)
library(conflicted)
library(glue)

conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::filter)
# conflicts_prefer(scales::alpha)

#! Make functions --------------------------------------
colanmes <- colnames
lenght <- length
`%!in%` <- Negate(`%in%`)

#! Source code -----------------------------------------

#! Import data -----------------------------------------
## file paths
RES_MOD_FILE <- "BTNW_step2_output_2025_08_03run1"
COEF_SPS_PATH <- "coefs_sps_sca"

## read files
res_mod <- read_rds(glue("data/model_res/{RES_MOD_FILE}.rds"))  # model file
dat_sca <- read_rds(glue("data/out/{COEF_SPS_PATH}.rds"))       # which betas are important

# get the data for the predictions
sps_loop <- substr(RES_MOD_FILE, 1, 4)
sps_dat_name <- glue("{sps_loop}_step1_jagsdata")
                                                               
DATA_SPS_PATH <- 
      list.files(path = file.path(getwd(),"data/ana_file/"),
                                          pattern = sps_dat_name,
                                          full.names = FALSE)  %>% 
                as_tibble() %>% 
                slice(1) %>% 
                pull()
                
sps_data <- read_rds(glue("data/ana_file/{DATA_SPS_PATH}"))

# beginig of the loop ;)

dat_sca_loop <- dat_sca %>% 
                    filter(sps == sps_loop,
                    scale_selected == 1) 

unique(dat_sca_loop$mod_res)
