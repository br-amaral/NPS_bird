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
context <- "multiple_single_sps_BA.R" #rstudioapi::getSourceEditorContext()
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
if(!require(freshr)){install.packages('freshr')}
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
niterations <- 50000
nburnin <- 20000
nchains <- 8
nthin <- 10

#! Source code and Import data -----------------------------------------
## file paths

## read files
#sps_list <- read_rds("data/src/guilds.rds")  %>% 
#                filter(Response_Guild == "InteriorForestObligate") %>% 
#                select(AOU_Code) %>% 
#                distinct() %>% 
#                pull()

# removing forest stage, species, 
#sps_list <- sps_list[-1]
sps_list1 <- c("GCFL", 
               "AMGO", 
               "DOWO", 
               "RBWO", 
               "COYE"
               )

yearbo1 <- c('yes')

#for_stage1 <- c('late', 'mature', 'pole')

master_tab <- expand_grid(sps_list1, yearbo1) %>% 
                mutate(res_name = glue("{sps_list1}_b0{yearbo1}"))

colnames(master_tab) <- c("sps_list", "yearbo", "res_name")
## BTNW not working
for (i in 1:nrow(master_tab)){
    sps_loop <- master_tab[i,1] %>% pull()
    yearbo <- master_tab[i,2] %>% pull()
    # for_stage <- master_tab[i,3] %>% pull()
    sps_loop2 <- master_tab[i,3] %>% pull()
    print(sps_loop2)
    source("code/fit_model/back2d_covs_scales_2min_BA.R")
}

cat(paste('\n ************************************** \n \n \n 
        ---------------- all DONE Lol ----------------', 
        '\n\n \n ************************************** \n'))
