#? *********************************************************************************
#? ----------------------------   7_run_step1_step2.R   ----------------------------
#? *********************************************************************************
#
#! Code to run model fitting for step 1 and step two, according to the species key in 
#!        code/fit_model/mod_key.csv file
#
#! Source ---------------------------------------------
#           - code/fit_model/back2d_covs_scales_2min.R:
#!              - Input:
#                   - data/y_dat8.rds: tibble with bird data
#                   - data/X.rds: tibble with covariate data
#                   - data/out/nsite_pk.rds: vector with number of sites in each park
#                   - data/src/key_park.rds: vector of all parks being analyzed:
#!              - Output: 
#                   - data/ana_file/{sps_loop}_step{step_numb}_jagsdata_{date_step1}.rds: jags data for species for analysis.
#                   - data/ana_file/{sps_loop}_step{step_numb}_model_{date_step1}.txt: model file for species.
#                   - data/model_res/{sps_loop}_step{step_numb}_output_{date_step1}{index_run}.rds: model results from jags model.
#                   - data/ana_file/{sps_loop}_step{step_numb}_metadata_{date_step1}_int.txt: metadata for the analysis (species, covariates, iterations, step, date, etc).
#                   - data/model_res/{file_name2}_{quant_name}_SCA_SEL_PARS.rds: file with which scales where selected as most influential.
#
#! Input ----------------------------------------------
#           - models/mod_all_covs.txt : model files with all covariates for step 1
#           - code/fit_model/mod_key.csv : table with a key to run models, that include: species names, step, result file from step 1 name for step 2 analysis, selected scales file name for step 2 analysis.
#           - data/model_res/{tib_loop$select}.rds :  if step 2 of analysis, this will link to which covariates and scales were selected at step 1 ([data/model_res/{file_name2}_{quant_name}_SCA_SEL_PARS.rds](./data/model_res/{file_name2}_{quant_name}_SCA_SEL_PARS.rds))

freshr::freshr()

# hg <- httpgd::hgd()
# httpgd::hgd_browse()

#! Package library and versions -----------------------
#  Created a library repo?
#  (  )yes  (  )no
#  renv::init()

# Load an existing library?
#  renv::status()
#  renv::restore()

# Installed new packages?
#  renv::snapshot()

test <- FALSE 
#! DEFINE STEP HERE !!!!!
step_number_define <- 2
#! Load packages --------------------------------------
#library(conflicted)
library(tidyverse)
library(glue)

#! Make functions -------------------------------------
colanmes <- colnames
lenght <- length
`%!in%` <- Negate(`%in%`)

#! MCMC settings --------------------------------------
niterations <- 60000
nburnin <- 30000
nchains <- 6
nthin <- 3
nadapt_min <- 7000

#! Source code and Import data ------------------------
## read files
model_file <- "models/mod_all_covs.txt"

master_tab <- read_csv("code/fit_model/mod_key.csv") %>%
                    filter(run == "yes") %>% 
                    filter(step %in% c(step_number_define)) %>% 
                    distinct()


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
    #nburnin <- 30000

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
                        #filter(scale50 == "no") %>% 
                        pull(betas)
        source("code/fit_model/step2_analysis.R")
                        
        } else { 
            print("step 1 all scales")
            # step 1 selected scales
            source("code/fit_model/back2d_covs_scales_2min_spscov.R")
        }
    }

cat(paste('\n ********************************************** \n \n \n 
              ---------------- all DONE Lol ----------------', 
     '\n\n \n ********************************************** \n'))
