# *********************************************************************************
# -----------------------------    Step 2 analysis   ------------------------------
# *********************************************************************************
# Code to get the results from the multi scale model (multiple_single_sps_spscovs.R)
#   and analise the selected scales and covariates (multiple_single_sps_spscovsSTEP2.R)
#
# Source ---------------------------------------------
#           - :
#           - :
#
# Input ----------------------------------------------
#           - data/model_res/{date_step1}_{fil_nam}_{park_name}_{niterations}its_2min_spscov_yr_run{x}.rds
#           - :
#
# Output ----------------------------------------------
#           - :
#           - :
#
# detach packages and clear workspace
#freshr::freshr()

#! Load packages ---------------------------------------
library(conflicted)
library(tidyverse)
library(glue)
library(MCMCvis)
library(jagsUI)
library(rjags)
library(splitstackshape)

conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::filter)

#! Make functions --------------------------------------
colanmes <- colnames
lenght <- length
`%!in%` <- Negate(`%in%`)

#! Import data -----------------------------------------
# model output
#! Moving this row to a source file for multiple species
# file_name <- '2025_02_24_HAWO_parks_30000its_2min_spscov_run1'

(sps <- sps_loop)
system_time1 <- Sys.time()
(date_step2 <- glue("{substr(system_time1, 1,4)}_{substr(system_time1, 6,7)}_{substr(system_time1, 9,10)}"))
script_name <- "step2_analysis.R"

## file paths
# data
SPS_DATA_PATH <- glue('data/ana_file/{date_step1}_data_{sps}_parks.rds') 

# inital values (z)
Z_DATA_PATH <- glue("data/ana_file/{date_step1}_data_{sps}_Z.rds")

## read files
jags_data <- read_rds(SPS_DATA_PATH) 
z_mod <- read_rds(Z_DATA_PATH)$Zst2

#! Get only parameters and scales that are relevant --------------------------------------------
cov_key2 <- cov_key[,which(cov_key != 0)] %>% colnames()

cov_key2_numb <- which(cov_key != 0) - 1
cov_key2_numb <- cov_key2_numb[-1]

# get X objects being used
pars_sca_mod <- cbind(cov_key2, cov_key2_numb, scales_loop) %>% 
                  as_tibble() %>% 
                  rename(cov_name = cov_key2, 
                         cov_numb = cov_key2_numb,
                         scal = scales_loop) %>% 
                  mutate(cov_numb = as.numeric(cov_numb),
                         scal = as.numeric(scal),
                         X = cov_name)

# remove n_bs <- nrow(pars_sca_mod) + 1
n_bs_new <- nrow(pars_sca_mod) + 1
# remove beta_numbs <- glue("beta[{seq(1,n_bs-1,1)}]")

par_key <- as_tibble(cbind(c("X1", "X2", "X3", "X4", 
                             "X51", "X52", "X53", "X6", "X7"),
                           c("BA", "DEN", "SHR", "DIV", 
                             "EAR", "MID", "LAT", "CAN", "DEB"))) %>% 
                    rename(xobj = V1,
                           X = V2)

pars_mod <- left_join(pars_sca_mod, par_key, by = "X")

#! Run the step 2 model ------------------------------------------------------------------------
(covs_names <- paste(pars_mod$X, collapse = "_"))
(sca_names <- paste(pars_mod$scal, collapse = "_"))

params <- c("beta0", "beta", "alpha0", "alpha", 
            "mu.beta0", "tau.beta0", "mu.alpha0", "tau.alpha0") %>% # Z, psi
          as.character()

n_as <- 3

inits <- function() {
    list(
        Z = z_mod,
        beta = rnorm(n_bs_new, 0.5),
        mu.alpha0 = rnorm(1, 0.5),
        alpha = rnorm(n_as, 0.5)
    )
}

# Define the model file and the output file name
model_file <- mod_name_loop
mod_name   <- glue("data/ana_file/{sps}_step{step_numb}_model_{date_step2}.txt") %>% as.character()

# Read the content of the model file
mod_content <- readLines(model_file)

# Combine the content into a single string
mod_string <- paste(mod_content, collapse = "\n")

# Write the content to the output file
if(test == FALSE){writeLines(mod_string, mod_name)}

# get the right data for the model - remove the covariates that are no longer in use
old_pars <- names(jags_data)
old_pars2 <- old_pars[grep("^X", old_pars)] 
old_pars3 <- old_pars2[!old_pars2 %in% c("Xp","Xa","Xb")]

rm_xs <- old_pars3[which(old_pars3 %!in% pars_mod$xobj)] ## which covs I have to remove from the jags data?
jags_data2 <- jags_data[setdiff(names(jags_data), rm_xs)] # remove them!!
jags_data2$n_bs <- n_bs_new
names(jags_data2)
str(jags_data2)

if(test == TRUE){
  nchains <- 1
  niterations <- 6
  nburnin <- 1
  nthin <- 1
  print("test with 5 iterations")
}

## initialize JAGS
cat("\n\n\n running jags \n\n\n\n")

samples_jags <- jags(data = jags_data2,
                      inits = inits,
                      parameters.to.save = params,
                      model.file = model_file,
                      n.chains = nchains,
                      n.adapt = max(500, ceiling(.1 * niterations)),
                      n.iter = niterations,
                      n.burnin = nburnin,
                      n.thin = nthin,
                      parallel = TRUE,
                      n.cores = nchains)

cat("\n\n\n model is done!!! \n\n\n\n")

file_name <- glue("{sps}_step{step_numb}_output_{date_step2}")

file_name2 <- paste0(file_name, 'run',
                      length(list.files(path = file.path(getwd(),"data/model_res/"),
                                        pattern = file_name,
                                        full.names = FALSE)) + 1)

folder_path <- "data/model_res"

if (!file.exists(folder_path)) {
  dir.create(folder_path, recursive = TRUE)
}

if(test == FALSE) {
  write_rds(samples_jags,
            file = glue('data/model_res/{file_name2}.rds')
            )
}

if(niterations > 10000) {
  write_rds(samples_jags,
            file = glue('data/model_res/{file_name2}.rds')
            )
}

system_time2 <- Sys.time()
if(as.numeric(system_time2 - system_time1) < 60) {
  time_it_took <- round(difftime(system_time2, system_time1, units = c("mins")),2)
  unit_time <- "mins"}
if(as.numeric(system_time2 - system_time1) >= 60 & 
      as.numeric(system_time2 - system_time1) <= 1440) {
  time_it_took <- round(difftime(system_time2, system_time1, units = c("hours")),2)
  unit_time <- "hours"}
if(as.numeric(system_time2 - system_time1) > 1440) {
  time_it_took <- round(difftime(system_time2, system_time1, units = c("days")),2)
  unit_time <- "days"}

# Print info in slurm.out file
paste('\n ************************************** \n \n \n ---------------- DONE ----------------', '\n\n',
      'Output File Name = ', glue('{file_name2}.rds'), '\n', 
      'Script = ', script_name, '\n', 
      'Species =', sps, '\n',
      'Covariates =', covs_names, '\n',
      'Scales =', sca_names, '\n',
      'Iterations =', niterations, '\n',
      'Run number =', str_split(file_name2, 'run', simplify = TRUE)[2], '\n',
      'Started running on =', system_time1, '\n',
      'Stopped running on =', system_time2, '\n',
      'Time it took =', time_it_took , unit_time,  '\n \n \n',
      '**************************************  \n') %>% 
      cat()


meta_name <- file(glue("data/ana_file/{sps}_step{step_numb}_metadata_{date_step2}.txt"))
if(test == FALSE){
    writeLines(paste(

                  'Species =', sps, '\n',
                  'Step =', step_numb, '\n',
                  'Date =', date_step2, '\n',

                  'Metadata File Name =', meta_name, '\n', 
                  'Results File Name =', glue('{file_name2}.rds'), '\n', 
                  'Model File Name =', glue("{mod_name}"), '\n',
                  'Data File Name =', SPS_DATA_PATH, '\n', 
                  'Z File Name =', Z_DATA_PATH, '\n', 

                  'Covariates =', covs_names, '\n',
                  'Scales =', sca_names, '\n',
                                  
                  'Script =', script_name, '\n',
                  'Iterations =', niterations, '\n',
                  'Chains =', nchains, '\n',
                  'Burn-in =', nburnin, '\n',
                  'Thinning =', nthin, '\n',

                  'Run number =', str_split(file_name2, 'run', simplify = TRUE)[2], '\n',
                  'Started running on =', system_time1, '\n',
                  'Stopped running on =', system_time2, '\n',
                  'Time it took =', time_it_took , unit_time), 

            meta_name)

  close(meta_name)
}

if(test == TRUE){
  cat(glue("\n \n \n \n Test for {sps} and step {step_numb} done!  \n \n \n \n \n"))
}
