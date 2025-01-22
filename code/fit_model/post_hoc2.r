# *********************************************************************************
# -----------------------------   Post hoc analysis   -----------------------------
# *********************************************************************************
# Code to get the results from the multi scale model (multiple_single_sps_spscovs.R)
#   and analise the selected scales and covariates
#
# Source ---------------------------------------------
#           - :
#           - :
#
# Input ----------------------------------------------
#           - data/model_res/{date_out}_{fil_nam}_{park_name}_{niterations}its_2min_spscov_yr_run{x}.rds
#           - :
#
# Output ----------------------------------------------
#           - :
#           - :
#
# detach packages and clear workspace
#if(!require(freshr)){install.packages('freshr')}
#freshr::freshr()
#
#! Load packages ---------------------------------------
library(conflicted)
library(tidyverse)
library(glue)
library(MCMCvis)
library(jagsUI)
library(rjags)

conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::filter)

#! Make functions --------------------------------------
colanmes <- colnames
lenght <- length
`%!in%` <- Negate(`%in%`)

#! Import data -----------------------------------------
# file_name <- '2024_09_19_BHVI_parks_50000its_2min_spscov_run1'
(sps <- substr(file_name, 12, 15))
(date_out <- substr(file_name, 1,  10))
system_time1 <- Sys.time()
script_name <- "post_hoc2.r"
## file paths
# model data
MODEL_DATA_PATH <- glue('data/ana_file/{date_out}_data_{sps}_parks.rds') 
if(grepl("_yr_", file_name)){
    MODEL_DATA_PATH <- glue('data/ana_file/{date_out}_data_{sps}_parks_yr.rds')
}
# inital values (z)
Z_DATA_PATH <- glue("data/ana_file/{date_out}_data_{sps}_Z.rds")

## read files
jags_data <- read_rds(MODEL_DATA_PATH) 

sca_mod <- pars_sca_mod %>% as.numeric()
z_mod <- read_rds(Z_DATA_PATH)$Zst2

par_key <- as_tibble(cbind(c("X1", "X2", "X3", "X4", "X51", "X52", "X53", "X6", "X7"),
                           c("BA", "DEN", "SHR", "DIV", "EAR", "MID", "LAT", "CAN", "DEB"),
                           c(rep(1,9)))) %>% 
                    rename(X = V1,
                           par = V2,
                           ind = V3) %>% 
                    mutate(ind = as.numeric(ind))

n_bs_new <- length(pars_mod)

par_key_fil <- par_key %>% 
                filter(par %in% pars_mod_name)

sum(par_key_fil$ind) == n_bs_new

n_bs_new <- n_bs_new + 1

par_key_fil

#! Run the post hoc model ----------------------------------------------------------------------
covs_names <- paste(par_key_fil$par, collapse = "_")
sca_names <- paste(sca_mod, collapse = "_")

paste('\n ************************************* \n \n \n Running JAGS post-hoc for:', '\n',
      '  Species =', sps, '\n',
      '  Number of betas =', n_bs_new, '\n',
      '  Betas =', covs_names, '\n',
      '  Scales =', sca_names, '\n',
      '  Iterations =', niterations, '\n',
      '  Burn-in =', nburnin, '\n',
      '  Started running on =', Sys.time(),  '\n \n \n',
      '**************************************
      ') %>% cat()

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
model_file <- glue("models/{mod_name2}.txt")
mod_name   <- glue("models/{mod_name2}") %>% as.character()

# Read the content of the model file
mod_content <- readLines(model_file)

# Combine the content into a single string
mod_string <- paste(mod_content, collapse = "\n")

# Write the content to the output file
writeLines(mod_string, mod_name)

# get the right data for the model
rm_xs <- par_key[which(par_key$X %!in% par_key_fil$X),1] %>% pull()
rm_xs2 <- rm_xs[which(rm_xs %in% names(jags_data))]

jags_data2 <- jags_data[setdiff(names(jags_data), rm_xs2)]
jags_data2$n_bs <- n_bs_new
jags_data2$sca_mod <- sca_mod
names(jags_data2)
## initialize JAGS
cat("\n\n\n running first jags \n\n\n\n")

# jags_model <- rjags::jags.model(
#   file = model_file,
#   data = jags_data2,
#   inits = inits, 
#   n.chains = nchains,
#   n.adapt = max(100, ceiling(.1 * niterations)),
#   quiet = FALSE
# )

# cat("\n\n\n first done, running second \n\n\n\n") 

# # burn-in
# if (nburnin > 0) {
#   message(paste("burn-in:", nburnin, "iterations"))
#   rjags::jags.samples(
#     jags_model,
#     variable.names = params,
#     n.iter = niterations,
#     thin = nthin,
#     quiet = FALSE
#   )
# }

# # write_rds(jags_model,
# #           file = glue("data/model_res/M12.rds"))

# cat("\n\n\n second done, running third \n\n\n\n")

# # posterior simulation
# samples_jags <- coda.samples(
#   jags_model,
#   variable.names = params,
#   n.iter = niterations,
#   thin = nthin,
#   quiet = FALSE
# )

samples_jags <- jags(data = jags_data2,
                      inits = inits,
                      parameters.to.save = params,
                      model.file = model_file,
                      n.chains = nchains,
                      n.adapt = max(100, ceiling(.1 * niterations)),
                      n.iter = niterations,
                      n.burnin = nburnin,
                      n.thin = 2)

#   file = ,
#   data = ,
#   inits = inits, 
#   n.chains = nchains,
#   n.adapt = max(100, ceiling(.1 * niterations)),
#   quiet = FALSE
cat("\n\n\n third done!!! \n\n\n\n")

file_name <- glue("{date_out}_{sps}_{niterations}its_2min_spscov_yr_POSTHOCui_")

file_name2 <- paste0(file_name, 'run',
                      length(list.files(path = file.path(getwd(),"data/model_res/"),
                                        pattern = file_name,
                                        full.names = FALSE)) + 1)

folder_path <- "data/model_res"

if (!file.exists(folder_path)) {
  dir.create(folder_path, recursive = TRUE)
}

write_rds(samples_jags,
          file = glue('data/model_res/{file_name2}.rds')
          )

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

# Get covariate names
covs_names2 <- paste(pars_mod, collapse = "_")

# Print info in slurm.out file
paste('\n ************************************** \n \n \n ---------------- DONE ----------------', '\n\n',
      'Output File Name = ', glue('{file_name2}.rds'), '\n', 
      'Script = ', script_name, '\n', 
      'Species =', sps, '\n',
      'Covariates =', covs_names2, '\n',
      'Iterations =', niterations, '\n',
      'Run number =', str_split(file_name2, 'run', simplify = TRUE)[2], '\n',
      'Started running on =', system_time1, '\n',
      'Stopped running on =', system_time2, '\n',
      'Time it took =', time_it_took , unit_time,  '\n \n \n',
      '**************************************  \n') %>% 
      cat()


meta_name <- file(glue("data/ana_file/{date_out}_metadata_{sps}_yr_POSTHOC.txt"))
writeLines(paste(

                ' Results File Name =', glue('{file_name2}.rds'), '\n', 
                'Data File Name =', glue("data/ana_file/{date_out}_data_{sps}_yr_POSTHOC.rds"), '\n', 
                'Script =', script_name, '\n',
                'Model file =', glue("{mod_name}"), '\n',
                'Species =', sps, '\n',
                'Covariates =', covs_names2, '\n',
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

