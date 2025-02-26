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
#           - data/model_res/{date_out}_{fil_nam}_{park_name}_{niterations}its_2min_spscov_yr_run{x}.rds
#           - :
#
# Output ----------------------------------------------
#           - :
#           - :
#
# detach packages and clear workspace
freshr::freshr()

nchains <- 8
niterations <- 30000
nburnin <- 20000
nthin <- 5

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
file_name <- '2025_02_25_HETH_parks_30000its_2min_spscov_run1'
(sps <- substr(file_name, 12, 15))
(date_out <- substr(file_name, 1,  10))
system_time1 <- Sys.time()
script_name <- "step2_analysis.R"

## file paths
# selected parameters
PAR_MOD_PATH <- glue('data/model_res/{file_name}_SCA_SEL_PARS.rds')

# model results
MODEL_RES_PATH <- glue("data/model_res/{file_name}.rds")

# model data
MODEL_DATA_PATH <- glue('data/ana_file/{date_out}_data_{sps}_parks.rds') 

# meta data 
META_DATA_PATH <- glue('data/ana_file/{date_out}_metadata_{sps}_parks.txt')

# inital values (z)
Z_DATA_PATH <- glue("data/ana_file/{date_out}_data_{sps}_Z.rds")

## read files
print(meta_data <- readr::read_lines(META_DATA_PATH))
samples_jags <- read_rds(MODEL_RES_PATH)
jags_data <- read_rds(MODEL_DATA_PATH) 
pars_sca_mod <- read_rds(PAR_MOD_PATH)

# get X objects being used
pars_mod_name <- substr(meta_data[7], 15, nchar(meta_data[7]))
pars_mod_name2 <- gsub(" ", "", pars_mod_name)
split_vector <- strsplit(pars_mod_name2, "_")
pars_name_vec <- unlist(split_vector)

(pars_mod <- pars_sca_mod  %>% 
              mutate(X = pars_name_vec) %>% 
              filter(overlap0 == "no") %>% 
              mutate(betas = sub("(.{4})(.*)", "\\1[\\2", betas)) %>% 
              mutate(betas = sub("(.{6})(.*)", "\\1]\\2", betas)) %>% 
              mutate(b_sca_numb = ifelse(grepl("_", sca_sel), 2, 1),
                     b_sca_numb2 = b_sca_numb) %>% 
              expandRows(.,"b_sca_numb2")) 

sca_mod <- pars_sca_mod  %>% 
              filter(overlap0 == "no") %>% 
              select(sca_sel) %>% 
              pull()
sca_mod_split <- sca_mod %>% strsplit("_") %>% unlist()  %>% as.numeric()
pars_mod$uni_sca <- sca_mod_split

z_mod <- read_rds(Z_DATA_PATH)$Zst2

# get parameter names
scales_names <- grep("^scales_", colnames(samples_jags[[1]]), value = TRUE)
(all_params <- c("mu.alpha0", "mu.beta0", "beta", "alpha", scales_names))

n_bs <- length(pars_name_vec) + 1
n_bs_new <- c(length(unique(pars_mod$betas)),nrow(pars_mod))
beta_numbs <- glue("beta[{seq(1,n_bs-1,1)}]")

par_key <- as_tibble(cbind(c("X1", "X2", "X3", "X4", "X51", "X52", "X53", "X6", "X7"),
                           c("BA", "DEN", "SHR", "DIV", "EAR", "MID", "LAT", "CAN", "DEB"))) %>% 
                    rename(xobj = V1,
                           X = V2)

pars_mod <- left_join(pars_mod, par_key, by = "X")

#! Get only parameters and scales that are relevant --------------------------------------------
## quick check
MCMCsummary(samples_jags,
            params = all_params,
            round = 2)

#! Run the step 2 model ------------------------------------------------------------------------
covs_names <- paste(pars_mod$X, collapse = "_")
sca_names <- paste(pars_mod$uni_sca, collapse = "_")

params <- c("beta0", "beta", "alpha0", "alpha", 
            "mu.beta0", "tau.beta0", "mu.alpha0", "tau.alpha0") %>% # Z, psi
          as.character()

n_as <- 3

n_bs_new <- n_bs_new[2] + 1

inits <- function() {
    list(
        Z = z_mod,
        beta = rnorm(n_bs_new, 0.5),
        mu.alpha0 = rnorm(1, 0.5),
        alpha = rnorm(n_as, 0.5)
    )
}

# Define the model file and the output file name
model_file <- glue("models/mod_1_vector_spscov_{sps}_step2.txt")
mod_name   <- glue("data/ana_file/{date_out}_mod_{sps}_step2.txt") %>% as.character()

# Read the content of the model file
mod_content <- readLines(model_file)

# Combine the content into a single string
mod_string <- paste(mod_content, collapse = "\n")

# Write the content to the output file
writeLines(mod_string, mod_name)

# get the right data for the model - remove the covariates that are no longer in use
old_pars <- names(jags_data)
old_pars2 <- old_pars[grep("^X", old_pars)] 
old_pars3 <- old_pars2[!old_pars2 %in% c("Xp","Xa","Xb")]

rm_xs <- old_pars3[which(old_pars3 %!in% pars_mod$xobj)] ## which covs I have to remove from the jags data?
jags_data2 <- jags_data[setdiff(names(jags_data), rm_xs)] # remove them!!
jags_data2$n_bs <- n_bs_new
names(jags_data2)
str(jags_data2)

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
                      n.thin = nthin)

cat("\n\n\n model is done!!! \n\n\n\n")

file_name <- glue("{date_out}_{sps}_{niterations}its_2min_spscov_step2_")

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

# Get covariate names
covs_names2 <- paste(unique(pars_mod$X), collapse = "_")

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
      'Covariates =', covs_names2, '\n',
      'Iterations =', niterations, '\n',
      'Run number =', str_split(file_name2, 'run', simplify = TRUE)[2], '\n',
      'Started running on =', system_time1, '\n',
      'Stopped running on =', system_time2, '\n',
      'Time it took =', time_it_took , unit_time,  '\n \n \n',
      '**************************************  \n') %>% 
      cat()


meta_name <- file(glue("data/ana_file/{date_out}_metadata_{sps}_step2.txt"))
writeLines(paste(

                ' Results File Name =', glue('{file_name2}.rds'), '\n', 
                'Data File Name =', glue("data/ana_file/{date_out}_data_{sps}_step2.rds"), '\n', 
                'Script =', script_name, '\n',
                'Model file =', glue("{mod_name}"), '\n',
                'Species =', sps, '\n',
                'Number of betas =', n_bs_new, '\n',
                'Covariates =', covs_names2, '\n',
                'Scales =', sca_names, '\n',
                'Iterations =', niterations, '\n',
                'Chains =', nchains, '\n',
                'Burn-in =', nburnin, '\n',
                'Run number =', str_split(file_name2, 'run', simplify = TRUE)[2], '\n',
                'Started running on =', system_time1, '\n',
                'Stopped running on =', system_time2, '\n',
                'Time it took =', time_it_took , unit_time), 
          meta_name)

close(meta_name)
