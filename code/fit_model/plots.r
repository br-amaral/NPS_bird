# *********************************************************************************
# -------------------------------   Plot model output   ---------------------------
# *********************************************************************************
# Code to make plots
#
# setwd("/Volumes/zipkinlab/bamaral/NPS_bird_copy/")
# setwd("/Volumes/rs-025/zipkinlab/bamaral/NPS_bird_copy")
# detach packages and clear workspace
freshr::freshr()
#
# Load packages -------------------------------------------------------------------
library(conflicted)
library(tidyverse)
library(glue)
library(MCMCvis)
library(rjags)
library(BayesPostEst)
#
conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::filter)
# conflicts_prefer(scales::alpha)
#
# Make functions ------------------------------------------------------------------
colanmes <- colnames
lenght <- length
`%!in%` <- Negate(`%in%`)

#! Import data --------------------------------------------------------------------
## file paths and read files
# when loading the model results, get the most updated file?
file_name <- "BHVI_step2_output_2025_10_01run1"

samples_jags <- read_rds(glue("data/model_res/{file_name}.rds"))

# get parameter names
scales_names <- grep("^scales_", colnames(samples_jags[[1]]), value = TRUE)
all_params <- c("mu.alpha0", "mu.beta0", "beta", "beta_int", "alpha", scales_names)
if(substr(file_name, nchar(file_name)-2, nchar(file_name)) == "int"){all_params <- c(all_params, "beta_int")}

#! Par estimates ------------------------------------------------------------------
par(mfrow = c(1,1))
MCMCplot(samples_jags,
         params = all_params,
         main = file_name,
         ref_ovl = TRUE)

#! Traceplots ---------------------------------------------------------------------
MCMCtrace(samples_jags,
          params = all_params,
          #main = file_name,
          ind = TRUE,
          pdf = FALSE,
          exact = TRUE,
          Rhat = TRUE,
          n.eff = TRUE)

#! Summary ------------------------------------------------------------------------
MCMCsummary(samples_jags,
            params = all_params,
            round = 2)

#! get beta parameters and selected scales ----------------------------------------
# beta parameters that the 50 percent CI does not include 0
betas <- tidybayes::get_variables(samples_jags)
n_betas1 <- sub("\\[.*", "", betas) 
n_betas <- length(n_betas1[n_betas1 == "beta"]) - 1
betas_name <- paste0(n_betas1[n_betas1 == "beta"][-1], seq(1:n_betas))

quant_group <- c(0.3, 0.7)
# quant_group <- c(0.25, 0.75)

beta_key <- tibble(
  betas = betas_name, 
  overlap0 = as.character(NA), 
  sca_sel = as.character(NA),
  sca1 = as.numeric(NA),
  sca2 = as.numeric(NA),
  sca3 = as.numeric(NA),
  qt_lo = quant_group[1],
  qt_up = quant_group[2]
)

for(ii in 1:n_betas) {
# betas
  beta_loop1 <- MCMCchains(samples_jags, params = glue("beta"))
  beta_loop2 <- beta_loop1[,ii]
    
  #quantiles <- quantile(beta_loop2, )
  quantiles <- quantile(beta_loop2, quant_group)

  lower_quantile <- quantiles[1]
  upper_quantile <- quantiles[2]
  
  # Check if quantiles overlap zero
  if (lower_quantile <= 0 && upper_quantile >= 0) {
    beta_key$overlap0[ii] <- "yes"
  } else {
    beta_key$overlap0[ii] <- "no"
  }

# scales
  loop_sca <- glue("scales_beta{ii}")
  sca_beta <- MCMCchains(samples_jags, params = loop_sca)

  tb_mcmc_scales_i <- table(sca_beta)/sum(table(sca_beta))
  selected_scales <- as.integer(names(which.max(tb_mcmc_scales_i)))

  beta_key$sca_sel[ii] <- selected_scales
  beta_key$sca1[ii] <- tb_mcmc_scales_i[1]
  beta_key$sca2[ii] <- tb_mcmc_scales_i[2]
  beta_key$sca3[ii] <- tb_mcmc_scales_i[3]

}

beta_key

quant_name <- glue("{substr(quant_group[1], 3, 4)}_{substr(quant_group[2], 3, 4)}")
# save beta and scale selection values
write_rds(beta_key, file = glue("data/model_res/{file_name}_{quant_name}_SCA_SEL_PARS.rds"))

#! Coefficient tables --------------------------------------------------------------

if(substr(getwd(), 1, 3) == "/Us") {direc <- "local"} else {direc <- "hpc"}

if(direc == "local"){
    master_tab <- read_csv("/Users/bamaral/Documents/GitHub/NPS_bird_copy/code/fit_model/mod_key.csv") %>%
            #filter(run == "yes") %>% 
            filter(step %in% 3) %>% 
            distinct()

    } else {master_tab <- read_csv("code/fit_model/mod_key.csv") %>%
            #filter(run == "yes") %>% 
            filter(step %in% 3) %>% 
            distinct()
    }

master_tab <- master_tab %>% filter(AOU_Code != "BCCH")
# Initialize empty tibble outside the function
coef_fim <- tibble()

coef_tab <- function(file_name, species_code){
  samples_jags <- read_rds(glue("data/model_res/{file_name}.rds"))

  # get parameter names
  scales_names <- grep("^scales_", colnames(samples_jags[[1]]), value = TRUE)
  all_params <- c("mu.alpha0", "mu.beta0", "beta", "alpha", scales_names)
  if(substr(file_name, nchar(file_name)-2, nchar(file_name)) == "int"){all_params <- c(all_params, "beta_int")}

  print(file_name)
  sps_coef <- MCMCsummary(samples_jags,
                          params = all_params,
                          round = 2)  
  r_nam <- rownames(sps_coef)

  sps_coef <- sps_coef %>% 
                as_tibble() %>% 
                mutate(sps = species_code,
                       cov = r_nam ) %>% 
                relocate(sps, cov)
  
  return(sps_coef)
}

for(ii in 1:nrow(master_tab)){
  sps_result <- coef_tab(master_tab$result[ii], master_tab$AOU_Code[ii])
  coef_fim <- bind_rows(coef_fim, sps_result)
}
  
write_rds(coef_fim, file = "data/out/coef_fim.rds")

#! Density and traceplots --------------------------------------------------------------
den_tra_p <- function(file_name){

  samples_jags <- read_rds(glue("data/model_res/{file_name}.rds"))

  # get parameter names
  scales_names <- grep("^scales_", colnames(samples_jags[[1]]), value = TRUE)
  all_params <- c("mu.alpha0", "mu.beta0", "beta", "alpha", scales_names)
  if(substr(file_name, nchar(file_name)-2, nchar(file_name)) == "int"){all_params <- c(all_params, "beta_int")}

  print(file_name)
  
  # Save as PDF using MCMCtrace built-in functionality
  MCMCtrace(samples_jags,
            params = all_params,
            ind = TRUE,
            pdf = TRUE,
            filename = glue("figures/{file_name}_traceplots"),
            exact = TRUE,
            Rhat = TRUE,
            n.eff = TRUE)
  
  # # Save as multi-page SVG (one file per page)
  # svg(glue("figures/{file_name}_traceplots_%03d.svg"))
  # MCMCtrace(samples_jags,
  #           params = all_params,
  #           ind = TRUE,
  #           pdf = FALSE,
  #           exact = TRUE,
  #           Rhat = TRUE,
  #           n.eff = TRUE)
  # dev.off()
}

for(ii in 1:nrow(master_tab)){
  
  den_tra_p(master_tab$result[ii])
}
