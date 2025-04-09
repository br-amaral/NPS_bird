# *********************************************************************************
# -------------------------------   Plot model output   ---------------------------
# *********************************************************************************
# Code to make plots
#
# detach packages and clear workspace
freshr::freshr()

hg <- httpgd::hgd()
# Load packages -------------------------------------------------------------------
library(conflicted)
library(tidyverse)
library(glue)
library(MCMCvis)
library(rjags)
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
file_name <- "REVI_step1_output_2025_04_05run1"

samples_jags <- read_rds(glue("data/model_res/{file_name}.rds"))
  
# get parameter names
scales_names <- grep("^scales_", colnames(samples_jags[[1]]), value = TRUE)

if(class(samples_jags) == "jagsUI"){
    all_params <- c("mu.alpha0", "mu.beta0", "beta", "alpha")
    all_params <- all_params[which(all_params %in% samples_jags$parameters)]
  } else {
    if(length(scales_names) != 0) {
      all_params <- c("mu.alpha0", "mu.beta0", "beta", "alpha", scales_names)
        } else {
          if("beta[1]" %in% varnames(samples_jags) | "beta" %in% varnames(samples_jags)) {
            all_params <- c("mu.alpha0", "mu.beta0", "beta", "alpha")
          } else { 
            all_params <- c("mu.alpha0", "mu.beta0", "alpha")
        }
      }
    }

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

beta_key <- tibble(
  betas = betas_name, 
  overlap0 = as.character(NA), 
  sca_sel = as.character(NA),
  sca1 = as.numeric(NA),
  sca2 = as.numeric(NA),
  sca3 = as.numeric(NA)
)

for(ii in 1:n_betas) {
# betas
  beta_loop1 <- MCMCchains(samples_jags, params = glue("beta"))
  beta_loop2 <- beta_loop1[,ii]
    
  quantiles <- quantile(beta_loop2, c(0.25, 0.75))
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

# save beta and scale selection values
write_rds(beta_key, file = glue("data/model_res/{file_name}_SCA_SEL_PARS.rds"))
