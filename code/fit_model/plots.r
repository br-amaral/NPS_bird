# *********************************************************************************
# -------------------------------   Plot model output   ---------------------------
# *********************************************************************************
# Code to make plots
#
hg <- httpgd::hgd()
# detach packages and clear workspace
freshr::freshr()
#
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
file_name <- "2025_02_22_BHVI_10000its_2min_spscov_step2_run2"

samples_jags <- read_rds(glue("data/model_res/{file_name}.rds"))

# get parameter names
scales_names <- grep("^scales_", colnames(samples_jags[[1]]), value = TRUE)
all_params <- c("mu.alpha0", "mu.beta0", "beta", "alpha", scales_names)

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

# check how similar scales are if more than one was selected
two_sca <- beta_key %>% 
  filter(grepl("_", sca_sel))
file_name_data <- glue("data/ana_file/{substr(file_name, 1, 10)}_data_{substr(file_name, 12, 15)}_parks.rds")
jags_data <- read_rds(file_name_data)
str(jags_data)

dat_sca <- cbind(jags_data$X52[,2],
                 jags_data$X52[,1],
                 jags_data$y[,2]) %>% 
                 as_tibble() %>% 
                 rename(cov1 = V1, cov2 = V2, park = V3)  %>% 
                 mutate(cov1 = as.numeric(cov1),
                        cov2 = as.numeric(cov2),
                        park = as.factor(park))

ggplot(dat_sca) +
    geom_point(aes(x = cov1, y = cov2, 
                   colour = park),
               alpha = 0.3) + 
    theme_bw() + ggtitle(glue("correlation = {round(cor(dat_sca[,1], dat_sca[,2], method = 'spearman'),2)}"))



