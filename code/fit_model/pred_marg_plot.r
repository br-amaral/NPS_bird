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

hg <- httpgd::hgd()
httpgd::hgd_browse()

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
COEF_SPS_PATH <- "data/out/coefs_sps_sca.rds"
STEP2_INFO_PATH <- "data/mod_key2.csv"

## read files
res_mod <- read_rds(glue("data/model_res/{RES_MOD_FILE}.rds"))  # model file
dat_sca <- read_rds(COEF_SPS_PATH)       # which betas are important

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

beta_step2_get <- read_csv(STEP2_INFO_PATH) %>%  
                filter(step == 2,
                       AOU_Code == sps_loop) %>% 
                pull(select)

beta_step2 <- read_rds(glue("data/model_res/{beta_step2_get}.rds")) %>% 
                  filter(overlap0 == "no") %>% 
                  pull(betas)
  
sps_data <- read_rds(glue("data/ana_file/{DATA_SPS_PATH}"))

cov_key <- cbind(rbind("X1", "X2", "X3", "X4", "X5", "X5"),
                 rbind("beta1", "beta2", "beta3", "beta4", "beta5", "beta6"),
                 rbind("Tree Density",
                       "Conifer Density",
                       "Late Successional Tree Density",
                      "Shrub Basal Area",
                      "Tree Basal Area",
                      "Tree Basal Area Squared")) %>% 
            as_tibble() %>% 
            rename(data_tab = V1,
                   coef_ori = V2,
                   Covariate = V3)

#! TODO: begining of the loop ;)

dat_sca2 <- dat_sca %>% 
                    filter(sps == sps_loop)  %>% 
                    left_join(., cov_key, by = "Covariate") %>% 
                    relocate(data_tab, coef_ori) %>% 
                    filter(scale_selected == 1)

for(ii in 1:nrow(dat_sca2)){  # Fixed: dat_sca2 not dat_sca_loop

  beta_loop <- dat_sca2[ii,]
  scale_loop <- beta_loop %>% pull(scale) %>% as.numeric()
  
  element_name <- beta_loop$data_tab
  X_loop <- sps_data[[element_name]]
  X_loop2 <- X_loop[, scale_loop]
  
  X_range <- seq(from = min(X_loop2), to = max(X_loop2), length.out = 100)
  
  # Get beta index
  beta_index <- as.numeric(str_extract(beta_loop$coef_ori, "\\d+"))
  
  # Get posterior samples (efficient way)
  all_beta0_samples <- res_mod$sims.list$beta0  # Assuming this is a vector
  all_beta_samples <- res_mod$sims.list$beta[, beta_index]
  
  # Vectorized prediction
  n_samples <- length(all_beta_samples)
  predictions <- matrix(NA, nrow = n_samples, ncol = length(X_range))
   
  if(beta_index != 6){ for(s in 1:n_samples) {
      predictions[s, ] <- plogis(all_beta0_samples[s] + all_beta_samples[s] * X_range)
      }
  }
  
  if(beta_index == 6){ for(s in 1:n_samples) {
      predictions[s, ] <- plogis(all_beta0_samples[s] + all_beta_samples[s] * X_range  +  res_mod$sims.list$beta[s, 5] * X_range)
      }
  }
  
  # Calculate summary statistics (transpose to match your original code)
  array_psi_pred <- t(predictions)
  pred_mean <- apply(array_psi_pred, 1, mean)
  pred_median <- apply(array_psi_pred, 1, median)
  pred_lower <- apply(array_psi_pred, 1, quantile, 0.025)
  pred_upper <- apply(array_psi_pred, 1, quantile, 0.975)
  
# Store results
    pred_data <- tibble(
      covariate = beta_loop$Covariate,
      x_value = X_range,
      pred_mean = pred_mean,
      pred_median = pred_median,
      pred_lower = pred_lower,
      pred_upper = pred_upper
    )
    
    # Create plot
    p <- ggplot(pred_data, aes(x = x_value)) +
      geom_ribbon(aes(ymin = pred_lower, ymax = pred_upper), 
                  alpha = 0.3, fill = "steelblue") +
      geom_line(aes(y = pred_mean), color = "darkblue", size = 1.2) +
      labs(x = beta_loop$Covariate, 
           y = "Predicted Probability",
           title = glue("{sps_loop}: {beta_loop$Covariate}"),
           subtitle = glue("Scale: {scale_loop}")) +
      theme_minimal()
    
    print(p)
    
  }
