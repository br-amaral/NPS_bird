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
# TODO: overlay species curves
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
library(MCMCvis)
library(jagsUI)
library(rjags)
library(BayesPostEst)

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
COEF_SPS_PATH <- "data/out/coefs_sps_sca.rds"
STEP2_INFO_PATH <- "data/mod_key.csv"

## read files
dat_sca <- read_rds(COEF_SPS_PATH)       # which betas are important
beta_key <- read_csv(STEP2_INFO_PATH) %>% 
              filter(step == 2,
                     run == "yes") %>% 
              filter(AOU_Code != "BHVI")

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

# get the data for the predictions
for(sps_res in 1:nrow(beta_key)){
  RES_MOD_FILE <- beta_key$result[sps_res]
  res_mod <- read_rds(glue("data/model_res/{RES_MOD_FILE}.rds"))  # model file
  sps_loop <- substr(RES_MOD_FILE, 1, 4)
  sps_dat_name <- glue("{sps_loop}_step1_jagsdata")

  DATA_SPS_PATH <- 
        list.files(path = file.path(getwd(),"data/ana_file/"),
                                            pattern = sps_dat_name,
                                            full.names = FALSE)  %>% 
                  as_tibble() %>% 
                  slice(1) %>% 
                  pull()

  beta_step2_get <- beta_key %>%  
                  filter(AOU_Code == sps_loop) %>% 
                  pull(select)

  beta_step2 <- read_rds(glue("data/model_res/{beta_step2_get}.rds")) %>% 
                    filter(overlap0 == "no") %>% 
                    pull(betas)
    
  sps_data <- read_rds(glue("data/ana_file/{DATA_SPS_PATH}"))

  dat_sca2 <- dat_sca %>% 
                      filter(sps == sps_loop,  
                      #! plot only non-overlapping zeros
                             includes_zero == "black")  %>%
                      left_join(., cov_key, by = "Covariate") %>% 
                      relocate(data_tab, coef_ori) %>% 
                      filter(scale_selected == 1) %>% 
                      arrange(data_tab)

  for(ii in 1:nrow(dat_sca2)){  # Fixed: dat_sca2 not dat_sca_loop

    beta_loop <- dat_sca2[ii,]
    scale_loop <- beta_loop %>% pull(scale) %>% as.numeric()
    
    element_name <- beta_loop$data_tab
    X_loop <- sps_data[[element_name]]
    X_loop2 <- X_loop[, scale_loop]
    
    X_range <- seq(from = min(X_loop2), to = max(X_loop2), length.out = 100)
    
    # Get beta index
    beta_index <- as.numeric(str_extract(beta_loop$coef_ori, "\\d+"))
    
    beta_param <- glue("beta[{beta_index}]")
  
    # Extract all posterior samples from all chains
    all_chains <- do.call(rbind, res_mod)  # Combine all chains
  
    # Get posterior samples for the specific beta coefficient
    if(beta_param %in% colnames(all_chains)) {
      all_beta_samples <- all_chains[, beta_param]
    } else {
      print(glue("Parameter {beta_param} not found"))
      next
    }

    # Get intercept - use the mean intercept across sites
    if("mu.beta0" %in% colnames(all_chains)) {
      all_beta0_samples <- all_chains[, "mu.beta0"]
    } else {
      # Alternative: use mean of all beta0 parameters
      beta0_cols <- grep("beta0\\[", colnames(all_chains), value = TRUE)
      all_beta0_samples <- apply(all_chains[, beta0_cols], 1, mean)
    }

    # Get posterior samples (efficient way)
    # all_beta0_samples <- res_mod$sims.list$beta0  # Assuming this is a vector
    # all_beta_samples <- res_mod$sims.list$beta[, beta_index]
    
    # Vectorized prediction
    n_samples <- length(all_beta_samples)
    predictions <- matrix(NA, nrow = n_samples, ncol = length(X_range))
    
    if(beta_index != 6){ 
      for(s in 1:n_samples) {
        predictions[s, ] <- plogis(all_beta0_samples[s] + all_beta_samples[s] * X_range)
      }
    }
    
    if(beta_index == 6){ 
      # Get beta[5] for quadratic term
      all_beta5_samples <- all_chains[, "beta[5]"]
      for(s in 1:n_samples) {
        predictions[s, ] <- plogis(all_beta0_samples[s] + all_beta_samples[s] * X_range + all_beta5_samples[s] * (X_range)^2)
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
    ) %>% 
    mutate(sps = sps_loop, scale = scale_loop)
    
    assign(glue("pred_{sps_loop}_beta{beta_index}_scale{scale_loop}"), pred_data) 
    # Create plot
    p <- ggplot(pred_data, aes(x = x_value)) +
      geom_ribbon(aes(ymin = pred_lower, ymax = pred_upper), 
                  alpha = 0.3, fill = "steelblue") +
      geom_line(aes(y = pred_mean), color = "darkblue", linewidth = 1.2) +
      labs(x = beta_loop$Covariate, 
          y = "Predicted Probability",
          title = glue("{sps_loop}: {beta_loop$Covariate}"),
          subtitle = glue("Scale: {scale_loop}")) +
      theme_minimal()
    
    print(p)
    
  }
}

# get park ranges
XDAT_PATH <- "data/X.rds"
X10 <- read_rds(file = XDAT_PATH)

pacman::p_load(rcartocolor, patchwork)
safe_pal <- carto_pal(12, "Safe")

#? TREE DENSITY -----------------------------------------------------------------
tree_den_axis_s <- X10 %>% 
                    select(park, treeden_ha_site, treeden_ha_park, treeden_ha_coun) %>% 
                    group_by(park) %>% 
                    summarise(mean_treeden_ha_site = mean(treeden_ha_site, na.rm = T),
                              mean_treeden_ha_park = mean(treeden_ha_park, na.rm = T),
                              mean_treeden_ha_coun = mean(treeden_ha_coun, na.rm = T),
                              lo_treeden_ha_site = quantile(treeden_ha_site, probs = 0.05, na.rm = T),
                              lo_treeden_ha_park = quantile(treeden_ha_park, probs = 0.05, na.rm = T),
                              lo_treeden_ha_coun = quantile(treeden_ha_coun, probs = 0.05, na.rm = T),
                              up_treeden_ha_site = quantile(treeden_ha_site, probs = 0.95, na.rm = T),
                              up_treeden_ha_park = quantile(treeden_ha_park, probs = 0.95, na.rm = T),
                              up_treeden_ha_coun = quantile(treeden_ha_coun, probs = 0.95, na.rm = T))

baww_lims <- read_rds("data/out/X_vals_BAWW.rds")
blbw_lims <- read_rds("data/out/X_vals_BLBW.rds")
brcr_lims <- read_rds("data/out/X_vals_BRCR.rds")
btbw_lims <- read_rds("data/out/X_vals_BTBW.rds")
btnw_lims <- read_rds("data/out/X_vals_BTNW.rds")
dowo_lims <- read_rds("data/out/X_vals_DOWO.rds")
hawo_lims <- read_rds("data/out/X_vals_HAWO.rds")
heth_lims <- read_rds("data/out/X_vals_HETH.rds")
oven_lims <- read_rds("data/out/X_vals_OVEN.rds")
revi_lims <- read_rds("data/out/X_vals_REVI.rds")
scta_lims <- read_rds("data/out/X_vals_SCTA.rds")
veer_lims <- read_rds("data/out/X_vals_VEER.rds")
wbnu_lims <- read_rds("data/out/X_vals_WBNU.rds")

beta1_preds <- rbind(
                      pred_BLBW_beta1_scale1 %>% mutate(x_ori = (x_value * blbw_lims$siteDEN_sd) + blbw_lims$siteDEN_mean),
                      pred_BTNW_beta1_scale1 %>% mutate(x_ori = (x_value * btnw_lims$siteDEN_sd) + btnw_lims$siteDEN_mean),
                      pred_DOWO_beta1_scale1 %>% mutate(x_ori = (x_value * dowo_lims$siteDEN_sd) + dowo_lims$siteDEN_mean),
                      pred_BRCR_beta1_scale2 %>% mutate(x_ori = (x_value * brcr_lims$parkDEN_sd) + brcr_lims$parkDEN_mean),
                      pred_REVI_beta1_scale2 %>% mutate(x_ori = (x_value * revi_lims$parkDEN_sd) + revi_lims$parkDEN_mean),
                      pred_VEER_beta1_scale2 %>% mutate(x_ori = (x_value * veer_lims$parkDEN_sd) + veer_lims$parkDEN_mean),
                      pred_BAWW_beta1_scale3 %>% mutate(x_ori = ((x_value * baww_lims$counDEN_sd) + baww_lims$counDEN_mean)/4),
                      pred_BTBW_beta1_scale3 %>% mutate(x_ori = ((x_value * btbw_lims$counDEN_sd) + btbw_lims$counDEN_mean)/4),
                      pred_OVEN_beta1_scale3 %>% mutate(x_ori = ((x_value * oven_lims$counDEN_sd) + oven_lims$counDEN_mean)/4)
)

ggplot(beta1_preds, aes(x = x_ori, y = pred_mean)) +
  geom_line(aes(color = factor(sps)), linewidth = 1.2) +
  facet_wrap(~ scale, scales = "free_x",
             labeller = labeller(scale = c("3" = "Landscape Scale", 
                                           "2" = "Park Scale", 
                                           "1" = "Local Scale"))) +  
  labs(x = glue("\n\n{unique(beta1_preds$covariate)[1]}"), 
       y = "Predicted Occupancy Probability\n",
       title = glue("Tree Density (stems/ha)\n"),
       color = "Species") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.border = element_rect(color = "black", linewidth = 0.6, fill = NA),  # Added fill = NA
        panel.background = element_rect(fill = "white", color = NA),  # Ensure white background
        strip.text = element_text(face = "bold", size = 16),      # Facet titles
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),      # Main title
        axis.title = element_text(size = 14),                     # Axis titles
        axis.text = element_text(size = 12),                      # Axis text
        legend.title = element_text(size = 14, face = "bold", hjust = 0.5),    # Legend title
        legend.text = element_text(size = 12)                     # Legend text
  ) +
  scale_color_manual(values = safe_pal)

#? CONIFER BA -----------------------------------------------------------------
BA_con_axis_s <- X10 %>% 
                    select(park, BA_m2ha_perc_con_site, BA_m2ha_perc_con_park, BA_m2ha_perc_con_coun) %>% 
                    group_by(park) %>% 
                    summarise(mean_BA_m2ha_perc_con_site = mean(BA_m2ha_perc_con_site, na.rm = T),
                              mean_BA_m2ha_perc_con_park = mean(BA_m2ha_perc_con_park, na.rm = T),
                              mean_BA_m2ha_perc_con_coun = mean(BA_m2ha_perc_con_coun, na.rm = T),
                              lo_BA_m2ha_perc_con_site = quantile(BA_m2ha_perc_con_site, probs = 0.05, na.rm = T),
                              lo_BA_m2ha_perc_con_park = quantile(BA_m2ha_perc_con_park, probs = 0.05, na.rm = T),
                              lo_BA_m2ha_perc_con_coun = quantile(BA_m2ha_perc_con_coun, probs = 0.05, na.rm = T),
                              up_BA_m2ha_perc_con_site = quantile(BA_m2ha_perc_con_site, probs = 0.95, na.rm = T),
                              up_BA_m2ha_perc_con_park = quantile(BA_m2ha_perc_con_park, probs = 0.95, na.rm = T),
                              up_BA_m2ha_perc_con_coun = quantile(BA_m2ha_perc_con_coun, probs = 0.95, na.rm = T))

beta2_preds <- rbind(
  pred_BAWW_beta2_scale1 %>% mutate(x_ori = (x_value * baww_lims$siteBAcon_sd) + baww_lims$siteBAcon_mean),
  pred_BRCR_beta2_scale1 %>% mutate(x_ori = (x_value * brcr_lims$siteBAcon_sd) + brcr_lims$siteBAcon_mean),
  pred_BTNW_beta2_scale1 %>% mutate(x_ori = (x_value * btnw_lims$siteBAcon_sd) + btnw_lims$siteBAcon_mean),
  pred_HAWO_beta2_scale1 %>% mutate(x_ori = (x_value * hawo_lims$siteBAcon_sd) + hawo_lims$siteBAcon_mean)
)

ggplot(beta2_preds, aes(x = x_ori, y = pred_mean)) +
  geom_line(aes(color = factor(sps)), linewidth = 1.2) +
  facet_wrap(~ scale, scales = "free_x",
             labeller = labeller(scale = c("3" = "Landscape Scale", 
                                           "2" = "Park Scale", 
                                           "1" = "Local Scale"))) +  
  labs(x = glue("\n\n{unique(beta2_preds$covariate)[1]}"), 
       y = "Predicted Occupancy Probability\n",
       title = glue("Conifer % Basal area\n"),
       color = "Species") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.border = element_rect(color = "black", linewidth = 0.6, fill = NA),  # Added fill = NA
        panel.background = element_rect(fill = "white", color = NA),  # Ensure white background
        strip.text = element_text(face = "bold", size = 16),      # Facet titles
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),      # Main title
        axis.title = element_text(size = 14),                     # Axis titles
        axis.text = element_text(size = 12),                      # Axis text
        legend.title = element_text(size = 14, face = "bold", hjust = 0.5),    # Legend title
        legend.text = element_text(size = 12)                     # Legend text
  ) +
  scale_color_manual(values = safe_pal)

#? LATE SUCCESSIONAL BASAL AREA -----------------------------------------------------------------
BA_latesuc_axis_s <- X10 %>% 
                    select(park, BA_m2ha_perc_latesuc_site, BA_m2ha_perc_latesuc_park, BA_m2ha_perc_latesuc_coun) %>% 
                    group_by(park) %>% 
                    summarise(mean_BA_m2ha_perc_latesuc_site = mean(BA_m2ha_perc_latesuc_site, na.rm = T),
                              mean_BA_m2ha_perc_latesuc_park = mean(BA_m2ha_perc_latesuc_park, na.rm = T),
                              mean_BA_m2ha_perc_latesuc_coun = mean(BA_m2ha_perc_latesuc_coun, na.rm = T),
                              lo_BA_m2ha_perc_latesuc_site = quantile(BA_m2ha_perc_latesuc_site, probs = 0.05, na.rm = T),
                              lo_BA_m2ha_perc_latesuc_park = quantile(BA_m2ha_perc_latesuc_park, probs = 0.05, na.rm = T),
                              lo_BA_m2ha_perc_latesuc_coun = quantile(BA_m2ha_perc_latesuc_coun, probs = 0.05, na.rm = T),
                              up_BA_m2ha_perc_latesuc_site = quantile(BA_m2ha_perc_latesuc_site, probs = 0.95, na.rm = T),
                              up_BA_m2ha_perc_latesuc_park = quantile(BA_m2ha_perc_latesuc_park, probs = 0.95, na.rm = T),
                              up_BA_m2ha_perc_latesuc_coun = quantile(BA_m2ha_perc_latesuc_coun, probs = 0.95, na.rm = T))

beta3_preds <- rbind(
  pred_BLBW_beta3_scale1 %>% mutate(x_ori = (x_value * blbw_lims$siteBAlar_sd) + blbw_lims$siteBAlar_mean),
  pred_BTBW_beta3_scale1 %>% mutate(x_ori = (x_value * btbw_lims$siteBAlar_sd) + btbw_lims$siteBAlar_mean),
  pred_BTNW_beta3_scale1 %>% mutate(x_ori = (x_value * btnw_lims$siteBAlar_sd) + btnw_lims$siteBAlar_mean),
  pred_DOWO_beta3_scale1 %>% mutate(x_ori = (x_value * dowo_lims$siteBAlar_sd) + dowo_lims$siteBAlar_mean),
  pred_REVI_beta3_scale1 %>% mutate(x_ori = (x_value * revi_lims$siteBAlar_sd) + revi_lims$siteBAlar_mean),
  pred_WBNU_beta3_scale1 %>% mutate(x_ori = (x_value * wbnu_lims$siteBAlar_sd) + wbnu_lims$siteBAlar_mean),
  pred_OVEN_beta3_scale2 %>% mutate(x_ori = (x_value * oven_lims$parkBAlar_sd) + oven_lims$parkBAlar_mean),
  pred_VEER_beta3_scale2 %>% mutate(x_ori = (x_value * veer_lims$parkBAlar_sd) + veer_lims$parkBAlar_mean),
  pred_WOTH_beta3_scale2 %>% mutate(x_ori = (x_value * woth_lims$parkBAlar_sd) + woth_lims$parkBAlar_mean)
)

ggplot(beta3_preds, aes(x = x_ori, y = pred_mean)) +
  geom_line(aes(color = factor(sps)), linewidth = 1.2) +
  facet_wrap(~ scale, scales = "free_x",
             labeller = labeller(scale = c("3" = "Landscape Scale", 
                                           "2" = "Park Scale", 
                                           "1" = "Local Scale"))) +  
  labs(x = glue("\n\n{unique(beta3_preds$covariate)[1]}"), 
       y = "Predicted Occupancy Probability\n",
       title = glue("Late Success. Forest % Basal area\n"),
       color = "Species") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.border = element_rect(color = "black", linewidth = 0.6, fill = NA),  # Added fill = NA
        panel.background = element_rect(fill = "white", color = NA),  # Ensure white background
        strip.text = element_text(face = "bold", size = 16),      # Facet titles
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),      # Main title
        axis.title = element_text(size = 14),                     # Axis titles
        axis.text = element_text(size = 12),                      # Axis text
        legend.title = element_text(size = 14, face = "bold", hjust = 0.5),    # Legend title
        legend.text = element_text(size = 12)                     # Legend text
  ) +
  scale_color_manual(values = safe_pal)

#? SHRUB BASAL AREA -----------------------------------------------------------------
shrub_BA_axis_s <- X10 %>% 
                    select(park, shrub_BA_perc_site, shrub_BA_perc_park, shrub_BA_perc_coun) %>% 
                    group_by(park) %>% 
                    summarise(mean_shrub_BA_perc_site = mean(shrub_BA_perc_site, na.rm = T),
                              mean_shrub_BA_perc_park = mean(shrub_BA_perc_park, na.rm = T),
                              mean_shrub_BA_perc_coun = mean(shrub_BA_perc_coun, na.rm = T),
                              lo_shrub_BA_perc_site = quantile(shrub_BA_perc_site, probs = 0.05, na.rm = T),
                              lo_shrub_BA_perc_park = quantile(shrub_BA_perc_park, probs = 0.05, na.rm = T),
                              lo_shrub_BA_perc_coun = quantile(shrub_BA_perc_coun, probs = 0.05, na.rm = T),
                              up_shrub_BA_perc_site = quantile(shrub_BA_perc_site, probs = 0.95, na.rm = T),
                              up_shrub_BA_perc_park = quantile(shrub_BA_perc_park, probs = 0.95, na.rm = T),
                              up_shrub_BA_perc_coun = quantile(shrub_BA_perc_coun, probs = 0.95, na.rm = T))

beta4_preds <- rbind(
  pred_BTBW_beta4_scale1 %>% mutate(x_ori = (x_value * btbw_lims$siteSHR_sd) + btbw_lims$siteSHR_mean),
  pred_BTNW_beta4_scale1 %>% mutate(x_ori = (x_value * btnw_lims$siteSHR_sd) + btnw_lims$siteSHR_mean),
  pred_DOWO_beta4_scale1 %>% mutate(x_ori = (x_value * dowo_lims$siteSHR_sd) + dowo_lims$siteSHR_mean),
  pred_VEER_beta4_scale2 %>% mutate(x_ori = (x_value * veer_lims$parkSHR_sd) + veer_lims$parkSHR_mean),
  pred_REVI_beta4_scale3 %>% mutate(x_ori = ((x_value * revi_lims$counSHR_sd) + revi_lims$counSHR_mean)/4)
)

ggplot(beta4_preds, aes(x = x_ori, y = pred_mean)) +
  geom_line(aes(color = factor(sps)), linewidth = 1.2) +
  facet_wrap(~ scale, scales = "free_x",
             labeller = labeller(scale = c("3" = "Landscape Scale", 
                                           "2" = "Park Scale", 
                                           "1" = "Local Scale"))) +  
  labs(x = glue("\n\n{unique(beta4_preds$covariate)[1]}"), 
       y = "Predicted Occupancy Probability\n",
       title = glue("Shrub % Cover\n"),
       color = "Species") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.border = element_rect(color = "black", linewidth = 0.6, fill = NA),  # Added fill = NA
        panel.background = element_rect(fill = "white", color = NA),  # Ensure white background
        strip.text = element_text(face = "bold", size = 16),      # Facet titles
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),      # Main title
        axis.title = element_text(size = 14),                     # Axis titles
        axis.text = element_text(size = 12),                      # Axis text
        legend.title = element_text(size = 14, face = "bold", hjust = 0.5),    # Legend title
        legend.text = element_text(size = 12)                     # Legend text
  ) +
  scale_color_manual(values = safe_pal)

#? TREE BASAL AREA -----------------------------------------------------------------
tree_BA_axis_s <- X10 %>% 
                    select(park, tree_BA_m2ha_site, tree_BA_m2ha_park, tree_BA_m2ha_coun) %>% 
                    group_by(park) %>% 
                    summarise(mean_tree_BA_m2ha_site = mean(tree_BA_m2ha_site, na.rm = T),
                              mean_tree_BA_m2ha_park = mean(tree_BA_m2ha_park, na.rm = T),
                              mean_tree_BA_m2ha_coun = mean(tree_BA_m2ha_coun, na.rm = T),
                              lo_tree_BA_m2ha_site = quantile(tree_BA_m2ha_site, probs = 0.05, na.rm = T),
                              lo_tree_BA_m2ha_park = quantile(tree_BA_m2ha_park, probs = 0.05, na.rm = T),
                              lo_tree_BA_m2ha_coun = quantile(tree_BA_m2ha_coun, probs = 0.05, na.rm = T),
                              up_tree_BA_m2ha_site = quantile(tree_BA_m2ha_site, probs = 0.95, na.rm = T),
                              up_tree_BA_m2ha_park = quantile(tree_BA_m2ha_park, probs = 0.95, na.rm = T),
                              up_tree_BA_m2ha_coun = quantile(tree_BA_m2ha_coun, probs = 0.95, na.rm = T))
                              
beta56_preds <- rbind(
  # Linear effects (beta5)
  pred_HETH_beta5_scale1 %>% mutate(x_ori = (x_value * heth_lims$siteBA_sd) + heth_lims$siteBA_mean),
  pred_VEER_beta5_scale1 %>% mutate(x_ori = (x_value * veer_lims$siteBA_sd) + veer_lims$siteBA_mean),
  # Quadratic effects (beta6)
  pred_BRCR_beta6_scale1 %>% mutate(x_ori = (x_value * brcr_lims$siteBA_sd) + brcr_lims$siteBA_mean),
  pred_BTNW_beta6_scale1 %>% mutate(x_ori = (x_value * btnw_lims$siteBA_sd) + btnw_lims$siteBA_mean),
  pred_OVEN_beta6_scale1 %>% mutate(x_ori = (x_value * oven_lims$siteBA_sd) + oven_lims$siteBA_mean),
  pred_REVI_beta6_scale1 %>% mutate(x_ori = (x_value * revi_lims$siteBA_sd) + revi_lims$siteBA_mean),
  pred_SCTA_beta6_scale1 %>% mutate(x_ori = (x_value * scta_lims$siteBA_sd) + scta_lims$siteBA_mean),
  pred_WBNU_beta6_scale1 %>% mutate(x_ori = (x_value * wbnu_lims$siteBA_sd) + wbnu_lims$siteBA_mean),
  pred_WOTH_beta6_scale1 %>% mutate(x_ori = (x_value * woth_lims$siteBA_sd) + woth_lims$siteBA_mean),
  pred_DOWO_beta6_scale3 %>% mutate(x_ori = ((x_value * dowo_lims$counBA_sd) + dowo_lims$counBA_mean)/4)
)

ggplot(beta56_preds, aes(x = x_ori, y = pred_mean)) +
  geom_line(aes(color = factor(sps)), linewidth = 1.2) +
  facet_wrap(~ scale, scales = "free_x",
             labeller = labeller(scale = c("3" = "Landscape Scale", 
                                           "2" = "Park Scale", 
                                           "1" = "Local Scale"))) +  
  labs(x = glue("\n\n{unique(beta56_preds$covariate)[1]}"), 
       y = "Predicted Occupancy Probability\n",
       title = glue("Tree Basal area (m²/ha)\n"),
       color = "Species") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.border = element_rect(color = "black", linewidth = 0.6, fill = NA),  # Added fill = NA
        panel.background = element_rect(fill = "white", color = NA),  # Ensure white background
        strip.text = element_text(face = "bold", size = 16),      # Facet titles
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),      # Main title
        axis.title = element_text(size = 14),                     # Axis titles
        axis.text = element_text(size = 12),                      # Axis text
        legend.title = element_text(size = 14, face = "bold", hjust = 0.5),    # Legend title
        legend.text = element_text(size = 12)                     # Legend text
  ) +
  scale_color_manual(values = safe_pal)

