#? *********************************************************************************
#? ------------------------------   pred_marg_plot.r   -----------------------------
#? *********************************************************************************
#
#! #TODO: check conifer BA x axis
#! #TODO: create panel with park values on bottom of plots
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
STEP2_INFO_PATH <- "code/fit_model/mod_key.csv"
if(substr(getwd(), 1, 3) == "/Us") {direc <- "local"} else {direc <- "hpc"}
if(direc == "local"){STEP2_INFO_PATH <- glue("/Users/bamaral/Documents/GitHub/NPS_bird_copy/{STEP2_INFO_PATH}")}

## read files
dat_sca <- read_rds(COEF_SPS_PATH)       # which betas are important
beta_key <- read_csv(STEP2_INFO_PATH) %>% 
              filter(step == 3,
                     run == "yes") #%>% 
              # filter(AOU_Code != "BHVI",
              #        AOU_Code != "YBSA"
              #        )

baww_lims <- read_rds("data/out/X_vals_BAWW.rds")
bhvi_lims <- read_rds("data/out/X_vals_BHVI.rds")
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
woth_lims <- read_rds("data/out/X_vals_WOTH.rds")
ybsa_lims <- read_rds("data/out/X_vals_YBSA.rds")

cov_key <- cbind(rbind("X1", "X2", "X3", "X4", "X5"),
                rbind("beta1", "beta2", "beta3", "beta4", "beta5"),
                rbind("Tree Density",
                      "Conifer Density",
                      "Late Successional Tree Density",
                      "Shrub Basal Area",
                      "Tree Basal Area")) %>% 
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
                      arrange(data_tab) %>% 
                      filter(!is.na(coef_ori))
  if(nrow(dat_sca2) != 0){
    for(ii in 1:nrow(dat_sca2)){  # Fixed: dat_sca2 not dat_sca_loop

      beta_loop <- dat_sca2[ii,]
      scale_loop <- beta_loop %>% pull(scale) %>% as.numeric()
      
      element_name <- beta_loop$data_tab
      X_loop <- sps_data[[element_name]]
      X_loop2 <- X_loop[, scale_loop]
      
      X_range <- seq(from = min(X_loop2), to = max(X_loop2), length.out = 100)
      
      # Get beta index
      beta_index <- as.numeric(str_extract(beta_loop$coef_ori, "\\d+"))
      beta_index_order <- as.numeric(str_extract(beta_loop$coef, "\\d+"))

      beta_param <- glue("beta[{beta_index}]")
    
      # # Extract all posterior samples from all chains
      # all_chains <- do.call(rbind, res_mod)  # Combine all chains
    
      # # Get posterior samples for the specific beta coefficient
      # if(beta_param %in% colnames(all_chains)) {
      #   all_beta_samples <- all_chains[, beta_param]
      # } else {
      #   print(glue("Parameter {beta_param} not found"))
      #   next
      # }

      # # Get intercept - use the mean intercept across sites
      # if("mu.beta0" %in% colnames(all_chains)) {
      #   all_beta0_samples <- all_chains[, "mu.beta0"]
      # } else {
      #   # Alternative: use mean of all beta0 parameters
      #   beta0_cols <- grep("beta0\\[", colnames(all_chains), value = TRUE)
      #   all_beta0_samples <- apply(all_chains[, beta0_cols], 1, mean)
      # }

      # Get posterior samples (efficient way)
      all_beta0_samples <- res_mod$sims.list$mu.beta0  # Assuming this is a vector
      all_beta_samples <- res_mod$sims.list$beta[, beta_index_order]
      
      # Vectorized prediction
      n_samples <- length(all_beta_samples)
      predictions <- matrix(NA, nrow = n_samples, ncol = length(X_range))
      
      for(s in 1:n_samples) {
        predictions[s, ] <- plogis(all_beta0_samples[s] + all_beta_samples[s] * X_range)
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
      
      print(glue("pred_{sps_loop}_beta{beta_index}_scale{scale_loop}"))
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
}

# get park ranges
XDAT_PATH <- "data/X.rds"
X10 <- read_rds(file = XDAT_PATH)

pacman::p_load(rcartocolor, patchwork)
safe_pal <- carto_pal(12, "Safe")
set.seed(123)
safe_pal <- sample(safe_pal)

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



#? AUTOMATED PREDICTION PROCESSING FUNCTION ---------------------------------------
process_beta_predictions <- function(beta_num, covariate_suffix) {
  
  # Get all prediction objects for this beta
  pred_pattern <- glue("pred_.*_beta{beta_num}_scale[1-3]")
  pred_objects <- ls(pattern = pred_pattern, envir = .GlobalEnv)
  
  if(length(pred_objects) == 0) {
    warning(glue("No prediction objects found for beta{beta_num}"))
    return(NULL)
  }
  
  # Process each prediction object
  pred_list <- list()
  
  for(obj_name in pred_objects) {
    # Extract species and scale from object name
    sps <- str_extract(obj_name, "(?<=pred_)[A-Z]{4}")
    scale_num <- str_extract(obj_name, "(?<=scale)[1-3]") %>% as.numeric()
    
    # Get the prediction data
    pred_data <- get(obj_name, envir = .GlobalEnv)
    
    # Get the corresponding limits object - real scale, not standardized
    lims_obj_name <- glue("{tolower(sps)}_lims")
    
    if(exists(lims_obj_name)) {
      lims_data <- get(lims_obj_name, envir = .GlobalEnv)
      
      # Determine scale-specific column names
      scale_suffix <- case_when(
        scale_num == 1 ~ "site",
        scale_num == 2 ~ "park", 
        scale_num == 3 ~ "coun"
      )
      
      sd_col <- glue("{scale_suffix}{covariate_suffix}_sd")
      mean_col <- glue("{scale_suffix}{covariate_suffix}_mean")
      
      # Check if columns exist in limits data
      if(sd_col %in% names(lims_data) & mean_col %in% names(lims_data)) {
        
        # Apply unstandardization based on scale
        pred_data_processed <- pred_data %>%
          mutate(
             x_ori = #case_when(
            #   # Special handling for county-level tree density 
            #   scale_num == 3 & covariate_suffix == "DEN" ~ 
            #     ((x_value * lims_data[[sd_col]]) + lims_data[[mean_col]]),
              
            #   # Special handling for county-level shrub cover 
            #   scale_num == 3 & covariate_suffix == "SHR" ~ 
            #     ((x_value * lims_data[[sd_col]]) + lims_data[[mean_col]]),
              
            #   # Special handling for county-level basal area 
            #   scale_num == 3 & covariate_suffix == "BA" ~ 
            #     (x_value * lims_data[[sd_col]]) + (lims_data[[mean_col]]),
              
              # Standard unstandardization for all other cases
              #TRUE ~ 
              (x_value * lims_data[[sd_col]]) + lims_data[[mean_col]]
            #)
          )
        
        pred_list[[obj_name]] <- pred_data_processed
        
      } else {
        warning(glue("Columns {sd_col} or {mean_col} not found in {lims_obj_name}"))
      }
    } else {
      warning(glue("Limits object {lims_obj_name} not found"))
    }
  }
  
  # Combine all processed predictions
  if(length(pred_list) > 0) {
    combined_preds <- do.call(rbind, pred_list)
    return(combined_preds)
  } else {
    warning(glue("No valid predictions processed for beta{beta_num}"))
    return(NULL)
  }
}

#? APPLY FUNCTION TO ALL BETAS ------------------------------------------------
# Define covariate mappings
beta_covariates <- list(
  "1" = "DEN",     # Tree Density
  "2" = "BAcon",   # Conifer Basal Area  
  "3" = "BAlar",   # Late Successional Basal Area
  "4" = "SHR",     # Shrub Cover
  "5" = "BA",      # Tree Basal Area (linear)
)

# Process all betas automatically
beta1_preds <- process_beta_predictions(1, beta_covariates[["1"]])
beta2_preds <- process_beta_predictions(2, beta_covariates[["2"]])
beta3_preds <- process_beta_predictions(3, beta_covariates[["3"]])
beta4_preds <- process_beta_predictions(4, beta_covariates[["4"]])
beta5_preds <- process_beta_predictions(5, beta_covariates[["5"]]) 

#? TREE DENSITY -----------------------------------------------------------------

beta1_lims <- c(floor(min(beta1_preds$x_ori) / 5) * 5, ceiling(max(beta1_preds$x_ori) / 5) * 5)

ggplot(beta1_preds, aes(x = x_ori, y = pred_mean)) +
  geom_line(aes(color = factor(sps)), linewidth = 1.2) +
  facet_wrap(~ scale, scales = "free_x",
             labeller = labeller(scale = c("3" = "County Scale", 
                                           "2" = "Park Scale", 
                                           "1" = "Local Scale"))) +  
  labs(x = glue("\nTree Density (stems/ha)"), 
       y = "Predicted Occupancy Probability\n",
       title = glue("Tree Density\n"),
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
  scale_color_manual(values = safe_pal) +
  ylim(0, 1) #+
  #xlim(beta1_lims)

ggsave("figures/pred_den.svg", plot = last_plot(), device = "svg", width = 6, height = 6)
ggsave("figures/pred_den.png", plot = last_plot(), device = "png", width = 6, height = 6)

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

beta2_lims <- c(floor(min(beta2_preds$x_ori) / 5) * 5, ceiling(max(beta2_preds$x_ori) / 5) * 5)

ggplot(beta2_preds, aes(x = x_ori, y = pred_mean)) +
  geom_line(aes(color = factor(sps)), linewidth = 1.2) +
  facet_wrap(~ scale, scales = "free_x",
             labeller = labeller(scale = c("3" = "Landscape Scale", 
                                           "2" = "Park Scale", 
                                           "1" = "Local Scale"))) +  
  labs(x = glue("\nConifer Basal area  (m²/ha)"), 
       y = "Predicted Occupancy Probability\n",
       title = glue("Conifers\n"),
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
  scale_color_manual(values = safe_pal) +
  ylim(0, 1) #+
  #xlim(beta2_lims)

ggsave("figures/pred_con.svg", plot = last_plot(), device = "svg", width = 10, height = 6)
ggsave("figures/pred_con.png", plot = last_plot(), device = "png", width = 10, height = 6)

#? LATE SUCCESSIONAL BASAL AREA -----------------------------------------------------------------
BA_latesuc_axis_s <- X10 %>% 
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

beta3_lims <- c(floor(min(beta3_preds$x_ori) / 5) * 5, ceiling(max(beta3_preds$x_ori) / 5) * 5)

ggplot(beta3_preds, aes(x = x_ori, y = pred_mean)) +
  geom_line(aes(color = factor(sps)), linewidth = 1.2) +
  facet_wrap(~ scale, scales = "free_x",
             labeller = labeller(scale = c("3" = "Landscape Scale", 
                                           "2" = "Park Scale", 
                                           "1" = "Local Scale"))) +  
  labs(x = glue("Late Success. Forest Basal area (m²/ha)\n"), 
       y = "Predicted Occupancy Probability\n",
       title = glue("Late Success.\n"),
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
  scale_color_manual(values = safe_pal) +
  ylim(0, 1) #+
  #xlim(beta3_lims)

ggsave("figures/pred_lat.svg", plot = last_plot(), device = "svg", width = 14, height = 6)
ggsave("figures/pred_lat.png", plot = last_plot(), device = "png", width = 14, height = 6)

#? SHRUB BASAL AREA -----------------------------------------------------------------
shrub_BA_axis_s <- X10 %>% 
                    select(park, shrub_avg_cov_site, shrub_avg_cov_park, shrub_cov_coun) %>% 
                    group_by(park) %>% 
                    summarise(mean_shrub_avg_cov_site = mean(shrub_avg_cov_site, na.rm = T),
                              mean_shrub_avg_cov_park = mean(shrub_avg_cov_park, na.rm = T),
                              mean_shrub_cov_coun = mean(shrub_cov_coun, na.rm = T),
                              lo_shrub_avg_cov_site = quantile(shrub_avg_cov_site, probs = 0.05, na.rm = T),
                              lo_shrub_avg_cov_park = quantile(shrub_avg_cov_park, probs = 0.05, na.rm = T),
                              lo_shrub_cov_coun = quantile(shrub_cov_coun, probs = 0.05, na.rm = T),
                              up_shrub_avg_cov_site = quantile(shrub_avg_cov_site, probs = 0.95, na.rm = T),
                              up_shrub_avg_cov_park = quantile(shrub_avg_cov_park, probs = 0.95, na.rm = T),
                              up_shrub_cov_coun = quantile(shrub_cov_coun, probs = 0.95, na.rm = T))

beta4_lims <- c(floor(min(beta4_preds$x_ori) / 5) * 5, ceiling(max(beta4_preds$x_ori) / 5) * 5)

ggplot(beta4_preds, aes(x = x_ori, y = pred_mean)) +
  geom_line(aes(color = factor(sps)), linewidth = 1.2) +
  facet_wrap(~ scale, scales = "free_x",
             labeller = labeller(scale = c("3" = "Landscape Scale", 
                                           "2" = "Park Scale", 
                                           "1" = "Local Scale"))) +  
  labs(x = glue("\nShrub Cover %"), 
       y = "Predicted Occupancy Probability\n",
       title = glue("Shrub Cover\n"),
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
  scale_color_manual(values = safe_pal) +
  ylim(0, 1) #+
  # xlim(beta4_lims)

ggsave("figures/pred_shr.svg", plot = last_plot(), device = "svg", width = 14, height = 6)
ggsave("figures/pred_shr.png", plot = last_plot(), device = "png", width = 14, height = 6)

#? TREE BASAL AREA -----------------------------------------------------------------
tree_BA_axis_s <- X10 %>% 
                    select(park, BA_m2ha_site, BA_m2ha_park, BA_m2ha_coun) %>% 
                    group_by(park) %>% 
                    summarise(mean_BA_m2ha_site = mean(BA_m2ha_site, na.rm = T),
                              mean_BA_m2ha_park = mean(BA_m2ha_park, na.rm = T),
                              mean_BA_m2ha_coun = mean(BA_m2ha_coun, na.rm = T),
                              lo_BA_m2ha_site = quantile(BA_m2ha_site, probs = 0.05, na.rm = T),
                              lo_BA_m2ha_park = quantile(BA_m2ha_park, probs = 0.05, na.rm = T),
                              lo_BA_m2ha_coun = quantile(BA_m2ha_coun, probs = 0.05, na.rm = T),
                              up_BA_m2ha_site = quantile(BA_m2ha_site, probs = 0.95, na.rm = T),
                              up_BA_m2ha_park = quantile(BA_m2ha_park, probs = 0.95, na.rm = T),
                              up_BA_m2ha_coun = quantile(BA_m2ha_coun, probs = 0.95, na.rm = T))

beta5_lims <- c(floor(min(beta5_preds$x_ori) / 5) * 5, ceiling(max(beta5_preds$x_ori) / 5) * 5)

ggplot(beta5_preds, aes(x = x_ori, y = pred_mean)) +
  geom_line(aes(color = factor(sps)), linewidth = 1.2) +
  facet_wrap(~ scale, scales = "free_x",
             labeller = labeller(scale = c("3" = "Landscape Scale", 
                                           "2" = "Park Scale", 
                                           "1" = "Local Scale"))) +  
  labs(x = glue("Tree Basal Area (m²/ha)\n"), 
       y = "Predicted Occupancy Probability\n",
       title = glue("Tree Basal Area\n"),
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
  scale_color_manual(values = safe_pal) +
  ylim(0, 1) #+
  #xlim(beta5_lims)

ggsave("figures/pred_BA.svg", plot = last_plot(), device = "svg", width = 14, height = 6)
ggsave("figures/pred_BA.png", plot = last_plot(), device = "png", width = 14, height = 6)

