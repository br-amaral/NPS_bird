#? *********************************************************************************
#? ------------------------------   pred_marg_plot.r   -----------------------------
#? *********************************************************************************
#
#! #TODO: get better color pallete
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

# hg <- httpgd::hgd()
# httpgd::hgd_browse()

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

#if(direc == "local"){STEP2_INFO_PATH <- glue("/Users/bamaral/Documents/GitHub/NPS_bird_copy/{STEP2_INFO_PATH}")}
if(direc == "local"){STEP2_INFO_PATH <- glue("/Users/bamaral/Library/CloudStorage/OneDrive-MichiganStateUniversity/GitHubOne/NPS_bird_copy/{STEP2_INFO_PATH}")}

## read files
dat_sca <- read_rds(COEF_SPS_PATH) %>% as_tibble()    # which betas are important
beta_key <- read_csv(STEP2_INFO_PATH) %>% 
              filter(step == 3,
               #      run == "yes") #%>% 
              AOU_Code != "BCCH"
              #        AOU_Code != "YBSA"
                      )

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
                   Covariate = V3) %>% 
            mutate(mean = as.character(NA), sd = as.character(NA), min = as.character(NA), max = as.character(NA))

sca_key <- cbind(rbind("1", "2", "3"),
                rbind("site", "park", "coun"),
                rbind("Site",
                      "Park",
                      "County")) %>% 
            as_tibble() %>% 
            rename(sca_num = V1,
                   sca_key = V2,
                   sca_nam = V3) %>% 
            mutate(sca_num = as.numeric(sca_num))

cov_key[which(cov_key$coef_ori == 'beta1'), 4:7] <- 
                  as.list(c("DEN_mean", "DEN_sd", "DEN_min", "DEN_max"))

cov_key[which(cov_key$coef_ori == 'beta2'), 4:7] <- 
                  as.list(c("BAcon_mean", "BAcon_sd", "BAcon_min", "BAcon_max"))

cov_key[which(cov_key$coef_ori == 'beta3'), 4:7] <- 
                  as.list(c("BAlar_mean", "BAlar_sd", "BAlar_min", "BAlar_max"))

cov_key[which(cov_key$coef_ori == 'beta4'), 4:7] <- 
                  as.list(c("SHR_mean", "SHR_sd", "SHR_min", "SHR_max"))

cov_key[which(cov_key$coef_ori == 'beta5'), 4:7] <- 
                  as.list(c("BA_mean", "BA_sd", "BA_min", "BA_max"))
cov_key <- cov_key %>% mutate(coef = coef_ori) %>% rename(meanc = mean, sdc = sd)

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
                    #filter(overlap0 == "no") %>% 
                    pull(betas)
    
  sps_data <- read_rds(glue("data/ana_file/{DATA_SPS_PATH}"))

  dat_sca2 <- dat_sca %>% 
                      mutate(overlap0med = overlap0)  %>% 
                      filter(sps == sps_loop,  
                      #! plot only non-overlapping zeros
                             overlap0med == "no")  %>%
                      left_join(., cov_key, by = c("coef", "Covariate")) %>% 
                      relocate(data_tab, coef_ori) %>% 
                      filter(scale_selected == 1) %>% 
                      arrange(data_tab) %>% 
                      filter(!is.na(coef_ori))
                      
  if(nrow(dat_sca2) != 0){
    for(ii in 1:nrow(dat_sca2)){  

      beta_loop <- dat_sca2[ii,]
      scale_loop <- beta_loop %>% pull(scale) %>% as.numeric()
      
      sca_key_sps <- sca_key %>% 
                        filter(sca_num == scale_loop) %>% 
                        pull(sca_key)

      cov_key_sps <- cov_key  %>% 
                        filter(coef_ori == beta_loop$coef_ori) %>% 
                        mutate(meanc = glue("{sca_key_sps}{meanc}"),
                               sdc = glue("{sca_key_sps}{sdc}"),
                               min = glue("{sca_key_sps}{min}"),
                               max = glue("{sca_key_sps}{max}"))

      # element_name <- beta_loop$data_tab
      # X_loop <- sps_data[[element_name]]
      # X_loop2 <- X_loop[, scale_loop]
      
      # X_range <- seq(from = min(X_loop2), to = max(X_loop2), length.out = 100)

      lims_obj_name <- glue("{tolower(sps_loop)}_lims")

      if(exists(lims_obj_name)) {
        lims_data <- get(lims_obj_name, envir = .GlobalEnv)  %>% 
                        select(cov_key_sps$meanc,
                               cov_key_sps$sdc,
                               cov_key_sps$min,
                               cov_key_sps$max) 
        colnames(lims_data) <- c("meanc", "sdc", "min", "max")
                        }
        
      # sd_col <- glue("{scale_suffix}{covariate_suffix}_sd")
      # mean_col <- glue("{scale_suffix}{covariate_suffix}_mean")

      X_range_ori <- seq(from = lims_data$min, to = lims_data$max, length.out = 100)
      X_range <- (X_range_ori - lims_data$meanc) / lims_data$sdc

      # Get beta index
      beta_index <- as.numeric(str_extract(beta_loop$coef_ori, "\\d+"))

      beta_index_order <- as.numeric(str_extract(beta_loop$coef_ori, "\\d+")) # as.numeric(str_extract(beta_loop$betas, "\\d+"))

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
      
      # for(s in 1:n_samples) {
      #   predictions[s, ] <- plogis(all_beta0_samples[s] + all_beta_samples[s] * X_range)
      # }
      X <- cbind(1, X_range)  # N x 2
      B <- cbind(all_beta0_samples, all_beta_samples)  # S x 2

      linear_pred <- B %*% t(X)  # S x N matrix
      predictions <- plogis(linear_pred)  # S x N matrix of predicted probabilities

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
        X_range_ori = X_range_ori,
        pred_mean = pred_mean,
        pred_median = pred_median,
        pred_lower = pred_lower,
        pred_upper = pred_upper
      ) %>% 
      mutate(sps = sps_loop, scale = scale_loop)
      
      print(glue("pred_{sps_loop}_beta{beta_index}_scale{scale_loop}"))
      assign(glue("pred_{sps_loop}_beta{beta_index}_scale{scale_loop}"), pred_data) 
      
      # Create plot
      p <- ggplot(pred_data, aes(x = X_range_ori)) +
        geom_ribbon(aes(ymin = pred_lower, ymax = pred_upper), 
                    alpha = 0.3, fill = "steelblue") +
        geom_line(aes(y = pred_mean), color = "darkblue", linewidth = 1.2) +
        labs(x = beta_loop$Covariate, 
            y = "Predicted Probability",
            title = glue("{sps_loop}: {beta_loop$Covariate}"),
            subtitle = glue("Scale: {scale_loop} ; Median: {round(median(all_beta_samples),2)}")) +
        theme_minimal()
      
      print(p)
      
    }
  }
}

# save.image(file = "data/predictions_sps3_step2_2.RData")
# load(file = "data/predictions_sps3.RData")
# dat_sca3 <- read_rds(file = "data/dat_sca3.rds")
# get park ranges
XDAT_PATH <- "data/X.rds"
X10 <- read_rds(file = XDAT_PATH)

# Replace the microViz line with:
if (!require("microViz", quietly = TRUE)) {
  # Fallback color palette if microViz is not available
  safe_pal <- c("#F0A3FF", "#0075DC", "#993F00", "#4C005C", "#191919", 
                "#005C31", "#2BCE48", "#FFCC99", "#808080", "#94FFB5", 
                "#8F7C00", "#C20088", "#FFA405", "#FFA8BB", "#426600", "#4ECDC4", "#07a1cb")
} else {
  safe_pal <- microViz::distinct_palette(pal = "kelly")
}

safe_pal <- c(
  VEER = "#5b3d32",   # warm cinnamon rose-brown
  HETH = "#8C7A58",   # muted olive-tan
  WOTH = "#ba7a50",   # brighter warm orange-brown

  OVEN = "#446506",   # clearer bright olive
  BAWW = "#1b1919",   # charcoal (contrast anchor)

  BTNW = "#81ca3d",   # vivid yellow-green
  BTBW = "#0c3889",   # bright blue
  BLBW = "#ef6f1f",   # flame orange

  SCTA = "#CC2F4A",   # *magenta-red* (CB-safe vs green)
  REVI = "#4e0707",   # cleaner light green
  BHVI = "#8296d7",   # **violet** to differentiate from blue

  BRCR = "#9E886A",   # neutral brown
  WBNU = "#7cb7e5",   # icy blue

  DOWO = "#aa48a5",   # black (anchor)
  HAWO = "#4d167e",   # **deep purple**
  YBSA = "#E3C228"    # bright yellow-gold
)


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
    pred_list[[obj_name]] <- pred_data
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
  "5" = "BA"       # Tree Basal Area (linear)
)

# Process all betas automatically
beta1_preds <- process_beta_predictions(1, beta_covariates[["1"]])
beta2_preds <- process_beta_predictions(2, beta_covariates[["2"]])
beta3_preds <- process_beta_predictions(3, beta_covariates[["3"]])
beta4_preds <- process_beta_predictions(4, beta_covariates[["4"]])
beta5_preds <- process_beta_predictions(5, beta_covariates[["5"]]) 

scale_covs <-  as_tibble(cbind(c(3, 2, 1), c("coun", "park", "site"))) %>% 
                  rename(scale = V1, scale_name = V2)

 save.image(file = "data/predictions_sps3_step2_2.RData")
# load("data/predictions_sps3_step2_1.RData")

#? TREE DENSITY -----------------------------------------------------------------
beta1_lims <- c(floor(min(beta1_preds$X_range_ori) / 5) * 5, ceiling(max(beta1_preds$X_range_ori) / 5) * 5)

treeden_covs <- X10 %>%
                    select(park, Point_Name, treeden_ha_site, treeden_ha_park, treeden_ha_coun) %>% 
                    distinct() %>% 
                    pivot_longer(cols = starts_with("treeden_ha"),
                                 names_to = "scale_name",
                                 values_to = "treeden_ha") %>% 
                    distinct() %>% 
                    mutate(scale_name = substr(scale_name, nchar(scale_name) - 3, nchar(scale_name))) %>% 
                    select(-Point_Name) %>% 
                    distinct() %>% 
                    left_join(., scale_covs, by = "scale_name") 

treeden_covs_pkpos <- treeden_covs %>% 
                          select(park)  %>% 
                          mutate(park = factor(park, levels = rev(sort(unique(park))))) %>% 
                          arrange(park) %>%
                          distinct() %>% 
                          mutate(y_pos = as.numeric(NA))

treeden_covs_pkpos[1,2] <- 1.05
for(jj in 2:nrow(treeden_covs_pkpos)){treeden_covs_pkpos[jj,2] <- treeden_covs_pkpos[jj - 1 ,2] + 0.04}

treeden_covs2 <- left_join(treeden_covs, treeden_covs_pkpos, by = "park")  %>% 
                    filter(scale %in% beta1_preds$scale) %>% 
                    drop_na() %>% 
                    mutate(covariate = "cov",
                           scale = as.numeric(scale)) %>% 
                    rename(pred_mean = treeden_ha)

max_pos_treeden <- max(treeden_covs2$y_pos)
beta1_preds$y_pos <- as.numeric(NA)
beta1_preds2 <- full_join(beta1_preds, treeden_covs2) %>% 
                  mutate(park = factor(park, levels = rev(sort(unique(park))))) 

nrow(treeden_covs2) + nrow(beta1_preds) == nrow(beta1_preds2)

ggplot() +
  geom_point(data = beta1_preds2 %>% filter(covariate == "cov"), 
             aes(y = y_pos, x = pred_mean, col = park), size = 2, show.legend = FALSE) +
  geom_hline(yintercept = 1, color = "black", linewidth = 0.4) +
  geom_line(data = beta1_preds2 %>% filter(covariate == "Tree Density"), 
            aes(x = X_range_ori, y = pred_mean, color = factor(sps)), linewidth = 1.2) +
  facet_wrap(~ scale, scales = "free_x",
             labeller = labeller(scale = c("3" = "County Scale", 
                                           "2" = "Park Scale", 
                                           "1" = "Local Scale"))) +  
  labs(x = glue("\n\n Tree Density (stems/ha)"), 
       y = "Predicted Occupancy Probability\n",
       title = glue("Tree Density\n"),
       color = "Species") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.border = element_rect(color = "black", linewidth = 0.6, fill = NA),  # Added fill = NA
        panel.background = element_rect(fill = "white", color = NA),  # Ensure white background
        plot.background = element_rect(fill = "white", color = NA),    # White plot background
        strip.text = element_text(face = "bold", size = 16),      # Facet titles
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),      # Main title
        axis.title = element_text(size = 14),                     # Axis titles
        axis.text = element_text(size = 12),                      # Axis text
        legend.title = element_text(size = 14, face = "bold", hjust = 0.5),    # Legend title
        legend.text = element_text(size = 12)                     # Legend text
  ) +
  scale_color_manual(values = safe_pal) +
  scale_y_continuous(
    limits = c(0, max_pos_treeden), 
    breaks = c(0, 0.25, 0.5, 0.75, 1, treeden_covs_pkpos$y_pos),
    labels = c(0, 0.25, 0.5, 0.75, 1, as.character(treeden_covs_pkpos$park)))
  #xlim(beta1_lims)

# ggsave("figures/pred_den.svg", plot = last_plot(), device = "svg", width = 10.2, height = 7.2)
# ggsave("figures/pred_den.png", plot = last_plot(), device = "png", width = 10.2, height = 7.2)

#? CONIFER BA -----------------------------------------------------------------
beta2_lims <- c(floor(min(beta2_preds$X_range_ori) / 5) * 5, ceiling(max(beta2_preds$X_range_ori) / 5) * 5)

treecon_covs <- X10 %>%
                    select(park, Point_Name, BA_m2ha_Conifer_site, BA_m2ha_Conifer_park, BA_m2ha_Conifer_coun) %>% 
                    rename(treecon_ha_site = BA_m2ha_Conifer_site,
                           treecon_ha_park = BA_m2ha_Conifer_park,
                           treecon_ha_coun = BA_m2ha_Conifer_coun) %>% 
                    distinct() %>% 
                    pivot_longer(cols = starts_with("treecon_ha"),
                                 names_to = "scale_name",
                                 values_to = "treecon_ha") %>% 
                    distinct() %>% 
                    mutate(scale_name = substr(scale_name, nchar(scale_name) - 3, nchar(scale_name))) %>% 
                    select(-Point_Name) %>% 
                    distinct() %>% 
                    left_join(., scale_covs, by = "scale_name") 

treecon_covs_pkpos <- treecon_covs %>% 
                          select(park) %>% 
                          mutate(park = factor(park, levels = rev(sort(unique(park))))) %>% 
                          arrange(park) %>%
                          distinct() %>% 
                          mutate(y_pos = as.numeric(NA))

treecon_covs_pkpos[1,2] <- 1.05
for(jj in 2:nrow(treecon_covs_pkpos)){treecon_covs_pkpos[jj,2] <- treecon_covs_pkpos[jj - 1 ,2] + 0.04}

treecon_covs2 <- left_join(treecon_covs, treecon_covs_pkpos, by = "park")  %>% 
                    filter(scale %in% beta2_preds$scale) %>% 
                    drop_na() %>% 
                    mutate(covariate = "cov",
                           scale = as.numeric(scale)) %>% 
                    rename(pred_mean = treecon_ha)

max_pos_treecon <-  max(treecon_covs2$y_pos)
beta2_preds$y_pos <- as.numeric(NA)
beta2_preds2 <- full_join(beta2_preds, treecon_covs2) %>% 
                  mutate(park = factor(park, levels = rev(sort(unique(park))))) 

nrow(treecon_covs2) + nrow(beta2_preds) == nrow(beta2_preds2)

ggplot() +
  geom_point(data = beta2_preds2 %>% filter(covariate == "cov"), 
             aes(y = y_pos, x = pred_mean, col = park), size = 2, show.legend = FALSE) +
  geom_hline(yintercept = 1, color = "black", linewidth = 0.4) +
  geom_line(data = beta2_preds2 %>% filter(covariate == "Conifer Density"), 
            aes(x = X_range_ori, y = pred_mean, color = factor(sps)), linewidth = 1.2) +
  facet_wrap(~ scale, scales = "free_x",
             labeller = labeller(scale = c("3" = "County Scale", 
                                           "2" = "Park Scale", 
                                           "1" = "Local Scale"))) +  
  labs(x = glue("\n\n Conifer Basal Area (m²/ha))"), 
       y = "Predicted Occupancy Probability\n",
       title = glue("Conifer Trees\n"),
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
    scale_y_continuous(
      limits = c(0, max_pos_treecon), 
      breaks = c(0, 0.25, 0.5, 0.75, 1, treecon_covs_pkpos$y_pos),
      labels = c(0, 0.25, 0.5, 0.75, 1, as.character(treecon_covs_pkpos$park))) 
  #xlim(beta2_lims)

# ggsave("figures/pred_con.svg", plot = last_plot(), device = "svg", width = 14, height = 7.2)
# ggsave("figures/pred_con.png", plot = last_plot(), device = "png", width = 14, height = 7.2)

#? LATE SUCCESSIONAL BASAL AREA -----------------------------------------------------------------
beta3_lims <- c(floor(min(beta3_preds$X_range_ori) / 5) * 5, ceiling(max(beta3_preds$X_range_ori) / 5) * 5)

treelat_covs <- X10 %>%
                    select(park, Point_Name, BA_m2ha_large_site, BA_m2ha_large_park, BA_m2ha_large_coun) %>% 
                    rename(treelat_ha_site = BA_m2ha_large_site,
                           treelat_ha_park = BA_m2ha_large_park,
                           treelat_ha_coun = BA_m2ha_large_coun) %>% 
                    distinct() %>% 
                    pivot_longer(cols = starts_with("treelat_ha"),
                                 names_to = "scale_name",
                                 values_to = "treelat_ha") %>% 
                    distinct() %>% 
                    mutate(scale_name = substr(scale_name, nchar(scale_name) - 3, nchar(scale_name))) %>% 
                    select(-Point_Name) %>% 
                    distinct() %>% 
                    left_join(., scale_covs, by = "scale_name") 

treelat_covs_pkpos <- treelat_covs %>% 
                          select(park)  %>% 
                          mutate(park = factor(park, levels = rev(sort(unique(park))))) %>% 
                          arrange(park) %>%
                          distinct() %>% 
                          mutate(y_pos = as.numeric(NA))
treelat_covs_pkpos[1,2] <- 1.05
for(jj in 2:nrow(treelat_covs_pkpos)){treelat_covs_pkpos[jj,2] <- treelat_covs_pkpos[jj - 1 ,2] + 0.04}

treelat_covs2 <- left_join(treelat_covs, treelat_covs_pkpos, by = "park")  %>% 
                    #filter(scale %in% beta3_preds$scale) %>% 
                    drop_na() %>% 
                    mutate(covariate = "cov",
                           scale = as.numeric(scale)) %>% 
                    rename(pred_mean = treelat_ha)

max_pos_treelat <- max(treelat_covs2$y_pos)
beta3_preds$y_pos <- as.numeric(NA)
beta3_preds2 <- full_join(beta3_preds, treelat_covs2) %>% 
                  mutate(park = factor(park, levels = rev(sort(unique(park))))) 

nrow(treelat_covs2) + nrow(beta3_preds) == nrow(beta3_preds2)

ggplot() +
  geom_point(data = beta3_preds2 %>% filter(covariate == "cov"), 
             aes(y = y_pos, x = pred_mean, col = park), size = 2, show.legend = FALSE) +
  geom_hline(yintercept = 1, color = "black", linewidth = 0.4) +
  geom_line(data = beta3_preds2 %>% filter(covariate == "Late Successional Tree Density"), 
            aes(x = X_range_ori, y = pred_mean, color = factor(sps)), linewidth = 1.2) +
  facet_wrap(~ scale, scales = "free_x",
             labeller = labeller(scale = c("3" = "County Scale", 
                                           "2" = "Park Scale", 
                                           "1" = "Local Scale"))) +  
  labs(x = glue("\n\n Late Succ. Tree Basal Area (m²/ha)"), 
       y = "Predicted Occupancy Probability\n",
       title = glue("Late Successional Trees\n"),
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
  scale_y_continuous(
    limits = c(0, max_pos_treelat), 
    breaks = c(0, 0.25, 0.5, 0.75, 1, treelat_covs_pkpos$y_pos),
    labels = c(0, 0.25, 0.5, 0.75, 1, as.character(treelat_covs_pkpos$park))) 
   #xlim(beta3_lims)

# ggsave("figures/pred_lat.svg", plot = last_plot(), device = "svg", width = 14, height = 7.2)
# ggsave("figures/pred_lat.png", plot = last_plot(), device = "png", width = 14, height = 7.2)

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

shrub_covs <- X10 %>%
                    select(park, Point_Name, shrub_avg_cov_site, shrub_avg_cov_park, shrub_cov_coun) %>% 
                    rename(shrub_ha_site = shrub_avg_cov_site,
                           shrub_ha_park = shrub_avg_cov_park,
                           shrub_ha_coun = shrub_cov_coun) %>% 
                    distinct() %>% 
                    pivot_longer(cols = starts_with("shrub_ha"),
                                 names_to = "scale_name",
                                 values_to = "shrub_ha") %>% 
                    distinct() %>% 
                    mutate(scale_name = substr(scale_name, nchar(scale_name) - 3, nchar(scale_name))) %>% 
                    select(-Point_Name) %>% 
                    distinct() %>% 
                    left_join(., scale_covs, by = "scale_name")                           

beta4_lims <- c(floor(min(beta4_preds$X_range_ori) / 5) * 5, ceiling(max(beta4_preds$X_range_ori) / 5) * 5)

shrub_covs_pkpos <- shrub_covs %>% 
                        select(park) %>% 
                        mutate(park = factor(park, levels = rev(sort(unique(park))))) %>% 
                        arrange(park) %>%
                        distinct() %>% 
                        mutate(y_pos = as.numeric(NA))

shrub_covs_pkpos[1,2] <- 1.05
for(jj in 2:nrow(shrub_covs_pkpos)){shrub_covs_pkpos[jj,2] <- shrub_covs_pkpos[jj - 1 ,2] + 0.04}

shrub_covs2 <- left_join(shrub_covs, shrub_covs_pkpos, by = "park")  %>% 
                    filter(scale %in% beta4_preds$scale) %>% 
                    drop_na() %>% 
                    mutate(covariate = "cov",
                           scale = as.numeric(scale)) %>% 
                    rename(pred_mean = shrub_ha)

max_pos_shrub <- max(shrub_covs2$y_pos)
beta4_preds$y_pos <- as.numeric(NA)
beta4_preds2 <- full_join(beta4_preds, shrub_covs2) %>% 
                    mutate(park = factor(park, levels = rev(sort(unique(park))))) 

nrow(shrub_covs2) + nrow(beta4_preds) == nrow(beta4_preds2)

ggplot() +
  geom_point(data = beta4_preds2 %>% filter(covariate == "cov"), 
             aes(y = y_pos, x = pred_mean, col = park), size = 2, show.legend = FALSE) +
  geom_hline(yintercept = 1, color = "black", linewidth = 0.4) +
  geom_line(data = beta4_preds2 %>% filter(covariate == "Shrub Basal Area"), 
            aes(x = X_range_ori, y = pred_mean, color = factor(sps)), linewidth = 1.2) +
  facet_wrap(~ scale, scales = "free_x",
             labeller = labeller(scale = c("3" = "County Scale", 
                                           "2" = "Park Scale", 
                                           "1" = "Local Scale"))) +  
  labs(x = glue("\n\n Shrub Coverage (%)"), 
       y = "Predicted Occupancy Probability\n",
       title = glue("Shrubs\n"),
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
  scale_y_continuous(
    limits = c(0, max_pos_shrub), 
    breaks = c(0, 0.25, 0.5, 0.75, 1, shrub_covs_pkpos$y_pos),
    labels = c(0, 0.25, 0.5, 0.75, 1, as.character(shrub_covs_pkpos$park)))

# ggsave("figures/pred_shr.svg", plot = last_plot(), device = "svg", width = 14, height = 7.2)
# ggsave("figures/pred_shr.png", plot = last_plot(), device = "png", width = 14, height = 7.2)

#? TREE BASAL AREA -----------------------------------------------------------------

beta5_lims <- c(floor(min(beta5_preds$X_range_ori) / 5) * 5, ceiling(max(beta5_preds$X_range_ori) / 5) * 5)

treeba_covs <- X10 %>%
                    select(park, Point_Name, BA_m2ha_site, BA_m2ha_park, BA_m2ha_coun) %>% 
                    rename(treeba_ha_site = BA_m2ha_site,
                           treeba_ha_park = BA_m2ha_park,
                           treeba_ha_coun = BA_m2ha_coun) %>% 
                    distinct() %>% 
                    pivot_longer(cols = starts_with("treeba_ha"),
                                 names_to = "scale_name",
                                 values_to = "treeba_ha") %>% 
                    distinct() %>% 
                    mutate(scale_name = substr(scale_name, nchar(scale_name) - 3, nchar(scale_name))) %>% 
                    select(-Point_Name) %>% 
                    distinct() %>% 
                    left_join(., scale_covs, by = "scale_name") 

treeba_covs_pkpos <- treeba_covs %>% 
                        select(park) %>% 
                        distinct() %>% 
                        mutate(park = factor(park, levels = rev(sort(unique(park))))) %>% 
                        arrange(park) %>% 
                        mutate(y_pos = as.numeric(NA))

treeba_covs_pkpos[1,2] <- 1.05
for(jj in 2:nrow(treeba_covs_pkpos)){treeba_covs_pkpos[jj,2] <- treeba_covs_pkpos[jj - 1 ,2] + 0.04}

treeba_covs2 <- left_join(treeba_covs, treeba_covs_pkpos, by = "park")  %>% 
                    filter(scale %in% beta5_preds$scale) %>% 
                    drop_na() %>% 
                    mutate(covariate = "cov",
                           scale = as.numeric(scale)) %>% 
                    rename(pred_mean = treeba_ha)

max_pos_treeba <- max(treeba_covs2$y_pos)
beta5_preds$y_pos <- as.numeric(NA)
beta5_preds2 <- full_join(beta5_preds, treeba_covs2)  %>% 
                  mutate(park = factor(park, levels = rev(sort(unique(park))))) 

nrow(treeba_covs2) + nrow(beta5_preds) == nrow(beta5_preds2)

ggplot() +
  geom_point(data = beta5_preds2 %>% filter(covariate == "cov"), 
             aes(y = y_pos, x = pred_mean, col = park), size = 2, show.legend = FALSE) +
  geom_hline(yintercept = 1, color = "black", linewidth = 0.4) +
  geom_line(data = beta5_preds2 %>% filter(covariate == "Tree Basal Area"), 
            aes(x = X_range_ori, y = pred_mean, color = factor(sps)), linewidth = 1.2) +
  facet_wrap(~ scale, scales = "free_x",
             labeller = labeller(scale = c("3" = "County Scale", 
                                           "2" = "Park Scale", 
                                           "1" = "Local Scale"))) +  
  labs(x = glue("\n\n Tree Basal Area (m²/ha)"), 
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
  scale_y_continuous(
    limits = c(0, max_pos_treeba), 
    breaks = c(0, 0.25, 0.5, 0.75, 1, treeba_covs_pkpos$y_pos),
    labels = c(0, 0.25, 0.5, 0.75, 1, as.character(treeba_covs_pkpos$park))) 
            #xlim(beta5_lims)

# ggsave("figures/pred_BA.svg", plot = last_plot(), device = "svg", width = 10.2, height = 7.2)
# ggsave("figures/pred_BA.png", plot = last_plot(), device = "png", width = 10.2, height = 7.2)

## new figures - park values on the bottom

beta5_lims <- c(floor(min(beta5_preds$X_range_ori) / 5) * 5, ceiling(max(beta5_preds$X_range_ori) / 5) * 5)

treeba_covs <- X10 %>%
                    select(park, Point_Name, BA_m2ha_site, BA_m2ha_park, BA_m2ha_coun) %>% 
                    rename(treeba_ha_site = BA_m2ha_site,
                           treeba_ha_park = BA_m2ha_park,
                           treeba_ha_coun = BA_m2ha_coun) %>% 
                    distinct() %>% 
                    pivot_longer(cols = starts_with("treeba_ha"),
                                 names_to = "scale_name",
                                 values_to = "treeba_ha") %>% 
                    distinct() %>% 
                    mutate(scale_name = substr(scale_name, nchar(scale_name) - 3, nchar(scale_name))) %>% 
                    select(-Point_Name) %>% 
                    distinct() %>% 
                    left_join(., scale_covs, by = "scale_name") 

treeba_covs_pkpos <- treeba_covs %>% 
                        select(park) %>% 
                        distinct() %>% 
                        mutate(park = factor(park, levels = rev(sort(unique(park))))) %>% 
                        arrange(park) %>% 
                        mutate(y_pos = as.numeric(NA))

treeba_covs_pkpos[1,2] <- - 0.33
for(jj in 2:nrow(treeba_covs_pkpos)){treeba_covs_pkpos[jj,2] <- treeba_covs_pkpos[jj - 1 ,2] + 0.04}

treeba_covs2 <- left_join(treeba_covs, treeba_covs_pkpos, by = "park")  %>% 
                    #filter(scale %in% beta5_preds$scale) %>% 
                    drop_na() %>% 
                    mutate(covariate = "cov",
                           scale = as.numeric(scale)) %>% 
                    rename(pred_mean = treeba_ha)

min_pos_treeba <- min(treeba_covs2$y_pos)
beta5_preds$y_pos <- as.numeric(NA)
beta5_preds2 <- full_join(beta5_preds, treeba_covs2)  %>% 
                  mutate(park = factor(park, levels = rev(sort(unique(park))))) 

nrow(treeba_covs2) + nrow(beta5_preds) == nrow(beta5_preds2)

ggplot() +
  geom_point(data = beta5_preds2 %>% filter(covariate == "cov"), 
             aes(y = y_pos, x = pred_mean, col = park), size = 2, show.legend = FALSE) +
  geom_hline(yintercept = -0.001, color = "black", linewidth = 0.4) +
  geom_hline(yintercept = -0.025, color = "black", linewidth = 0.4) +
  geom_line(data = beta5_preds2 %>% filter(covariate == "Tree Basal Area"), 
            aes(x = X_range_ori, y = pred_mean, color = factor(sps)), linewidth = 1.2) +
  facet_wrap(~ scale, scales = "free_x",
             labeller = labeller(scale = c("3" = "County Scale", 
                                           "2" = "Park Scale", 
                                           "1" = "Local Scale"))) +  
  labs(x = glue("\n\n Tree Basal Area (m²/ha)"), 
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
        axis.text.x = element_text(size = 12),                      # Axis text
        axis.text.y = element_text(size = 12),                      # Axis text
        legend.title = element_text(size = 14, face = "bold", hjust = 0.5),    # Legend title
        legend.text = element_text(size = 12)                     # Legend text
  ) +
  scale_color_manual(values = safe_pal) +
  scale_y_continuous(
    limits = c(min_pos_treeba, 1), 
    breaks = c(0, 0.25, 0.5, 0.75, 1, treeba_covs_pkpos$y_pos),
    labels = c(0, 0.25, 0.5, 0.75, 1, as.character(treeba_covs_pkpos$park))) 

# ggsave("figures/pred_BA2.svg", plot = last_plot(), device = "svg", width = 10.2, height = 6.5)
# ggsave("figures/pred_BA2.png", plot = last_plot(), device = "png", width = 10.2, height = 6.5)

#? LATE SUCCESSIONAL BASAL AREA - park values on the bottom ----------------------

beta3_lims <- c(floor(min(beta3_preds$X_range_ori) / 5) * 5, ceiling(max(beta3_preds$X_range_ori) / 5) * 5)

treelat_covs <- X10 %>%
                    select(park, Point_Name, BA_m2ha_large_site, BA_m2ha_large_park, BA_m2ha_large_coun) %>% 
                    rename(treelat_ha_site = BA_m2ha_large_site,
                           treelat_ha_park = BA_m2ha_large_park,
                           treelat_ha_coun = BA_m2ha_large_coun) %>% 
                    distinct() %>% 
                    pivot_longer(cols = starts_with("treelat_ha"),
                                 names_to = "scale_name",
                                 values_to = "treelat_ha") %>% 
                    distinct() %>% 
                    mutate(scale_name = substr(scale_name, nchar(scale_name) - 3, nchar(scale_name))) %>% 
                    select(-Point_Name) %>% 
                    distinct() %>% 
                    left_join(., scale_covs, by = "scale_name") 

treelat_covs_pkpos <- treelat_covs %>% 
                        select(park) %>% 
                        distinct() %>% 
                        mutate(park = factor(park, levels = rev(sort(unique(park))))) %>% 
                        arrange(park) %>% 
                        mutate(y_pos = as.numeric(NA))

# Position park points at the bottom (starting from negative values)
treelat_covs_pkpos[1,2] <- -0.33
for(jj in 2:nrow(treelat_covs_pkpos)){
  treelat_covs_pkpos[jj,2] <- treelat_covs_pkpos[jj - 1 ,2] + 0.04
}

treelat_covs2 <- left_join(treelat_covs, treelat_covs_pkpos, by = "park")  %>% 
                    #filter(scale %in% beta3_preds$scale) %>%  # Include all scales
                    drop_na() %>% 
                    mutate(covariate = "cov",
                           scale = as.numeric(scale)) %>% 
                    rename(pred_mean = treelat_ha)

min_pos_treelat <- min(treelat_covs2$y_pos)
beta3_preds$y_pos <- as.numeric(NA)
beta3_preds2 <- full_join(beta3_preds, treelat_covs2) %>% 
                  mutate(park = factor(park, levels = rev(sort(unique(park))))) 

nrow(treelat_covs2) + nrow(beta3_preds) == nrow(beta3_preds2)

ggplot() +
  geom_point(data = beta3_preds2 %>% filter(covariate == "cov"), 
             aes(y = y_pos, x = pred_mean, col = park), size = 2, show.legend = FALSE) +
  # Add horizontal reference lines
  geom_hline(yintercept = -0.001, color = "black", linewidth = 0.4) +
  geom_hline(yintercept = -0.025, color = "black", linewidth = 0.4) +
  geom_line(data = beta3_preds2 %>% filter(covariate == "Late Successional Tree Density"), 
            aes(x = X_range_ori, y = pred_mean, color = factor(sps)), linewidth = 1.2) +
  facet_wrap(~ scale, scales = "free_x",
             labeller = labeller(scale = c("3" = "County Scale", 
                                           "2" = "Park Scale", 
                                           "1" = "Local Scale"))) +  
  labs(x = glue("\n\n Late Succ. Tree Basal Area (m²/ha)"), 
       y = "Predicted Occupancy Probability\n",
       title = glue("Late Successional Trees\n"),
       color = "Species") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.border = element_rect(color = "black", linewidth = 0.6, fill = NA),
        panel.background = element_rect(fill = "white", color = NA),
        strip.text = element_text(face = "bold", size = 16),
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.text = element_text(size = 12)
  ) +
  scale_color_manual(values = safe_pal) +
  scale_y_continuous(
    limits = c(min_pos_treelat, 1), 
    breaks = c(0, 0.25, 0.5, 0.75, 1, treelat_covs_pkpos$y_pos),
    labels = c(0, 0.25, 0.5, 0.75, 1, as.character(treelat_covs_pkpos$park))) 

# ggsave("figures/pred_lat2.svg", plot = last_plot(), device = "svg", width = 10.2, height = 6.5)
# ggsave("figures/pred_lat2.png", plot = last_plot(), device = "png", width = 10.2, height = 6.5)

#? SHRUB BASAL AREA - park values on the bottom ----------------------

beta4_lims <- c(floor(min(beta4_preds$X_range_ori) / 5) * 5, ceiling(max(beta4_preds$X_range_ori) / 5) * 5)

shrub_covs <- X10 %>%
                    select(park, Point_Name, shrub_avg_cov_site, shrub_avg_cov_park, shrub_cov_coun) %>% 
                    rename(shrub_ha_site = shrub_avg_cov_site,
                           shrub_ha_park = shrub_avg_cov_park,
                           shrub_ha_coun = shrub_cov_coun) %>% 
                    distinct() %>% 
                    pivot_longer(cols = starts_with("shrub_ha"),
                                 names_to = "scale_name",
                                 values_to = "shrub_ha") %>% 
                    distinct() %>% 
                    mutate(scale_name = substr(scale_name, nchar(scale_name) - 3, nchar(scale_name))) %>% 
                    select(-Point_Name) %>% 
                    distinct() %>% 
                    left_join(., scale_covs, by = "scale_name")

shrub_covs_pkpos <- shrub_covs %>% 
                        select(park) %>% 
                        distinct() %>% 
                        mutate(park = factor(park, levels = rev(sort(unique(park))))) %>% 
                        arrange(park) %>% 
                        mutate(y_pos = as.numeric(NA))

# Position park points at the bottom (starting from negative values)
shrub_covs_pkpos[1,2] <- -0.33
for(jj in 2:nrow(shrub_covs_pkpos)){
  shrub_covs_pkpos[jj,2] <- shrub_covs_pkpos[jj - 1 ,2] + 0.04
}

shrub_covs2 <- left_join(shrub_covs, shrub_covs_pkpos, by = "park")  %>% 
                    #filter(scale %in% beta4_preds$scale) %>%  # Include all scales
                    drop_na() %>% 
                    mutate(covariate = "cov",
                           scale = as.numeric(scale)) %>% 
                    rename(pred_mean = shrub_ha)

min_pos_shrub <- min(shrub_covs2$y_pos)
beta4_preds$y_pos <- as.numeric(NA)
beta4_preds2 <- full_join(beta4_preds, shrub_covs2) %>% 
                    mutate(park = factor(park, levels = rev(sort(unique(park))))) 

nrow(shrub_covs2) + nrow(beta4_preds) == nrow(beta4_preds2)

ggplot() +
  geom_point(data = beta4_preds2 %>% filter(covariate == "cov"), 
             aes(y = y_pos, x = pred_mean, col = park), size = 2, show.legend = FALSE) +
  # Add horizontal reference lines
  geom_hline(yintercept = -0.001, color = "black", linewidth = 0.4) +
  geom_hline(yintercept = -0.025, color = "black", linewidth = 0.4) +
  geom_line(data = beta4_preds2 %>% filter(covariate == "Shrub Basal Area"), 
            aes(x = X_range_ori, y = pred_mean, color = factor(sps)), linewidth = 1.2) +
  facet_wrap(~ scale, scales = "free_x",
             labeller = labeller(scale = c("3" = "County Scale", 
                                           "2" = "Park Scale", 
                                           "1" = "Local Scale"))) +  
  labs(x = glue("\n\n Shrub Coverage (%)"), 
       y = "Predicted Occupancy Probability\n",
       title = glue("Shrubs\n"),
       color = "Species") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.border = element_rect(color = "black", linewidth = 0.6, fill = NA),
        panel.background = element_rect(fill = "white", color = NA),
        strip.text = element_text(face = "bold", size = 16),
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.text = element_text(size = 12)
  ) +
  scale_color_manual(values = safe_pal) +
  scale_y_continuous(
    limits = c(min_pos_shrub, 1), 
    breaks = c(0, 0.25, 0.5, 0.75, 1, shrub_covs_pkpos$y_pos),
    labels = c(0, 0.25, 0.5, 0.75, 1, as.character(shrub_covs_pkpos$park)))

# ggsave("figures/pred_shr2.svg", plot = last_plot(), device = "svg", width = 10.2, height = 6.5)
# ggsave("figures/pred_shr2.png", plot = last_plot(), device = "png", width = 10.2, height = 6.5)

#? CONIFER BA - park values on the bottom ----------------------

beta2_lims <- c(floor(min(beta2_preds$X_range_ori) / 5) * 5, ceiling(max(beta2_preds$X_range_ori) / 5) * 5)

treecon_covs <- X10 %>%
                    select(park, Point_Name, BA_m2ha_Conifer_site, BA_m2ha_Conifer_park, BA_m2ha_Conifer_coun) %>% 
                    rename(treecon_ha_site = BA_m2ha_Conifer_site,
                           treecon_ha_park = BA_m2ha_Conifer_park,
                           treecon_ha_coun = BA_m2ha_Conifer_coun) %>% 
                    distinct() %>% 
                    pivot_longer(cols = starts_with("treecon_ha"),
                                 names_to = "scale_name",
                                 values_to = "treecon_ha") %>% 
                    distinct() %>% 
                    mutate(scale_name = substr(scale_name, nchar(scale_name) - 3, nchar(scale_name))) %>% 
                    select(-Point_Name) %>% 
                    distinct() %>% 
                    left_join(., scale_covs, by = "scale_name") 

treecon_covs_pkpos <- treecon_covs %>% 
                          select(park) %>% 
                          distinct() %>% 
                          mutate(park = factor(park, levels = rev(sort(unique(park))))) %>% 
                          arrange(park) %>% 
                          mutate(y_pos = as.numeric(NA))

# Position park points at the bottom (starting from negative values)
treecon_covs_pkpos[1,2] <- -0.33
for(jj in 2:nrow(treecon_covs_pkpos)){
  treecon_covs_pkpos[jj,2] <- treecon_covs_pkpos[jj - 1 ,2] + 0.04
}

treecon_covs2 <- left_join(treecon_covs, treecon_covs_pkpos, by = "park")  %>% 
                    #filter(scale %in% beta2_preds$scale) %>%  # Include all scales
                    drop_na() %>% 
                    mutate(covariate = "cov",
                           scale = as.numeric(scale)) %>% 
                    rename(pred_mean = treecon_ha)

min_pos_treecon <- min(treecon_covs2$y_pos)
beta2_preds$y_pos <- as.numeric(NA)
beta2_preds2 <- full_join(beta2_preds, treecon_covs2) %>% 
                  mutate(park = factor(park, levels = rev(sort(unique(park))))) 

nrow(treecon_covs2) + nrow(beta2_preds) == nrow(beta2_preds2)

ggplot() +
  geom_point(data = beta2_preds2 %>% filter(covariate == "cov"), 
             aes(y = y_pos, x = pred_mean, col = park), size = 2, show.legend = FALSE) +
  # Add horizontal reference lines
  geom_hline(yintercept = -0.001, color = "black", linewidth = 0.4) +
  geom_hline(yintercept = -0.025, color = "black", linewidth = 0.4) +
  geom_hline(yintercept = min_pos_treecon, color = "black", linewidth = 0.4) +
  geom_line(data = beta2_preds2 %>% filter(covariate == "Conifer Density"), 
            aes(x = X_range_ori, y = pred_mean, color = factor(sps)), linewidth = 1.2) +
  facet_wrap(~ scale, scales = "free_x",
             labeller = labeller(scale = c("3" = "County Scale", 
                                           "2" = "Park Scale", 
                                           "1" = "Local Scale"))) +  
  labs(x = glue("\n\n Conifer Basal Area (m²/ha)"), 
       y = "Predicted Occupancy Probability\n",
       title = glue("Conifer Trees\n"),
       color = "Species") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.border = element_rect(color = "black", linewidth = 0.6, fill = NA),
        panel.background = element_rect(fill = "white", color = NA),
        strip.text = element_text(face = "bold", size = 16),
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.text = element_text(size = 12)
  ) +
  scale_color_manual(values = safe_pal) +
  scale_y_continuous(
    limits = c(min_pos_treecon, 1), 
    breaks = c(0, 0.25, 0.5, 0.75, 1, treecon_covs_pkpos$y_pos),
    labels = c(0, 0.25, 0.5, 0.75, 1, as.character(treecon_covs_pkpos$park)))

ggsave("figures/pred_con2.svg", plot = last_plot(), device = "svg", width = 10.2, height = 6.5)
ggsave("figures/pred_con2.png", plot = last_plot(), device = "png", width = 10.2, height = 6.5)


#? TREE DENSITY - park values on the bottom ----------------------

beta1_lims <- c(floor(min(beta1_preds$X_range_ori) / 5) * 5, ceiling(max(beta1_preds$X_range_ori) / 5) * 5)

treeden_covs <- X10 %>%
                    select(park, Point_Name, treeden_ha_site, treeden_ha_park, treeden_ha_coun) %>% 
                    distinct() %>% 
                    pivot_longer(cols = starts_with("treeden_ha"),
                                 names_to = "scale_name",
                                 values_to = "treeden_ha") %>% 
                    distinct() %>% 
                    mutate(scale_name = substr(scale_name, nchar(scale_name) - 3, nchar(scale_name))) %>% 
                    select(-Point_Name) %>% 
                    distinct() %>% 
                    left_join(., scale_covs, by = "scale_name") 

treeden_covs_pkpos <- treeden_covs %>% 
                          select(park) %>% 
                          distinct() %>% 
                          mutate(park = factor(park, levels = rev(sort(unique(park))))) %>% 
                          arrange(park) %>% 
                          mutate(y_pos = as.numeric(NA))

# Position park points at the bottom (starting from negative values)
treeden_covs_pkpos[1,2] <- -0.33
for(jj in 2:nrow(treeden_covs_pkpos)){
  treeden_covs_pkpos[jj,2] <- treeden_covs_pkpos[jj - 1 ,2] + 0.04
}

treeden_covs2 <- left_join(treeden_covs, treeden_covs_pkpos, by = "park")  %>% 
                    #filter(scale %in% beta1_preds$scale) %>%  # Include all scales
                    drop_na() %>% 
                    mutate(covariate = "cov",
                           scale = as.numeric(scale)) %>% 
                    rename(pred_mean = treeden_ha)

min_pos_treeden <- min(treeden_covs2$y_pos)
beta1_preds$y_pos <- as.numeric(NA)
beta1_preds2 <- full_join(beta1_preds, treeden_covs2) %>% 
                  mutate(park = factor(park, levels = rev(sort(unique(park))))) 

nrow(treeden_covs2) + nrow(beta1_preds) == nrow(beta1_preds2)

ggplot() +
  geom_point(data = beta1_preds2 %>% filter(covariate == "cov"), 
             aes(y = y_pos, x = pred_mean, col = park), size = 2, show.legend = FALSE) +
  geom_hline(yintercept = -0.001, color = "black", linewidth = 0.4) +
  geom_hline(yintercept = -0.025, color = "black", linewidth = 0.4) +
  geom_line(data = beta1_preds2 %>% filter(covariate == "Tree Density"), 
            aes(x = X_range_ori, y = pred_mean, color = factor(sps)), linewidth = 1.2) +
  facet_wrap(~ scale, scales = "free_x",
            strip.position = "bottom",
            labeller = labeller(scale = c("3" = "County Scale", 
                                          "2" = "Park Scale", 
                                          "1" = "Local Scale"))) +
  labs(y = "Predicted Occupancy Probability\n",
       title = glue("Tree Density (stems/ha)"),
       color = "Species") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.border = element_rect(color = "black", linewidth = 0.6, fill = NA),
        panel.background = element_rect(fill = "white", color = NA),
        # Key changes for strip positioning:
        strip.text.x = element_text(face = "plain", size = 14 , margin = margin(t = 3)),  # Add top margin
        strip.placement = "outside",     # Place strips outside the plot area
        # Remove x-axis title
        axis.title.x = element_blank(),  # Remove x-axis title completely
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        axis.title.y = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.text = element_text(size = 12)
  ) +
  scale_color_manual(values = safe_pal) +
  scale_y_continuous(
    limits = c(min_pos_treeden, 1), 
    breaks = c(0, 0.25, 0.5, 0.75, 1, treeden_covs_pkpos$y_pos),
    labels = c(0, 0.25, 0.5, 0.75, 1, as.character(treeden_covs_pkpos$park)))

ggsave("figures/pred_den2.svg", plot = last_plot(), device = "svg", width = 10.2, height = 6.5)
ggsave("figures/pred_den2.png", plot = last_plot(), device = "png", width = 10.2, height = 6.5)

# Get min, max, and middle for each scale
x_stats <- beta1_preds2 %>% 
  filter(covariate == "cov") %>% 
  select(pred_mean, scale) %>% 
  group_by(scale) %>% 
  summarise(
    min_val = min(pred_mean),
    max_val = max(pred_mean),
    middle_val = (min(pred_mean) + max(pred_mean)) / 2,  # Arithmetic middle
    median_val = median(pred_mean)  # Your current approach
  )

# Extract just the middle values for centering
x_cen <- x_stats %>% pull(middle_val) %>% as.vector()

ggplot() +
  geom_point(data = beta1_preds2 %>% filter(covariate == "cov"), 
             aes(y = y_pos, x = pred_mean, col = park), size = 2, show.legend = FALSE) +
  geom_hline(yintercept = -0.001, color = "black", linewidth = 0.4) +
  geom_hline(yintercept = -0.025, color = "black", linewidth = 0.4) +
  geom_line(data = beta1_preds2 %>% filter(covariate == "Tree Density"), 
            aes(x = X_range_ori, y = pred_mean, color = factor(sps)), linewidth = 1.2) +
  # Add text labels INSIDE each panel - CENTERED
  geom_text(data = data.frame(scale = c(1, 2, 3), 
                              label = c("Local Scale", "Park Scale", "County Scale"),
                              x = c(x_cen[1], x_cen[2], x_cen[3]),      # Changed from 0 to Inf (right side)
                              y = c(0.99, 0.99, 0.99)),  # Changed from 0.99 to 0.95 (slightly lower)
            aes(x = x, y = y, label = label), 
            hjust =0.5, vjust = 0, size = 5, fontface = "plain",  # Changed hjust and vjust
            color = "black") +
  facet_wrap(~ scale, scales = "free_x") +  # Remove strip labels completely
  labs(y = "Predicted Occupancy Probability\n",
       title = glue("Tree Density (stems/ha)"),
       color = "Species") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.border = element_rect(color = "black", linewidth = 0.6, fill = NA),
        panel.background = element_rect(fill = "white", color = NA),
        # Remove strip completely
        strip.text = element_blank(),     # No strip text
        strip.background = element_blank(), # No strip background
        axis.title.x = element_blank(),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        axis.title.y = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.position = "none"
        # legend.title = element_text(size = 14, face = "bold", hjust = 0.5),
        # legend.text = element_text(size = 12)
  ) +
  scale_color_manual(values = safe_pal) +
  scale_y_continuous(
    limits = c(min_pos_treeden, 1), 
    breaks = c(0, 0.25, 0.5, 0.75, 1, treeden_covs_pkpos$y_pos),
    labels = c(0, 0.25, 0.5, 0.75, 1, as.character(treeden_covs_pkpos$park)))

#


## rugggg

# Get min, max, and middle for each scale
x_stats_den <- beta1_preds2 %>% 
  filter(covariate == "cov") %>% 
  select(pred_mean, scale) %>% 
  group_by(scale) %>% 
  summarise(
    min_val = min(pred_mean),
    max_val = max(pred_mean),
    middle_val = (min(pred_mean) + max(pred_mean)) / 2,
    median_val = median(pred_mean)
  )

x_cen_den <- x_stats_den %>% pull(middle_val) %>% as.vector()

ggplot() +
  geom_line(data = beta1_preds2 %>% filter(covariate == "Tree Density"), 
            aes(x = X_range_ori, y = pred_mean, color = factor(sps)), linewidth = 1.2, show.legend = FALSE) +
  # Add rug plot for park values
  geom_rug(data = beta1_preds2 %>% filter(covariate == "cov"), 
           aes(x = pred_mean, color = park), 
           sides = "b", linewidth = 0.8, length = unit(0.05, "npc"), show.legend = FALSE) +
  # Add text labels INSIDE each panel - CENTERED
  geom_text(data = data.frame(scale = c(1, 2, 3), 
                              label = c("Local Scale", "Park Scale", "County Scale"),
                              x = c(x_cen_den[1], x_cen_den[2], x_cen_den[3]),
                              y = c(0.95, 0.95, 0.95)),
            aes(x = x, y = y, label = label), 
            hjust = 0.5, vjust = 0, size = 5, fontface = "plain",
            color = "black") +
  facet_wrap(~ scale, scales = "free_x") +
  labs(y = "Predicted Occupancy Probability\n",
       title = glue("Tree Density (stems/ha)")) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.border = element_rect(color = "black", linewidth = 0.6, fill = NA),
        panel.background = element_rect(fill = "white", color = NA),
        strip.text = element_blank(),
        strip.background = element_blank(),
        axis.title.x = element_blank(),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        axis.title.y = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12)
  ) +
  scale_color_manual(values = safe_pal) +
  ylim(0, 1)

ggsave("figures/pred_den4.svg", plot = last_plot(), device = "svg", width = 10.2, height = 4)
ggsave("figures/pred_den4.png", plot = last_plot(), device = "png", width = 10.2, height = 4)

# tree ba
# Get min, max, and middle for each scale
x_stats_ba <- beta5_preds2 %>% 
  filter(covariate == "cov") %>% 
  select(pred_mean, scale) %>% 
  group_by(scale) %>% 
  summarise(
    min_val = min(pred_mean),
    max_val = max(pred_mean),
    middle_val = (min(pred_mean) + max(pred_mean)) / 2,
    median_val = median(pred_mean)
  )

x_cen_ba <- x_stats_ba %>% pull(middle_val) %>% as.vector()

ggplot() +
  geom_line(data = beta5_preds2 %>% filter(covariate == "Tree Basal Area"), 
            aes(x = X_range_ori, y = pred_mean, color = factor(sps)), linewidth = 1.2, show.legend = FALSE) +
  # Add rug plot for park values
  geom_rug(data = beta5_preds2 %>% filter(covariate == "cov"), 
           aes(x = pred_mean, color = park), 
           sides = "b", size = 0.8, length = unit(0.05, "npc"), show.legend = FALSE) +
  # Add text labels INSIDE each panel - CENTERED
  geom_text(data = data.frame(scale = c(1, 2, 3), 
                              label = c("Local Scale", "Park Scale", "County Scale"),
                              x = c(x_cen_ba[1], x_cen_ba[2], x_cen_ba[3]),
                              y = c(0.95, 0.95, 0.95)),
            aes(x = x, y = y, label = label), 
            hjust = 0.5, vjust = 0, size = 5, fontface = "plain",
            color = "black") +
  facet_wrap(~ scale, scales = "free_x") +
  labs(y = "Predicted Occupancy Probability\n",
       title = glue("Tree Basal Area (m²/ha)")) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.border = element_rect(color = "black", linewidth = 0.6, fill = NA),
        panel.background = element_rect(fill = "white", color = NA),
        strip.text = element_blank(),
        strip.background = element_blank(),
        axis.title.x = element_blank(),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        axis.title.y = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12)
  ) +
  scale_color_manual(values = safe_pal) +
  ylim(0, 1)

ggsave("figures/pred_BA4.svg", plot = last_plot(), device = "svg", width = 10.2, height = 4)
ggsave("figures/pred_BA4.png", plot = last_plot(), device = "png", width = 10.2, height = 4)

## lat
# Get min, max, and middle for each scale
x_stats_lat <- beta3_preds2 %>% 
  filter(covariate == "cov") %>% 
  select(pred_mean, scale) %>% 
  group_by(scale) %>% 
  summarise(
    min_val = min(pred_mean),
    max_val = max(pred_mean),
    middle_val = (min(pred_mean) + max(pred_mean)) / 2,
    median_val = median(pred_mean)
  )

x_cen_lat <- x_stats_lat %>% pull(middle_val) %>% as.vector()

ggplot() +
  geom_line(data = beta3_preds2 %>% filter(covariate == "Late Successional Tree Density"), 
            aes(x = X_range_ori, y = pred_mean, color = factor(sps)), linewidth = 1.2, show.legend = FALSE) +
  # Add rug plot for park values
  geom_rug(data = beta3_preds2 %>% filter(covariate == "cov"), 
           aes(x = pred_mean, color = park), 
           sides = "b", size = 0.8, length = unit(0.05, "npc"), show.legend = FALSE) +
  # Add text labels INSIDE each panel - CENTERED
  geom_text(data = data.frame(scale = c(1, 2, 3), 
                              label = c("Local Scale", "Park Scale", "County Scale"),
                              x = c(x_cen_lat[1], x_cen_lat[2], x_cen_lat[3]),
                              y = c(0.95, 0.95, 0.95)),
            aes(x = x, y = y, label = label), 
            hjust = 0.5, vjust = 0, size = 5, fontface = "plain",
            color = "black") +
  facet_wrap(~ scale, scales = "free_x") +
  labs(y = "Predicted Occupancy Probability\n",
       title = glue("Late Successional Trees (m²/ha)")) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.border = element_rect(color = "black", linewidth = 0.6, fill = NA),
        panel.background = element_rect(fill = "white", color = NA),
        strip.text = element_blank(),
        strip.background = element_blank(),
        axis.title.x = element_blank(),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        axis.title.y = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12)
  ) +
  scale_color_manual(values = safe_pal) +
  ylim(0, 1)

ggsave("figures/pred_lat4.svg", plot = last_plot(), device = "svg", width = 10.2, height = 4)
ggsave("figures/pred_lat4.png", plot = last_plot(), device = "png", width = 10.2, height = 4)


# shrub

# Get min, max, and middle for each scale
x_stats_shr <- beta4_preds2 %>% 
  filter(covariate == "cov") %>% 
  select(pred_mean, scale) %>% 
  group_by(scale) %>% 
  summarise(
    min_val = min(pred_mean),
    max_val = max(pred_mean),
    middle_val = (min(pred_mean) + max(pred_mean)) / 2,
    median_val = median(pred_mean)
  )

x_cen_shr <- x_stats_shr %>% pull(middle_val) %>% as.vector()

ggplot() +
  geom_line(data = beta4_preds2 %>% filter(covariate == "Shrub Basal Area"), 
            aes(x = X_range_ori, y = pred_mean, color = factor(sps)), linewidth = 1.2, show.legend = FALSE) +
  # Add rug plot for park values
  geom_rug(data = beta4_preds2 %>% filter(covariate == "cov"), 
           aes(x = pred_mean, color = park), 
           sides = "b", size = 0.8, length = unit(0.05, "npc"), show.legend = FALSE) +
  # Add text labels INSIDE each panel - CENTERED
  geom_text(data = data.frame(scale = c(1, 2, 3), 
                              label = c("Local Scale", "Park Scale", "County Scale"),
                              x = c(x_cen_shr[1], x_cen_shr[2], x_cen_shr[3]),
                              y = c(0.95, 0.95, 0.95)),
            aes(x = x, y = y, label = label), 
            hjust = 0.5, vjust = 0, size = 5, fontface = "plain",
            color = "black") +
  facet_wrap(~ scale, scales = "free_x") +
  labs(y = "Predicted Occupancy Probability\n",
       title = glue("Shrub Coverage (%)")) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.border = element_rect(color = "black", linewidth = 0.6, fill = NA),
        panel.background = element_rect(fill = "white", color = NA),
        strip.text = element_blank(),
        strip.background = element_blank(),
        axis.title.x = element_blank(),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        axis.title.y = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12)
  ) +
  scale_color_manual(values = safe_pal) +
  ylim(0, 1)

ggsave("figures/pred_shr4.svg", plot = last_plot(), device = "svg", width = 10.2, height = 4)
ggsave("figures/pred_shr4.png", plot = last_plot(), device = "png", width = 10.2, height = 4)

# conifer
# Get min, max, and middle for each scale
x_stats_con <- beta2_preds2 %>% 
  filter(covariate == "cov") %>% 
  select(pred_mean, scale) %>% 
  group_by(scale) %>% 
  summarise(
    min_val = min(pred_mean),
    max_val = max(pred_mean),
    middle_val = (min(pred_mean) + max(pred_mean)) / 2,
    median_val = median(pred_mean)
  )

x_cen_con <- x_stats_con %>% pull(middle_val) %>% as.vector()

ggplot() +
  geom_line(data = beta2_preds2 %>% filter(covariate == "Conifer Density"), 
            aes(x = X_range_ori, y = pred_mean, color = factor(sps)), linewidth = 1.2, show.legend = FALSE) +
  # Add rug plot for park values
  geom_rug(data = beta2_preds2 %>% filter(covariate == "cov"), 
           aes(x = pred_mean, color = park), 
           sides = "b", size = 0.8, length = unit(0.05, "npc"), show.legend = FALSE) +
  # Add text labels INSIDE each panel - CENTERED
  geom_text(data = data.frame(scale = c(1, 2, 3), 
                              label = c("Local Scale", "Park Scale", "County Scale"),
                              x = c(x_cen_con[1], x_cen_con[2], x_cen_con[3]),
                              y = c(0.95, 0.95, 0.95)),
            aes(x = x, y = y, label = label), 
            hjust = 0.5, vjust = 0, size = 5, fontface = "plain",
            color = "black") +
  facet_wrap(~ scale, scales = "free_x") +
  labs(y = "Predicted Occupancy Probability\n",
       title = glue("Conifer Trees (m²/ha)")) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.border = element_rect(color = "black", linewidth = 0.6, fill = NA),
        panel.background = element_rect(fill = "white", color = NA),
        strip.text = element_blank(),
        strip.background = element_blank(),
        axis.title.x = element_blank(),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        axis.title.y = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12)
  ) +
  scale_color_manual(values = safe_pal) +
  ylim(0, 1)

ggsave("figures/pred_con4.svg", plot = last_plot(), device = "svg", width = 10.2, height = 4)
ggsave("figures/pred_con4.png", plot = last_plot(), device = "png", width = 10.2, height = 4)




### gray rug and species legend

# Create gray palette specifically for parks
park_names <- unique(beta1_preds2$park[!is.na(beta1_preds2$park)])
n_parks <- length(park_names)
gray_pal <- gray.colors(n_parks, start = 0.3, end = 0.8)
names(gray_pal) <- sort(park_names)

# Keep species palette separate
SPS_ORDER_PATH <- "data/src/sps_order.csv"

sps_order <- read_csv(SPS_ORDER_PATH) %>% 
                rename(sps = Aou_code) %>% 
                mutate(sps_name = factor(sps_name, levels = sps_name)) 

species_names <- unique(sps_order$sps)
n_species <- length(species_names)
safe_pal_species <- safe_pal[1:n_species]
names(safe_pal_species) <- sort(species_names)

x_stats_den <- beta1_preds2 %>% 
  filter(covariate == "cov") %>% 
  select(pred_mean, scale) %>% 
  group_by(scale) %>% 
  summarise(
    min_val = min(pred_mean),
    max_val = max(pred_mean),
    middle_val = (min(pred_mean) + max(pred_mean)) / 2,
    median_val = median(pred_mean)
  )

x_cen_den <- x_stats_den %>% pull(middle_val) %>% as.vector()

(plot_den4 <- ggplot() +
  geom_line(data = beta1_preds2 %>% filter(covariate == "Tree Density"), 
            aes(x = X_range_ori, y = pred_mean, color = factor(sps)), linewidth = 1.2, show.legend = FALSE) +
  # Add rug plot for park values with explicit gray colors
  geom_rug(data = beta1_preds2 %>% filter(covariate == "cov"), 
           aes(x = pred_mean), 
           color = "gray50",  # Single gray color for all parks
           sides = "b", size = 0.8, length = unit(0.05, "npc"), show.legend = FALSE) +
  geom_text(data = data.frame(scale = c(1, 2, 3), 
                              label = c("Local Scale", "Park Scale", "County Scale"),
                              x = c(x_cen_den[1], x_cen_den[2], x_cen_den[3]),
                              y = c(0.97, 0.97, 0.97)),
            aes(x = x, y = y, label = label), 
            hjust = 0.5, vjust = 0, size = 5, fontface = "plain",
            color = "black") +
  facet_wrap(~ scale, scales = "free_x") +
  labs(y = "Predicted Occupancy Probability\n",
       title = "Tree Density (stems/ha)") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.border = element_rect(color = "black", linewidth = 0.6, fill = NA),
        panel.background = element_rect(fill = "white", color = NA),
        strip.text = element_blank(),
        strip.background = element_blank(),
        axis.title.x = element_blank(),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        axis.title.y = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12)
  ) +
  scale_color_manual(values = safe_pal_species) +  # Only species colors
  ylim(0, 1))


ggsave("figures/pred_den4.svg", plot = plot_den4, device = "svg", width = 10.2, height = 4)
ggsave("figures/pred_den4.png", plot = plot_den4, device = "png", width = 10.2, height = 4)

# Tree Basal Area
x_stats_ba <- beta5_preds2 %>% 
  filter(covariate == "cov") %>% 
  select(pred_mean, scale) %>% 
  group_by(scale) %>% 
  summarise(
    min_val = min(pred_mean), max_val = max(pred_mean),
    middle_val = (min(pred_mean) + max(pred_mean)) / 2, median_val = median(pred_mean))

x_cen_ba <- x_stats_ba %>% pull(middle_val) %>% as.vector()

(plot_ba4 <- ggplot() +
  geom_line(data = beta5_preds2 %>% filter(covariate == "Tree Basal Area"), 
            aes(x = X_range_ori, y = pred_mean, color = factor(sps)), linewidth = 1.2, show.legend = FALSE) +
  geom_rug(data = beta5_preds2 %>% filter(covariate == "cov"), 
           aes(x = pred_mean), 
           color = "gray50", sides = "b", size = 0.8, length = unit(0.05, "npc"), show.legend = FALSE) +
  geom_text(data = data.frame(scale = c(1, 2, 3), 
                              label = c("Local Scale", "Park Scale", "County Scale"),
                              x = c(x_cen_ba[1], x_cen_ba[2], x_cen_ba[3]),
                              y = c(0.97, 0.97, 0.97)),
            aes(x = x, y = y, label = label), 
            hjust = 0.5, vjust = 0, size = 5, fontface = "plain", color = "black") +
  facet_wrap(~ scale, scales = "free_x") +
  labs(y = "Predicted Occupancy Probability\n", title = "Tree Basal Area (m²/ha)") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.border = element_rect(color = "black", linewidth = 0.6, fill = NA),
        panel.background = element_rect(fill = "white", color = NA),
        strip.text = element_blank(), strip.background = element_blank(),
        axis.title.x = element_blank(),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        axis.title.y = element_text(size = 14),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12)) +
  scale_color_manual(values = safe_pal_species) + ylim(0, 1))

ggsave("figures/pred_BA4.svg", plot = plot_ba4, device = "svg", width = 10.2, height = 4)
ggsave("figures/pred_BA4.png", plot = plot_ba4, device = "png", width = 10.2, height = 4)

# Late Successional Trees
x_stats_lat <- beta3_preds2 %>% 
  filter(covariate == "cov") %>% 
  select(pred_mean, scale) %>% 
  group_by(scale) %>% 
  summarise(
    min_val = min(pred_mean), max_val = max(pred_mean),
    middle_val = (min(pred_mean) + max(pred_mean)) / 2, median_val = median(pred_mean))

x_cen_lat <- x_stats_lat %>% pull(middle_val) %>% as.vector()

(plot_lat4 <- ggplot() +
  geom_line(data = beta3_preds2 %>% filter(covariate == "Late Successional Tree Density"), 
            aes(x = X_range_ori, y = pred_mean, color = factor(sps)), linewidth = 1.2, show.legend = FALSE) +
  geom_rug(data = beta3_preds2 %>% filter(covariate == "cov"), 
           aes(x = pred_mean), 
           color = "gray50", sides = "b", size = 0.8, length = unit(0.05, "npc"), show.legend = FALSE) +
  geom_text(data = data.frame(scale = c(1, 2, 3), 
                              label = c("Local Scale", "Park Scale", "County Scale"),
                              x = c(x_cen_lat[1], x_cen_lat[2], x_cen_lat[3]),
                              y = c(0.97, 0.97, 0.97)),
            aes(x = x, y = y, label = label), 
            hjust = 0.5, vjust = 0, size = 5, fontface = "plain", color = "black") +
  facet_wrap(~ scale, scales = "free_x") +
  labs(y = "Predicted Occupancy Probability\n", title = "Late Successional Trees (m²/ha)") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.border = element_rect(color = "black", linewidth = 0.6, fill = NA),
        panel.background = element_rect(fill = "white", color = NA),
        strip.text = element_blank(), strip.background = element_blank(),
        axis.title.x = element_blank(),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        axis.title.y = element_text(size = 14),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12)) +
  scale_color_manual(values = safe_pal_species) + ylim(0, 1))

ggsave("figures/pred_lat4.svg", plot = plot_lat4, device = "svg", width = 10.2, height = 4)
ggsave("figures/pred_lat4.png", plot = plot_lat4, device = "png", width = 10.2, height = 4)

# Shrub Coverage
x_stats_shr <- beta4_preds2 %>% 
  filter(covariate == "cov") %>% 
  select(pred_mean, scale) %>% 
  group_by(scale) %>% 
  summarise(
    min_val = min(pred_mean), max_val = max(pred_mean),
    middle_val = (min(pred_mean) + max(pred_mean)) / 2, median_val = median(pred_mean))

x_cen_shr <- x_stats_shr %>% pull(middle_val) %>% as.vector()

(plot_shr4 <- ggplot() +
  geom_line(data = beta4_preds2 %>% filter(covariate == "Shrub Basal Area"), 
            aes(x = X_range_ori, y = pred_mean, color = factor(sps)), linewidth = 1.2, show.legend = FALSE) +
  geom_rug(data = beta4_preds2 %>% filter(covariate == "cov"), 
           aes(x = pred_mean), 
           color = "gray50", sides = "b", size = 0.8, length = unit(0.05, "npc"), show.legend = FALSE) +
  geom_text(data = data.frame(scale = c(1, 2, 3), 
                              label = c("Local Scale", "Park Scale", "County Scale"),
                              x = c(x_cen_shr[1], x_cen_shr[2], x_cen_shr[3]),
                              y = c(0.97, 0.97, 0.97)),
            aes(x = x, y = y, label = label), 
            hjust = 0.5, vjust = 0, size = 5, fontface = "plain", color = "black") +
  facet_wrap(~ scale, scales = "free_x") +
  labs(y = "Predicted Occupancy Probability\n", title = "Shrub Coverage (%)") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.border = element_rect(color = "black", linewidth = 0.6, fill = NA),
        panel.background = element_rect(fill = "white", color = NA),
        strip.text = element_blank(), strip.background = element_blank(),
        axis.title.x = element_blank(),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        axis.title.y = element_text(size = 14),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12)) +
  scale_color_manual(values = safe_pal_species) + ylim(0, 1))

ggsave("figures/pred_shr4.svg", plot = plot_shr4, device = "svg", width = 10.2, height = 4)
ggsave("figures/pred_shr4.png", plot = plot_shr4, device = "png", width = 10.2, height = 4)

# Conifer Trees
x_stats_con <- beta2_preds2 %>% 
  filter(covariate == "cov") %>% 
  select(pred_mean, scale) %>% 
  group_by(scale) %>% 
  summarise(
    min_val = min(pred_mean), max_val = max(pred_mean),
    middle_val = (min(pred_mean) + max(pred_mean)) / 2, median_val = median(pred_mean))

x_cen_con <- x_stats_con %>% pull(middle_val) %>% as.vector()

(plot_con4 <- ggplot() +
  geom_line(data = beta2_preds2 %>% filter(covariate == "Conifer Density"), 
            aes(x = X_range_ori, y = pred_mean, color = factor(sps)), linewidth = 1.2, show.legend = FALSE) +
  geom_rug(data = beta2_preds2 %>% filter(covariate == "cov"), 
           aes(x = pred_mean), 
           color = "gray50", sides = "b", size = 0.8, length = unit(0.05, "npc"), show.legend = FALSE) +
  geom_text(data = data.frame(scale = c(1, 2, 3), 
                              label = c("Local Scale", "Park Scale", "County Scale"),
                              x = c(x_cen_con[1], x_cen_con[2], x_cen_con[3]),
                              y = c(0.97, 0.97, 0.97)),
            aes(x = x, y = y, label = label), 
            hjust = 0.5, vjust = 0, size = 5, fontface = "plain", color = "black") +
  facet_wrap(~ scale, scales = "free_x") +
  labs(y = "Predicted Occupancy Probability\n", title = "Conifer Trees (m²/ha)") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.border = element_rect(color = "black", linewidth = 0.6, fill = NA),
        panel.background = element_rect(fill = "white", color = NA),
        strip.text = element_blank(), strip.background = element_blank(),
        axis.title.x = element_blank(),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        axis.title.y = element_text(size = 14),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12)) +
  scale_color_manual(values = safe_pal_species) + ylim(0, 1))

ggsave("figures/pred_con4.svg", plot = plot_con4, device = "svg", width = 10.2, height = 4)
ggsave("figures/pred_con4.png", plot = plot_con4, device = "png", width = 10.2, height = 4)

# Create species legend plot
legend_plot <- ggplot() +
  geom_line(data = beta1_preds2 %>% filter(covariate == "Tree Density") %>% slice(1:17), 
            aes(x = X_range_ori, y = pred_mean, color = factor(sps)), linewidth = 1.2) +
  scale_color_manual(values = safe_pal_species, name = "Species") +
  theme_void() +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 12),
        legend.key.width = unit(1.5, "cm")) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE))

# Extract just the legend
library(cowplot)
# Create a minimal legend-only plot with unique species data
legend_data <- data.frame(
  sps = names(safe_pal_species),
  x = 1:length(safe_pal_species),
  y = 1
)

legend_plot <- ggplot(legend_data, aes(x = x, y = y, color = sps)) +
  geom_point(size = 0) +  # Invisible points, just for legend
  geom_line(aes(group = sps), size = 2) +  # Thick lines for legend
  scale_color_manual(values = safe_pal_species, name = "Species") +
  theme_void() +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 12),
        legend.key.width = unit(1.5, "cm")) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE))

# Extract legend (should be cleaner now)
species_legend <- get_legend(legend_plot)

# Save the legend
ggsave("figures/species_legend.svg", plot = legend_plot, device = "svg", width = 12, height = 12)
ggsave("figures/species_legend.png", plot = legend_plot, device = "png", width = 12, height = 12)
