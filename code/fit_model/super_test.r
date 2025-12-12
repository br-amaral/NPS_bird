# super test anbd coefficients!

# script to get the results from step 1, step 2 all sca old, and step 2 new res and compare them
# het all coeffient and values to plot so I dont have to look at the .rds model results to make any figures

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

if(substr(getwd(), 1, 3) == "/Us") {direc <- "local"} else {direc <- "hpc"}

if(direc == "local"){
    master_tab <- read_csv("/Users/bamaral/Library/CloudStorage/OneDrive-MichiganStateUniversity/GitHubOne/NPS_bird_copy/code/fit_model/mod_key.csv") %>%
            #filter(run == "yes") %>% 
            filter(step %in% c(2,3,4)) %>% 
            distinct() %>% 
            arrange(AOU_Code, step)

    } else {master_tab <- read_csv("code/fit_model/mod_key.csv") %>%
            #filter(run == "yes") %>% 
            filter(step %in% c(2,3,4)) %>% 
            distinct()%>% 
            arrange(AOU_Code, step)
    }

master_tab <- master_tab %>% filter(AOU_Code != "BCCH")

coef_tab <- function(row_index){
    sps_loop <- master_tab[row_index,]

    (spslp <- sps_loop$AOU_Code)
    (steplp <- sps_loop$step)
    (resultlp <- sps_loop$result)
    (scalelp <- sps_loop$select)

    res_mod <- read_rds(glue("data/model_res/{resultlp}.rds"))

    scales_names <- grep("^scales_", colnames(res_mod[[1]]), value = TRUE) 
    (all_params <- c("mu.alpha0", "mu.beta0", "beta", #"beta_int", 
                "alpha", scales_names))

    coef_tablp <- MCMCsummary(res_mod,
                              params = all_params,
                              probs = c(0.1, 0.5, 0.9),  # 80% credible intervals (10%, 50%, 90%)
                              round = 2)

    # get coefs names
     coef_tablp1 <- coef_tablp %>%
        rownames_to_column("coef") %>%
        # Add species and step information
        mutate(
            sps = spslp,
            step = steplp,
            result_file = resultlp,
            select_file = scalelp,
            # Check if 80% CI overlaps zero
            overlap_zero = case_when(
              # Both bounds positive - doesn't contain zero (significant positive)
              `10%` > 0 & `90%` > 0 ~ "no",
              # Both bounds negative - doesn't contain zero (significant negative)  
              `10%` < 0 & `90%` < 0 ~ "no",
              # Lower bound ≤ 0 AND upper bound ≥ 0 - contains zero (not significant)
              `10%` <= 0 & `90%` >= 0 ~ "yes",
              `90%` <= 0 & `10%` >= 0 ~ "yes",
              # Edge case: shouldn't happen but safety net
              TRUE ~ "unknown"
            ),
            # Additional helper columns for interpretation
            effect_direction = case_when(
                `10%` > 0 & `90%` > 0 ~ "positive",
                `10%` < 0 & `90%` < 0 ~ "negative",
                `10%` <= 0 & `90%` >= 0 ~ "non_significant",
                TRUE ~ "unclear"
            ),
            is_significant_80 = overlap_zero == "no"
        )

        # Get selected scales from the scale selection model
        if(str_detect(resultlp, "step2") == TRUE) {  # Only for step 2 models
        
            sca_mod <- read_rds(glue("data/model_res/{scalelp}.rds"))

            sca_mod1 <- sca_mod %>% 
                          mutate(coef = as.character(glue("{substr(betas, 1, 4)}[{substr(betas, 5, 5)}]"))) %>% 
                          select(coef, sca_sel, sca1, sca2, sca3)
            
            # Add selected scale info to coefficient table
            coef_tablp2 <- coef_tablp1 %>%
                              relocate(sps, step,
                                       coef, mean, sd, `10%`, `50%`, `90%`, Rhat, n.eff,
                                       overlap_zero, effect_direction, is_significant_80)  %>% 
                              as_tibble() %>% 
                              left_join(., sca_mod1, by = "coef")
        } else {coef_tablp2 <- coef_tablp1 %>%
                              relocate(sps, step,
                                       coef, mean, sd, `10%`, `50%`, `90%`, Rhat, n.eff,
                                       overlap_zero, effect_direction, is_significant_80)  %>% 
                              as_tibble()}
    
    return(coef_tablp2)
}

# Initialize empty tibble outside the function
coef_fim <- tibble()

# for(ii in 1:nrow(master_tab)){
# # Check if all three strings are equal (different steps and models for the same species)
#   if(substr(master_tab$result[ii], 1, 4) == master_tab$AOU_Code[ii] && 
#       master_tab$AOU_Code[ii] == substr(master_tab$select[ii], 1, 4)) {
#       # All three are equal
#     } else {
#       stop(glue("\n\n\n error on {master_tab$result[ii]} on row {ii}\n\n\n"))
#     }
#   print(ii)
#   sps_result <- coef_tab(ii)
#   coef_fim <- bind_rows(coef_fim, sps_result)
# }

# write_rds(coef_fim, file = "data/out/super_test_table.rds")

 coef_fim <- read_rds(file = "data/out/super_test_table.rds")  

# compare model results for different steps
coef_fim2 <- coef_fim %>% 
                  select(sps, coef, step, mean) %>% 
                  pivot_wider(names_from = step,
                              values_from = mean) %>%
                  mutate(`2_3` = `2` - `3`,
                        `3_4` = `3` - `4`) 

#write_csv(coef_fim2, file = "data/out/super_test_table2.csv")

# make figures with only the right step
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
                rbind("beta[1]", "beta[2]", "beta[3]", "beta[4]", "beta[5]"),
                rbind("Tree Density",
                      "Conifer Density",
                      "Late Successional Tree Density",
                      "Shrub Basal Area",
                      "Tree Basal Area")) %>% 
            as_tibble() %>% 
            rename(data_tab = V1,
                   coef = V2,
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

cov_key[which(cov_key$coef == 'beta[1]'), 4:7] <- 
                  as.list(c("DEN_mean", "DEN_sd", "DEN_min", "DEN_max"))

cov_key[which(cov_key$coef == 'beta[2]'), 4:7] <- 
                  as.list(c("BAcon_mean", "BAcon_sd", "BAcon_min", "BAcon_max"))

cov_key[which(cov_key$coef == 'beta[3]'), 4:7] <- 
                  as.list(c("BAlar_mean", "BAlar_sd", "BAlar_min", "BAlar_max"))

cov_key[which(cov_key$coef == 'beta[4]'), 4:7] <- 
                  as.list(c("SHR_mean", "SHR_sd", "SHR_min", "SHR_max"))

cov_key[which(cov_key$coef == 'beta[5]'), 4:7] <- 
                  as.list(c("BA_mean", "BA_sd", "BA_min", "BA_max"))
cov_key <- cov_key %>% rename(meanc = mean, sdc = sd)

coef_fim3_mu <- coef_fim %>% 
                    filter(step == 3) %>% 
                    filter(coef == "mu.beta0") 

coef_fim3_be <- coef_fim %>% 
                    filter(step == 3) %>% 
                    filter(substr(coef, 1, 4) == "beta") %>% 
                    filter(overlap_zero == "no",
                           coef != 'beta[6]')

coef_fim3 <- rbind(coef_fim3_mu, coef_fim3_be) %>% 
                  arrange(sps)

sps_list <- coef_fim3 %>% select(sps) %>% distinct() %>% pull()

cov_list <- cov_key %>% select(coef) %>% pull()

files <- tibble()

# for(ii in 1:nrow(coef_fim3_be)){ 
#   beta_loop <- coef_fim3_be$coef[ii]

#   scale_loop <- sca_key %>% filter(sca_num == coef_fim3_be$sca_sel[ii]) %>% pull(sca_key)
          
#   sps_loop <- coef_fim3_be$sps[ii]

#   coef_fim3_be1 <- coef_fim3_be[ii,]

#   cov_key_sps <- cov_key  %>% 
#                     filter(coef == beta_loop) %>% 
#                     mutate(meanc = glue("{scale_loop}{meanc}"),
#                             sdc = glue("{scale_loop}{sdc}"),
#                             min = glue("{scale_loop}{min}"),
#                             max = glue("{scale_loop}{max}"))

#   lims_obj_name <- glue("{tolower(sps_loop)}_lims")

#   if(exists(lims_obj_name)) {
#     lims_data <- get(lims_obj_name, envir = .GlobalEnv)  %>% 
#                     select(cov_key_sps$meanc,
#                            cov_key_sps$sdc,
#                            cov_key_sps$min,
#                            cov_key_sps$max) 
#     colnames(lims_data) <- c("meanc", "sdc", "min", "max")
#   }

#   X_range_ori <- seq(from = lims_data$min, to = lims_data$max, length.out = 100)
#   X_range <- (X_range_ori - lims_data$meanc) / lims_data$sdc

#   beta_param <- coef_fim3_be1$coef 
#   beta_index_order <- as.numeric(substr(beta_param, 6, 6))

#   res_mod <- read_rds(glue("data/model_res/{coef_fim3_be1$result_file}.rds"))

#   # Get posterior samples (efficient way)
#   all_beta0_samples <- res_mod$sims.list$mu.beta0
#   all_beta_samples <- res_mod$sims.list$beta[, beta_index_order]
  
#   # Vectorized prediction
#   n_samples <- length(all_beta_samples)
#   predictions <- matrix(NA, nrow = n_samples, ncol = length(X_range))
  
#   X <- cbind(1, X_range)  # N x 2
#   B <- cbind(all_beta0_samples, all_beta_samples)  # S x 2

#   linear_pred <- B %*% t(X)  # S x N matrix
#   predictions <- plogis(linear_pred)  # S x N matrix of predicted probabilities

#   # Calculate summary statistics (transpose to match your original code)
#   array_psi_pred <- t(predictions)
#   pred_mean      <- apply(array_psi_pred, 1, mean)
#   pred_median    <- apply(array_psi_pred, 1, median)
#   pred_lower     <- apply(array_psi_pred, 1, quantile, 0.025)
#   pred_upper     <- apply(array_psi_pred, 1, quantile, 0.975)
  
#   # Store results
#   pred_data <- tibble(
#     covariate = beta_loop,
#     x_value = X_range,
#     X_range_ori = X_range_ori,
#     pred_mean = pred_mean,
#     pred_median = pred_median,
#     pred_lower = pred_lower,
#     pred_upper = pred_upper) %>% 
#     mutate(sps = sps_loop, scale = scale_loop)
  
#   print(glue("pred_{sps_loop}_beta{as.numeric(substr(beta_loop, 6, 6))}_scale{coef_fim3_be$sca_sel[ii]}"))
#   assign(glue("pred_{sps_loop}_beta{as.numeric(substr(beta_loop, 6, 6))}_scale{coef_fim3_be$sca_sel[ii]}"), pred_data) 
#   write_rds(pred_data, file = glue("data/out/pred_{sps_loop}_beta{as.numeric(substr(beta_loop, 6, 6))}_scale{coef_fim3_be$sca_sel[ii]}.rds"))
  
#   files <- rbind(files, glue("data/out/pred_{sps_loop}_beta{as.numeric(substr(beta_loop, 6, 6))}_scale{coef_fim3_be$sca_sel[ii]}.rds"))
#   colnames(files) <- "pred_name"
#   write_rds(files, file = "data/out/pred_file_names.rds")

#   Create plot
#   p <- ggplot(pred_data, aes(x = X_range_ori)) +
#     geom_ribbon(aes(ymin = pred_lower, ymax = pred_upper), 
#                 #alpha = 0.3, 
#                 fill = "#c4dce5") +
#     geom_line(aes(y = pred_mean), color = "darkblue", linewidth = 1.2) +
#     labs(x = cov_key %>% filter(coef == beta_loop) %>% pull(Covariate), 
#         y = "Predicted Probability",
#         title = glue("{sps_loop}: {cov_key %>% filter(coef == beta_loop) %>% pull(Covariate)}"),
#         subtitle = glue("Scale: {scale_loop} ; Mean: {round(mean(all_beta_samples),2)}")) +
#     theme_minimal()
  
#   print(p)

#   ggsave(file = glue("figures/pred_{sps_loop}_beta{as.numeric(substr(beta_loop, 6, 6))}_scale{coef_fim3_be$sca_sel[ii]}.png"),
#          plot = p, device = "png", width = 7, height = 6)
# }

# make sure all preds arewe loaded
preds_load <- coef_fim3_be  %>% 
                mutate(pred_name = glue("pred_{sps}_beta{as.numeric(substr(coef, 6, 6))}_scale{sca_sel}"),
                       pred_file = glue("data/out/pred_{sps}_beta{as.numeric(substr(coef, 6, 6))}_scale{sca_sel}.rds")) %>% 
                select(pred_name, pred_file)

for(jj in 1:nrow(preds_load)){
  pred_load <- read_rds(preds_load$pred_file[jj])
  assign(preds_load$pred_name[jj], pred_load)
}

# get park ranges
XDAT_PATH <- "data/X.rds"
X10 <- read_rds(file = XDAT_PATH)

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

## TREE DENSITY PLOT ---------------------------------------------
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
                    drop_na() %>% 
                    mutate(covariate = "cov",
                           scale = as.numeric(scale)) %>% 
                    rename(pred_mean = treeden_ha)

min_pos_treeden <- min(treeden_covs2$y_pos)
beta1_preds$y_pos <- as.numeric(NA)
beta1_preds <- beta1_preds %>%
                  mutate(scale_name = scale,
                         scale = case_when(
                            scale_name == "site" ~ 1,
                            scale_name == "park" ~ 2, 
                            scale_name == "coun" ~ 3,
                            TRUE ~ as.numeric(scale)  # Keep existing numeric values
                          ))

beta1_preds2 <- full_join(beta1_preds, treeden_covs2) %>% 
                  mutate(park = factor(park, levels = rev(sort(unique(park))))) 

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

(pred_den_rug <- ggplot() +
                  geom_line(data = beta1_preds2 %>% filter(covariate == "beta[1]"), 
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
                        axis.text.y = element_text(size = 12)) +
                  scale_color_manual(values = safe_pal) +
                  ylim(0, 1)
)

ggsave("figures/pred_den_rug.svg", plot = pred_den_rug, device = "svg", width = 10.2, height = 4)
ggsave("figures/pred_den_rug.png", plot = pred_den_rug, device = "png", width = 10.2, height = 4)

# CONIFER BA PLOT ------------------------------------------------
# Get min, max, and middle for each scale
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
                    drop_na() %>% 
                    mutate(covariate = "cov",
                           scale = as.numeric(scale)) %>% 
                    rename(pred_mean = treecon_ha)

min_pos_treecon <- min(treecon_covs2$y_pos)
beta2_preds$y_pos <- as.numeric(NA)

beta2_preds <- beta2_preds %>%
                  mutate(scale_name = scale,
                         scale = case_when(
                            scale_name == "site" ~ 1,
                            scale_name == "park" ~ 2, 
                            scale_name == "coun" ~ 3,
                            TRUE ~ as.numeric(scale)  # Keep existing numeric values
                          ))

beta2_preds2 <- full_join(beta2_preds, treecon_covs2) %>% 
                  mutate(park = factor(park, levels = rev(sort(unique(park))))) 

nrow(treecon_covs2) + nrow(beta2_preds) == nrow(beta2_preds2)

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

pred_con_rug <- ggplot() +
                  geom_line(data = beta2_preds2 %>% filter(covariate == "beta[2]"), 
                            aes(x = X_range_ori, y = pred_mean, color = factor(sps)), linewidth = 1.2, show.legend = FALSE) +
                  # Add rug plot for park values
                  geom_rug(data = beta2_preds2 %>% filter(covariate == "cov"), 
                           aes(x = pred_mean, color = park), 
                           sides = "b", linewidth = 0.8, length = unit(0.05, "npc"), show.legend = FALSE) +
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
                        axis.text.y = element_text(size = 12)) +
                  scale_color_manual(values = safe_pal) +
                  ylim(0, 1)

ggsave("figures/pred_con_rug.svg", plot = pred_con_rug, device = "svg", width = 10.2, height = 4)
ggsave("figures/pred_con_rug.png", plot = pred_con_rug, device = "png", width = 10.2, height = 4)

## LATE SUCCESSIONAL PLOT
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
                    drop_na() %>% 
                    mutate(covariate = "cov",
                           scale = as.numeric(scale)) %>% 
                    rename(pred_mean = treelat_ha)

min_pos_treelat <- min(treelat_covs2$y_pos)
beta3_preds$y_pos <- as.numeric(NA)

beta3_preds <- beta3_preds %>%
                  mutate(scale_name = scale,
                         scale = case_when(
                            scale_name == "site" ~ 1,
                            scale_name == "park" ~ 2, 
                            scale_name == "coun" ~ 3,
                            TRUE ~ as.numeric(scale)  # Keep existing numeric values
                          ))

beta3_preds2 <- full_join(beta3_preds, treelat_covs2) %>% 
                  mutate(park = factor(park, levels = rev(sort(unique(park))))) 

nrow(treelat_covs2) + nrow(beta3_preds) == nrow(beta3_preds2)

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

pred_lat_rug <- ggplot() +
        geom_line(data = beta3_preds2 %>% filter(covariate == "beta[3]"), 
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

ggsave("figures/pred_lat_rug.svg", plot = pred_lat_rug, device = "svg", width = 10.2, height = 4)
ggsave("figures/pred_lat_rug.png", plot = pred_lat_rug, device = "png", width = 10.2, height = 4)

## SHRUB PERCENTAGE PLOT
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
                    drop_na() %>% 
                    mutate(covariate = "cov",
                           scale = as.numeric(scale)) %>% 
                    rename(pred_mean = shrub_ha)

min_pos_shrub <- min(shrub_covs2$y_pos)
beta4_preds$y_pos <- as.numeric(NA)

beta4_preds <- beta4_preds %>%
                  mutate(scale_name = scale,
                         scale = case_when(
                            scale_name == "site" ~ 1,
                            scale_name == "park" ~ 2, 
                            scale_name == "coun" ~ 3,
                            TRUE ~ as.numeric(scale)  # Keep existing numeric values
                          ))

beta4_preds2 <- full_join(beta4_preds, shrub_covs2) %>% 
                    mutate(park = factor(park, levels = rev(sort(unique(park))))) 

nrow(shrub_covs2) + nrow(beta4_preds) == nrow(beta4_preds2)

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

pred_shr_rug <- ggplot() +
                  geom_line(data = beta4_preds2 %>% filter(covariate == "beta[4]"), 
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

ggsave("figures/pred_shr_rug.svg", plot = pred_shr_rug, device = "svg", width = 10.2, height = 4)
ggsave("figures/pred_shr_rug.png", plot = pred_shr_rug, device = "png", width = 10.2, height = 4)

# TREE BASAL AREA PLOT -------------------------------------------
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
                    drop_na() %>% 
                    mutate(covariate = "cov",
                           scale = as.numeric(scale)) %>% 
                    rename(pred_mean = treeba_ha)

max_pos_treeba <- max(treeba_covs2$y_pos)
beta5_preds$y_pos <- as.numeric(NA)

beta5_preds <- beta5_preds %>%
                  mutate(scale_name = scale,
                         scale = case_when(
                            scale_name == "site" ~ 1,
                            scale_name == "park" ~ 2, 
                            scale_name == "coun" ~ 3,
                            TRUE ~ as.numeric(scale)  # Keep existing numeric values
                          ))

beta5_preds2 <- full_join(beta5_preds, treeba_covs2)  %>% 
                  mutate(park = factor(park, levels = rev(sort(unique(park))))) 

nrow(treeba_covs2) + nrow(beta5_preds) == nrow(beta5_preds2)

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

pred_ba_rug <- ggplot() +
                  geom_line(data = beta5_preds2 %>% filter(covariate == "beta[5]"), 
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

ggsave("figures/pred_ba_rug.svg", plot = pred_ba_rug, device = "svg", width = 10.2, height = 4)
ggsave("figures/pred_ba_rug.png", plot = pred_ba_rug, device = "png", width = 10.2, height = 4)

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

## SCALE PLOT ----------------------------------------------------
dat <- coef_fim %>% 
    rename(sca_select = sca_sel,
           cov = coef) %>% 
    arrange(sca_select, cov, sps) %>% 
    as_tibble() %>%  # FIXED: Removed incomplete .name_repair argument
    mutate(sps = toupper(sps)) %>% 
    rename(Covariate = cov) %>%
    mutate(coef = case_when(
              str_detect(Covariate, "^beta\\[") ~ paste0("beta", str_extract(Covariate, "\\d+")),
              str_detect(Covariate, "^alpha\\[") ~ paste0("alpha", str_extract(Covariate, "\\d+")),
              Covariate == "mu.alpha0" ~ "mu.alpha0",
              Covariate == "mu.beta0" ~ "mu.beta0",
              TRUE ~ Covariate  # Keep anything else as is
    ))

cov_name <- cbind(c("beta1",
                    "beta2",
                    "beta3",
                    "beta4",
                    "beta5"),
                  c("Tree Density",
                    "Conifer Density",
                    "Late Successional Tree Density",
                    "Shrub Basal Area",
                    "Tree Basal Area")) %>% 
            as_tibble(.name_repair) %>% 
            rename(coef = V1,
                   cov_name = V2)  %>% 
            mutate(cov_name = factor(cov_name, 
                              levels = c("Tree Density",
                                         "Conifer Density",
                                         "Late Successional Tree Density",
                                         "Shrub Basal Area",
                                         "Tree Basal Area")))

sca_name <- cbind(unique(na.omit(dat$sca_select)),
                  c("Local Scale",
                    "Park Scale",
                    "County Scale")) %>% 
            as_tibble(.name_repair) %>% 
            rename(sca_select = V1,
                   sca_name = V2)  %>% 
            mutate(sca_name = factor(sca_name, 
                              levels = c("Local Scale",
                                         "Park Scale",
                                         "County Scale")),
                   sca_select = as.numeric(sca_select))

dat0 <- left_join(dat, cov_name, by = "coef") %>%
            select(-Covariate) %>%  
            mutate(sca_select = as.numeric(sca_select)) %>% 
            rename(Covariate = cov_name,
                    low = `10%`,  
                    median = `50%`, 
                    up = `90%`) %>% 
            arrange(sps, coef) %>% 
            relocate(sps)  %>% 
            left_join(., sca_name, by = "sca_select") 

dat1 <- dat0 %>% 
            filter(sps != "BCCH",
                   substr(coef, 1, 4) == "beta",
                   coef != "beta6") %>% 
            mutate(select_sca = pmax(sca1, sca2, sca3, na.rm = T),
                   cov_sps = glue("{Covariate}_{sps}")) %>% 
            arrange(Covariate, sps) %>%  # Sort by Covariate first, then sps alphabetically
            mutate(cov_sps = factor(cov_sps, levels = sort(unique(cov_sps)))) %>% 
            mutate(sps_p = glue("{row_number()}_{sps}"),
                   sps_p = factor(sps_p, levels = sps_p))

# Create categorical labels for legend
dat1$sca_col <- case_when(
  dat1$select_sca >= 0.33 & dat1$select_sca < 0.5 ~ "(33-50]%",
  dat1$select_sca >= 0.5 & dat1$select_sca < 0.75 ~ "(50-75]%", 
  dat1$select_sca >= 0.75 ~ ">75%",
  TRUE ~ NA_character_
)

dat_sca <- dat1 %>% 
              pivot_longer(cols =c("sca1", "sca2", "sca3"),
                            names_to = "scale", 
                            values_to = "selec_freq",
                            names_prefix = "sca_select")  %>% 
              group_by(Covariate, sps) %>% 
              ungroup() %>% 
              select(-scale, -selec_freq) %>% 
              distinct()
                  
SPS_ORDER_PATH <- "data/src/sps_order.csv"

sps_order <- read_csv(SPS_ORDER_PATH) %>% 
                rename(sps = Aou_code) %>% 
                mutate(sps_name = factor(sps_name, levels = sps_name))  # Use current order as levels

dat_sca <- dat_sca %>% 
              left_join(., sps_order, by = "sps")

(sca_plot_selec_sca <- ggplot() +
  geom_point(data = dat_sca %>% filter(sca_select == 3), 
             aes(x = Covariate, y = sps, fill = sca_col), 
             size = 31, shape = 21, stroke = 0.9) +
  geom_point(data = dat_sca %>% filter(sca_select == 2), 
             aes(x = Covariate, y = sps, fill = sca_col), 
             size = 24, shape = 21, stroke = 0.9) +
  geom_point(data = dat_sca %>% filter(sca_select == 1), 
             aes(x = Covariate, y = sps, fill = sca_col), 
             size = 18.5, shape = 21, stroke = 0.9) +
  geom_text(data = dat_sca %>% filter(sca_select == 3), 
            aes(x = Covariate, y = sps, label = glue("{round(select_sca * 100)}")), #\nCI:{round(low, 1)} : {round(up, 1)}")), #
            size = 7, color = "black") +
  geom_text(data = dat_sca %>% filter(sca_select == 2), 
            aes(x = Covariate, y = sps, label = glue("{round(select_sca * 100)}")), #\nCI:{round(low, 1)} : {round(up, 1)}")), 
            size = 7, color = "black") +
  geom_text(data = dat_sca %>% filter(sca_select == 1), 
            aes(x = Covariate, y = sps, label = glue("{round(select_sca * 100)}")), #\nCI:{round(low, 1)} : {round(up, 1)}")), 
            size = 6, color = "black") +
  scale_fill_identity() +  # This tells ggplot to use the hex color values as actual colors
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.margin = margin(t = 25, r = 0, b = 0, l = 0),  # Add top margin to legend
        axis.text.x = element_text(hjust = 0.5, size = 24),
        axis.text.y = element_text(hjust = 0, size = 26),    
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 30),
        legend.title = element_text(size = 22, face = "bold", hjust = 0.5),  
        legend.text = element_text(size = 22),
        panel.grid.major = element_line(color = "gray85", linetype = "solid", linewidth = 0.6),
        panel.grid.minor = element_line(color = "gray85", linetype = "solid", linewidth = 0.6)) +
  scale_y_discrete(limits = rev(levels(factor(dat_sca$sps)))) +  # Reverse y-axis order
  scale_x_discrete(labels = function(x) {
    cov_codes <- unique(dat_sca$Covariate)
      # Manually add line breaks
      case_when(
        cov_codes == "Tree Basal Area" ~ "Tree Basal\nArea",
        cov_codes == "Late Successional Tree Density" ~ "Late Success.\n Tree Basal Area",
        cov_codes == "Conifer Density" ~ "Conifer \nBasal Area  ",
        cov_codes == "Tree Density" ~ "Tree\nDensity",
        cov_codes == "Shrub Basal Area" ~ "Shrub\nCover",
        TRUE ~ cov_codes  # Keep others as is
      )}) +
  labs(x = NULL, # "\nForest Covariate", 
       y = "Species\n", fill = "Scale Selection\nFrequency\n") +
  # Create manual legend for discrete colors
  scale_fill_manual(
    name = "Scale Selection\nFrequency",
    values = c("(33-50]%" = "#e9e3e9", 
               "(50-75]%" = "#d0add0", 
               ">75%" = "#ad70ad"),
    breaks = c("(33-50]%", "(50-75]%", ">75%"),
    labels = c("(33-50]%", "(50-75]%", ">75%"),
    na.value = "white",  # Handle any NA values
    guide = guide_legend(
      title.position = "left",
      title.hjust = 0.5,
      title.vjust = 0.5,  # Center title vertically with legend items
      label.vjust = 0.5,  # Center labels vertically with legend items
      nrow = 1,
      byrow = TRUE,  # Arrange legend items by row
      keywidth = unit(1.5, "cm"),  # Width of legend keys (affects spacing)
      keyheight = unit(0.8, "cm"), # Height of legend keys
      override.aes = list(size = 10, shape = 21, stroke = 0.9)
    )
  )
)


(sca_plot_selec_sca2_long <- ggplot() +
  geom_point(data = dat_sca %>% filter(sca_select == 3), 
             aes(x = sps_name, y = Covariate, fill = sca_col),  
             size = 33, shape = 21, stroke = 0.8, color = "#4A4A4A") +
  geom_point(data = dat_sca %>% filter(sca_select == 2), 
             aes(x = sps_name, y = Covariate, fill = sca_col),  
             size = 23, shape = 21, stroke = 0.8, color = "#4A4A4A") +
  geom_point(data = dat_sca %>% filter(sca_select == 1), 
             aes(x = sps_name, y = Covariate, fill = sca_col),  
             size = 17, shape = 21, stroke = 0.8, color = "#4A4A4A") +
  geom_text(data = dat_sca %>% filter(sca_select == 3), 
            aes(x = sps_name, y = Covariate, label = glue("{round(select_sca * 100)}")),  
            size = 7, color = "black") +
  geom_text(data = dat_sca %>% filter(sca_select == 2), 
            aes(x = sps_name, y = Covariate, label = glue("{round(select_sca * 100)}")),  
            size = 7, color = "black") +
  geom_text(data = dat_sca %>% filter(sca_select == 1), 
            aes(x = sps_name, y = Covariate, label = glue("{round(select_sca * 100)}")),  
            size = 6, color = "black") +
  scale_fill_identity() +
  theme_minimal() +
  theme(legend.position = "right",  # Changed from "bottom" to "right"
        legend.margin = margin(t = 0, r = 0, b = 0, l = 25),  # Adjusted margin (left margin for spacing)
        axis.text.x = element_text(hjust = 1, size = 22, angle = 90, vjust = 0.5),  
        axis.text.y = element_text(hjust = 1, size = 26),    
        axis.title.x = element_text(size = 30),  
        axis.title.y = element_text(size = 25),  
        legend.title = element_text(size = 22, face = "bold", hjust = 0.5),  
        legend.text = element_text(size = 22),
        panel.grid.major = element_line(color = "#6A635E", linetype = "solid", linewidth = 0.6),
        panel.grid.minor = element_line(color = "#6A635E", linetype = "solid", linewidth = 0.6)) +
  scale_x_discrete(limits = levels(factor(dat_sca$sps_name))) +  
  scale_y_discrete(labels = function(y) {  
    case_when(
      y == "Tree Basal Area" ~ "Tree Basal\nArea",
      y == "Late Successional Tree Density" ~ "Late Success.\n Tree Basal Area",
      y == "Conifer Density" ~ "Conifer \nBasal Area  ",
      y == "Tree Density" ~ "Tree\nDensity",
      y == "Shrub Basal Area" ~ "Shrub\nCover",
      TRUE ~ y
    )},
    limits = rev(levels(factor(dat_sca$Covariate)))) +  
  labs(y = "Forest Covariate",  
       x = "Species", 
       fill = "Scale Selection\nFrequency\n\n") +
  scale_fill_manual(
    name = "Scale Selection\nFrequency",
    values = c("(33-50]%" = "#E8E3DA", 
               "(50-75]%" = "#C6B7C9", 
               ">75%" = "#a07499"),
    breaks = c("(33-50]%", "(50-75]%", ">75%"),
    labels = c("\n(33-50]%\n", "(50-75]%\n", ">75%\n"),
    na.value = "white",
    guide = guide_legend(
      title.position = "top",  # Changed from "left" to "top" for vertical legend
      title.hjust = 0.5,
      title.vjust = 0.5,  
      label.vjust = 0.5,  
      ncol = 1,  # Changed from nrow = 1 to ncol = 1 for vertical arrangement
      byrow = FALSE,  # Changed to FALSE for vertical arrangement
      keywidth = unit(1.5, "cm"),  
      keyheight = unit(0.8, "cm"), 
      override.aes = list(size = 10, shape = 21, stroke = 0.9)
    )
  )
)

 ggsave("figures/sca_plot_select_sca_noleg2long.png", plot = sca_plot_selec_sca2_long, device = "png", width = 24, height = 14, dpi = 800)

 ggsave("figures/sca_plot_select_sca_noleg2long.svg", plot = sca_plot_selec_sca2_long, device = "svg", width = 24, height = 14)

## COEFFICENT PLOTS ----------------------------------------------
dat_sca <- coef_fim3 %>% 
                  filter(coef != "mu.beta0") %>% 
                  #select(Covariate, sps, sca1, sca2, sca3, overlap0) %>% 
                  pivot_longer(cols =c("sca1", "sca2", "sca3"),
                               names_to = "scale", 
                               values_to = "selec_freq",
                               names_prefix = "sca")  %>%
                  filter(is.na(sca_sel) | sca_sel == as.numeric(scale))

dat_sca2 <- dat_sca

nrow(dat_sca2)
dat_sca2 <- dat_sca2  %>% distinct() %>% mutate(scale = as.numeric(scale))
nrow(dat_sca2)

cov_name <- cbind(c("den",
                    "con",
                    "lat",
                    "shr",
                    "bas"),
                  c("beta[1]",
                    "beta[2]",
                    "beta[3]",
                    "beta[4]",
                    "beta[5]"),
                  c("Tree Density",
                    "Conifer Density",
                    "Late Successional Tree Density",
                    "Shrub Basal Area",
                    "Tree Basal Area")) %>% 
            as_tibble(.name_repair) %>% 
            rename(cov_name = V1,
                   coef = V2,
                   cov_name2 = V3)  %>% 
            mutate(cov_name2 = factor(cov_name2, 
                              levels = c("Tree Density",
                                         "Conifer Density",
                                         "Late Successional Tree Density",
                                         "Shrub Basal Area",
                                         "Tree Basal Area")))

sca_name <- cbind(c(1, 2, 3),
                  c("Local Scale",
                    "Park Scale",
                    "County Scale")) %>% 
            as_tibble(.name_repair) %>% 
            rename(scale = V1,
                   sca_name = V2)  %>% 
            mutate(sca_name = factor(sca_name, 
                              levels = c("Local Scale",
                                         "Park Scale",
                                         "County Scale")),
                   scale = as.numeric(scale))

dat_sca3 <- dat_sca2  %>% 
                left_join(., sca_name, by = "scale") %>% 
                left_join(., cov_name, by = "coef") %>% 
                rename(Covariate = cov_name2,
                       median = '50%') %>% 
                arrange(sps, as.numeric(Covariate))  # Arrange by factor level order

table(dat_sca3$scale)
table(dat_sca3$sca_name)
table(dat_sca3$coef)
table(dat_sca3$Covariate)
table(dat_sca3$cov_name)

dat_sca3$overlap0med <- NA

for(ii in 1:nrow(dat_sca3)){  
  # Get the actual lower and upper bounds
  lower_bound <- min(dat_sca3$`10%`[ii], dat_sca3$`90%`[ii])
  upper_bound <- max(dat_sca3$`10%`[ii], dat_sca3$`90%`[ii])
  
  # Check if zero falls within the corrected interval
  if(lower_bound <= 0 && upper_bound >= 0) {
      dat_sca3$overlap0med[ii] <- "yes"
    } else {
      dat_sca3$overlap0med[ii] <- "no"
    }
}

table(dat_sca3$overlap0med ==  dat_sca3$overlap_zero)

# dat_sca3 <- dat_sca3 %>% mutate(Covariate = betas) %>% filter(substr(betas, 1, 4) == "beta")
dat_sca3 <- dat_sca3  %>% 
      left_join(., sps_order, by = "sps")  %>% 
      filter(!is.na(sca_sel))

dat_sca3$scale %>% table()

(circles_coefs <- ggplot() +
  geom_point(data = dat_sca3, #%>% filter(!is.na(Covariate)), 
             aes(x = sps_name, y = Covariate),  # Switched x and y to match sca_plot_selec_sca2
             size = 1, fill = "white", alpha = 0) +
  #only non overlating CIs
  geom_point(data = dat_sca3 %>% filter(scale == 3, overlap0med == "no"), 
             aes(x = sps_name, y = Covariate, fill = median),  # Switched x and y
             size = 31, shape = 21, stroke = 0.8, color = "#4A4A4A") +  # Match stroke color
  geom_point(data = dat_sca3 %>% filter(scale == 2, overlap0med == "no"), 
             aes(x = sps_name, y = Covariate, fill = median),  # Switched x and y
             size = 24, shape = 21, stroke = 0.8, color = "#4A4A4A") +  # Match stroke color
  geom_point(data = dat_sca3 %>% filter(scale == 1, overlap0med == "no"), 
             aes(x = sps_name, y = Covariate, fill = median),  # Switched x and y
             size = 18.5, shape = 21, stroke = 0.8, color = "#4A4A4A") +  # Match stroke color
  # no overlap
  geom_text(data = dat_sca3 %>% filter(scale == 3, overlap0med == "no"), 
           aes(x = sps_name, y = Covariate, label = glue("{round(median, 2)}")),  # Switched x and y
            size = 7, color = "black") +
  geom_text(data = dat_sca3 %>% filter(scale == 2, overlap0med == "no"), 
            aes(x = sps_name, y = Covariate, label = glue("{round(median, 2)}")),  # Switched x and y
            size = 7, color = "black") +
  geom_text(data = dat_sca3 %>% filter(scale == 1, overlap0med == "no"), 
            aes(x = sps_name, y = Covariate, label = glue("{round(median, 2)}")),  # Switched x and y
            size = 6, color = "black") +
  scale_color_identity() +  
  scale_fill_gradient2(low = "#ca0557",           # Keep your color scheme
                       high = "#0794b7",        
                       midpoint = 0,              
                       name = "Covariate\nEffect size\n") +
  theme_minimal() +
  theme(legend.position = "right",  # Changed to match sca_plot_selec_sca2
        legend.margin = margin(t = 0, r = 0, b = 0, l = 25),  # Match margin
        axis.text.x = element_text(hjust = 1, size = 22, angle = 90, vjust = 0.5),  # Match rotation and centering
        axis.text.y = element_text(hjust = 1, size = 26),  # Match right alignment    
        axis.title.x = element_text(size = 30),  # Match sizes
        axis.title.y = element_text(size = 25),  # Match sizes
        legend.title = element_text(size = 22, face = "bold", hjust = 0.5),  
        legend.text = element_text(size = 22),
        panel.grid.major = element_line(color = "#6A635E", linetype = "solid", linewidth = 0.6),  # Match grid color
        panel.grid.minor = element_line(color = "#6A635E", linetype = "solid", linewidth = 0.6)) +  # Match grid color
  scale_x_discrete(limits = levels(factor(dat_sca3$sps_name))) +  # Use sps_name and match order
  scale_y_discrete(labels = function(y) {  # Now y is covariates
      case_when(
        y == "Tree Basal Area" ~ "Tree Basal\nArea",
        y == "Late Successional Tree Density" ~ "Late Success.\n Tree Basal Area",
        y == "Conifer Density" ~ "Conifer \nBasal Area  ",
        y == "Tree Density" ~ "Tree\nDensity",
        y == "Shrub Basal Area" ~ "Shrub\nCover",
        TRUE ~ y
      )},
      limits = rev(levels(factor(dat_sca3$Covariate)))) +  # Match reversed covariate order
  labs(y = "Forest Covariate",  # Switched labels to match
       x = "Species", 
       fill = "Covariate\nEffect size\n") +
  guides(
    fill = guide_colorbar(
      barwidth = unit(1, "cm"),    # Narrower for vertical legend
      barheight = unit(8, "cm")    # Taller for vertical legend
    )
  )
)

# ggsave("figures/circles_coefs.png", plot = circles_coefs, device = "png", width = 24, height = 14, dpi = 800)

# ggsave("figures/circles_coefs.svg", plot = circles_coefs, device = "svg", width = 24, height = 14)

