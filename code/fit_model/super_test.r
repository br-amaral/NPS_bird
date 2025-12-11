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

# coef_tab <- function(row_index){
#     sps_loop <- master_tab[row_index,]

#     (spslp <- sps_loop$AOU_Code)
#     (steplp <- sps_loop$step)
#     (resultlp <- sps_loop$result)
#     (scalelp <- sps_loop$select)

#     res_mod <- read_rds(glue("data/model_res/{resultlp}.rds"))

#     scales_names <- grep("^scales_", colnames(res_mod[[1]]), value = TRUE) 
#     (all_params <- c("mu.alpha0", "mu.beta0", "beta", #"beta_int", 
#                 "alpha", scales_names))

#     coef_tablp <- MCMCsummary(res_mod,
#                               params = all_params,
#                               probs = c(0.1, 0.5, 0.9),  # 80% credible intervals (10%, 50%, 90%)
#                               round = 2)

#     # get coefs names
#      coef_tablp1 <- coef_tablp %>%
#         rownames_to_column("coef") %>%
#         # Add species and step information
#         mutate(
#             sps = spslp,
#             step = steplp,
#             result_file = resultlp,
#             select_file = scalelp,
#             # Check if 80% CI overlaps zero
#             overlap_zero = case_when(
#               # Both bounds positive - doesn't contain zero (significant positive)
#               `10%` > 0 & `90%` > 0 ~ "no",
#               # Both bounds negative - doesn't contain zero (significant negative)  
#               `10%` < 0 & `90%` < 0 ~ "no",
#               # Lower bound ≤ 0 AND upper bound ≥ 0 - contains zero (not significant)
#               `10%` <= 0 & `90%` >= 0 ~ "yes",
#               `90%` <= 0 & `10%` >= 0 ~ "yes",
#               # Edge case: shouldn't happen but safety net
#               TRUE ~ "unknown"
#             ),
#             # Additional helper columns for interpretation
#             effect_direction = case_when(
#                 `10%` > 0 & `90%` > 0 ~ "positive",
#                 `10%` < 0 & `90%` < 0 ~ "negative",
#                 `10%` <= 0 & `90%` >= 0 ~ "non_significant",
#                 TRUE ~ "unclear"
#             ),
#             is_significant_80 = overlap_zero == "no"
#         )

#         # Get selected scales from the scale selection model
#         if(str_detect(resultlp, "step2") == TRUE) {  # Only for step 2 models
        
#             sca_mod <- read_rds(glue("data/model_res/{scalelp}.rds"))

#             sca_mod1 <- sca_mod %>% 
#                           mutate(coef = as.character(glue("{substr(betas, 1, 4)}[{substr(betas, 5, 5)}]"))) %>% 
#                           select(coef, sca_sel, sca1, sca2, sca3)
            
#             # Add selected scale info to coefficient table
#             coef_tablp2 <- coef_tablp1 %>%
#                               relocate(sps, step,
#                                        coef, mean, sd, `10%`, `50%`, `90%`, Rhat, n.eff,
#                                        overlap_zero, effect_direction, is_significant_80)  %>% 
#                               as_tibble() %>% 
#                               left_join(., sca_mod1, by = "coef")
#         } else {coef_tablp2 <- coef_tablp1 %>%
#                               relocate(sps, step,
#                                        coef, mean, sd, `10%`, `50%`, `90%`, Rhat, n.eff,
#                                        overlap_zero, effect_direction, is_significant_80)  %>% 
#                               as_tibble()}
    
#     return(coef_tablp2)
# }

# # Initialize empty tibble outside the function
# coef_fim <- tibble()

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

write_csv(coef_fim2, file = "data/out/super_test_table2.csv")

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

for(ii in 1:length(coef_fim3_be)){ 
  beta_loop <- coef_fim3_be$coef[ii]

  scale_loop <- sca_key %>% filter(sca_num == coef_fim3_be$sca_sel[ii]) %>% pull(sca_key)
          
  sps_loop <- coef_fim3_be$sps[ii]

  coef_fim3_be1 <- coef_fim3_be[ii,]

  cov_key_sps <- cov_key  %>% 
                    filter(coef == beta_loop) %>% 
                    mutate(meanc = glue("{scale_loop}{meanc}"),
                            sdc = glue("{scale_loop}{sdc}"),
                            min = glue("{scale_loop}{min}"),
                            max = glue("{scale_loop}{max}"))

  lims_obj_name <- glue("{tolower(sps_loop)}_lims")

  if(exists(lims_obj_name)) {
    lims_data <- get(lims_obj_name, envir = .GlobalEnv)  %>% 
                    select(cov_key_sps$meanc,
                           cov_key_sps$sdc,
                           cov_key_sps$min,
                           cov_key_sps$max) 
    colnames(lims_data) <- c("meanc", "sdc", "min", "max")
  }

  X_range_ori <- seq(from = lims_data$min, to = lims_data$max, length.out = 100)
  X_range <- (X_range_ori - lims_data$meanc) / lims_data$sdc

  beta_param <- coef_fim3_be1$coef 
  beta_index_order <- as.numeric(substr(beta_param, 6, 6))

  res_mod <- read_rds(glue("data/model_res/{coef_fim3_be1$result_file}.rds"))

  # Get posterior samples (efficient way)
  all_beta0_samples <- res_mod$sims.list$mu.beta0
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
  pred_mean      <- apply(array_psi_pred, 1, mean)
  pred_median    <- apply(array_psi_pred, 1, median)
  pred_lower     <- apply(array_psi_pred, 1, quantile, 0.025)
  pred_upper     <- apply(array_psi_pred, 1, quantile, 0.975)
  
  # Store results
  pred_data <- tibble(
    covariate = beta_loop,
    x_value = X_range,
    X_range_ori = X_range_ori,
    pred_mean = pred_mean,
    pred_median = pred_median,
    pred_lower = pred_lower,
    pred_upper = pred_upper) %>% 
    mutate(sps = sps_loop, scale = scale_loop)
  
  print(glue("pred_{sps_loop}_beta{as.numeric(substr(beta_loop, 6, 6))}_scale{coef_fim3_be$sca_sel[ii]}"))
  assign(glue("pred_{sps_loop}_beta{as.numeric(substr(beta_loop, 6, 6))}_scale{coef_fim3_be$sca_sel[ii]}"), pred_data) 
  
  # Create plot
  p <- ggplot(pred_data, aes(x = X_range_ori)) +
    geom_ribbon(aes(ymin = pred_lower, ymax = pred_upper), 
                #alpha = 0.3, 
                fill = "#c4dce5") +
    geom_line(aes(y = pred_mean), color = "darkblue", linewidth = 1.2) +
    labs(x = cov_key %>% filter(coef == beta_loop) %>% pull(Covariate), 
        y = "Predicted Probability",
        title = glue("{sps_loop}: {cov_key %>% filter(coef == beta_loop) %>% pull(Covariate)}"),
        subtitle = glue("Scale: {scale_loop} ; Mean: {round(mean(all_beta_samples),2)}")) +
    theme_minimal()
  
  print(p)

  ggsave(file = glue("figures/pred_{sps_loop}_beta{as.numeric(substr(beta_loop, 6, 6))}_scale{coef_fim3_be$sca_sel[ii]}.png"),
          plot = p, device = "png", width = 7, height = 6)
}

save.image(file = "data/out/supertest1.RData")

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

save.image(file = "data/out/supertest2.RData")


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
