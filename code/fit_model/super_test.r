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

 coef_fim <- read_rds(file = "data/out/super_test_table.rds") %>% 

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
  
  print(glue("pred_{sps_loop}_beta{as.numeric(substr(beta_loop, 6, 6))}_scale{kk}"))
  assign(glue("pred_{sps_loop}_beta{as.numeric(substr(beta_loop, 6, 6))}_scale{kk}"), pred_data) 
  
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

  ggsave(file = glue("figures/pred_{sps_loop}_beta{as.numeric(substr(beta_loop, 6, 6))}_scale{kk}.png"),
          plot = p, device = "png", width = 7, height = 6)
}

save.image("data/out/supertest1.RData")

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
