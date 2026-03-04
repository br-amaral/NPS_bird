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
## when loading the model results, get the most updated file?
file_name <- "BAWW_step1_output_2025_11_16run1"

samples_jags <- read_rds(glue("data/model_res/{file_name}.rds"))

# # get parameter names
scales_names <- grep("^scales_", colnames(samples_jags[[1]]), value = TRUE)
(all_params <- c("mu.alpha0", "mu.beta0", "beta", #"beta_int", 
                "alpha", scales_names))
if(substr(file_name, nchar(file_name)-2, nchar(file_name)) == "int"){all_params <- c(all_params, "beta_int")}

# #! Par estimates ------------------------------------------------------------------
# par(mfrow = c(1,1))
# MCMCplot(samples_jags,
#          params = all_params,
#          main = file_name,
#          ref_ovl = TRUE,
#          ci = c(10,90))

# #! Traceplots ---------------------------------------------------------------------
# MCMCtrace(samples_jags,
#           params = all_params,
#           #main = file_name,
#           ind = TRUE,
#           pdf = FALSE,
#           exact = TRUE,
#           Rhat = TRUE,
#           n.eff = TRUE)

# #! Summary ------------------------------------------------------------------------
MCMCsummary(samples_jags,
            params = all_params,
            probs = c(0.1, 0.5, 0.9),  # 80% credible intervals (10%, 50%, 90%)
            round = 2)

# #! get beta parameters and selected scales ----------------------------------------
# # beta parameters that the 50 percent CI does not include 0
# betas <- tidybayes::get_variables(samples_jags)
# n_betas1 <- sub("\\[.*", "", betas) 
# n_betas <- length(n_betas1[n_betas1 == "beta"]) - 1
# betas_name <- paste0(n_betas1[n_betas1 == "beta"][-1], seq(1:n_betas))

# quant_group <- c(0.3, 0.7)
# # quant_group <- c(0.25, 0.75)

# beta_key <- tibble(
#   betas = betas_name, 
#   overlap0 = as.character(NA), 
#   scale50 = as.character(NA), 
#   sca_sel = as.character(NA),
#   sca1 = as.numeric(NA),
#   sca2 = as.numeric(NA),
#   sca3 = as.numeric(NA),
#   qt_lo = quant_group[1],
#   qt_up = quant_group[2],
#   qt_lo1 = as.numeric(NA),
#   qt_up1 = as.numeric(NA)
# )

# for(ii in 1:n_betas) {
# # betas
#   beta_loop1 <- MCMCchains(samples_jags, params = glue("beta"))
#   beta_loop2 <- beta_loop1[,ii]
    
#   #quantiles <- quantile(beta_loop2, )
#   quantiles <- quantile(beta_loop2, quant_group)

#   beta_key$qt_lo1[ii] <- lower_quantile <- quantiles[1]
#   beta_key$qt_up1[ii] <- upper_quantile <- quantiles[2]
  
#   # Check if quantiles overlap zero
#   if (lower_quantile <= 0 && upper_quantile >= 0) {
#     beta_key$overlap0[ii] <- "yes"
#   } else {
#     beta_key$overlap0[ii] <- "no"
#   }

# # scales
#   loop_sca <- glue("scales_beta{ii}")
#   sca_beta <- MCMCchains(samples_jags, params = loop_sca)

#   tb_mcmc_scales_i <- table(sca_beta)/sum(table(sca_beta))
#   selected_scales <- as.integer(names(which.max(tb_mcmc_scales_i)))

#   beta_key$sca_sel[ii] <- selected_scales
#   beta_key$sca1[ii] <- tb_mcmc_scales_i[1]
#   beta_key$sca2[ii] <- tb_mcmc_scales_i[2]
#   beta_key$sca3[ii] <- tb_mcmc_scales_i[3]
  
#   # get only covariates which scale was selected more than 50% of the time
#   if(TRUE %in% (beta_key[ii,] %>% select(sca1, sca2, sca3) > 0.5)){beta_key$scale50[ii] <- "no"} else {"yes"}
# }

# beta_key

# quant_name <- glue("{substr(quant_group[1], 3, 4)}_{substr(quant_group[2], 3, 4)}")
# # save beta and scale selection values
# write_rds(beta_key, file = glue("data/model_res/{file_name}_{quant_name}_SCA_SEL_PARS.rds"))

#! Coefficient tables --------------------------------------------------------------

if(substr(getwd(), 1, 3) == "/Us") {direc <- "local"} else {direc <- "hpc"}

if(direc == "local"){
    master_tab <- read_csv("/Users/bamaral/Library/CloudStorage/OneDrive-MichiganStateUniversity/GitHubOne/NPS_bird_copy/code/fit_model/mod_key.csv") %>%
            #filter(run == "yes") %>% 
            filter(step %in% c(3)) %>% 
            distinct()

    } else {master_tab <- read_csv("code/fit_model/mod_key.csv") %>%
            #filter(run == "yes") %>% 
            filter(step %in% c(3)) %>% 
            distinct()
    }

master_tab <- master_tab %>% filter(AOU_Code != "BCCH")
# Initialize empty tibble outside the function
coef_fim <- tibble()

coef_tab <- function(file_name, species_code, sca_select){
  samples_jags <- read_rds(glue("data/model_res/{file_name}.rds"))
  print(species_code)
  step_numb <- substr(file_name, 6, 10)

  # get parameter names
  if("step1" %in% step_numb) {
    scales_names <- grep("^scales_", colnames(samples_jags[[1]]), value = TRUE)
    all_params <- c("mu.alpha0", "mu.beta0", "beta", "alpha", scales_names)
      } else {
          all_params <- c("mu.alpha0", "mu.beta0", "beta", "alpha")
    }

  if(substr(file_name, nchar(file_name)-2, nchar(file_name)) == "int"){all_params <- c(all_params, "beta_int")}

  print(file_name)
  sps_coef <- MCMCsummary(samples_jags,
                          params = all_params,
                          probs = c(0.1, 0.5, 0.9),
                          round = 2)  
  r_nam <- rownames(sps_coef)

  sps_coef <- sps_coef %>% 
                as_tibble() %>% 
                mutate(sps = species_code,
                       cov = r_nam,
                       mod_ver = file_name,
                       step = step_numb) %>% 
                relocate(sps, step, cov) 

   beta_sca_names <- read_rds(glue("data/model_res/{sca_select}.rds")) %>% 
                select(-overlap0, -qt_lo, -qt_up) 
   beta_sca_names$cov <- c("beta[1]", "beta[2]", "beta[3]", "beta[4]", "beta[5]")
  
  sps_coef <- left_join(sps_coef, beta_sca_names, by = "cov")
  return(sps_coef)
}

#? check wheter I'm matching the species properly and run function!
memory.limit(size = 64000)
for(ii in 1:nrow(master_tab)){
# Check if all three strings are equal (different steps and models for the same species)
  if(substr(master_tab$result[ii], 1, 4) == master_tab$AOU_Code[ii] && 
      master_tab$AOU_Code[ii] == substr(master_tab$select[ii], 1, 4)) {
      # All three are equal
    } else {
      stop(glue("\n\n\n error on {master_tab$result[ii]} on row {ii}\n\n\n"))
    }

  sps_result <- coef_tab(master_tab$result[ii], master_tab$AOU_Code[ii], master_tab$select[ii])
  coef_fim <- bind_rows(coef_fim, sps_result)
}

out_dir <- "data/temp_scratch/coef_output"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

for(ii in 1:nrow(master_tab)) {
  # Check codes
  if(substr(master_tab$result[ii], 1, 4) != master_tab$AOU_Code[ii] ||
     master_tab$AOU_Code[ii]  != substr(master_tab$select[ii], 1, 4)) {
    stop(glue("error on {master_tab$result[ii]} on row {ii}\n"))
  }

  sps_result <- coef_tab(
    master_tab$result[ii],
    master_tab$AOU_Code[ii],
    master_tab$select[ii]
  )

  # Add an identifier so you can track origin
  sps_result$.species_id <- master_tab$AOU_Code[ii]

  # Write immediately; don't keep in RAM
  write_csv(
    sps_result,
    path = file.path(out_dir, sprintf("sp_%03d.csv", ii))
  )
}

# bind all CSV files at the end
files <- list.files(out_dir, pattern = "^sp_", full.names = TRUE)
coef_fim <- readr::read_csv(files, id = "source")

write_rds(coef_fim, file = "data/out/coef_fim_80_new4.rds")
write_csv(coef_fim, file = "data/out/coef_fim_80_new4.csv")

## order and organize things for plotting
phylo_order <- readxl::read_excel("data/src/original/AviList-v2025-11Jun-short.xlsx") %>% 
                  select(Sequence, Scientific_name)
my_sps <- read_csv("data/src/sps_order.csv")

my_sps[which(my_sps$Scientific_name %in% phylo_order$Scientific_name),]
my_sps[which(my_sps$Scientific_name %!in% phylo_order$Scientific_name),]

phylo_order2 <- phylo_order %>% 
                  filter(Scientific_name %in% my_sps$Scientific_name) %>% 
                  arrange(Sequence) %>% 
                  left_join(., my_sps, by = "Scientific_name") %>% 
                  mutate(sps_order = seq(1, n()))

nrow(phylo_order2) == nrow(my_sps)

write_rds(phylo_order2, file = "data/src/sps_phylo_order.rds")

head(coef_fim)
tables3 <- coef_fim %>% 
              filter(step == 2)

coef_fim$step %>% table()





plot_cov_steps <- function(sps) {
  
  # Filter data for the specified species
  sps_data <- coef_fim %>% 
    filter(sps == !!sps) %>%
    # Clean up covariate names (remove beta[ ] brackets and add new ones)
    mutate(cov_clean = case_when(
      str_detect(cov, "beta\\[1\\]") ~ "Tree Density",
      str_detect(cov, "beta\\[2\\]") ~ "Conifer Density", 
      str_detect(cov, "beta\\[3\\]") ~ "Late Successional Tree Density",
      str_detect(cov, "beta\\[4\\]") ~ "Shrub Basal Area",
      str_detect(cov, "beta\\[5\\]") ~ "Tree Basal Area",
      str_detect(cov, "beta\\[6\\]") ~ "Park Size",           
      str_detect(cov, "alpha\\[1\\]") ~ "Time of Day",        
      str_detect(cov, "alpha\\[2\\]") ~ "Day of Year",        
      str_detect(cov, "alpha\\[3\\]") ~ "Day of Year²",       
      str_detect(cov, "mu.beta0") ~ "Occupancy Intercept",    
      str_detect(cov, "mu.alpha0") ~ "Detection Intercept",   
      str_detect(cov, "beta_int") ~ "Interaction Term",
      # Add scale parameter labels
      str_detect(cov, "scales_beta1") ~ "Tree Density Scale",
      str_detect(cov, "scales_beta2") ~ "Conifer Density Scale", 
      str_detect(cov, "scales_beta3") ~ "Late Successional Tree Scale",
      str_detect(cov, "scales_beta4") ~ "Shrub Basal Area Scale",
      str_detect(cov, "scales_beta5") ~ "Tree Basal Area Scale",
      str_detect(cov, "scales_beta6") ~ "Park Size Scale",
      TRUE ~ cov
    )) %>%
    # Create simple step labels
    mutate(step_label = case_when(
      step == "step3" ~ "Step 1", 
      step == "step4" ~ "Step 2",
      TRUE ~ step
    ))
  
  # Check if there's data for this species
  if(nrow(sps_data) == 0) {
    stop(glue("No data found for species: {sps}"))
  }
  
  # DEBUG: Print what steps we actually have
  print(paste("Available steps in data:", paste(unique(sps_data$step), collapse = ", ")))
  print(paste("Available step_labels in data:", paste(unique(sps_data$step_label), collapse = ", ")))
  
  # Create proper ordering: Betas, Scales, Alphas, Intercepts
  desired_order <- c(
    # Beta coefficients first
    "Tree Density", "Conifer Density", "Late Successional Tree Density", 
    "Shrub Basal Area", "Tree Basal Area", "Park Size", "Interaction Term",
    # Scale parameters second  
    "Tree Density Scale", "Conifer Density Scale", "Late Successional Tree Scale",
    "Shrub Basal Area Scale", "Tree Basal Area Scale", "Park Size Scale",
    # Alpha coefficients third
    "Time of Day", "Day of Year", "Day of Year²",
    # Intercepts last
    "Detection Intercept", "Occupancy Intercept"
  )
  
  # Filter to only include covariates that exist in the data and create factor with desired order
  available_covs <- unique(sps_data$cov_clean)
  ordered_covs <- desired_order[desired_order %in% available_covs]
  
  # Create the factor with proper ordering (reversed for inverted y-axis)
  sps_data <- sps_data %>%
    mutate(cov_clean = factor(cov_clean, levels = rev(ordered_covs))) %>%
    # Get unique model versions and assign offsets
    arrange(mod_ver) %>%
    mutate(mod_ver_factor = as.factor(mod_ver),
           mod_ver_numeric = as.numeric(mod_ver_factor)) %>%
    # Add bigger vertical offset for each model version
    mutate(y_offset = case_when(
      mod_ver_numeric == 1 ~ -0.25,  # First mod_ver: bigger offset below
      mod_ver_numeric == 2 ~ 0.25,   # Second mod_ver: bigger offset above  
      mod_ver_numeric == 3 ~ -0.15,  # Third mod_ver (if exists): medium below
      mod_ver_numeric == 4 ~ 0.15,   # Fourth mod_ver (if exists): medium above
      TRUE ~ 0
    )) %>%
    # FIXED: Create y position based on factor levels, not the original ordering
    mutate(cov_y_pos = as.numeric(cov_clean) + y_offset)
  
  # Create color palette for model versions
  n_mod_vers <- length(unique(sps_data$mod_ver))
  mod_ver_colors <- RColorBrewer::brewer.pal(max(3, n_mod_vers), "Set1")[1:n_mod_vers]
  names(mod_ver_colors) <- sort(unique(sps_data$mod_ver))
  
  # Get the actual step labels in the data (not the hardcoded ones)
  actual_step_labels <- unique(sps_data$step_label)
  print(paste("Actual step labels found:", paste(actual_step_labels, collapse = ", ")))
  
  # Create shape values for the actual steps in the data
  available_shapes <- c(21, 24, 22, 23, 25)  # Circle, triangle, square, diamond, triangle down
  step_shapes <- setNames(available_shapes[1:length(actual_step_labels)], sort(actual_step_labels))
  
  print("Shape mapping being used:")
  print(step_shapes)
  
  # Create the plot
  p <- ggplot(sps_data, aes(color = mod_ver, fill = mod_ver)) +
    # Add thicker vertical line at zero
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray30", linewidth = 1.2) +
    # Add credible intervals as horizontal lines with vertical offset
    {if("10%" %in% colnames(sps_data) && "90%" %in% colnames(sps_data)) {
      geom_segment(aes(x = `10%`, xend = `90%`, 
                       y = cov_y_pos, yend = cov_y_pos),
                   linewidth = 0.8)
    }} +
    # Add mean points with vertical offset and different shapes
    geom_point(aes(x = mean, y = cov_y_pos, shape = step_label),
               size = 4,      
               color = "black",
               stroke = 0.7) +  
    # Dynamic shape scale based on actual data
    scale_shape_manual(
      values = step_shapes,  # Use the dynamic shape mapping
      name = "Model Step"
    ) +
    # Customize colors for model versions
    scale_fill_manual(values = mod_ver_colors, name = "Model Version") +
    scale_color_manual(values = mod_ver_colors, name = "Model Version") +
    # Set y-axis to show covariate names at correct positions
    scale_y_continuous(
      breaks = 1:length(levels(sps_data$cov_clean)),  
      labels = rev(levels(sps_data$cov_clean)),  # Reverse labels to match inverted axis
      name = "Covariate"
    ) +
    # Labels and theme
    labs(
      title = glue("Coefficient Estimates for {sps}"),
      subtitle = "Points show posterior means with 95% credible intervals",
      x = "Coefficient Value"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),  
      plot.subtitle = element_text(size = 10, color = "gray50", hjust = 0.5),  
      axis.title = element_text(size = 12),
      axis.text.y = element_text(size = 10),
      axis.text.x = element_text(size = 10),
      legend.position = "bottom",
      legend.title = element_text(size = 11, face = "bold"),
      legend.text = element_text(size = 10),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(color = "gray90", linewidth = 0.5),
      panel.grid.major.y = element_line(color = "gray90", linewidth = 0.5),
      legend.box = "vertical",
      legend.margin = margin(t = 10)
    ) +
    guides(
      fill = guide_legend(
        title = "Model Version",
        nrow = length(unique(sps_data$mod_ver)),  
        byrow = TRUE,
        order = 1
      ),
      color = guide_legend(
        title = "Model Version", 
        nrow = length(unique(sps_data$mod_ver)),
        byrow = TRUE,
        order = 1
      ),
      shape = guide_legend(
        title = "Model Step",
        nrow = length(actual_step_labels),  # Dynamic based on actual steps
        byrow = TRUE,
        order = 2
      )
    )
  
  return(p)
}

# plot_cov_steps("BAWW")

# Add this new function after your existing plot_cov_steps function:

plot_cov_steps_beta_only <- function(sps) {
  
  # Filter data for the specified species - ONLY beta[1] through beta[5]
  sps_data <- coef_fim %>% 
    filter(sps == !!sps) %>%
    # Filter to only include beta[1] through beta[5]
    filter(str_detect(cov, "^beta\\[[1-5]\\]$")) %>%
    # Clean up covariate names (remove beta[ ] brackets and add new ones)
    mutate(cov_clean = case_when(
      str_detect(cov, "beta\\[1\\]") ~ "Tree Density",
      str_detect(cov, "beta\\[2\\]") ~ "Conifer Density", 
      str_detect(cov, "beta\\[3\\]") ~ "Late Successional Tree Density",
      str_detect(cov, "beta\\[4\\]") ~ "Shrub Basal Area",
      str_detect(cov, "beta\\[5\\]") ~ "Tree Basal Area",
      TRUE ~ cov
    )) %>%
    # Create simple step labels
    mutate(step_label = case_when(
      step == "step3" ~ "Step 1", 
      step == "step4" ~ "Step 2",
      TRUE ~ step
    ))
  
  # Check if there's data for this species
  if(nrow(sps_data) == 0) {
    stop(glue("No beta[1-5] data found for species: {sps}"))
  }
  
  # Create proper ordering: Only betas 1-5
  desired_order <- c(
    "Tree Density", "Conifer Density", "Late Successional Tree Density", 
    "Shrub Basal Area", "Tree Basal Area"
  )
  
  # Filter to only include covariates that exist in the data and create factor with desired order
  available_covs <- unique(sps_data$cov_clean)
  ordered_covs <- desired_order[desired_order %in% available_covs]
  
  # Create the factor with proper ordering (reversed for inverted y-axis)
  sps_data <- sps_data %>%
    mutate(cov_clean = factor(cov_clean, levels = rev(ordered_covs))) %>%
    # Get unique model versions and assign offsets
    arrange(mod_ver) %>%
    mutate(mod_ver_factor = as.factor(mod_ver),
           mod_ver_numeric = as.numeric(mod_ver_factor)) %>%
    # Add bigger vertical offset for each model version
    mutate(y_offset = case_when(
      mod_ver_numeric == 1 ~ -0.25,  # First mod_ver: bigger offset below
      mod_ver_numeric == 2 ~ 0.25,   # Second mod_ver: bigger offset above  
      mod_ver_numeric == 3 ~ -0.15,  # Third mod_ver (if exists): medium below
      mod_ver_numeric == 4 ~ 0.15,   # Fourth mod_ver (if exists): medium above
      TRUE ~ 0
    )) %>%
    # Create y position based on factor levels
    #! TODO: remove next line
    mutate(y_offset <- 0) %>% 
    mutate(cov_y_pos = as.numeric(cov_clean) + y_offset)
  
  # Create color palette for model versions
  n_mod_vers <- length(unique(sps_data$mod_ver))
  mod_ver_colors <- RColorBrewer::brewer.pal(max(3, n_mod_vers), "Set1")[1:n_mod_vers]
  names(mod_ver_colors) <- sort(unique(sps_data$mod_ver))
  
  # Get the actual step labels in the data
  actual_step_labels <- unique(sps_data$step_label)
  
  # Create shape values for the actual steps in the data
  available_shapes <- c(21, 24, 22, 23, 25)  # Circle, triangle, square, diamond, triangle down
  step_shapes <- setNames(available_shapes[1:length(actual_step_labels)], sort(actual_step_labels))
  
  # Create the plot
  p <- ggplot(sps_data, aes(color = mod_ver, fill = mod_ver)) +
    # Add thicker vertical line at zero
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray30", linewidth = 1.2) +
    # Add credible intervals as horizontal lines with vertical offset
    {if("10%" %in% colnames(sps_data) && "90%" %in% colnames(sps_data)) {
      geom_segment(aes(x = `10%`, xend = `90%`, 
                       y = cov_y_pos, yend = cov_y_pos),
                   linewidth = 0.8)
    }} +
    # Add mean points with vertical offset and different shapes
    geom_point(aes(x = mean, y = cov_y_pos, shape = step_label),
               size = 4,      
               color = "black",
               stroke = 0.7) +  
    # Dynamic shape scale based on actual data
    scale_shape_manual(
      values = step_shapes,
      name = "Model Step"
    ) +
    # Customize colors for model versions
    scale_fill_manual(values = mod_ver_colors, name = "Model Version") +
    scale_color_manual(values = mod_ver_colors, name = "Model Version") +
    # Set y-axis to show covariate names at correct positions
    scale_y_continuous(
      breaks = 1:length(levels(sps_data$cov_clean)),  
      labels = rev(levels(sps_data$cov_clean)),
      name = "Covariate"
    ) +
    # Labels and theme
    labs(
      title = glue("Forest Covariate Estimates for {sps}"),
      subtitle = "Points show posterior means with 95% credible intervals",
      x = "Coefficient Value"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),  
      plot.subtitle = element_text(size = 10, color = "gray50", hjust = 0.5),  
      axis.title = element_text(size = 12),
      axis.text.y = element_text(size = 10),
      axis.text.x = element_text(size = 10),
      legend.position = "bottom",
      legend.title = element_text(size = 11, face = "bold"),
      legend.text = element_text(size = 10),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(color = "gray90", linewidth = 0.5),
      panel.grid.major.y = element_line(color = "gray90", linewidth = 0.5),
      legend.box = "vertical",
      legend.margin = margin(t = 10)
    ) +
    guides(
      fill = guide_legend(
        title = "Model Version",
        nrow = length(unique(sps_data$mod_ver)),  
        byrow = TRUE,
        order = 1
      ),
      color = guide_legend(
        title = "Model Version", 
        nrow = length(unique(sps_data$mod_ver)),
        byrow = TRUE,
        order = 1
      ),
      shape = guide_legend(
        title = "Model Step",
        nrow = length(actual_step_labels),
        byrow = TRUE,
        order = 2
      )
    )
  
  return(p)
}

# Updated plot_all_species function with both versions:

plot_all_species <- function() {
  species_list <- unique(coef_fim$sps)
  
  for(sp in species_list) {
    tryCatch({
      # Full plot (all parameters)
      p_full <- plot_cov_steps(sp)
      
      # Beta-only plot (beta[1] through beta[5] only)
      p_beta <- plot_cov_steps_beta_only(sp)
      
      # Save full plot as PDF
      ggsave(
        filename = glue("figures/coeff_plot_{sp}_full.pdf"),
        plot = p_full,
        width = 7.5, height = 10
      )
      
      # Save beta-only plot as PDF
      ggsave(
        filename = glue("figures/beta_only_coeff_plot_{sp}.pdf"),
        plot = p_beta,
        width = 7.5, height = 6  # Shorter height since fewer covariates
      )
      
      print(glue("Saved both plots for {sp} (full and beta-only versions)"))
      
    }, error = function(e) {
      print(glue("Error plotting {sp}: {e$message}"))
    })
  }
}

plot_all_species()

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
