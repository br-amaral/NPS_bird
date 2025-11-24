#? *********************************************************************************
#? ---------------------------   Manuscript Figures   ------------------------------
#? *********************************************************************************
#
#! Code to ...
#
#! Source -----------------------------------------------
#           - :
#           - :
#
#! Input ------------------------------------------------
#           - :
#           - :
#
#! Output -----------------------------------------------
#           - :
#           - :

#! Package library and versions -------------------------
#  Created a library repo?
#  (  )yes  (  )no
#  renv::init()
# Load an existing library?
#  renv::restore()
# Installed new packages?
#  renv::snapshot()

# detach packages and clear workspace
freshr::freshr() 

#! Load packages ----------------------------------------
library(tidyverse)
library(conflicted)
library(glue)
library(MCMCvis)
library(viridis)
library(svglite)
library(ggh4x)

conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::filter)

#! Make functions ---------------------------------------
colanmes <- colnames
lenght <- length
`%!in%` <- Negate(`%in%`)

#! Source code ------------------------------------------

#! Import data ------------------------------------------
## file paths
COEF_TABLE_PATH <- "code/fit_model/mod_key.csv"
if(substr(getwd(), 1, 3) == "/Us") {direc <- "local"} else {direc <- "hpc"}
#if(direc == "local"){COEF_TABLE_PATH <- glue("/Users/bamaral/Library/CloudStorage/OneDrive-MichiganStateUniversity/GitHubOne/NPS_bird_copy/{COEF_TABLE_PATH}")}
# if(direc == "local"){COEF_TABLE_PATH <- glue("/Users/bamaral/Documents/GitHub/NPS_bird_copy/{COEF_TABLE_PATH}")}

## read files
coef_path_file <- read_csv(COEF_TABLE_PATH) %>%
        filter(run == "yes") %>% 
        filter(step == 4) %>% 
        mutate(AOU_Code = substr(result, 1, 4)) %>% 
        filter(AOU_Code %!in% c("BCCH"))

for(ii in 1:nrow(coef_path_file)) {
    # ii <- 2
    (loop_sps <- substr(coef_path_file$result[ii], 1, 4))

    loop_run <- substr(coef_path_file$result[ii], nchar(coef_path_file$result[ii]) - 3, nchar(coef_path_file$result[ii]))

    quants <- "25_75" #ifelse((as.numeric(substr(loop_run, 4, 4)) %% 2 == 0) == TRUE, "25_75", "3_7")

    samples_jags <- read_rds(glue("data/model_res/{coef_path_file$result[ii]}.rds"))
    beta_sca_names <- read_rds(glue("data/model_res/{coef_path_file$select[ii]}.rds")) %>% 
          #filter(overlap0 == "no") %>%
          add_row(betas = "park_size") %>%  # Add empty row for 3 alphas + 1 beta park size
          add_row(betas = "alpha[1]") %>%  
          add_row(betas = "alpha[2]") %>%  
          add_row(betas = "alpha[3]")
    beta_sca_names[1:6,1] <- glue("beta[{seq(1,6,1)}]")
    numb_bet <- beta_sca_names  %>% 
        filter(substr(betas, 1, 4) == "beta") %>% 
        nrow()
    numb_bet <- numb_bet - 1

    params_mods <- c("beta", "alpha")

    if("int" %in% coef_path_file$result[ii]) {
      beta_int_add <- glue("beta_int{numb_bet}")
      #if(loop_sps == "YBSA"){beta_sca_names$betas[1] <- "beta"}
      beta_sca_names <- beta_sca_names %>% 
                            add_row(betas = beta_int_add)
      params_mods <- c("beta", "beta_int", "alpha")
    }
    
    # Get summary with median and credible intervals
    coef_summary <- MCMCsummary(samples_jags,
                            params = params_mods, #, "beta0", "alpha0"),  # specify parameters
                            probs = c(0.025, 0.5, 0.975),  # 2.5%, median, 97.5%
                            round = 3)  %>% 
                    cbind(beta_sca_names)

    if(unique(rownames(coef_summary) == coef_summary$betas) != TRUE){stop("scales and betas don't match!")}

    # Extract posterior samples as a matrix
    # For jagsUI object
    print("here")

    # Handle different MCMC object types
    if(class(samples_jags)[1] == "mcmc.list"){
        samps <- samples_jags  # Already an mcmc.list
    } else if("samples" %in% names(samples_jags)) {
        samps <- samples_jags$samples  # jagsUI object
    } else if("mcmc" %in% names(samples_jags)) {
        samps <- samples_jags$mcmc  # Different structure
    } else {
        # Try to convert directly
        samps <- coda::as.mcmc.list(samples_jags)
    }
    samps_mat <- do.call(rbind, samps)  # now all chains stacked
    dim(samps_mat)  # should be n_draws x n_parameters
    colnames(samps_mat) 

    # align parameter names safely
    common_pars <- intersect(rownames(coef_summary), colnames(samps_mat))
    if(length(common_pars)==0) stop("No common parameter names found between summary and samples.")

    # compute probabilities
    pct_gt_0  <- apply(samps_mat[, common_pars, drop=FALSE], 2, function(x) mean(x > 0) * 100)
    pct_lt_0  <- apply(samps_mat[, common_pars, drop=FALSE], 2, function(x) mean(x < 0) * 100)
    # Add columns to coef_summary (preserve original rows order)
    
    coef_summary$Pct_gt_0 <- round(pct_gt_0[rownames(coef_summary)], 1)
    coef_summary$Pct_lt_0 <- round(pct_lt_0[rownames(coef_summary)], 1)

    coef_summary <- coef_summary %>% 
                      mutate(coef = rownames(.)) %>% 
                      as_tibble() %>% 
                      relocate(coef) %>%
                      mutate(coef = gsub("\\[|\\]", "", coef))

    rm(samples_jags)

    if((nrow(beta_sca_names)) != nrow(coef_summary)) {
          stop(glue("error in {coef_path_file$result[ii]}"))            
        } else { coef_summary2 <- coef_summary %>% 
                                      mutate(sps = substr(coef_path_file$result[ii], 1, 4),
                                             mod_res = coef_path_file$select[ii])
                }
      if(ii == 1) {coef_summary3 <- coef_summary2} else {coef_summary3 <- rbind(coef_summary3, coef_summary2)}
}

# write_rds(coef_summary3, file = "data/out/coefs_step2_1_fig2.rds")
# coef_summary3<- read_rds(file = "data/out/coefs_step2_1_fig2.rds")

#? Figure 2 --------------------------------------------------------------
dat <- coef_summary3 %>% 
            rename(sca_select = sca_sel,
                   cov = betas) %>% 
            arrange(sca_select, cov, sps)  %>% 
            as_tibble(.name_repair = ) %>% 
            mutate(sps = toupper(sps)) %>% 
            rename(Covariate = cov)

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
                    low = `2.5%`,  
                    median = `50%`, 
                    up = `97.5%`) %>% 
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
                  
dat1 %>% filter(is.na(sca_col)) %>% select(sps, coef, overlap0, sca1, sca2, sca3, sca_name, sca_col, select_sca, cov_sps, Covariate)

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



(sca_plot_selec_sca2 <- ggplot() +
  geom_point(data = dat_sca %>% filter(sca_select == 3), 
             aes(x = Covariate, y = sps_name, fill = sca_col), 
             size = 31, shape = 21, stroke = 0.9) +
  geom_point(data = dat_sca %>% filter(sca_select == 2), 
             aes(x = Covariate, y = sps_name, fill = sca_col), 
             size = 24, shape = 21, stroke = 0.9) +
  geom_point(data = dat_sca %>% filter(sca_select == 1), 
             aes(x = Covariate, y = sps_name, fill = sca_col), 
             size = 18.5, shape = 21, stroke = 0.9) +
  geom_text(data = dat_sca %>% filter(sca_select == 3), 
            aes(x = Covariate, y = sps_name, label = glue("{round(select_sca * 100)}")), #\nCI:{round(low, 1)} : {round(up, 1)}")), #
            size = 7, color = "black") +
  geom_text(data = dat_sca %>% filter(sca_select == 2), 
            aes(x = Covariate, y = sps_name, label = glue("{round(select_sca * 100)}")), #\nCI:{round(low, 1)} : {round(up, 1)}")), 
            size = 7, color = "black") +
  geom_text(data = dat_sca %>% filter(sca_select == 1), 
            aes(x = Covariate, y = sps_name, label = glue("{round(select_sca * 100)}")), #\nCI:{round(low, 1)} : {round(up, 1)}")), 
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
  scale_y_discrete(limits = rev(levels(factor(dat_sca$sps_name)))) +  # Reverse y-axis order
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

(sca_plot_selec_sca2 <- ggplot() +
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

ggsave("figures/sca_plot_select_sca_noleg2long.png", plot = sca_plot_selec_sca2, device = "png", width = 24, height = 14, dpi = 800)

ggsave("figures/sca_plot_select_sca_noleg2long.svg", plot = sca_plot_selec_sca2, device = "svg", width = 24, height = 14)


ggsave("figures/sca_plot_select_sca_noleg.png", plot = sca_plot_selec_sca, device = "png", width = 13, height = 17, dpi = 800)

ggsave("figures/sca_plot_select_sca_noleg.svg", plot = sca_plot_selec_sca, 
       device = "svg", width = 13, height = 18)

#? Figure 3 --------------------------------------------------------------
coef_path_file2 <- read_csv(COEF_TABLE_PATH) %>%
        filter(run == "yes") %>% 
        filter(step == 4) %>% 
        mutate(AOU_Code = substr(result, 1, 4)) %>% 
        filter(AOU_Code %!in% c("BCCH"))

for(ii in 1:nrow(coef_path_file2)) {
#     ii <- 1
    (loop_sps <- substr(coef_path_file2$result[ii], 1, 4))

    loop_run <- substr(coef_path_file2$result[ii], nchar(coef_path_file2$result[ii]) - 3, nchar(coef_path_file2$result[ii]))

    quants <- "25_75" #ifelse((as.numeric(substr(loop_run, 4, 4)) %% 2 == 0) == TRUE, "25_75", "3_7")

    samples_jags2 <- read_rds(glue("data/model_res/{coef_path_file2$result[ii]}.rds"))

    params_mods2 <- c("beta", "alpha")

    if("int" %in% coef_path_file2$result[ii]) {
      beta_int_add2 <- glue("beta_int{numb_bet}")
      #if(loop_sps == "YBSA"){beta_sca_names$betas[1] <- "beta"}
      params_mods <- c("beta", "beta_int", "alpha")
    }
    
    # Get summary with median and credible intervals
    coef_summary2 <- MCMCsummary(samples_jags2,
                            params = params_mods2, #, "beta0", "alpha0"),  # specify parameters
                            #probs = c(0.025, 0.5, 0.975),  # 2.5%, median, 97.5%
                            probs = c(0.1, 0.5, 0.9),  # median and 0.8 percentile
                            round = 3)  
    coef_summary2$betas <- rownames(coef_summary2)
    coef_summary2 <- coef_summary2 %>% relocate(betas)

    beta_sca_names2 <- read_rds(glue("data/model_res/{coef_path_file2$select[ii]}.rds"))     
    beta_sca_names2[1:5,1] <- glue("beta[{seq(1,5,1)}]")
    beta_sca_names2$cov_name <- c("den", "con", "lat", "shr", "bas")
    numb_bet <- beta_sca_names2  %>% 
        #filter(overlap0 == "no") %>% 
        filter(substr(betas, 1, 4) == "beta") %>% 
        nrow()

    beta_sca_names2 <- beta_sca_names2  %>% 
        #filter(overlap0 == "no") %>% 
        filter(substr(betas, 1, 4) == "beta")
    
    beta_sca_names2$betas <- glue("beta[{seq(1,nrow(beta_sca_names2),1)}]")

    coef_summary2[which(coef_summary2$betas == glue("beta[{numb_bet + 1}]")),1] <- "park_size"
    coef_summary2 <- as_tibble(coef_summary2)
       
    coef_summary3 <- left_join(coef_summary2, beta_sca_names2, by = "betas") 
    coef_summary3$betaind <- coef_summary3$betas
    
    # Refer to column by name instead of number
    coef_summary3[which(coef_summary3$betaind == "park_size"), "betaind"] <- glue("beta[{numb_bet + 1}]")

    # Extract posterior samples as a matrix
    # For jagsUI object
   # Handle different MCMC object types
    if(class(samples_jags2)[1] == "mcmc.list"){
        samps <- samples_jags2  # Already an mcmc.list
    } else if("samples" %in% names(samples_jags2)) {
        samps <- samples_jags2$samples  # jagsUI object
    } else if("mcmc" %in% names(samples_jags2)) {
        samps <- samples_jags2$mcmc  # Different structure
    } else {
        # Try to convert directly
        samps <- coda::as.mcmc.list(samples_jags2)
    }

    samps_mat <- do.call(rbind, samps)  # now all chains stacked
    dim(samps_mat)  # should be n_draws x n_parameters
    colnames(samps_mat) 

    # align parameter names safely
    (common_pars <- intersect(coef_summary3$betaind, colnames(samps_mat)))
    if(length(common_pars)==0) stop("No common parameter names found between summary and samples.")

    # compute probabilities - pencentage of the posterior bellow and above zero
    pct_gt_0  <- apply(samps_mat[, common_pars, drop=FALSE], 2, function(x) mean(x > 0) * 100)
    pct_lt_0  <- apply(samps_mat[, common_pars, drop=FALSE], 2, function(x) mean(x < 0) * 100)
    # Add columns to coef_summary (preserve original rows order)
    
    coef_summary3$Pct_gt_0 <- round(pct_gt_0[coef_summary3$betaind], 1)
    coef_summary3$Pct_lt_0 <- round(pct_lt_0[coef_summary3$betaind], 1)
    coef_summary3$sps <- loop_sps

    rm(samples_jags2)
    if(ii == 1) {coef_summary4 <- coef_summary3} else {coef_summary4 <- rbind(coef_summary4, coef_summary3)}
    print(ii)
    write_rds(coef_summary4, file = "data/coef_summary4.rds")
}

# write_rds(coef_summary4, file = "data/out/coefs_step2_1_fig3.rds")
# coef_summary4 <- read_rds(file = "data/out/coefs_step2_1_fig3.rds")

table(coef_summary4$sps)
table(coef_summary4$cov_name)
table(coef_summary4$sca_sel)

dat_sca <- coef_summary4 %>% 
                  #select(Covariate, sps, sca1, sca2, sca3, overlap0) %>% 
                  pivot_longer(cols =c("sca1", "sca2", "sca3"),
                               names_to = "scale", 
                               values_to = "selec_freq",
                               names_prefix = "sca")  %>%
                  filter(is.na(sca_sel) | sca_sel == as.numeric(scale))

dat_sca2 <- dat_sca

dat_sca2[which(dat_sca2$betas == "park_size"),"scale"] <- NA
dat_sca2[which(substr(dat_sca2$betas, 1, 5) == "alpha"),"scale"] <- NA

nrow(dat_sca2)
dat_sca2 <- dat_sca2  %>% distinct() %>% mutate(scale = as.numeric(scale))
nrow(dat_sca2)

cov_name <- cbind(c("den",
                    "con",
                    "lat",
                    "shr",
                    "bas"),
                  c("Tree Density",
                    "Conifer Density",
                    "Late Successional Tree Density",
                    "Shrub Basal Area",
                    "Tree Basal Area")) %>% 
            as_tibble(.name_repair) %>% 
            rename(cov_name = V1,
                   cov_name2 = V2)  %>% 
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
                left_join(., cov_name, by = "cov_name") %>% 
                rename(Covariate = cov_name2,
                       median = '50%') %>% 
                arrange(sps, as.numeric(Covariate))  # Arrange by factor level order

table(dat_sca3$scale)
table(dat_sca3$sca_name)

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

# dat_sca3 <- dat_sca3 %>% mutate(Covariate = betas) %>% filter(substr(betas, 1, 4) == "beta")
dat_sca3 <- dat_sca3  %>% 
      left_join(., sps_order, by = "sps")  %>% 
      filter(!is.na(sca_sel),
             betas != "park_size")

dat_sca3$scale %>% table()

(circles_coefs <- ggplot() +
  geom_point(data = dat_sca3, #%>% filter(!is.na(Covariate)), 
             aes(x = Covariate, y = sps), 
             size = 1, fill = "white", alpha = 0) +
  #only non overlating CIs
  geom_point(data = dat_sca3 %>% filter(scale == 3, overlap0med == "no"), 
             aes(x = Covariate, y = sps, fill = median), 
             size = 31, shape = 21, stroke = 0.9) +
  geom_point(data = dat_sca3 %>% filter(scale == 2, overlap0med == "no"), 
             aes(x = Covariate, y = sps, fill = median), 
             size = 24, shape = 21, stroke = 0.9) +
  geom_point(data = dat_sca3 %>% filter(scale == 1, overlap0med == "no"), 
             aes(x = Covariate, y = sps, fill = median),
             size = 18.5, shape = 21, stroke = 0.9) +
#   # all values
#   geom_point(data = dat_sca3 %>% filter(scale == 3), 
#              aes(x = Covariate, y = sps, fill = median), 
#              size = 31, shape = 21, stroke = 0.9) +
#   geom_point(data = dat_sca3 %>% filter(scale == 2), 
#              aes(x = Covariate, y = sps, fill = median), 
#              size = 24, shape = 21, stroke = 0.9) +
#   geom_point(data = dat_sca3 %>% filter(scale == 1), 
#              aes(x = Covariate, y = sps, fill = median),
#              size = 18.5, shape = 21, stroke = 0.9) +
# # Add text labels for median values
#  # overlaps
#   geom_text(data = dat_sca3 %>% filter(scale == 3, overlap0med == "yes"), 
#            aes(x = Covariate, y = sps, label = glue("{round(median, 2)}")), #\nCI:{round(low, 1)} : {round(up, 1)}")), #
#             size = 7, color = "#626060") +
#   geom_text(data = dat_sca3 %>% filter(scale == 2, overlap0med == "yes"), 
#             aes(x = Covariate, y = sps, label = glue("{round(median, 2)}")), #\nCI:{round(low, 1)} : {round(up, 1)}")), 
#             size = 7, color = "#626060") +
#   geom_text(data = dat_sca3 %>% filter(scale == 1, overlap0med == "yes"), 
#             aes(x = Covariate, y = sps, label = glue("{round(median, 2)}")), #\nCI:{round(low, 1)} : {round(up, 1)}")), 
#             size = 6, color = "#626060") +
  # no overlap
  geom_text(data = dat_sca3 %>% filter(scale == 3, overlap0med == "no"), 
           aes(x = Covariate, y = sps, label = glue("{round(median, 2)}")), #\nCI:{round(low, 1)} : {round(up, 1)}")), #
            size = 7, color = "black") +
  geom_text(data = dat_sca3 %>% filter(scale == 2, overlap0med == "no"), 
            aes(x = Covariate, y = sps, label = glue("{round(median, 2)}")), #\nCI:{round(low, 1)} : {round(up, 1)}")), 
            size = 7, color = "black") +
  geom_text(data = dat_sca3 %>% filter(scale == 1, overlap0med == "no"), 
            aes(x = Covariate, y = sps, label = glue("{round(median, 2)}")), #\nCI:{round(low, 1)} : {round(up, 1)}")), 
            size = 6, color = "black") +
  scale_color_identity() +  # This tells ggplot to use the color names as actual colors for stroke color
  scale_fill_gradient2(low = "#ca0557",           # Negative values = blue
                       #mid = "white",           # Zero = white  
                       high = "#0794b7",        # Positive values = pink
                       midpoint = 0,              # Center point at zero
                       name = "Covariate\nEffect size\n") +
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
  scale_y_discrete(limits = rev(levels(factor(dat_sca3$sps)))) +  # Reverse y-axis order
  scale_x_discrete(labels = function(x) {
      # Manually add line breaks - x is the vector of axis labels
      case_when(
        x == "Tree Basal Area" ~ "Tree Basal\nArea",
        x == "Late Successional Tree Density" ~ "Late Success.\n Tree Basal Area",
        x == "Conifer Density" ~ "Conifer \nBasal Area  ",
        x == "Tree Density" ~ "Tree\nDensity",
        x == "Shrub Basal Area" ~ "Shrub\nCover",
        TRUE ~ x  # Keep others as is
      )}) +
  labs(x = NULL, #"\nForest Covariate", 
       y = "Species\n", fill = "Covariate\nEffect size\n") +
  guides(
    fill = guide_colorbar(
      barwidth = unit(8, "cm"),   # wider (horizontal) color strip
      barheight = unit(1, "cm")    # taller (vertical) color strip
    )
  )
)

ggsave("figures/TESTcoef_circles_step1_1.png", plot = circles_coefs, 
       device = "png", width = 13, height = 18)

ggsave("figures/coef_circles.svg", plot = circles_coefs, 
       device = "svg", width = 13, height = 18)


dat_sca3 %>% filter(overlap0med == "no")  %>% pull(sca_sel) %>% table()

dat_sca3 <- dat_sca3 %>% filter(sps != "HETH")

write_rds(dat_sca3, file = "data/dat_sca3.rds")


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

ggsave("figures/circles_coefs.png", plot = circles_coefs, device = "png", width = 24, height = 14, dpi = 800)

ggsave("figures/circles_coefsnoheth.svg", plot = circles_coefs, device = "svg", width = 24, height = 14)

