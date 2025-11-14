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
        #filter(run == "yes") %>% 
        filter(step == 2) %>% 
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
    samps <- coda::as.mcmc.list(samples_jags)  # this is a mcmc.list - not $samples

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

write_rds(coef_summary3, file = "data/out/coefs_step1.rds")

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

ggsave("figures/sca_plot_select_sca_noleg.png", plot = sca_plot_selec_sca, device = "png", width = 13, height = 17, dpi = 800)

ggsave("figures/sca_plot_select_sca_noleg.svg", plot = sca_plot_selec_sca, 
       device = "svg", width = 13, height = 18)

#? Figure 3 --------------------------------------------------------------
coef_path_file2 <- read_csv(COEF_TABLE_PATH) %>%
        filter(run == "yes") %>% 
        filter(step == 3) %>% 
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
        filter(overlap0 == "no") %>% 
        filter(substr(betas, 1, 4) == "beta") %>% 
        nrow()

    beta_sca_names2 <- beta_sca_names2  %>% 
        filter(overlap0 == "no") %>% 
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
    samps <- samples_jags2$samples  # this is a mcmc.list - not $samples

    samps_mat <- do.call(rbind, samps)  # now all chains stacked
    dim(samps_mat)  # should be n_draws x n_parameters
    colnames(samps_mat) 

    # align parameter names safely
    (common_pars <- intersect(coef_summary3$betaind, colnames(samps_mat)))
    if(length(common_pars)==0) stop("No common parameter names found between summary and samples.")

    # compute probabilities
    pct_gt_0  <- apply(samps_mat[, common_pars, drop=FALSE], 2, function(x) mean(x > 0) * 100)
    pct_lt_0  <- apply(samps_mat[, common_pars, drop=FALSE], 2, function(x) mean(x < 0) * 100)
    # Add columns to coef_summary (preserve original rows order)
    
    coef_summary3$Pct_gt_0 <- round(pct_gt_0[coef_summary3$betaind], 1)
    coef_summary3$Pct_lt_0 <- round(pct_lt_0[coef_summary3$betaind], 1)
    coef_summary3$sps <- loop_sps

    rm(samples_jags2)
    if(ii == 1) {coef_summary4 <- coef_summary3} else {coef_summary4 <- rbind(coef_summary4, coef_summary3)}
    print(ii)
}

# write_rds(coef_summary4, file = "data/out/coefs_step2.rds")

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
dat_sca2 <- dat_sca2  %>% distinct()  %>% mutate(scale = as.numeric(scale))
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
  if(dat_sca3$`2.5%`[ii] <= 0 && dat_sca3$`97.5%`[ii] >= 0) {
      dat_sca3$overlap0med[ii] <- "yes"
    } else {
      dat_sca3$overlap0med[ii] <- "no"
    }
  }

(circles_coefs <- ggplot() +
  geom_point(data = dat_sca3 %>% filter(!is.na(Covariate)), 
             aes(x = Covariate, y = sps), 
             size = 1, fill = "white", alpha = 0) +
  geom_point(data = dat_sca3 %>% filter(scale == 3), 
             aes(x = Covariate, y = sps, fill = median), 
             size = 31, shape = 21, stroke = 0.9) +
  geom_point(data = dat_sca3 %>% filter(scale == 2), 
             aes(x = Covariate, y = sps, fill = median), 
             size = 24, shape = 21, stroke = 0.9) +
  geom_point(data = dat_sca3 %>% filter(scale == 1), 
             aes(x = Covariate, y = sps, fill = median),
             size = 18.5, shape = 21, stroke = 0.9) +
# Add text labels for median values
 # overlaps
  geom_text(data = dat_sca3 %>% filter(scale == 3, overlap0med == "yes"), 
           aes(x = Covariate, y = sps, label = glue("{round(median, 2)}")), #\nCI:{round(low, 1)} : {round(up, 1)}")), #
            size = 7, color = "#626060") +
  geom_text(data = dat_sca3 %>% filter(scale == 2, overlap0med == "yes"), 
            aes(x = Covariate, y = sps, label = glue("{round(median, 2)}")), #\nCI:{round(low, 1)} : {round(up, 1)}")), 
            size = 7, color = "#626060") +
  geom_text(data = dat_sca3 %>% filter(scale == 1, overlap0med == "yes"), 
            aes(x = Covariate, y = sps, label = glue("{round(median, 2)}")), #\nCI:{round(low, 1)} : {round(up, 1)}")), 
            size = 6, color = "#626060") +
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

ggsave("figures/coef_circles.svg", plot = circles_coefs, 
       device = "svg", width = 13, height = 18)


#####

# Create pie chart data function for circles with median-based colors and directional percentages
create_pie_data_median_directional <- function(x_pos, y_pos, pct_gt_0, pct_lt_0, median_val, radius = 0.3, n_segments = 100) {
  # Determine which percentage to use based on the sign of the median
  if(median_val >= 0) {
    # For positive effects, use pct_gt_0
    pie_percentage <- pct_gt_0
  } else {
    # For negative effects, use pct_lt_0
    pie_percentage <- pct_lt_0
  }
  
  # Create angles for pie segments
  angle_colored <- (pie_percentage / 100) * 2 * pi  # Proportion for colored segment
  
  # Colored segment (colored by median effect size)
  if(pie_percentage > 0) {
    angles_colored <- seq(0, angle_colored, length.out = max(3, ceiling(n_segments * pie_percentage / 100)))
    x_colored_seg <- x_pos + radius * cos(angles_colored)
    y_colored_seg <- y_pos + radius * sin(angles_colored)
    
    colored_segment <- data.frame(
      x = c(x_pos, x_colored_seg, x_pos),
      y = c(y_pos, y_colored_seg, y_pos),
      segment = "colored",
      median = median_val
    )
  } else {
    colored_segment <- data.frame(x = numeric(0), y = numeric(0), segment = character(0), median = numeric(0))
  }
  
  # Remaining segment (white)
  if(pie_percentage < 100) {
    angles_remaining <- seq(angle_colored, 2 * pi, length.out = max(3, ceiling(n_segments * (100 - pie_percentage) / 100)))
    x_remaining_seg <- x_pos + radius * cos(angles_remaining)
    y_remaining_seg <- y_pos + radius * sin(angles_remaining)
    
    remaining_segment <- data.frame(
      x = c(x_pos, x_remaining_seg, x_pos),
      y = c(y_pos, y_remaining_seg, y_pos),
      segment = "remaining",
      median = NA
    )
  } else {
    remaining_segment <- data.frame(x = numeric(0), y = numeric(0), segment = character(0), median = numeric(0))
  }
  
  rbind(colored_segment, remaining_segment)
}

# Prepare data for pie charts with different positions for each scale
dat_pie_median_dir <- dat_sca3 %>% 
  filter(!is.na(Covariate)) %>%
  mutate(
    # Convert factors to numeric for positioning
    x_base = as.numeric(factor(Covariate, levels = levels(Covariate))),
    y_base = as.numeric(factor(sps, levels = rev(levels(factor(sps))))),
    # Offset x position based on scale
    x_offset = case_when(
      scale == 1 ~ -0.25,  # Local scale (left)
      scale == 2 ~ 0,      # Park scale (center) 
      scale == 3 ~ 0.25    # County scale (right)
    ),
    x_pos = x_base + x_offset,
    y_pos = y_base,
    # Set radius based on scale - convert size to radius
    radius = case_when(
      scale == 3 ~ 0.42,   # Largest (size 31)
      scale == 2 ~ 0.32,   # Medium (size 24)
      scale == 1 ~ 0.24    # Smallest (size 18.5)
    ),
    # Calculate the percentage to display based on effect direction
    display_pct = ifelse(median >= 0, Pct_gt_0, Pct_lt_0)
  )

# Create pie chart data for all points
pie_data_median_dir_list <- list()
for(i in 1:nrow(dat_pie_median_dir)) {
  row_data <- dat_pie_median_dir[i, ]
  pie_segments <- create_pie_data_median_directional(
    x_pos = row_data$x_pos,
    y_pos = row_data$y_pos, 
    pct_gt_0 = row_data$Pct_gt_0,
    pct_lt_0 = row_data$Pct_lt_0,
    median_val = row_data$median,
    radius = row_data$radius
  )
  # Add metadata
  if(nrow(pie_segments) > 0) {
    pie_segments$scale <- row_data$scale
    pie_segments$sps <- row_data$sps
    pie_segments$Covariate <- row_data$Covariate
    pie_segments$overlap0med <- row_data$overlap0med
    pie_segments$Pct_gt_0 <- row_data$Pct_gt_0
    pie_segments$Pct_lt_0 <- row_data$Pct_lt_0
    pie_segments$display_pct <- row_data$display_pct
    pie_segments$original_median <- row_data$median
    
    pie_data_median_dir_list[[i]] <- pie_segments
  }
}

# Combine all pie data
all_pie_data_median_dir <- do.call(rbind, pie_data_median_dir_list)

# Create the pie chart plot with directional percentages
(pie_coefs_median_dir <- ggplot() +
  # Colored segments based on median effect size
  geom_polygon(data = all_pie_data_median_dir %>% filter(segment == "colored"), 
               aes(x = x, y = y, group = interaction(sps, Covariate, scale), fill = median),
               color = "white", linewidth = 0.5) +
  # Remaining segments in white
  geom_polygon(data = all_pie_data_median_dir %>% filter(segment == "remaining"), 
               aes(x = x, y = y, group = interaction(sps, Covariate, scale)),
               fill = "white", color = "white", linewidth = 0.5) +
  # Add text labels for median values (keeping the same text labels as your original)
  # Overlapping CI (gray text)
  geom_text(data = dat_pie_median_dir %>% filter(scale == 3, overlap0med == "yes"), 
           aes(x = x_pos, y = y_pos, label = glue("{round(median, 2)}")), 
            size = 7, color = "#626060") +
  geom_text(data = dat_pie_median_dir %>% filter(scale == 2, overlap0med == "yes"), 
            aes(x = x_pos, y = y_pos, label = glue("{round(median, 2)}")), 
            size = 7, color = "#626060") +
  geom_text(data = dat_pie_median_dir %>% filter(scale == 1, overlap0med == "yes"), 
            aes(x = x_pos, y = y_pos, label = glue("{round(median, 2)}")), 
            size = 6, color = "#626060") +
  # Non-overlapping CI (black text)
  geom_text(data = dat_pie_median_dir %>% filter(scale == 3, overlap0med == "no"), 
           aes(x = x_pos, y = y_pos, label = glue("{round(median, 2)}")), 
            size = 7, color = "black") +
  geom_text(data = dat_pie_median_dir %>% filter(scale == 2, overlap0med == "no"), 
            aes(x = x_pos, y = y_pos, label = glue("{round(median, 2)}")), 
            size = 7, color = "black") +
  geom_text(data = dat_pie_median_dir %>% filter(scale == 1, overlap0med == "no"), 
            aes(x = x_pos, y = y_pos, label = glue("{round(median, 2)}")), 
            size = 6, color = "black") +
  # Color scale for effect sizes (fixed range from -2 to 2)
  scale_fill_gradient2(low = "#ca0557",           # Negative values = red
                       high = "#0794b7",          # Positive values = blue
                       midpoint = 0,              # Center point at zero
                       name = "Covariate\nEffect size\n") +   
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.margin = margin(t = 25, r = 0, b = 0, l = 0),
        axis.text.x = element_text(hjust = 0.5, size = 24),
        axis.text.y = element_text(hjust = 0, size = 26),    
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 30),
        legend.title = element_text(size = 22, face = "bold", hjust = 0.5),  
        legend.text = element_text(size = 22),
        panel.grid.major = element_line(color = "black", linetype = "solid", linewidth = 0.6),
        panel.grid.minor = element_line(color = "black", linetype = "solid", linewidth = 0.6)) + 
  scale_y_continuous(breaks = 1:length(unique(dat_pie_median_dir$sps)), 
                    labels = rev(levels(factor(dat_sca3$sps)))) +
  scale_x_continuous(breaks = 1:length(unique(dat_pie_median_dir$Covariate)), 
                    labels = function(x) {
                      cov_levels <- levels(unique(dat_pie_median_dir$Covariate))
                      case_when(
                        cov_levels == "Tree Basal Area" ~ "Tree Basal\nArea",
                        cov_levels == "Late Successional Tree Density" ~ "Late Success.\n Tree Basal Area",
                        cov_levels == "Conifer Density" ~ "Conifer \nBasal Area  ",
                        cov_levels == "Tree Density" ~ "Tree\nDensity",
                        cov_levels == "Shrub Basal Area" ~ "Shrub\nCover",
                        TRUE ~ cov_levels
                      )}) +
  labs(x = NULL, 
       y = "Species\n", 
       fill = "Covariate\nEffect size\n") +
  guides(
    fill = guide_colorbar(
      barwidth = unit(8, "cm"),   
      barheight = unit(1, "cm")    
    )
  ) +
  coord_fixed(ratio = 1) # Maintain circular pie shapes
)

# Save the pie chart plot



       # Create the pie chart plot with directional percentages
(pie_coefs_median_dir <- ggplot() +
  # Colored segments based on median effect size with black borders
  geom_polygon(data = all_pie_data_median_dir %>% filter(segment == "colored"), 
               aes(x = x, y = y, group = interaction(sps, Covariate, scale), fill = median),
               color = "black", linewidth = 0.8) +
  # Remaining segments in white with black borders
  geom_polygon(data = all_pie_data_median_dir %>% filter(segment == "remaining"), 
               aes(x = x, y = y, group = interaction(sps, Covariate, scale)),
               fill = "white", color = "black", linewidth = 0.8) +
  # Add text labels for median values (keeping the same text labels as your original)
  # Overlapping CI (gray text)
  geom_text(data = dat_pie_median_dir %>% filter(scale == 3, overlap0med == "yes"), 
           aes(x = x_pos, y = y_pos, label = glue("{round(median, 2)}")), 
            size = 7, color = "#626060") +
  geom_text(data = dat_pie_median_dir %>% filter(scale == 2, overlap0med == "yes"), 
            aes(x = x_pos, y = y_pos, label = glue("{round(median, 2)}")), 
            size = 7, color = "#626060") +
  geom_text(data = dat_pie_median_dir %>% filter(scale == 1, overlap0med == "yes"), 
            aes(x = x_pos, y = y_pos, label = glue("{round(median, 2)}")), 
            size = 6, color = "#626060") +
  # Non-overlapping CI (black text)
  geom_text(data = dat_pie_median_dir %>% filter(scale == 3, overlap0med == "no"), 
           aes(x = x_pos, y = y_pos, label = glue("{round(median, 2)}")), 
            size = 7, color = "black") +
  geom_text(data = dat_pie_median_dir %>% filter(scale == 2, overlap0med == "no"), 
            aes(x = x_pos, y = y_pos, label = glue("{round(median, 2)}")), 
            size = 7, color = "black") +
  geom_text(data = dat_pie_median_dir %>% filter(scale == 1, overlap0med == "no"), 
            aes(x = x_pos, y = y_pos, label = glue("{round(median, 2)}")), 
            size = 6, color = "black") +
  # Color scale for effect sizes (fixed range from -2 to 2)
  scale_fill_gradient2(low = "#ca0557",           # Negative values = red
                       high = "#0794b7",          # Positive values = blue
                       midpoint = 0,              # Center point at zero
                       limits = c(-2, 2),         # Fixed scale from -2 to 2
                       name = "Covariate\nEffect size\n",
                       oob = scales::squish) +    # Squish values outside limits
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.margin = margin(t = 25, r = 0, b = 0, l = 0),
        axis.text.x = element_text(hjust = 0.5, size = 24),
        axis.text.y = element_text(hjust = 0, size = 26),    
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 30),
        legend.title = element_text(size = 22, face = "bold", hjust = 0.5),  
        legend.text = element_text(size = 22),
        panel.grid.major = element_line(color = "gray85", linetype = "solid", linewidth = 0.6),
        panel.grid.minor = element_line(color = "gray85", linetype = "solid", linewidth = 0.6)) + 
  scale_y_continuous(breaks = 1:length(unique(dat_pie_median_dir$sps)), 
                    labels = rev(levels(factor(dat_sca3$sps)))) +
  scale_x_continuous(breaks = 1:length(unique(dat_pie_median_dir$Covariate)), 
                    labels = function(x) {
                      # Get the actual covariate names in the correct order
                      cov_levels <- levels(factor(dat_pie_median_dir$Covariate))
                      # Match the position x to the actual covariate name
                      covariate_names <- cov_levels[x]
                      case_when(
                        covariate_names == "Tree Basal Area" ~ "Tree Basal\nArea",
                        covariate_names == "Late Successional Tree Density" ~ "Late Success.\n Tree Basal Area",
                        covariate_names == "Conifer Density" ~ "Conifer \nBasal Area  ",
                        covariate_names == "Tree Density" ~ "Tree\nDensity",
                        covariate_names == "Shrub Basal Area" ~ "Shrub\nCover",
                        TRUE ~ covariate_names
                      )}) +
  labs(x = NULL, 
       y = "Species\n", 
       fill = "Covariate\nEffect size\n") +
  guides(
    fill = guide_colorbar(
      barwidth = unit(8, "cm"),   
      barheight = unit(1, "cm")    
    )
  ) +
  coord_fixed(ratio = 1) # Maintain circular pie shapes
)

ggsave("figures/pie_coefs_directional.svg", plot = pie_coefs_median_dir, 
       device = "svg", width = 13, height = 18)
