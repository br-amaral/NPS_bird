#? *********************************************************************************
#? -------------------------------  coef_extract.r  --------------------------------
#? *********************************************************************************
#
##! TODO: get beta interaction coefs
#! Code to get coeficient estimates of all model results and create figures 2 and 3
#!       of the manuscript, that repreent the scale selection and effect sizes
#
#! Input ----------------------------------------------
#           - data/mod_key2.csv : table ith the path to all model results
#           - :
#
#! Output ----------------------------------------------
#           - data/out/coefs_sps_sca.rds : table with all the beta coefficient estimates with their scales
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
library(viridis)
library(svglite)
library(ggh4x)
#library(ggforce)

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
COEF_TABLE_PATH <- "code/fit_model/mod_key.csv"
if(substr(getwd(), 1, 3) == "/Us") {direc <- "local"} else {direc <- "hpc"}
# if(direc == "local"){COEF_TABLE_PATH <- glue("/Users/bamaral/Library/CloudStorage/OneDrive-MichiganStateUniversity/GitHubOne/NPS_bird_copy/{COEF_TABLE_PATH}")}
# if(direc == "local"){COEF_TABLE_PATH <- glue("/Users/bamaral/Documents/GitHub/NPS_bird_copy/{COEF_TABLE_PATH}")}

#! Figure formating ------------------------------------
axis_labels_size <- 22
title_size <- 36
corr_labels_size <- 7
legend_label_size <- 24
legend_title_size <- 25

## read files
coef_path_file <- read_csv(COEF_TABLE_PATH) %>%
        #filter(run == "yes") %>% 
        filter(step == 3) %>% 
        mutate(AOU_Code = substr(result, 1, 4)) #%>% 
        # filter(AOU_Code %!in% c("DOWO", "HAWO", "VEER", "SCTA", "REVI"))

coef_path_file <- coef_path_file %>% filter(AOU_Code != "BCCH")

for(ii in 1:nrow(coef_path_file)) {

    (loop_sps <- substr(coef_path_file$result[ii], 1, 4))

    loop_run <- substr(coef_path_file$result[ii], nchar(coef_path_file$result[ii]) - 11, nchar(coef_path_file$result[ii]) - 8)

    #quants <- ifelse((as.numeric(substr(loop_run, 4, 4)) %% 2 == 0) == TRUE, "25_75", "3_7")
    print(loop_sps)
    samples_jags <- read_rds(glue("data/model_res/{coef_path_file$result[ii]}.rds"))
    beta_sca_names <- read_rds(glue("data/model_res/{coef_path_file$select[ii]}.rds")) %>% 
          #filter(overlap0 == "no") %>%
          add_row(betas = "park_size") %>%  # Add empty row for 3 alphas + 1 beta park size
          add_row(betas = "alpha1") %>%  
          add_row(betas = "alpha2") %>%  
          add_row(betas = "alpha3")

    numb_bet <- beta_sca_names  %>% 
        filter(substr(betas, 1, 4) == "beta") %>% 
        nrow()
    
    # beta_int_add <- glue("beta_int{numb_bet}")
    # #if(loop_sps == "YBSA"){beta_sca_names$betas[1] <- "beta"}

    # beta_sca_names <- beta_sca_names %>% 
    #                       add_row(betas = beta_int_add)

    # Get summary with median and credible intervals
    coef_summary <- MCMCsummary(samples_jags,
                            params = c("beta", "alpha"), #, "beta0", "alpha0", "beta_int"),  # specify parameters
                            probs = c(0.1, 0.5, 0.9),  # 2.5%, median, 97.5%
                            round = 3) 

    # Extract posterior samples as a matrix
    # For jagsUI object
    samps <- samples_jags$samples  # this is a mcmc.list

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
    
    coef_summary$Pct_gt_0        <- round(pct_gt_0[rownames(coef_summary)], 1)
    coef_summary$Pct_lt_0        <- round(pct_lt_0[rownames(coef_summary)], 1)

    coef_summary <- coef_summary %>% 
                      mutate(coef = rownames(.)) %>% 
                      as_tibble() %>% 
                      relocate(coef) %>%
                      mutate(coef = gsub("\\[|\\]", "", coef))

      rm(samples_jags)
      if((nrow(beta_sca_names)) != nrow(coef_summary)) {
            stop(glue("error in {coef_path_file$result[ii]}"))
            
            } else { coef_summary2 <- cbind(coef_summary, beta_sca_names) %>% 
                                          mutate(sps = substr(coef_path_file$result[ii], 1, 4),
                                                 mod_res = coef_path_file$select[ii])
            }
      if(ii == 1) {coef_summary3 <- coef_summary2} else {coef_summary3 <- rbind(coef_summary3, coef_summary2)}
      print(ii)
}

   write_rds(coef_summary3, file = "data/out/coef_summary4_sep2.rds")
#  coef_summary3 <- read_rds(file = "data/out/coef_summary4_sep.rds")

coef_summary3 <- as_tibble(coef_summary3) %>% 
                      filter(betas != "beta6") %>% 
                      mutate(overlap0 = 0 >= pmin(`10%`, `90%`) & 0 <= pmax(`10%`, `90%`))

phylo_order2 <- read_rds(file = "data/src/sps_phylo_order.rds")  %>% 
                rename(sps = Aou_code) %>% 
                mutate(sps_name = factor(sps_name, levels = sps_name))  # Use current order as levels

coef_summary3 <- coef_summary3 %>% 
              left_join(., phylo_order2, by = "sps")

#! Figure: park size -------------------------------------------
(park_sizeP <- 
  coef_summary3 %>% 
        filter(sps != "BCCH") %>% 
        filter(betas == "park_size") %>% 
        filter(Rhat < 1.1) %>% 
  mutate(effect_dir = case_when(
    !overlap0 & `50%` <= 0 ~ "Negative",
    !overlap0 & `50%` >= 0 ~ "Positive", 
    overlap0               ~ "Overlap",
    TRUE                   ~ "Overlap")) %>%
  mutate(effect_dir = fct_relevel(effect_dir, "Overlap", "Negative", "Positive")) %>% 
  
        ggplot() +
          geom_vline(xintercept = 0, linetype = "dashed", color = "#5f5c5c", linewidth = 0.8) +
          geom_segment(aes(x = `10%`, xend = `90%`, y = sps_name, yend = sps_name, col = effect_dir), 
                      linewidth = 1.2) +
          geom_point(aes(x = `50%`, y = sps_name, col = effect_dir), 
                    size = 4) +
  scale_color_manual(values = c(
    "Overlap"  = "#999999",    # Grey
    "Negative" = "#E69F00",    # Orange 
    "Positive" = "#009E73"     # Teal/Green
    )) +
          theme_minimal() +
          theme(legend.position = "none",
                axis.text.x = element_text(size = 18, hjust = 0.5),
                axis.text.y = element_text(size = 16, hjust = 1),
                axis.title.x = element_text(size = 18, hjust = 0.52, vjust = -0.2),
                axis.title.y = element_blank(),
                panel.grid.major.x = element_line(color = "grey85", linewidth = 0.6),
                panel.grid.minor.x = element_blank(),
                panel.grid.major.y = element_line(color = "grey85", linewidth = 0.2),
                panel.grid.minor.y = element_blank(),              
                plot.margin = margin(10, 10, 10, 20)) +
          labs(x = "Park size effect on occurrence") +
          scale_x_continuous(breaks = c(-2, -1, 0, 1, 2, 3)) + 
          scale_y_discrete(limits = rev))

  ggsave("figures/park_size2.svg", plot = park_sizeP, device = "svg", width = 11, height = 8, dpi = 1200)
  ggsave("figures/park_size2.pdf", plot = park_sizeP, device = "pdf", width = 11, height = 8, dpi = 1200)
#  ggsave("figures/park_size.png", plot = park_sizeP, device = "png", width = 11, height = 9, dpi = 1200)

#! Figure 2: scale selection circles -----------------------------------------
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
                  
dat1 %>% filter(is.na(sca_col)) %>% select(sps, coef, overlap0, sca1, sca2, sca3, sca_name, sca_col, select_sca, cov_sps, Covariate)

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
  #scale_fill_identity() +
  theme_minimal() +
  theme(legend.position = "right",  # Changed from "bottom" to "right"
        legend.margin = margin(t = 0, r = 0, b = 0, l = 25),  # Adjusted margin (left margin for spacing)
        axis.text.x = element_text(hjust = 1, size = 22, angle = 35, vjust = 1),  
        axis.text.y = element_text(hjust = 1, size = 26),    
        axis.title.x = element_text(size = 30),  
        axis.title.y = element_text(size = 25),  
        legend.title = element_text(size = legend_title_size, face = "bold"),
        legend.text  = element_text(size = legend_label_size),
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

ggsave("figures/sca_plot_select_sca_noleg2long_phylo2.png", plot = sca_plot_selec_sca2, device = "png", width = 24, height = 14, dpi = 800)
ggsave("figures/sca_plot_select_sca_noleg2long_phylo2.svg", plot = sca_plot_selec_sca2, device = "svg", width = 24, height = 14)

dat_sca3 <- dat_sca %>%  filter(Rhat < 1.1)

(circles_coefs <- ggplot() +
  geom_point(data = dat_sca3, #%>% filter(!is.na(Covariate)), 
             aes(x = sps_name, y = Covariate),  # Switched x and y to match sca_plot_selec_sca2
             size = 1, fill = "white", alpha = 0) +
  #only non overlating CIs
  geom_point(data = dat_sca3 %>% filter(sca_select == 3, overlap0 == FALSE), 
             aes(x = sps_name, y = Covariate, fill = median),  # Switched x and y
             size = 31, shape = 21, stroke = 0.8, color = "#4A4A4A") +  # Match stroke color
  geom_point(data = dat_sca3 %>% filter(sca_select == 2, overlap0 == FALSE), 
             aes(x = sps_name, y = Covariate, fill = median),  # Switched x and y
             size = 24, shape = 21, stroke = 0.8, color = "#4A4A4A") +  # Match stroke color
  geom_point(data = dat_sca3 %>% filter(sca_select == 1, overlap0 == FALSE), 
             aes(x = sps_name, y = Covariate, fill = median),  # Switched x and y
             size = 18.5, shape = 21, stroke = 0.8, color = "#4A4A4A") +  # Match stroke color
  # no overlap
  geom_text(data = dat_sca3 %>% filter(sca_select == 3, overlap0 == FALSE), 
           aes(x = sps_name, y = Covariate, label = glue("{round(median, 2)}")),  # Switched x and y
            size = 7, color = "black") +
  geom_text(data = dat_sca3 %>% filter(sca_select == 2, overlap0 == FALSE), 
            aes(x = sps_name, y = Covariate, label = glue("{round(median, 2)}")),  # Switched x and y
            size = 7, color = "black") +
  geom_text(data = dat_sca3 %>% filter(sca_select == 1, overlap0 == FALSE), 
            aes(x = sps_name, y = Covariate, label = glue("{round(median, 2)}")),  # Switched x and y
            size = 6, color = "black") +
  scale_color_identity() +  
  scale_fill_gradient2(    
    low  = "#E69F00",   # negative
    mid  = "#ffffff",   # near 0
    high =  "#009E73" ,   # positive
    midpoint = 0,       # numeric!
    name = "Covariate\nEffect size\n") +
  theme_minimal() +
  theme(legend.position = "right",  # Changed from "bottom" to "right"
        legend.margin = margin(t = 0, r = 0, b = 0, l = 25),  # Adjusted margin (left margin for spacing)
        axis.text.x = element_text(hjust = 1, size = 22, angle = 35, vjust = 1),  
        axis.text.y = element_text(hjust = 1, size = 26),    
        axis.title.x = element_text(size = 30),  
        axis.title.y = element_text(size = 25),  
        panel.grid.major = element_line(color = "#6A635E", linetype = "solid", linewidth = 0.6),
        panel.grid.minor = element_line(color = "#6A635E", linetype = "solid", linewidth = 0.6),
        legend.title.align = 0.5,
        legend.title       = element_text(size = legend_title_size, face = "bold"),
        legend.text        = element_text(size = legend_label_size)) +
  scale_x_discrete(limits = levels(factor(dat_sca3$sps_name))) +
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
    guides(fill = guide_colorbar(
           title.hjust = 0.5,              # Centers title exactly over bar
           title.vjust = 1,                 # Pulls title closer to bar
           barheight = unit(9, "cm"),       # Your larger size
           barwidth  = unit(1.5, "cm")
    )))


ggsave("figures/circles_coefs_long_phylo2.pdf", plot = circles_coefs, device = "pdf", width = 23, height = 14, dpi = 800)

ggsave("figures/circles_coefs_long_phylo2.svg", plot = circles_coefs, device = "svg", width = 23, height = 14)




