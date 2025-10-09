#? *********************************************************************************
#? -------------------------------  coef_extract.r  --------------------------------
#? *********************************************************************************
#
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
if(direc == "local"){COEF_TABLE_PATH <- glue("/Users/bamaral/Library/CloudStorage/OneDrive-MichiganStateUniversity/GitHubOne/NPS_bird_copy/{COEF_TABLE_PATH}")}
# if(direc == "local"){COEF_TABLE_PATH <- glue("/Users/bamaral/Documents/GitHub/NPS_bird_copy/{COEF_TABLE_PATH}")}

## read files
coef_path_file <- read_csv(COEF_TABLE_PATH) %>%
        filter(run == "yes") %>% 
        filter(step == 3) %>% 
        mutate(AOU_Code = substr(result, 1, 4)) #%>% 
        # filter(AOU_Code %!in% c("DOWO", "HAWO", "VEER", "SCTA", "REVI"))

for(ii in 1:nrow(coef_path_file)) {

    (loop_sps <- substr(coef_path_file$result[ii], 1, 4))

    loop_run <- substr(coef_path_file$result[ii], nchar(coef_path_file$result[ii]) - 7, nchar(coef_path_file$result[ii]) - 4)

    quants <- ifelse((as.numeric(substr(loop_run, 4, 4)) %% 2 == 0) == TRUE, "25_75", "3_7")

    samples_jags <- read_rds(glue("data/model_res/{coef_path_file$result[ii]}.rds"))
    beta_sca_names <- read_rds(glue("data/model_res/{coef_path_file$select[ii]}.rds")) %>% 
          filter(overlap0 == "no") %>%
          add_row(betas = "park_size") %>%  # Add empty row for 3 alphas + 1 be4ta park size
          add_row(betas = "alpha1") %>%  
          add_row(betas = "alpha2") %>%  
          add_row(betas = "alpha3")     

    #if(loop_sps == "YBSA"){beta_sca_names$betas[1] <- "beta"}

    # Get summary with median and credible intervals
    coef_summary <- MCMCsummary(samples_jags,
                            params = c("beta", "alpha"), #, "beta0", "alpha0"),  # specify parameters
                            probs = c(0.025, 0.5, 0.975),  # 2.5%, median, 97.5%
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
}

#    write_rds(coef_summary3, file = "data/out/coef_summary3_sep.rds")
   coef_summary3 <- read_rds(file = "data/out/coef_summary3_sep.rds")

table(coef_summary3$mod_res)

coef_summary3 <- as_tibble(coef_summary3) %>% 
                      filter(betas != "beta6") %>% 
                      mutate(overlap0 = ifelse(`2.5%` <= 0 & `97.5%` >= 0, "yes", "no"))

#! Figure: park size -------------------------------------------
(park_sizeP <- 
  coef_summary3 %>% 
        filter(sps != "BCCH") %>% 
        filter(betas == "park_size") %>% 
        arrange(desc(sps)) %>%
        mutate(sps = factor(sps, levels = unique(sps))) %>% 
        arrange(sps) %>% 
        ggplot() +
          geom_vline(xintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.8) +
          geom_segment(aes(x = `2.5%`, xend = `97.5%`, y = sps, yend = sps, col = overlap0), 
                      linewidth = 1.2) +
          geom_point(aes(x = `50%`, y = sps, col = overlap0), 
                    size = 4) +
          scale_color_manual(
              values = c(
                "no" = "black",
                "yes" = "darkgrey")) +
          theme_minimal() +
          theme(legend.position = "none",
                axis.text.x = element_text(hjust = 0.5, size = 18),
                axis.text.y = element_text(hjust = 0, size = 19),    
                axis.title.x = element_text(size = 20),
                axis.title.y = element_text(size = 20),
                legend.title = element_text(size = 15, face = "bold", hjust = 0.5),  
                legend.text = element_text(size = 13),
                panel.grid.major = element_line(color = "gray85", linetype = "solid", linewidth = 0.6),
                panel.grid.minor = element_line(color = "gray85", linetype = "solid", linewidth = 0.6)) +
          labs(
            x = "\nPark size",
            y = "Species\n"
          ) +
          scale_x_continuous(breaks = c(-2, -1, 0, 1, 2, 3)))

ggsave("figures/park_size.svg", plot = park_sizeP, device = "svg", width = 11, height = 11, dpi = 1200)

ggsave("figures/park_size.png", plot = park_sizeP, device = "png", width = 11, height = 9, dpi = 1200)


dat <- coef_summary3 %>% 
            #filter(overlap0 == "no") %>% 
            rename(sca = sca_sel,
                   cov = betas) %>% 
            arrange(sca, cov, sps)  %>% 
            mutate(sps_p = glue("{row_number()}_{sps}")) %>% 
            as_tibble() %>% 
            mutate(sps = toupper(sps),
                   sps_p = factor(sps_p, levels = sps_p)) %>% 
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
            as_tibble() %>% 
            rename(Covariate = V1,
                   cov_name = V2)  %>% 
            mutate(cov_name = factor(cov_name, 
                              levels = c("Tree Density",
                                         "Conifer Density",
                                         "Late Successional Tree Density",
                                         "Shrub Basal Area",
                                         "Tree Basal Area")))

sca_name <- cbind(unique(na.omit(dat$sca)),
                  c("Local Scale",
                    "Park Scale",
                    "County Scale")) %>% 
            as_tibble() %>% 
            rename(sca = V1,
                   sca_name = V2)  %>% 
            mutate(sca_name = factor(sca_name, 
                              levels = c("Local Scale",
                                         "Park Scale",
                                         "County Scale")),
                   sca = as.numeric(sca))

dat <- left_join(dat, cov_name, by = "Covariate") %>% 
       select(-Covariate)  %>% 
       rename(Covariate = cov_name) %>% 
       mutate(sca = as.numeric(sca)) %>% 
       left_join(.,sca_name, by = "sca") %>% 
       select(-sca) %>% 
       rename(sca = sca_name,
              low = `2.5%`,  
              median = `50%`, 
              up = `97.5%`)

write_rds(dat, "data/out/coef_dat_ext.rds")

dat1 <- dat %>% 
            mutate(sca_col = "darkolivegreen2") %>% 
            mutate(cov_sps = glue("{Covariate}_{sps}")) %>% 
            arrange(Covariate, sps) %>%  # Sort by Covariate first, then sps alphabetically
            mutate(cov_sps = factor(cov_sps, levels = sort(unique(cov_sps))))  %>% 
            filter(sps != "BCCH")

dat1$sca_col <- ifelse(dat1$sca == "Park Scale", "darkolivegreen3", dat1$sca_col)
dat1$sca_col <- ifelse(dat1$sca == "County Scale", "darkolivegreen4", dat1$sca_col)

#! Figure 3 ---------------------------------------------
sca_col <- c("#B0EDB9", "#64CC81", "#088A0F")
sca <- c("Local Scale","Park Scale","County Scale")
dat_col <- as_tibble(cbind(sca_col, sca))  %>% 
                  mutate(sca = factor(sca, 
                        levels = c("Local Scale","Park Scale","County Scale")))

dat1 <- dat1 %>%
  mutate(includes_zero = ifelse(overlap0 == "yes", "#a9a9a9", "black"))  %>% 
  filter(!is.na(Covariate))

##? Figure 3 with only species with data ----------------------------------------------------------
ggplot() +
  geom_rect(data = dat_col, aes(fill = sca_col),
            xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.3) +
            scale_fill_identity() +
        geom_linerange(data = dat1, aes(x = cov_sps, ymin = low, ymax = up, color = includes_zero), linewidth = 1) +
        geom_point(data = dat1, aes(x = cov_sps, y = mean, color = includes_zero), size = 2.5) +
        scale_color_identity() + 
        theme_bw() +
        theme(panel.grid = element_blank(),
              axis.text.x = element_text(size = 10, angle = 90, hjust = 1, vjust = 0.5), 
              axis.text.y = element_text(size = 10),
              axis.title.x = element_text(size = 14), 
              axis.title.y = element_text(size = 14),
              axis.line = element_line(color = "black", linewidth = 0.7), # Adjusts axis line thickness
              strip.text = element_text(size = 12, face = "bold"),
              panel.spacing = unit(0,"line"),
              panel.border = element_rect(color = "black", linewidth = 0.6), #  borders thickness
              strip.background=element_rect(color="black", fill="white", linewidth = 0.8)) +
        geom_hline(yintercept = 0,  color = "darkgray", linetype = "dashed") +
        scale_y_continuous(breaks = seq(-5, 4, by = 1), limits = c(-5.5, 4)) + # Sets y-axis breaks from -5 to 5 with step of 1
        labs(x = NULL,  # Removes the x-axis title
             y = "Covariate effect size \n") + # Adds a title to the y-axis
        scale_x_discrete(labels = function(x) toupper(str_extract(x, "[A-Z]{4}$"))) +
        facet_nested(sca ~ Covariate, scales = "free_x", space = "free_x",
                     labeller = labeller(Covariate = c("Tree Basal Area" = "Tree Basal \nArea", 
                                                       "Late Successional Tree Density" = "Late Success. \nTree Density"
                     )))

ggsave("manus_figs/fig3somesps.svg", plot = last_plot(), device = "svg", width = 12, height = 8)

##? Figure 3 with all species ('zeros') ----------------------------------------------------------
cov_name2 <- cov_name 
colnames(cov_name2) <- c("coef", "Covariate")

# 7 species * 6 betas * 3 scales
dat2 <- dat1 %>%
      mutate(step = 3) %>% 
      complete(Covariate, sps, sca,
            fill = list(low = 0, median = 0, up = 0, mean = 0, 
                        overlap0 = "yes", step = 1)) %>%
      arrange(sca, Covariate, sps)  %>% 
      select(-coef) %>% 
      left_join(., cov_name2, by = "Covariate")

dat2 <- dat2 %>% mutate(sca_col = "darkolivegreen2")
dat2$sca_col <- ifelse(dat2$sca == "Park Scale", "darkolivegreen3", dat2$sca_col)
dat2$sca_col <- ifelse(dat2$sca == "County Scale", "darkolivegreen4", dat2$sca_col)

dat2$sps2 <- ifelse(dat2$step != 3, "grey", dat2$sps)

dat2 <- dat2 %>% 
    mutate(cov_sps = glue("{coef}_{sps}")) %>%
    arrange(coef, sps) %>%  # Sort by covariate first, then sps alphabetically
    mutate(cov_sps = factor(cov_sps, levels = unique(cov_sps)))

# Update data_zero and dat_sps with the same factor levels
data_zero <- dat2 %>%
    filter(sps2 == "grey") %>%
    mutate(cov_sps = factor(cov_sps, levels = levels(dat2$cov_sps)))

dat_sps <- dat2 %>%
    filter(sps2 != "grey") %>%
    mutate(cov_sps = factor(cov_sps, levels = levels(dat2$cov_sps)))

data_zero <- dat2 %>%
    filter(sps2 == "grey") %>%
    mutate(cov_sps = factor(cov_sps, levels = levels(dat2$cov_sps)))

dat_sps <- dat2 %>%
    filter(sps2 != "grey") %>%
    mutate(cov_sps = factor(cov_sps, levels = levels(dat2$cov_sps)))
  
ggplot() +
  geom_rect(data = dat_col, aes(fill = sca_col),
            xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.3) +
            scale_fill_identity() +
        geom_linerange(data = data_zero, aes(x = cov_sps, ymin = low, ymax = up), color = "#A8B3AA", linewidth = 1) +
        #geom_point(data = data_zero, aes(x = cov_sps, y = mean), color = "#A8B3AA", size = 2.5) +
        geom_linerange(data = dat_sps, aes(x = cov_sps, ymin = low, ymax = up, color = includes_zero), linewidth = 1) +
        geom_point(data = dat_sps, aes(x = cov_sps, y = mean, color = includes_zero), size = 2.5) +
        scale_color_identity() + 
        theme_bw() +
        theme(panel.grid = element_blank(),
              axis.text.x = element_text(size = 10, angle = 90, hjust = 1, vjust = 0.5), 
              axis.text.y = element_text(size = 10),
              axis.title.x = element_text(size = 14), 
              axis.title.y = element_text(size = 14),
              axis.line = element_line(color = "black", linewidth = 0.7), # Adjusts axis line thickness
              strip.text = element_text(size = 12, face = "bold"),
              panel.spacing = unit(0,"line"),
              panel.border = element_rect(color = "black", linewidth = 0.6), #  borders thickness
              strip.background=element_rect(color="black", fill="white", linewidth = 0.8)) +
        geom_hline(yintercept = 0,  color = "darkgray", linetype = "dashed") +
        scale_y_continuous(breaks = seq(-5, 4, by = 1), limits = c(-5.5, 4)) + # Sets y-axis breaks from -5 to 5 with step of 1
        labs(x = NULL,  # Removes the x-axis title
             y = "Covariate effect size \n") + # Adds a title to the y-axis
        scale_x_discrete(labels = function(x) toupper(str_extract(x, "[A-Z]{4}$"))) +
        facet_nested(sca ~ Covariate, scales = "free_x", space = "free_x",
                     labeller = labeller(Covariate = c("Tree Basal Area" = "Tree Basal \nArea", 
                                                       "Late Successional Tree Density" = "Late Success. \nTree Density"
                     )))

#! Figure 2: scale selection circles -----------------------------------------
dat_sca <- dat1  %>% 
                  #select(Covariate, sps, sca1, sca2, sca3, overlap0) %>% 
                  pivot_longer(cols =c("sca1", "sca2", "sca3"),
                               names_to = "scale", 
                               values_to = "selec_freq",
                               names_prefix = "sca")  %>% 
                  group_by(Covariate, sps) %>% 
                  mutate(scale_selected = ifelse(row_number() == which.max(selec_freq), 1, 0)) %>% 
                  ungroup() 

#? remove the scales that overlaps with zero on step one
dat_sca2 <- dat_sca %>% 
                  filter(scale_selected == 1)  

dat_sca <- dat_sca %>% filter(sps != "BCCH")
dat_sca2 <- dat_sca2 %>% filter(sps != "BCCH")

(sca_plot_selec_sca <- ggplot() +
  geom_point(data = dat_sca, 
             aes(x = Covariate, y = sps), fill = "white", color = "white", alpha = 0) + # plot empty points to keep all specis and covariates present in the data
  geom_point(data = dat_sca2 %>% filter(scale == 3), 
             aes(x = Covariate, y = sps, fill = selec_freq, 
                 color = ifelse(scale_selected == 1, "black","#8d8888")), 
             size = 26, shape = 21, stroke = 0.9, alpha = 0.5) +
  geom_point(data = dat_sca2 %>% filter(scale == 2), 
             aes(x = Covariate, y = sps, fill = selec_freq, 
                 color = ifelse(scale_selected == 1, "black","#8d8888")), 
             size = 19, shape = 21, stroke = 0.9, alpha = 0.5) +
  geom_point(data = dat_sca2 %>% filter(scale == 1), 
             aes(x = Covariate, y = sps, fill = selec_freq, 
                 color = ifelse(scale_selected == 1, "black","#8d8888")), 
             size = 10, shape = 21, stroke = 0.9, alpha = 0.5) +
    # no overlap
  geom_text(data = dat_sca2 %>% filter(scale == 3), 
           aes(x = Covariate, y = sps, label = glue("{round(selec_freq * 100)}")), #\nCI:{round(low, 1)} : {round(up, 1)}")), #
            size = 5.5, color = "black") +
  geom_text(data = dat_sca2 %>% filter(scale == 2), 
            aes(x = Covariate, y = sps, label = glue("{round(selec_freq * 100)}")), #\nCI:{round(low, 1)} : {round(up, 1)}")), 
            size = 5.5, color = "black") +
  geom_text(data = dat_sca2 %>% filter(scale == 1), 
            aes(x = Covariate, y = sps, label = glue("{round(selec_freq * 100)}")), #\nCI:{round(low, 1)} : {round(up, 1)}")), 
           size = 5, color = "black") +
  scale_color_identity() +  # This tells ggplot to use the color names as actual colors for stroke color
  scale_fill_viridis_c(option = "plasma", direction = -1, na.value = "#f6f5ee",
                       limits = c(0.3,1),
                       breaks = c(0.5, 0.75, 1), 
                       labels = scales::percent(c(0.5, 0.75, 1))) +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(hjust = 0.5, size = 18),
        axis.text.y = element_text(hjust = 0, size = 19),    
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        legend.title = element_text(size = 15, face = "bold", hjust = 0.5),  
        legend.text = element_text(size = 13)) +
  scale_y_discrete(limits = rev(levels(factor(dat_sca$sps)))) +  # Reverse y-axis order
  scale_x_discrete(labels = function(x) {
    cov_codes <- unique(dat_sca$Covariate)
      # Manually add line breaks
      case_when(
        cov_codes == "Tree Basal Area" ~ "Tree Basal\nArea",
        cov_codes == "Late Successional Tree Density" ~ "Late Success.\nTree Basal Area",
        cov_codes == "Conifer Density" ~ "Conifer\nBasal Area",
        cov_codes == "Tree Density" ~ "Tree\nDensity",
        cov_codes == "Shrub Basal Area" ~ "Shrub\nCover",
        TRUE ~ cov_codes  # Keep others as is
      )}) +
  labs(x = NULL, # "\nForest Covariate", 
       y = "Species\n", fill = "Scale Selection\nFrequency\n") +
  guides(fill = guide_colorbar(override.aes = list(alpha = 0.2, size = 5)))+
  guides(
    fill = guide_colorbar(
      barwidth = unit(8, "cm"),   # wider (horizontal) color strip
      barheight = unit(1, "cm")    # taller (vertical) color strip
      )
    )
  )

ggsave("figures/sca_plot_select_sca_noleg.svg", plot = sca_plot_selec_sca, device = "svg", width = 12, height = 16)
ggsave("figures/sca_plot_select_sca_noleg.png", plot = sca_plot_selec_sca, device = "png", width = 12, height = 16, dpi = 1200)

#? ploting not the coefficient effect sizes, not the scale values, and only the ones that did not overlap zero (?)
dat_sca3 <- dat_sca2 %>% 
                  filter(includes_zero == "black")  ## this remove the coeficient that overlaps with zero
dat_sca3_0 <- dat_sca2 %>% 
                  filter(includes_zero == "#a9a9a9")  ## this remove the coeficient that does not overlaps with zero

write_rds(dat_sca, "data/out/coefs_sps_sca.rds")

(circles_coefs <- ggplot() +
# plot empty points to keep all species and covariates present in the data
  geom_point(data = dat_sca, 
             aes(x = Covariate, y = sps), fill = "white", color = "white", alpha = 0) + 
  # overlaps zero
  geom_point(data = dat_sca3_0 %>% filter(scale == 3), 
             aes(x = Covariate, y = sps), fill = "#e4e1e1", alpha = 0.6,
             size = 27, shape = 21, stroke = 0.9, color = "#e4e1e1") +
  geom_point(data = dat_sca3_0 %>% filter(scale == 2), 
             aes(x = Covariate, y = sps), fill = "#e4e1e1", alpha = 0.6,
             size = 20, shape = 21, stroke = 0.9, color = "#e4e1e1") +
  geom_point(data = dat_sca3_0 %>% filter(scale == 1), 
             aes(x = Covariate, y = sps), fill = "#e4e1e1", alpha = 0.6,
             size = 11, shape = 21, stroke = 0.9, color = "#e4e1e1") +
  # no overlap
  geom_point(data = dat_sca3 %>% filter(scale == 3), 
             aes(x = Covariate, y = sps, fill = median, alpha = 0.95), 
             size = 27, shape = 21, stroke = 0.9, color = "#8d8888") +
  geom_point(data = dat_sca3 %>% filter(scale == 2), 
             aes(x = Covariate, y = sps, fill = median, alpha = 0.95), 
             size = 20, shape = 21, stroke = 0.9, color = "#8d8888") +
  geom_point(data = dat_sca3 %>% filter(scale == 1), 
             aes(x = Covariate, y = sps, fill = median, alpha = 0.95),
             size = 11, shape = 21, stroke = 0.9, color = "#8d8888") +
# Add text labels for median values
 # overlaps
  geom_text(data = dat_sca3_0 %>% filter(scale == 3), 
           aes(x = Covariate, y = sps, label = glue("{round(median, 2)}")), #\nCI:{round(low, 1)} : {round(up, 1)}")), #
            size = 5, color = "#626060") +
  geom_text(data = dat_sca3_0 %>% filter(scale == 2), 
            aes(x = Covariate, y = sps, label = glue("{round(median, 2)}")), #\nCI:{round(low, 1)} : {round(up, 1)}")), 
            size = 5, color = "#626060") +
  geom_text(data = dat_sca3_0 %>% filter(scale == 1), 
            aes(x = Covariate, y = sps, label = glue("{round(median, 2)}")), #\nCI:{round(low, 1)} : {round(up, 1)}")), 
            size = 4, color = "#626060") +
  # no overlap
  geom_text(data = dat_sca3 %>% filter(scale == 3), 
           aes(x = Covariate, y = sps, label = glue("{round(median, 2)}")), #\nCI:{round(low, 1)} : {round(up, 1)}")), #
            size = 5.5, color = "black") +
  geom_text(data = dat_sca3 %>% filter(scale == 2), 
            aes(x = Covariate, y = sps, label = glue("{round(median, 2)}")), #\nCI:{round(low, 1)} : {round(up, 1)}")), 
            size = 5.5, color = "black") +
  geom_text(data = dat_sca3 %>% filter(scale == 1), 
            aes(x = Covariate, y = sps, label = glue("{round(median, 2)}")), #\nCI:{round(low, 1)} : {round(up, 1)}")), 
           size = 5, color = "black") +
  scale_color_identity() +  # This tells ggplot to use the color names as actual colors for stroke color
  scale_fill_gradient2(low = "#e90061",           # Negative values = blue
                       #mid = "white",           # Zero = white  
                       high = "#00a8d2",        # Positive values = pink
                       midpoint = 0,              # Center point at zero
                       name = "Covariate\nEffect size\n") +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(hjust = 0.5, size = 18),
        axis.text.y = element_text(hjust = 0, size = 19),    
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        legend.title = element_text(size = 15, face = "bold", hjust = 0.5),  
        legend.text = element_text(size = 13),
        panel.grid.major = element_line(color = "gray85", linetype = "solid", linewidth = 0.6),
        panel.grid.minor = element_line(color = "gray85", linetype = "solid", linewidth = 0.6)) + 
  scale_y_discrete(limits = rev(levels(factor(dat_sca$sps)))) +  # Reverse y-axis order
  scale_x_discrete(labels = function(x) {
    cov_codes <- unique(dat_sca$Covariate)
      # Manually add line breaks
      case_when(
        cov_codes == "Tree Basal Area" ~ "Tree Basal\nArea",
        cov_codes == "Late Successional Tree Density" ~ "Late Success.\nTree Basal Area",
        cov_codes == "Conifer Density" ~ "Conifer\nBasal Area",
        cov_codes == "Tree Density" ~ "Tree\nDensity",
        cov_codes == "Shrub Basal Area" ~ "Shrub\nCover",
        TRUE ~ cov_codes  # Keep others as is
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

ggsave("figures/circles_coefs.svg", plot = circles_coefs, device = "svg", width = 12, height = 16)

#ggsave("figures/circles_coefs.png", plot = circles_coefs, device = "png", width = 12, height = 16, dpi = 1200)
