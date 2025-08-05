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

hg <- httpgd::hgd()
httpgd::hgd_browse()

#! Load packages ---------------------------------------
library(tidyverse)
library(conflicted)
library(glue)
library(MCMCvis)
library(viridis)
library(svglite)
library(ggh4x)
library(ggforce)

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
COEF_TABLE_PATH <- "data/mod_key2.csv"

## read files
coef_path_file <- read_csv(COEF_TABLE_PATH) %>%
        filter(run == "yes") %>% 
        filter(step == 3) %>% 
        mutate(AOU_Code = substr(result, 1, 4))

for(ii in 1:nrow(coef_path_file)) {

    (loop_sps <- substr(coef_path_file$result[ii], 1, 4))

    loop_run <- substr(coef_path_file$result[ii], nchar(coef_path_file$result[ii]) - 7, nchar(coef_path_file$result[ii]) - 4)

    quants <- ifelse((as.numeric(substr(loop_run, 4, 4)) %% 2 == 0) == TRUE, "25_75", "3_7")

     selec_files <- 
      list.files(path = file.path(getwd(),"data/model_res/"),
                                          pattern = "SCA_SEL_PARS",
                                          full.names = FALSE)  %>% 
                as_tibble() %>% 
                mutate(sps = substr(value, 1, 4)) %>% 
                filter(sps == loop_sps) %>% 
                filter(str_detect(value, quants)) %>%  # Filter for rows containing the quants text
                pull(value)

      if(lenght(selec_files) == 1) {coef_path_file$select[ii] <- selec_files}
      if(lenght(selec_files) == 2) {
        
        if(selec_files[1] %in% coef_path_file$select) {coef_path_file$select[ii] <- selec_files[2]}
        if(selec_files[1] %!in% coef_path_file$select) {coef_path_file$select[ii] <- selec_files[1]}
      
      }

      samples_jags <- read_rds(glue("data/model_res/{coef_path_file$result[ii]}"))
      beta_sca_names <- read_rds(glue("data/model_res/{coef_path_file$select[ii]}")) %>% 
            filter(overlap0 == "no") %>%
            add_row(betas = "park_size") %>%  # Add empty row for 3 alphas + 1 be4ta park size
            add_row(betas = "alpha1") %>%  
            add_row(betas = "alpha2") %>%  
            add_row(betas = "alpha3")     

      if(loop_sps == "YBSA"){beta_sca_names$betas[1] <- "beta"}

      # Get summary with median and credible intervals
      coef_summary <- MCMCsummary(samples_jags,
                              params = c("beta", "alpha"), #, "beta0", "alpha0"),  # specify parameters
                              probs = c(0.025, 0.5, 0.975),  # 2.5%, median, 97.5%
                              round = 3) %>% 
                        mutate(coef = rownames(.)) %>% 
                        as_tibble() %>% 
                        relocate(coef) %>%
                        mutate(coef = gsub("\\[|\\]", "", coef))

      if((nrow(beta_sca_names)) != nrow(coef_summary)) {
            stop(glue("error in {coef_path_file$result[ii]}"))
            
            } else { coef_summary2 <- cbind(coef_summary, beta_sca_names) %>% 
                                          mutate(sps = substr(coef_path_file$result[ii], 1, 4),
                                                 mod_res = coef_path_file$select[ii])
            }
      if(ii == 1) {coef_summary3 <- coef_summary2} else {coef_summary3 <- rbind(coef_summary3, coef_summary2)}
}

table(coef_summary3$mod_res)

coef_summary3 <- as_tibble(coef_summary3)

dat <- coef_summary3 %>% 
            filter(overlap0 == "no") %>% 
            rename(sca = sca_sel,
                   cov = betas) %>% 
            arrange(sca, cov, sps)  %>% 
            mutate(sps_p = glue("{row_number()}_{sps}")) %>% 
            as_tibble() %>% 
            mutate(sps = toupper(sps),
                   sps_p = factor(sps_p, levels = sps_p)) %>% 
            rename(Covariate = cov)

cov_name <- cbind(sort(unique(dat$Covariate)),
                  c("Tree Density",
                    "Conifer Density",
                    "Late Successional Tree Density",
                    "Shrub Basal Area",
                    "Tree Basal Area",
                    "Tree Basal Area Squared")) %>% 
            as_tibble() %>% 
            rename(Covariate = V1,
                   cov_name = V2)  %>% 
            mutate(cov_name = factor(cov_name, 
                              levels = c("Tree Density",
                    "Conifer Density",
                    "Late Successional Tree Density",
                    "Shrub Basal Area",
                    "Tree Basal Area",
                    "Tree Basal Area Squared")))

sca_name <- cbind(unique(dat$sca),
                  c("Site Scale",
                    "Park Scale",
                    "Landscape Scale")) %>% 
            as_tibble() %>% 
            rename(sca = V1,
                   sca_name = V2)  %>% 
            mutate(sca_name = factor(sca_name, 
                              levels = c("Site Scale",
                                         "Park Scale",
                                         "Landscape Scale")),
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

dat1 <- dat %>% 
            mutate(sca_col = "darkolivegreen2") %>% 
            mutate(cov_sps = glue("{Covariate}_{sps}")) %>% 
            arrange(Covariate, sps) %>%  # Sort by Covariate first, then sps alphabetically
            mutate(cov_sps = factor(cov_sps, levels = sort(unique(cov_sps))))

dat1$sca_col <- ifelse(dat1$sca == "Park Scale", "darkolivegreen3", dat1$sca_col)
dat1$sca_col <- ifelse(dat1$sca == "Landscape Scale", "darkolivegreen4", dat1$sca_col)

#! Figure 3 ---------------------------------------------
sca_col <- c("#B0EDB9", "#64CC81", "#088A0F")
sca <- c("Site Scale","Park Scale","Landscape Scale")
dat_col <- as_tibble(cbind(sca_col, sca))  %>% 
                  mutate(sca = factor(sca, 
                        levels = c("Site Scale","Park Scale","Landscape Scale")))

dat1 <- dat1 %>%
  mutate(includes_zero = ifelse(low <= 0 & up >= 0, "#a9a9a9", "black")) 

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
        geom_hline(yintercept = 0,  color = "gray", linetype = "dashed") +
        scale_y_continuous(breaks = seq(-5, 3, by = 1), limits = c(-4.6, 3.17)) + # Sets y-axis breaks from -5 to 5 with step of 1
        labs(x = NULL,  # Removes the x-axis title
             y = "Covariate effect size \n") + # Adds a title to the y-axis
        scale_x_discrete(labels = toupper(str_extract(levels(dat1$cov_sps), "[A-Z]{4}$"))) +        #facet_wrap(~sca, nrow = 3)
        facet_nested(sca ~ Covariate, scales = "free_x", space = "free_x",
                     labeller = labeller(Covariate = c("Tree Basal Area" = "Tree Basal \nArea", 
                                                       "Tree Basal Area Squared" = "Tree Basal \nArea Squared",
                                                       "Late Successional Tree Density" = "Late Success. \nTree Density"
                     )))

ggsave("manus_figs/fig3somesps.svg", plot = last_plot(), device = "svg", width = 12, height = 8)

#? Figure 3 with all species ('zeros') ----------------------------------------------------------
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
dat2$sca_col <- ifelse(dat2$sca == "Landscape Scale", "darkolivegreen4", dat2$sca_col)

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
        geom_hline(yintercept = 0,  color = "gray", linetype = "dashed") +
        scale_y_continuous(breaks = seq(-2, 3, by = 1), limits = c(-4.6, 3.17)) + # Sets y-axis breaks from -5 to 5 with step of 1
        labs(x = NULL,  # Removes the x-axis title
             y = "Covariate effect size \n") + # Adds a title to the y-axis
        scale_x_discrete(labels = toupper(str_extract(levels(dat_sps$cov_sps), "[A-Z]{4}$"))) +        #facet_wrap(~sca, nrow = 3)
        facet_nested(sca ~ Covariate, scales = "free_x", space = "free_x",
                     labeller = labeller(Covariate = c("Tree Basal Area" = "Tree Basal \nArea", 
                                                       "Tree Basal Area Squared" = "Tree Basal \nArea Squared",
                                                       "Late Successional Tree Density" = "Late Success. \nTree Density"
                     )))

# Create new grouping variables: species horizontal, coefficient values in the x-axis, cocariate names on the y-axis
dat_sps_restructured <- dat_sps %>%
  mutate(
    # Create species-covariate combination for x-axis
    sps_cov = paste(sps, Covariate, sep = "_"),
    # Sort covariates within each species - NEW ORDER
    cov_order = case_when(
      Covariate == "Tree Density" ~ 7,                           # Tree Density on top
      Covariate == "Conifer Density" ~ 6,                        # Conifer Density second
      Covariate == "Late Successional Tree Density" ~ 5,         # Large Tree Density third
      Covariate == "Shrub Basal Area" ~ 5,                       # Shrub fourth
      Covariate == "Tree Basal Area" ~ 3,                        # Basal Area fifth
      Covariate == "Tree Basal Area Squared" ~ 2,                # Basal Area Squared last
      TRUE ~ 1
    )
  ) %>%
  arrange(sps, cov_order) %>%
  mutate(
    # Create ordered factor for x-axis positioning
    sps_cov = factor(sps_cov, levels = unique(sps_cov))
  )

data_zero_restructured <- data_zero %>%
  mutate(
    sps_cov = paste(sps, Covariate, sep = "_"),
    # Sort covariates within each species - NEW ORDER
    cov_order = case_when(
      Covariate == "Tree Density" ~ 6,                           # Tree Density on top
      Covariate == "Conifer Density" ~ 5,                        # Conifer Density second
      Covariate == "Late Successional Tree Density" ~ 4,         # Large Tree Density third
      Covariate == "Shrub Basal Area" ~ 3,                      # Shrub fourth
      Covariate == "Tree Basal Area" ~ 2,                       # Basal Area fifth
      Covariate == "Tree Basal Area Squared" ~ 1,               # Basal Area Squared last
    )
  ) %>%
  arrange(sps, cov_order) %>%
  mutate(sps_cov = factor(sps_cov, levels = levels(dat_sps_restructured$sps_cov)))

ggplot() +
  geom_rect(data = dat_col, aes(fill = sca_col),
            xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.3) +
  scale_fill_identity() +
  geom_hline(yintercept = 0, color = "gray", linetype = "dashed") +
  geom_linerange(data = data_zero_restructured, 
                 aes(x = Covariate, ymin = low, ymax = up), 
                 color = "#A8B3AA", linewidth = 0.8) +
  geom_linerange(data = dat_sps_restructured, 
                 aes(x = Covariate, ymin = low, ymax = up, color = includes_zero), 
                 linewidth = 0.8) +
  geom_point(data = dat_sps_restructured, 
             aes(x = Covariate, y = mean, color = includes_zero), 
             size = 2) +
  scale_color_identity() + 
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(size = 8, hjust = 0.5), 
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 14), 
        axis.title.y = element_text(size = 14),
        strip.text = element_text(size = 10, face = "bold"),
        panel.spacing = unit(0.1, "line"),
        panel.border = element_rect(color = "black", linewidth = 0.6),
        strip.background = element_rect(color = "black", fill = "white", linewidth = 0.8)) +
  scale_y_continuous(breaks = seq(-4, 3, by = 1), limits = c(-4.6, 3.17)) +
  labs(x = "Covariate\n", 
       y = "\nCovariate effect size") +
  scale_x_discrete(labels = function(x) {
    case_when(
      x == "Tree Basal Area" ~ "Tree Basal Area",
      x == "Tree Basal Area Squared" ~ "Tree Basal Area²",
      x == "Late Successional Tree Density" ~ "Late Success.\nTree Density",
      x == "Tree Density" ~ "Tree Density",
      x == "Conifer Density" ~ "Conifer Density", 
      x == "Shrub Basal Area" ~ "Shrub Basal Area",
      TRUE ~ x
    )
  }) +
  facet_nested(sca ~ sps, scales = "free_x", space = "free_x") +
  coord_flip()


#! Figure 2 scale selection -----------------------------------------
#? all scales with the color gradient
dat_sca <- dat1  %>% 
                  #select(Covariate, sps, sca1, sca2, sca3, overlap0) %>% 
                  pivot_longer(cols =c("sca1", "sca2", "sca3"),
                               names_to = "scale", 
                               values_to = "selec_freq",
                               names_prefix = "sca")  %>% 
                  group_by(Covariate, sps) %>% 
                  mutate(scale_selected = ifelse(row_number() == which.max(selec_freq), 1, 0)) %>% 
                  ungroup()
# yes legend
(sca_plot_wleg <- ggplot() +
  geom_point(data = dat_sca %>% filter(scale == 3), 
             aes(x = Covariate, y = sps, fill = selec_freq, alpha = 0.1,
                 color = ifelse(scale_selected == 1, "black","#8d8888")), 
             size = 26, shape = 21, stroke = 0.9) +
  geom_point(data = dat_sca %>% filter(scale == 2), 
             aes(x = Covariate, y = sps, fill = selec_freq, alpha = 0.1,
                 color = ifelse(scale_selected == 1, "black","#8d8888")), 
             size = 19, shape = 21, stroke = 0.9) +
  geom_point(data = dat_sca %>% filter(scale == 1), 
             aes(x = Covariate, y = sps, fill = selec_freq, alpha = 0.1,
                 color = ifelse(scale_selected == 1, "black","#8d8888")), 
             size = 10, shape = 21, stroke = 0.9) +
  scale_color_identity() +  # This tells ggplot to use the color names as actual colors for stroke color
  scale_fill_viridis_c(option = "plasma", direction = -1, na.value = "#fff8c5",
                       limits = c(-0.001,1),
                       breaks = c(0, 0.25, 0.5, 0.75, 1), 
                       labels = scales::percent(c(0, 0.25, 0.5, 0.75, 1))) +
  theme_minimal() +
  theme(axis.text.x = element_text(hjust = 0.5, size = 12),
        axis.text.y = element_text(hjust = 0, size = 12),    
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.title = element_text(size = 15, face = "bold", hjust = 0.5),  
        legend.text = element_text(size = 13)) +
  scale_y_discrete(limits = rev(levels(factor(dat_sca$sps)))) +  # Reverse y-axis order
  scale_x_discrete(labels = function(x) {
  cov_codes <- unique(dat_sca$Covariate)
    # Manually add line breaks
    case_when(
      cov_codes == "Tree Basal Area" ~ "Tree Basal \nArea",
      cov_codes == "Tree Basal Area Squared" ~ "Tree Basal \nArea Squared", 
      cov_codes == "Late Successional Tree Density" ~ "Late Success. \nTree Density",
      TRUE ~ cov_codes  # Keep others as is
    )}) +
  labs(x = "\nForest Covariate", y = "Species\n", fill = "Scale Selection\nFrequency\n") +
  guides(fill = guide_colorbar(override.aes = list(alpha = 0.2, size = 5)))   # Control legend appearance
)

ggsave("figures/sca_plot.svg", plot = sca_plot_wleg, device = "svg", width = 11, height = 14)

# no legend 
(sca_plot_noleg <- ggplot() +
  geom_point(data = dat_sca %>% filter(scale == 3), 
             aes(x = Covariate, y = sps, fill = selec_freq, alpha = 0.1,
                 color = ifelse(scale_selected == 1, "black","#8d8888")), 
             size = 26, shape = 21, stroke = 0.9) +
  geom_point(data = dat_sca %>% filter(scale == 2), 
             aes(x = Covariate, y = sps, fill = selec_freq, alpha = 0.1,
                 color = ifelse(scale_selected == 1, "black","#8d8888")), 
             size = 19, shape = 21, stroke = 0.9) +
  geom_point(data = dat_sca %>% filter(scale == 1), 
             aes(x = Covariate, y = sps, fill = selec_freq, alpha = 0.1,
                 color = ifelse(scale_selected == 1, "black","#8d8888")), 
             size = 10, shape = 21, stroke = 0.9) +
  scale_color_identity() +  # This tells ggplot to use the color names as actual colors for stroke color
  scale_fill_viridis_c(option = "plasma", direction = -1, na.value = "#fff8c5",
                       limits = c(-0.001,1),
                       breaks = c(0, 0.25, 0.5, 0.75, 1), 
                       labels = scales::percent(c(0, 0.25, 0.5, 0.75, 1))) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(hjust = 0.5, size = 12),
        axis.text.y = element_text(hjust = 0, size = 12),    
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.title = element_text(size = 15, face = "bold", hjust = 0.5),  
        legend.text = element_text(size = 13)) +
  scale_y_discrete(limits = rev(levels(factor(dat_sca$sps)))) +  # Reverse y-axis order
  scale_x_discrete(labels = function(x) {
  cov_codes <- unique(dat_sca$Covariate)
    # Manually add line breaks
    case_when(
      cov_codes == "Tree Basal Area" ~ "Tree Basal \nArea",
      cov_codes == "Tree Basal Area Squared" ~ "Tree Basal \nArea Squared", 
      cov_codes == "Late Successional Tree Density" ~ "Late Success. \nTree Density",
      TRUE ~ cov_codes  # Keep others as is
    )}) +
  labs(x = "\nForest Covariate", y = "Species\n", fill = "Scale Selection\nFrequency\n") +
  guides(fill = guide_colorbar(override.aes = list(alpha = 0.2, size = 5)))   # Control legend appearance
)

ggsave("figures/sca_plot_noleg.svg", plot = sca_plot_noleg, device = "svg", width = 9, height = 14)

#? remove the scales that overlaps with zero on step one
dat_sca2 <- dat_sca %>% 
                  filter(scale_selected == 1)  

(sca_plot_selec_sca <- ggplot() +
  geom_point(data = dat_sca, 
             aes(x = Covariate, y = sps), fill = "white", color = "white", alpha = 0) + # plot empty points to keep all specis and covariates present in the data
  geom_point(data = dat_sca2 %>% filter(scale == 3), 
             aes(x = Covariate, y = sps, fill = selec_freq, alpha = 0.1,
                 color = ifelse(scale_selected == 1, "black","#8d8888")), 
             size = 26, shape = 21, stroke = 0.9) +
  geom_point(data = dat_sca2 %>% filter(scale == 2), 
             aes(x = Covariate, y = sps, fill = selec_freq, alpha = 0.1,
                 color = ifelse(scale_selected == 1, "black","#8d8888")), 
             size = 19, shape = 21, stroke = 0.9) +
  geom_point(data = dat_sca2 %>% filter(scale == 1), 
             aes(x = Covariate, y = sps, fill = selec_freq, alpha = 0.1,
                 color = ifelse(scale_selected == 1, "black","#8d8888")), 
             size = 10, shape = 21, stroke = 0.9) +
  scale_color_identity() +  # This tells ggplot to use the color names as actual colors for stroke color
  scale_fill_viridis_c(option = "plasma", direction = -1, na.value = "#fff8c5",
                       limits = c(-0.001,1),
                       breaks = c(0, 0.25, 0.5, 0.75, 1), 
                       labels = scales::percent(c(0, 0.25, 0.5, 0.75, 1))) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(hjust = 0.5, size = 12),
        axis.text.y = element_text(hjust = 0, size = 12),    
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.title = element_text(size = 15, face = "bold", hjust = 0.5),  
        legend.text = element_text(size = 13)) +
  scale_y_discrete(limits = rev(levels(factor(dat_sca2$sps)))) +  # Reverse y-axis order
  scale_x_discrete(labels = function(x) {
  cov_codes <- unique(dat_sca2$Covariate)
    # Manually add line breaks
    case_when(
      cov_codes == "Tree Basal Area" ~ "Tree Basal \nArea",
      cov_codes == "Tree Basal Area Squared" ~ "Tree Basal \nArea Squared", 
      cov_codes == "Late Successional Tree Density" ~ "Late Success. \nTree Density",
      TRUE ~ cov_codes  # Keep others as is
    )}) +
  labs(x = "\nForest Covariate", y = "Species\n", fill = "Scale Selection\nFrequency\n") +
  guides(fill = guide_colorbar(override.aes = list(alpha = 0.2, size = 5)))   # Control legend appearance
)

ggsave("figures/sca_plot_select_sca_noleg.svg", plot = sca_plot_selec_sca, device = "svg", width = 9, height = 14)

#? ploting not the coefficient effect sizes, not the scale values, and only the ones that did not overlap zero (?)

dat_sca3 <- dat_sca2 %>% 
                  filter(includes_zero == "black")  ## this remove the coeficient that overlaps with zero

write_rds(dat_sca, "data/out/coefs_sps_sca.rds")

(circles_coefs <- ggplot() +
# plot empty points to keep all species and covariates present in the data
  geom_point(data = dat_sca, 
             aes(x = Covariate, y = sps), fill = "white", color = "white", alpha = 0) + 
  geom_point(data = dat_sca3 %>% filter(scale == 3), 
             aes(x = Covariate, y = sps, fill = median), 
             size = 26, shape = 21, stroke = 0.9, color = "#8d8888") +
  geom_point(data = dat_sca3 %>% filter(scale == 2), 
             aes(x = Covariate, y = sps, fill = median), 
             size = 19, shape = 21, stroke = 0.9, color = "#8d8888") +
  geom_point(data = dat_sca3 %>% filter(scale == 1), 
             aes(x = Covariate, y = sps, fill = median),
             size = 10, shape = 21, stroke = 0.9, color = "#8d8888") +
# Add text labels for median values
#   geom_text(data = dat_sca3 %>% filter(scale == 3), 
#            aes(x = Covariate, y = sps, label = glue("{round(median, 2)}")), #\nCI:{round(low, 1)} : {round(up, 1)}")), #
#             size = 4, color = "black") +
#   geom_text(data = dat_sca3 %>% filter(scale == 2), 
#             aes(x = Covariate, y = sps, label = glue("{round(median, 2)}")), #\nCI:{round(low, 1)} : {round(up, 1)}")), 
#             size = 4, color = "black") +
#   geom_text(data = dat_sca3 %>% filter(scale == 1), 
#             aes(x = Covariate, y = sps, label = glue("{round(median, 2)}")), #\nCI:{round(low, 1)} : {round(up, 1)}")), 
#            size = 3.5, color = "black") +
  scale_color_identity() +  # This tells ggplot to use the color names as actual colors for stroke color
  scale_fill_gradient2(low = "#078a42",           # Negative values = blue
                       mid = "white",           # Zero = white  
                       high = "#cc00df",        # Positive values = pink
                       midpoint = 0,              # Center point at zero
                       name = "Covariate\nEffect size\n") +
  theme_minimal() +
  theme(axis.text.x = element_text(hjust = 0.5, size = 12),
        axis.text.y = element_text(hjust = 0, size = 12),    
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.title = element_text(size = 15, face = "bold", hjust = 0.5),  
        legend.text = element_text(size = 13)) +
  scale_y_discrete(limits = rev(levels(factor(dat_sca$sps)))) +  # Reverse y-axis order
  scale_x_discrete(labels = function(x) {
  cov_codes <- unique(dat_sca$Covariate)
    # Manually add line breaks
    case_when(
      cov_codes == "Tree Basal Area" ~ "Tree Basal \nArea",
      cov_codes == "Tree Basal Area Squared" ~ "Tree Basal \nArea Squared", 
      cov_codes == "Late Successional Tree Density" ~ "Late Success. \nTree Density",
      TRUE ~ cov_codes  # Keep others as is
    )}) +
  labs(x = "\nForest Covariate", y = "Species\n", fill = "Covariate\nEffect size\n")# +
)

ggsave("figures/circles_coefs.svg", plot = circles_coefs, device = "svg", width = 11, height = 14)

# remove legend
(circles_coefs_noleg <- ggplot() +
# plot empty points to keep all species and covariates present in the data
  geom_point(data = dat_sca, 
             aes(x = Covariate, y = sps), fill = "white", color = "white", alpha = 0) + 
  geom_point(data = dat_sca3 %>% filter(scale == 3), 
             aes(x = Covariate, y = sps, fill = median), 
             size = 26, shape = 21, stroke = 0.9, color = "#8d8888") +
  geom_point(data = dat_sca3 %>% filter(scale == 2), 
             aes(x = Covariate, y = sps, fill = median), 
             size = 19, shape = 21, stroke = 0.9, color = "#8d8888") +
  geom_point(data = dat_sca3 %>% filter(scale == 1), 
             aes(x = Covariate, y = sps, fill = median),
             size = 10, shape = 21, stroke = 0.9, color = "#8d8888") +
# Add text labels for median values
#   geom_text(data = dat_sca3 %>% filter(scale == 3), 
#            aes(x = Covariate, y = sps, label = glue("{round(median, 2)}")), #\nCI:{round(low, 1)} : {round(up, 1)}")), #
#             size = 4, color = "black") +
#   geom_text(data = dat_sca3 %>% filter(scale == 2), 
#             aes(x = Covariate, y = sps, label = glue("{round(median, 2)}")), #\nCI:{round(low, 1)} : {round(up, 1)}")), 
#             size = 4, color = "black") +
#   geom_text(data = dat_sca3 %>% filter(scale == 1), 
#             aes(x = Covariate, y = sps, label = glue("{round(median, 2)}")), #\nCI:{round(low, 1)} : {round(up, 1)}")), 
#            size = 3.5, color = "black") +
  scale_color_identity() +  # This tells ggplot to use the color names as actual colors for stroke color
  scale_fill_gradient2(low = "#078a42",           # Negative values = blue
                       mid = "white",           # Zero = white  
                       high = "#cc00df",        # Positive values = pink
                       midpoint = 0,              # Center point at zero
                       name = "Covariate\nEffect size\n") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(hjust = 0.5, size = 12),
        axis.text.y = element_text(hjust = 0, size = 12),    
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.title = element_text(size = 15, face = "bold", hjust = 0.5),  
        legend.text = element_text(size = 13)) +
  scale_y_discrete(limits = rev(levels(factor(dat_sca$sps)))) +  # Reverse y-axis order
  scale_x_discrete(labels = function(x) {
  cov_codes <- unique(dat_sca$Covariate)
    # Manually add line breaks
    case_when(
      cov_codes == "Tree Basal Area" ~ "Tree Basal \nArea",
      cov_codes == "Tree Basal Area Squared" ~ "Tree Basal \nArea Squared", 
      cov_codes == "Late Successional Tree Density" ~ "Late Success. \nTree Density",
      TRUE ~ cov_codes  # Keep others as is
    )}) +
  labs(x = "\nForest Covariate", y = "Species\n", fill = "Covariate\nEffect size\n")# +
#   guides(fill = guide_colorbar(override.aes = list(alpha = 0.2, size = 5))) 

)

ggsave("figures/circles_coefs_noleg.svg", plot = circles_coefs_noleg, device = "svg", width = 9, height = 14)

## plot with nothing, just axis
(empty_noleg <- ggplot() +
# plot empty points to keep all species and covariates present in the data
  geom_point(data = dat_sca, 
             aes(x = Covariate, y = sps), fill = "white", color = "white", alpha = 0) + 
  scale_color_identity() +  # This tells ggplot to use the color names as actual colors for stroke color
  scale_fill_gradient2(low = "#078a42",           # Negative values = blue
                       mid = "white",           # Zero = white  
                       high = "#cc00df",        # Positive values = pink
                       midpoint = 0,              # Center point at zero
                       name = "Covariate\nEffect size\n") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(hjust = 0.5, size = 12),
        axis.text.y = element_text(hjust = 0, size = 12),    
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.title = element_text(size = 15, face = "bold", hjust = 0.5),  
        legend.text = element_text(size = 13)) +
  scale_y_discrete(limits = rev(levels(factor(dat_sca$sps)))) +  # Reverse y-axis order
  scale_x_discrete(labels = function(x) {
  cov_codes <- unique(dat_sca$Covariate)
    # Manually add line breaks
    case_when(
      cov_codes == "Tree Basal Area" ~ "Tree Basal \nArea",
      cov_codes == "Tree Basal Area Squared" ~ "Tree Basal \nArea Squared", 
      cov_codes == "Late Successional Tree Density" ~ "Late Success. \nTree Density",
      TRUE ~ cov_codes  # Keep others as is
    )}) +
  labs(x = "\nForest Covariate", y = "Species\n", fill = "Covariate\nEffect size\n")# +
)

ggsave("figures/empty_noleg.svg", plot = empty_noleg, device = "svg", width = 9, height = 14)

## plot with the empty circles
(empty_cir_noleg <- ggplot() +  
  geom_point(data = dat_sca, 
             aes(x = Covariate, y = sps), fill = "white", color = "white", alpha = 0) + # plot empty points to keep all specis and covariates present in the data
  geom_point(data = dat_sca %>% filter(scale == 3), 
             aes(x = Covariate, y = sps), fill = "white", alpha = 0.6, color = "#8d8888", 
             size = 26, shape = 21, stroke = 0.9) +
  geom_point(data = dat_sca %>% filter(scale == 2), 
             aes(x = Covariate, y = sps), fill = "white", alpha = 0.6, color = "#8d8888",  
             size = 19, shape = 21, stroke = 0.9) +
  geom_point(data = dat_sca %>% filter(scale == 1), 
             aes(x = Covariate, y = sps), fill = "white", alpha = 0.6, color = "#8d8888", 
             size = 10, shape = 21, stroke = 0.9) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(hjust = 0.5, size = 12),
        axis.text.y = element_text(hjust = 0, size = 12),    
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.title = element_text(size = 15, face = "bold", hjust = 0.5),  
        legend.text = element_text(size = 13)) +
  scale_y_discrete(limits = rev(levels(factor(dat_sca$sps)))) +  # Reverse y-axis order
  scale_x_discrete(labels = function(x) {
  cov_codes <- unique(dat_sca$Covariate)
    # Manually add line breaks
    case_when(
      cov_codes == "Tree Basal Area" ~ "Tree Basal \nArea",
      cov_codes == "Tree Basal Area Squared" ~ "Tree Basal \nArea Squared", 
      cov_codes == "Late Successional Tree Density" ~ "Late Success. \nTree Density",
      TRUE ~ cov_codes  # Keep others as is
    )}) +
  labs(x = "\nForest Covariate", y = "Species\n", fill = "Scale Selection\nFrequency\n") 
)

ggsave("figures/empty_cir_noleg.svg", plot = empty_cir_noleg, device = "svg", width = 9, height = 14)
