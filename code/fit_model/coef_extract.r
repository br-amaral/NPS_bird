#? *********************************************************************************
#? -------------------------------  coef_extract.r  --------------------------------
#? *********************************************************************************
#
#! Code to ...
#
#! Source ---------------------------------------------
#           - :
#           - :
#
#! Input ----------------------------------------------
#           - :
#           - :
#
#! Output ----------------------------------------------
#           - :
#           - :

# Print script file name

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
COEF_TABLE_PATH <- "data/mod_key.csv"

## read files
coef_path_file <- read_csv(COEF_TABLE_PATH) %>%
        filter(run == "yes") %>% 
        filter(step == 3) %>% 
        mutate(AOU_Code = substr(result, 1, 4))

for(ii in 1:nrow(coef_path_file)) {

    loop_sps <- substr(coef_path_file$result[ii], 1, 4)

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

# Extract specific columns
# median_estimates <- coef_summary$`50%`
# lower_ci <- coef_summary$`2.5%`
# upper_ci <- coef_summary$`97.5%`

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
        scale_y_continuous(breaks = seq(-2, 3, by = 1), limits = c(-2.15, 3.17)) + # Sets y-axis breaks from -5 to 5 with step of 1
        labs(x = NULL,  # Removes the x-axis title
             y = "Covariate effect size \n") + # Adds a title to the y-axis
        scale_x_discrete(labels = toupper(str_extract(levels(dat1$cov_sps), "[A-Z]{4}$"))) +        #facet_wrap(~sca, nrow = 3)
        facet_nested(sca ~ Covariate, scales = "free_x", space = "free_x",
                     labeller = labeller(Covariate = c("Tree Basal Area" = "Tree Basal \nArea", 
                                                       "Tree Basal Area Squared" = "Tree Basal \nArea Squared",
                                                       "Late Successional Tree Density" = "Late Success. \nTree Density"
                     )))

#ggsave("manus_figs/fig1new.svg", plot = last_plot(), device = "svg", width = 12, height = 8)

#### Figure 3 with zeros ----------------------------------------------------------
cov_name2 <- cov_name 
colnames(cov_name2) <- c("coef", "Covariate")

# 7 species * 6 betas * 3 scales
dat2 <- dat %>%
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


sca_col <- c("#B0EDB9", "#64CC81", "#088A0F")
sca <- c("Site Scale","Park Scale","Landscape Scale")
dat_col <- as_tibble(cbind(sca_col, sca))  %>% 
                  mutate(sca = factor(sca, 
                        levels = c("Site Scale","Park Scale","Landscape Scale")))

data_zero$sps2 %>% unique()

dat_sps <- dat_sps %>%
  mutate(includes_zero = ifelse(low <= 0 & up >= 0, "#a9a9a9", "black")) 
dat_sps %>% select(low, up, includes_zero)

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
        scale_y_continuous(breaks = seq(-2, 3, by = 1), limits = c(-2.15, 3.17)) + # Sets y-axis breaks from -5 to 5 with step of 1
        labs(x = NULL,  # Removes the x-axis title
             y = "Covariate effect size \n") + # Adds a title to the y-axis
        scale_x_discrete(labels = toupper(str_extract(levels(dat_sps$cov_sps), "[A-Z]{4}$"))) +        #facet_wrap(~sca, nrow = 3)
        facet_nested(sca ~ Covariate, scales = "free_x", space = "free_x",
                     labeller = labeller(Covariate = c("Tree Basal Area" = "Tree Basal \nArea", 
                                                       "Tree Basal Area Squared" = "Tree Basal \nArea Squared",
                                                       "Late Successional Tree Density" = "Late Success. \nTree Density"
                     )))

ggsave("manus_figs/fig3.svg", plot = last_plot(), device = "svg", width = 12, height = 8)

## Figure 3 scal selection -----------------------------------------
dat_sca <- dat  %>% 
                  select(Covariate, sps, sca1, sca2, sca3, overlap0) %>% 
                  pivot_longer(cols = starts_with("sca"),
                               names_to = "scale", 
                               values_to = "selec_freq",
                               names_prefix = "sca") 

ggplot(dat_sca, aes(X, Y, fill= Z)) + 
  geom_tile()
