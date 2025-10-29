#? *********************************************************************************
#? ---------------------------   Manuscaript Figuyres   ----------------------------
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
## Figure 2a
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
        } else { coef_summary2 <- coef_summary %>% 
                                      mutate(sps = substr(coef_path_file$result[ii], 1, 4),
                                             mod_res = coef_path_file$select[ii])
                }
      if(ii == 1) {coef_summary3 <- coef_summary2} else {coef_summary3 <- rbind(coef_summary3, coef_summary2)}
}

write_rds(coef_summary3, file = "data/out/coefs_step1.rds")

#? Figure 2 --------------------------------------------------------------
dat <- coef_summary3 %>% 
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
            as_tibble(.name_repair) %>% 
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
            as_tibble(.name_repair) %>% 
            rename(sca = V1,
                   sca_name = V2)  %>% 
            mutate(sca_name = factor(sca_name, 
                              levels = c("Local Scale",
                                         "Park Scale",
                                         "County Scale")),
                   sca = as.numeric(sca))

dat0 <- left_join(dat, cov_name, by = "Covariate") %>% 
       select(-Covariate)  %>% 
       rename(Covariate = cov_name) %>% 
       mutate(sca = as.numeric(sca)) %>% 
       rename(low = `2.5%`,  
              median = `50%`, 
              up = `97.5%`) %>% 
       arrange(sps, coef) %>% 
       relocate(sps)  %>% 
       left_join(., sca_name, by = "sca") 

dat1 <- dat0 %>% 
            filter(sps != "BCCH",
                   substr(coef, 1, 4) == "beta",
                   coef != "beta6") %>% 
            mutate(sca_col = NA,
                   select_sca = pmax(sca1, sca2, sca3),
                   cov_sps = glue("{Covariate}_{sps}")) %>% 
            arrange(Covariate, sps) %>%  # Sort by Covariate first, then sps alphabetically
            mutate(cov_sps = factor(cov_sps, levels = sort(unique(cov_sps))))


dat1$sca_col <- ifelse(dat1$select_sca >= 0.33 & dat1$select_sca < 0.5, "#c2c2c2", dat1$sca_col)
dat1$sca_col <- ifelse(dat1$select_sca >= 0.5 & dat1$select_sca < 0.75, "#7f5b95", dat1$sca_col)
dat1$sca_col <- ifelse(dat1$select_sca >= 0.75, "#601c8b", dat1$sca_col)

dat_sca <- dat1  %>% 
                  #select(Covariate, sps, sca1, sca2, sca3, overlap0) %>% 
                  pivot_longer(cols =c("sca1", "sca2", "sca3"),
                               names_to = "scale", 
                               values_to = "selec_freq",
                               names_prefix = "sca")  %>% 
                  group_by(Covariate, sps) %>% 
                  mutate(scale_selected = ifelse(row_number() == which.max(selec_freq), 1, 0)) %>% 
                  ungroup() %>% 
                  select(Covariate, sps, sca_col, sca, sca_name, selec_freq)
                  
dat1 %>% filter(is.na(sca_col)) %>% select(sps, coef, overlap0, sca1, sca2, sca3, sca_name, sca_col, select_sca, cov_sps, Covariate)

(sca_plot_selec_sca <- ggplot() +
  geom_point(data = dat_sca %>% filter(sca == 1), 
             aes(x = Covariate, y = sps, fill = sca_col), 
                 size = 26, shape = 21, stroke = 0.9) +
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
        axis.text.x = element_text(hjust = 0.5, size = 31),
        axis.text.y = element_text(hjust = 0, size = 31),    
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 28),
        legend.title = element_text(size = 26, face = "bold", hjust = 0.5),  
        legend.text = element_text(size = 24),
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

