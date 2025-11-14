
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
save.image("data/coef_circlestest3yes.RData")

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

save.image("data/coef_circlestest3yes.RData")

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

ggsave("figures/coef_circlestest3yes.svg", plot = circles_coefs, 
       device = "svg", width = 13, height = 18)

save.image("data/coef_circlestest3yes.RData")