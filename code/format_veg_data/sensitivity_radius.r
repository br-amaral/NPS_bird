#? *********************************************************************************
#? ---------------------------   Sensitivity of radius   ---------------------------
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

## read files

radi_dist_loop <- c(250, 500, 750)

for(kk in 1:length(radi_dist_loop)){

    radi_dist <- radi_dist_loop[kk]

    source("/Users/bamaral/Library/CloudStorage/OneDrive-MichiganStateUniversity/GitHubOne/NPS_bird_copy/code/format_veg_data/get_site_data_rad.R")

    assign(paste0('covs_sensi_', radi_dist), close_points_f2)
}

covs_sensi <- left_join(
    covs_sensi_250 %>% select(bird_sit, siteBA) %>% rename(siteBA250 = siteBA) %>% distinct(),
    covs_sensi_500 %>% select(bird_sit, siteBA) %>% rename(siteBA500 = siteBA) %>% distinct(),
    by = "bird_sit")  %>% 
    left_join(.,
        covs_sensi_750 %>% select(bird_sit, siteBA) %>% rename(siteBA750 = siteBA) %>% distinct(),
        by = "bird_sit")

# Convert to long format for ggplot
covs_sensi_long <- covs_sensi %>%
  pivot_longer(cols = c(siteBA250, siteBA500, siteBA750),
               names_to = "radius",
               values_to = "siteBA") %>%
  mutate(radius = case_when(
    radius == "siteBA250" ~ "250m",
    radius == "siteBA500" ~ "500m", 
    radius == "siteBA750" ~ "750m"
  ),
  radius = factor(radius, levels = c("250m", "500m", "750m"))) %>% 
  mutate(park = substr(bird_sit, 1, 4))

# Create ggplot
ggplot(covs_sensi_long, aes(x = bird_sit, y = siteBA, color = radius)) +
  geom_point(size = 2, alpha = 0.4) +
  scale_color_manual(values = c("250m" = "red", "500m" = "blue", "750m" = "darkgreen")) +
  labs(x = "Bird Site", y = "Site Basal Area", 
       title = "Sensitivity Analysis: Basal Area by Radius",
       color = "Radius") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "top") +
  facet_wrap(~park, scales = "free", nrow = 2)
