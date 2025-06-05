#? *********************************************************************************
#? ------------------------------   get_site_data.R   ------------------------------
#? *********************************************************************************
#! Code to ...
#
#
#! Source ---------------------------------------------
#           - :
#           - :
#
#! Input ----------------------------------------------
#           - data/out/for_sit_covs_nei.rds : matrix with info on who's is who's neighbour (forest plot and bird site)
#
#! Output ----------------------------------------------
#           - data/out/close_points_f.rds : tibble with combinations of forest and bird sites, and the distances between them
#
# detach packages and clear workspace
freshr::freshr()
#
#! Load packages ---------------------------------------
library(conflicted)
library(tidyverse)
library(glue)
library(sp)
#library(rgdal)
library(sf)
library(reshape2)
library(ggplot2)
library(ggh4x)
#library("MetBrewer")
library(forestNETN)
library(ggbiplot) #For graphing PCA's in ggplot style.

conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::filter)
#conflicts_prefer(scales::alpha)

#! Make functions --------------------------------------
colanmes <- colnmaes <- colnames
lenght <- length
`%!in%` <- Negate(`%in%`)

Modes <- function(x) {
  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  if(length(ux[tab == max(tab)]) > 1) {
    sample(ux[tab == max(tab)], 1)
    } else {
      ux[tab == max(tab)]}
}
#! Define settings -------------------------------------
radi_dist <- 500

#! Import data -----------------------------------------
## file paths
NEIGH_FILE_PATH <- glue("data/out/neighbor_grp_{radi_dist}m.rds")

neighbor <- read_rds(file = NEIGH_FILE_PATH)

path <- glue("{getwd()}/data/veg_kateaaron") 
importCSV(path, zip_name = "NETN_Forest_20231106.zip")

for_cov <- forestNETN::joinStandData(park = "all") %>%
          as_tibble()  %>% 
                 group_by(Plot_Name) %>% 
                 mutate(pct_cro = mean(Pct_Crown_Closure, na.rm = T),
                        pct_low = mean(Pct_Understory_Low, na.rm = T), 
                        pct_mid = mean(Pct_Understory_Mid, na.rm = T), 
                        pct_hig = mean(Pct_Understory_High, na.rm = T), 
                        pct_wat = mean(Pct_Water, na.rm = T), 
                        sta_str = Modes(Stand_Structure)) %>% 
                 ungroup() %>% 
                 arrange(Plot_Name) %>% 
                 select(Plot_Name, ParkUnit, xCoordinate, yCoordinate,
                        pct_cro, pct_low, pct_mid, pct_hig, sta_str) %>% 
                distinct()
 
cwd_cov <- joinCWDData(park = 'all') %>% # coarse wood debris
                as_tibble() %>% 
                group_by(Plot_Name) %>% 
                mutate(CWD_vol_mean = mean(CWD_Vol, na.rm = T)) %>%   #!TODO: any calculations like for BA?
                ungroup() %>%                 
                arrange(Plot_Name) %>% 
                select(Plot_Name, ParkUnit, CWD_vol_mean) %>%  
                distinct()

tre_cov <- joinTreeData(park = 'all', status = "live") %>% 
                as_tibble() %>% 
                group_by(Plot_Name) %>% 
                mutate(BA_m2ha = sum(BA_cm2, na.rm = T)/400)  %>% #cm2 to m2 cancels out, so just /400m2 plot.
                ungroup() %>% 
                arrange(Plot_Name) %>% 
                select(Plot_Name, ParkUnit, BA_m2ha, ScientificName) %>% 
                distinct()

div_cov <- for_tre %>% 
                group_by(Plot_Name) %>% 
                mutate(tree_rich = n_distinct(ScientificName)) %>%   #!TODO:
                ungroup() %>%
                arrange(Plot_Name) %>% 
                select(Plot_Name, ParkUnit, tree_rich) %>% 
                distinct()

tre_cov <- tre_cov  %>% 
             select(-ScientificName) %>% 
                distinct()

shr_cov <- joinMicroShrubData(park = 'all') %>% 
              filter(InvasiveNETN == T,
                     Shrub == 1) %>%   #!TODO:
              group_by(Plot_Name) %>% 
              mutate(pct_shr = mean(shrub_pct_freq, na.rm = T)) %>% 
              ungroup() %>% 
              select(Plot_Name, ParkUnit, pct_shr) %>% 
              arrange(Plot_Name) %>% 
              distinct()

stg_cov <- sumStrStage(park = "all") %>% 
              as_tibble() %>% 
              mutate(pct_sum1 = rowSums(across(c(pctBA_mature, pctBA_large)), na.rm = TRUE)) %>% 
              group_by(Plot_Name) %>% 
              mutate(pct_sum = mean(pct_sum1, na.rm = T),
                     pct_mat = mean(pctBA_mature, na.rm = T),
                     pct_lar = mean(pctBA_large, na.rm = T),
                     stg = Modes(Stage)) %>% 
              ungroup() %>% 
              select(Plot_Name, ParkUnit, pct_sum, pct_mat, pct_lar, stg) %>% 
              distinct()  %>% 
              arrange(Plot_Name)

#? merge all plots data and connect plot data with bird data - neighbours
forest_covs <- left_join(for_cov, cwd_cov, by = c("Plot_Name", "ParkUnit")) %>% 
                     left_join(., tre_cov, by = c("Plot_Name", "ParkUnit")) %>% 
                     left_join(., div_cov, by = c("Plot_Name", "ParkUnit")) %>% 
                     left_join(., shr_cov, by = c("Plot_Name", "ParkUnit")) %>% 
                     left_join(., stg_cov, by = c("Plot_Name", "ParkUnit"))  %>% 
                     filter(ParkUnit %!in% c("ACAD", "ELRO", "SAIR"))  %>%
                     rename(for_sit = Plot_Name)


bird_covs <- left_join(neighbor, forest_covs, by = "for_sit")  %>% 
                     relocate(bird_sit)


#? run PCA!!!
# site 
site_data_full <- bird_covs %>%
  select(-bird_sit, -for_sit, -xCoordinate, -yCoordinate, -sta_str, -stg) %>%
  filter(complete.cases(.))

site_data_full1 <- site_data_full  %>% select(-ParkUnit)

site.pca <- prcomp(site_data_full1, center = TRUE, scale. = TRUE)

ggbiplot(site.pca, groups = site_data_full$ParkUnit, ellipse = TRUE) +
  scale_color_discrete(name = "ParkUnit")
  
summary(site.pca)

site.pca$rotation

# park INCORRECT FOR NOW!!!!!!!!
park_data_full <- site_data_full  %>% 
                    group_by(ParkUnit) %>% 
                    mutate(pct_cro = mean(pct_cro, na.rm = T),
                           pct_low = mean(pct_low, na.rm = T),
                           pct_mid = mean(pct_mid, na.rm = T),
                           pct_hig  = mean(pct_hig, na.rm = T),
                           CWD_vol_mean  = mean(CWD_vol_mean, na.rm = T),
                           BA_m2ha  = mean(BA_m2ha, na.rm = T),
                           tree_rich  = mean(tree_rich, na.rm = T),
                           pct_shr  = mean(pct_shr, na.rm = T),
                           pct_sum  = mean(pct_sum, na.rm = T),
                           pct_mat  = mean(pct_mat, na.rm = T),
                           pct_lar = mean(pct_lar, na.rm = T))  %>% 
                    ungroup() %>% 
                    distinct()

park_data_full1 <- park_data_full  %>% select(-ParkUnit) 


park.pca <- prcomp(park_data_full1, center = TRUE, scale. = TRUE)

ggbiplot(park.pca, groups = park_data_full$ParkUnit, ellipse = TRUE) +
  scale_color_discrete(name = "ParkUnit")
  
summary(park.pca)

park.pca$rotation