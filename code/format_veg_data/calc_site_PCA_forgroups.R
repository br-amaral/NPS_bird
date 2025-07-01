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
#library(sp)
#library(rgdal)
#library(sf)
#library(reshape2)
#library(ggplot2)
#library(ggh4x)
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

#! Import data -----------------------------------------
## file paths
path <- glue("{getwd()}/data/veg_kateaaron") 
importCSV(path, zip_name = "NETN_Forest_20231106.zip")

# coverage by stratum
for_cov <- forestNETN::joinStandData(park = "all") %>%
          as_tibble() %>% 
                 group_by(Plot_Name) %>% 
                 mutate(pct_cro = mean(Pct_Crown_Closure, na.rm = T),
                        pct_low = mean(Pct_Understory_Low, na.rm = T), 
                        pct_mid = mean(Pct_Understory_Mid, na.rm = T), 
                        pct_hig = mean(Pct_Understory_High, na.rm = T), 
                        pct_wat = mean(Pct_Water, na.rm = T), 
                        # Avg_Height_Codom
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
                mutate(BA_m2ha = sum(BA_cm2, na.rm = T)/400,
                       treeden_ha = sum(num_stems, na.rm = T)*25)  %>% #cm2 to m2 cancels out, so just /400m2 plot.
                ungroup() %>% 
                arrange(Plot_Name) %>% 
                select(Plot_Name, ParkUnit, BA_m2ha, treeden_ha) %>% 
                distinct()

div_cov <- joinTreeData(park = 'all', status = "live") %>% 
                as_tibble() %>% 
                group_by(Plot_Name) %>% 
                mutate(tree_rich = n_distinct(ScientificName)) %>%   #!TODO:
                ungroup() %>%
                arrange(Plot_Name) %>% 
                select(Plot_Name, ParkUnit, tree_rich) %>% 
                distinct()

shr_cov <- joinMicroShrubData(park = 'all') %>% 
              filter(#InvasiveNETN == F,
                     Shrub == 1) %>%   #!TODO:
              group_by(Plot_Name) %>% 
              mutate(pct_shr = mean(shrub_avg_cov, na.rm = T)) %>% 
              ungroup() %>% 
              select(Plot_Name, ParkUnit, pct_shr) %>% 
              arrange(Plot_Name) %>% 
              distinct()

stg_cov <- sumStrStage(park = "all") %>% 
              as_tibble() %>% 
              group_by(Plot_Name) %>% 
              mutate(pct_pol = mean(pctBA_pole, na.rm = T),
                     pct_mat = mean(pctBA_mature, na.rm = T),
                     pct_lar = mean(pctBA_large, na.rm = T),
                     stg = Modes(Stage)) %>% 
              ungroup() %>% 
              select(Plot_Name, ParkUnit, pct_pol, pct_mat, pct_lar, stg) %>% 
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

#? Check for NAs 
forest_covs %>% 
  summarise(across(everything(), ~ sum(is.na(.)), .names = "NA_{.col}")) %>% 
  pivot_longer(everything(), names_to = "column", values_to = "na_count") %>% 
  print()

#? Replace NA values with zeros
forest_covs <- forest_covs %>% 
  mutate(across(where(is.numeric), ~ replace_na(.x, 0)))

# Check if NAs are gone
cat("NA counts after replacing with zeros:\n")
forest_covs %>% 
  summarise(across(everything(), ~ sum(is.na(.)), .names = "NA_{.col}")) %>% 
  pivot_longer(everything(), names_to = "column", values_to = "na_count") %>% 
  print()

#? run PCA!!!
# site 

ifelse(is.na(forest_covs$pct_shr), forest_covs$pct_shr <- 0, forest_covs$pct_shr <- forest_covs$pct_shr)
site_data_full <- forest_covs %>%
  select(-for_sit, -xCoordinate, -yCoordinate, -sta_str, -stg#, -pct_shr
              ) %>%
  filter(complete.cases(.))

site_data_full1 <- site_data_full  %>% select(-ParkUnit)

site.pca <- prcomp(site_data_full1, center = TRUE, scale. = TRUE)

ggbiplot(site.pca, groups = site_data_full$ParkUnit, ellipse = TRUE) +
  scale_color_discrete(name = "ParkUnit")
  
summary(site.pca)

site.pca$rotation



