#? *********************************************************************************
#? ------------------------------   get_conhar_baden.R   ------------------------------
#? *********************************************************************************
#! Code to get the percentage of conifer and hardwood basal area in each forest plot
#
#
#! Source ---------------------------------------------
#           - :
#           - :
#
#! Input ----------------------------------------------
#           - data/tree_sps_harcon.csv :
#           - NETN_Forest_20231106.zip :
#
#! Output ----------------------------------------------
#           - :
#
# detach packages and clear workspace
# freshr::freshr()
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
library(forestNETN)  # remotes::install_github("KateMMiller/forestNETN")

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

tree_cat <- read_csv("data/tree_sps_harcon.csv")


tre_cov <- joinTreeData(park = 'all', status = "live") %>% 
                as_tibble() 

tree_sps <- as_tibble(sort(unique((tre_cov$ScientificName))))  %>% 
                rename(sps = value) %>% 
                mutate(genus = word(sps, 1)) %>% 
                filter(genus != "Unknown",
                       genus != "None")

tree_sps$genus %>% unique()

tree_sps <- left_join(tree_sps, tree_cat, by = "genus")

tre_cov2 <- left_join(tre_cov %>% rename(sps = ScientificName), tree_sps, by = "sps")  %>% 
                select(Plot_Name, Network, ParkUnit, SampleYear,
                    sps, genus, type, BA_cm2)  %>% 
                filter(ParkUnit %!in% c("ACAD", "ELRO", "SAIR")) %>% 
                group_by(Plot_Name, type) %>% 
                mutate(BA_m2ha = sum(BA_cm2, na.rm = T)/400,
                       density = n()) %>% 
                select(Plot_Name, type, BA_m2ha, density) %>% 
                distinct() %>% 
                rename(PlotID = Plot_Name)

tre_cov3 <- tre_cov2 %>% 
              group_by(PlotID) %>%
              mutate(type_plot = type[which.max(BA_m2ha)])

ggplot(data = tre_cov3) +
        geom_point(aes(x = density, y = BA_m2ha, color = type_plot))

