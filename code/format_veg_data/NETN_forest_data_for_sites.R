#? *********************************************************************************
#? ------------------------  NETN_forest_data_for_sites.R  -------------------------
#? *********************************************************************************
#! Code to ...
#
#
#! Source ---------------------------------------------
#           - :
#           - :
#
#! Input ----------------------------------------------
#           - NETN_Forest_20231106.zip : folder with all the forest data for the parks
#           - :
#
#! Output ----------------------------------------------
#           - data/veg_kateaaron/NETN_forest_data_2006-2023.rds : site forest covariates
#           - data/veg_kateaaron/for_sites.rds : name of all the sites with they XY coordinates
#           - data/veg_kateaaron/NETN_tree_dens_spp_2006-2023.rds : tree species abundance
#           - data/veg_kateaaron/NETN_forest_metadata.csv : meta data with info of the columns of the forest variables
#
# detach packages and clear workspace
freshr::freshr()
#
#? Load packages ---------------------------------------
library(tidyr)
library(conflicted)
library(tidyverse)
library(glue)
library(forestNETN)
#
conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::filter)
# conflicts_prefer(scales::alpha)
#
#? Make functions --------------------------------------
colanmes <- colnames
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

#
#? Source code -----------------------------------------
#
#? Import data -----------------------------------------
## file paths
path <- glue("{getwd()}/data/veg_kateaaron") #"C:/NETN/collaborators/Bruna/"

## read files
importCSV(path, zip_name = "ForestNETN2024.zip")

tree_cat <- read_csv("data/tree_sps_harcon.csv")

#? Unique Forest Plots
plots <- joinLocEvent() %>%
  select(Plot_Name, ParkUnit, X = xCoordinate, Y = yCoordinate, UTMZone = ZoneCode) %>% 
  distinct()

#? Density and Basal Area
# by species
table(table(joinTreeData(status = "live") %>% select(Plot_Name, SampleYear))>1)

tree_den_spp <- joinTreeData(status = "live") %>% 
                    filter(ParkUnit %!in% c("ACAD", "ELRO", "SAIR"),
                                  ScientificName != "None present") %>% 
                    group_by(Plot_Name, SampleYear, ScientificName) %>%
                    summarize(treeden_ha = sum(num_stems, na.rm = T)*25, # conversion to stems/ha = 10000/400
                              BA_m2ha = sum(BA_cm2, na.rm = T)/400) %>%  # cm2 to m2 cancels out, so just /400m2 plot.
                    group_by(Plot_Name, ScientificName) %>%
                    summarize(treeden_ha = mean(treeden_ha, na.rm = T),  
                              BA_m2ha = mean(BA_m2ha, na.rm = T))
                              
# total
tree_den <- tree_den_spp %>% 
                    group_by(Plot_Name) %>%
                    summarize(treeden_ha = sum(treeden_ha, na.rm = T),  
                              BA_m2ha = sum(BA_m2ha, na.rm = T))

tree_den %>% 
          mutate(park = substr(Plot_Name, 1, 4)) %>% 
          group_by(park) %>% 
          summarise(treeden_ham = mean(treeden_ha, na.rm =T),
          BA_m2ham = mean(BA_m2ha, na.rm = T))


# by size class: classify each tree accoding to DBH in BA and density of pole, mature, and large
# size classes are 10-25.9 cm DBH (pole), 26-45.9 cm DBH (mature) and ≥ 46 cm DBH (large).
tree_den_sizeclass <- joinTreeData(status = "live") %>% 
                          filter(ParkUnit %!in% c("ACAD", "ELRO", "SAIR"),
                                  ScientificName != "None present") %>% 
                          mutate(size_class = case_when(
                                  DBHcm >= 10 & DBHcm < 26 ~ "pole",
                                  DBHcm >= 26 & DBHcm < 46 ~ "mature", 
                                  DBHcm >= 46 ~ "large",
                                  TRUE ~ "other"  # for DBH < 10cm or any other cases
                              )) %>%
                          group_by(Plot_Name, SampleYear, size_class) %>%
                          summarize(treeden_ha = sum(num_stems, na.rm = T)*25, # conversion to stems/ha = 10000/400
                                    BA_m2ha = sum(BA_cm2, na.rm = T)/400) %>%  # cm2 to m2 cancels out, so just /400m2 plot.
                          group_by(Plot_Name, size_class) %>%
                          summarize(treeden_ha = mean(treeden_ha, na.rm = T),  
                                    BA_m2ha = mean(BA_m2ha, na.rm = T)) %>% 
                          ungroup() %>% 
                          distinct()

tree_den_sizeclass  %>%  filter(size_class == "other")

tree_sizeclass <- tree_den_sizeclass %>%
  complete(Plot_Name, size_class = c("pole", "mature", "large"), 
           fill = list(treeden_ha = 0, BA_m2ha = 0)) %>%
  pivot_wider(names_from = size_class, values_from = c(treeden_ha, BA_m2ha))
# Only olds: BA > 60? Kate's suggestion

#? Percent conifer and hardwood
conha_cov <- joinTreeData(park = 'all', status = "live") %>% 
                as_tibble() 

tree_sps <- as_tibble(sort(unique((conha_cov$ScientificName))))  %>% 
                rename(ScientificName = value) %>% 
                mutate(genus = word(ScientificName, 1)) %>% 
                filter(genus != "Unknown",
                       genus != "None")

tree_sps$genus %>% unique()

tree_sps <- left_join(tree_sps, tree_cat, by = "genus")

# tre_cov_test <- left_join(tre_cov %>% rename(sps = ScientificName), tree_sps, by = "sps")  %>% 
#                 select(Plot_Name, Network, ParkUnit, SampleYear,
#                     sps, genus, type, BA_cm2) %>% 
#         filter(Plot_Name == "MABI-013") 

# table(tre_cov_test$genus)

# table(tre_cov %>% 
#         filter(Plot_Name == "MABI-013")  %>% select(SampleYear))

har_con <- joinTreeData(status = "live") %>% 
                filter(ParkUnit %!in% c("ACAD", "ELRO", "SAIR"),
                       ScientificName != "None present") %>% 
                left_join(., tree_sps  %>% select(-genus), by = "ScientificName") %>% 
                group_by(Plot_Name, SampleYear, type) %>%
                summarize(treeden_ha = sum(num_stems, na.rm = T)*25, # conversion to stems/ha = 10000/400
                          BA_m2ha = sum(BA_cm2, na.rm = T)/400) %>%
                arrange(Plot_Name, type)  %>% 
                group_by(Plot_Name, type) %>% 
                summarize(treeden_ha = mean(treeden_ha, na.rm = T),  
                          BA_m2ha = mean(BA_m2ha, na.rm = T)) %>% 
                ungroup()

# fill in 0% of conifer or hardwood in plots with only one forest type
all_plots <- sort(unique(har_con$Plot_Name))
all_types <- c("Conifer", "Hardwood")

# Complete the har_con data to include all combinations
har_con <- har_con %>%
              complete(Plot_Name = all_plots, 
                      type = all_types, 
                      fill = list(treeden_ha = 0, BA_m2ha = 0)) %>% 
              group_by(Plot_Name) %>% 
              mutate(per_den = treeden_ha[type == "Conifer"] / sum(treeden_ha),
                    per_BA = BA_m2ha[type == "Conifer"] / sum(BA_m2ha)) %>%
              ungroup() %>% 
              pivot_wider(names_from = "type", values_from = c("treeden_ha", "BA_m2ha",
                                                               "per_den", "per_BA")) %>% 
              select(-per_den_Hardwood, -per_BA_Hardwood) %>% 
              rename(BA_m2ha_perc_con = per_BA_Conifer)

#? percentage of each state
table(table(sumStrStage() %>% select(Plot_Name, SampleYear))>1)

stand <- sumStrStage() %>% 
            select(Plot_Name, SampleYear, Stage, pctBA_pole, pctBA_mature, pctBA_large) %>% 
             group_by(Plot_Name, SampleYear) %>%
                summarize(Stage = Modes(Stage),
                          pctBA_pole = mean(pctBA_pole), 
                          pctBA_mature = mean(pctBA_mature), 
                          pctBA_large = mean(pctBA_large)) %>% 
             group_by(Plot_Name) %>%
                summarize(Stage = Modes(Stage),
                          #test = sum(pctBA_pole, pctBA_mature, pctBA_large),
                          pctBA_pole = mean(pctBA_pole), 
                          pctBA_mature = mean(pctBA_mature), 
                          pctBA_large = mean(pctBA_large))

#? density of sapling 
table(table(joinRegenData(units = "sq.m") %>% select(Plot_Name, SampleYear))>1)

reg <- joinRegenData(units = "sq.m") %>% 
          select(Plot_Name, SampleYear, seed_den, sap_den, regen_den) %>%
          group_by(Plot_Name, SampleYear) %>%
                    summarize(seed_den = sum(seed_den, na.rm = T), 
                              sap_den = sum(sap_den, na.rm = T), 
                              regen_den = sum(regen_den, na.rm = T)) %>%  
          group_by(Plot_Name) %>%
          summarize(seed_den_m2 = mean(seed_den, na.rm = T),
                    sap_den_m2 = mean(sap_den, na.rm = T),
                    regen_den_m2 = mean(regen_den, na.rm = T))

table(table(joinMicroShrubData() %>% select(Plot_Name, SampleYear))>1)
table(joinMicroShrubData() %>% select(InvasiveNETN))
table(joinMicroShrubData() %>% select(Shrub))
table(joinMicroShrubData() %>% select(Vine))

#? shrub
# shrub2 <- joinMicroShrubData() %>%  
#             as_tibble() %>% 
#             filter(Shrub == 1,
#                    ParkUnit %!in% c("ACAD", "ELRO", "SAIR")) %>% # shrub and vine? to match FIA - forbes?
#             select(Plot_Name, SampleYear, shrub_avg_cov) %>% 
#             group_by(Plot_Name) %>%
#             summarize(shrub_avg_cov = mean(shrub_avg_cov, na.rm = T)) %>% 
#             mutate(across(everything(), ~replace_na(.x, 0)))

            #select(Plot_Name, SampleYear, Shrub, shrub_avg_cov, Exotic, InvasiveNETN) %>% 
            # mutate(test = ifelse(Exotic == InvasiveNETN, T, F))  %>% 
            # mutate(non_nat = (Exotic == TRUE | InvasiveNETN == TRUE)) %>% 
# shrub[,c(29,24,25,26,28)] %>% distinct()
            # summarize(shrub_avg_cov = sum(shrub_avg_cov, na.rm = T)) 
            # group_by(Plot_Name, SampleYear, non_nat) %>%
            # summarize(shrub_avg_cov = sum(shrub_avg_cov, na.rm = T)) %>%  
            # group_by(Plot_Name, non_nat) %>%
            # summarize(shrub_avg_cov = mean(shrub_avg_cov, na.rm = T))  %>% 
            # pivot_wider(names_from = non_nat, 
            #             values_from = shrub_avg_cov, 
            #             names_prefix = "shrub_cov_invasive_") %>% 
            # mutate(across(everything(), ~replace_na(.x, 0))) %>% 
            # rename(shrub_cov_nat = shrub_cov_invasive_FALSE,
            #        shrub_cov_nonat = shrub_cov_invasive_TRUE)

shrub_cats <- as_tibble(cbind(CoverClassLabel = rbind( "0%", "1-5%", "5-25%", "25-50%", "50-75%", "75-95%", "95-100%"),
                        shrub_avg_cov = rbind(0, 
                                              mean(c(1, 5))/100, 
                                              mean(c(5, 25))/100, 
                                              mean(c(25, 50))/100, 
                                              mean(c(50, 75))/100, 
                                              mean(c(75, 95))/100, 
                                              mean(c(95, 100))/100))) %>% 
                        rename(CoverClassLabel = V1, shrub_avg_cov = V2) %>% 
                        mutate(shrub_avg_cov = as.numeric(shrub_avg_cov))

shrub <- VIEWS_NETN$StandPlantCoverStrata_NETN  %>% 
            as_tibble() %>% 
            filter(StrataLabel %in% c("Ground", "Mid-understory"),
                   ParkUnit %!in% c("ACAD", "ELRO", "SAIR"),
                   CoverClassLabel != "Permanently Missing") %>% 
            select(Plot_Name, SampleYear, CoverClassCode, CoverClassLabel) %>% 
            # get the mode for the intervals of forest percentage
            left_join(., shrub_cats, by = "CoverClassLabel") %>% 
            group_by(Plot_Name) %>%
            summarize(shrub_avg_cov = mean(shrub_avg_cov, na.rm = T)) %>% 
            mutate(across(everything(), ~replace_na(.x, 0)))

#? Coarse wood debris?
cwd <- joinCWDData(park = 'all') %>% # coarse wood debris
          as_tibble() %>%        
          filter(ParkUnit %!in% c("ACAD", "ELRO", "SAIR"),
                 ScientificName != "None present") %>%    
          select(Plot_Name, SampleYear, ParkUnit, CWD_Vol) %>% 
          group_by(Plot_Name, SampleYear) %>% 
          summarize(deb_m = sum(CWD_Vol, na.rm = T)) %>% 
          group_by(Plot_Name) %>% 
          summarize(cwd = mean(deb_m, na.rm = T)) %>%
          distinct()

#? Combine data 
comb <- full_join(plots , tree_den, by = "Plot_Name") %>% 
              full_join(., shrub, by = "Plot_Name") %>% 
              full_join(., har_con, by = "Plot_Name") %>% 
              full_join(., reg, by = "Plot_Name") %>% 
              full_join(., stand, by = "Plot_Name") %>% 
              full_join(., tree_sizeclass, by = "Plot_Name") %>% 
              full_join(., cwd, by = "Plot_Name") %>% 
              as_tibble() %>% 
              filter(ParkUnit %in% c("MABI", "MIMA", "MORR", "SAGA", "SARA", "ROVA", "WEFA"))

comb %>% DT::datatable()

table(comb$ParkUnit)
#table(comb %>% filter(!is.na(shrub_cov_nat)) %>% select(ParkUnit))  # SAGA has a lot of NAs here

comb_sites <- comb %>% 
    select(Plot_Name, X, Y, UTMZone) %>% 
    distinct() %>% 
    arrange()

write_rds(comb, file = "data/out/for_plot_covs.rds")

# # Brief metadata for each column name
# metadata <- data.frame(column = names(comb), 
#                        description = c("Unique name of the plot", 
#                                        "Year plot was sampled",
#                                        "4-letter park code",
#                                        "X coordinate in UTM NAD83",
#                                        "Y coordinate in UTM NAD83",
#                                        "UTM Zone- either 18N or 19N",
#                                        "Density of live trees >=10cm DBH in stems/ha",
#                                        "Basal area of live trees >=10cm DBH in m2/ha",
#                                        "Number of species in the live tree strata per 400m2 plot",
#                                        "Structural stage of tree stand",
#                                        "percent BA in pole size (>=10 and < 26cm)",
#                                        "percent BA in mature size (>=26 and < 45.9cm)",
#                                        "percent BA in large size (>=45.9cm DBH)",
#                                        "sapling density in stems/m2",
#                                        "Average percent shrub cover. Note that we started collecting this in 2010, the start of cycle 2."
#                                        ))

# # Write to file
# write_rds(comb, paste0(path, "/", "NETN_forest_data_2006-2023.rds"))
# write_rds(comb_sites, paste0(path, "/", "for_sites.rds"))
# write_rds(tree_den_spp, paste0(path, "/", "NETN_tree_dens_spp_2006-2023.rds"))
# write.csv(metadata, paste0(path, "/", "NETN_forest_metadata.csv"), row.names = F)

site_data_full <- comb %>%
                      filter(complete.cases(.)) %>%
                      filter(if_all(where(is.numeric), is.finite))

site.pca <- prcomp(site_data_full %>% select(-Plot_Name, -ParkUnit, -X, -Y, -UTMZone, -Stage), center = TRUE, scale. = TRUE)

ggbiplot::ggbiplot(site.pca, groups = site_data_full$ParkUnit, ellipse = TRUE) +
  scale_color_discrete(name = "ParkUnit") +
  theme_bw()
  
summary(site.pca)

site.pca$rotation
