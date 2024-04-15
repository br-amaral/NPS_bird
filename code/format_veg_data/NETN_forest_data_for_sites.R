#? *********************************************************************************
#? -------------------------------   Amazing Title   -------------------------------
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
if(!require(freshr)){install.packages('freshr')}
freshr::freshr()
#
# Load packages ---------------------------------------
library(conflicted)
library(tidyverse)
library(glue)
library(forestNETN)
#
conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::filter)
# conflicts_prefer(scales::alpha)
#
# Make functions --------------------------------------
colanmes <- colnames
lenght <- length
`%!in%` <- Negate(`%in%`)
#
# Source code -----------------------------------------
#
# Import data -----------------------------------------
## file paths
path <- glue("{getwd()}/data/veg_kateaaron") #"C:/NETN/collaborators/Bruna/"

## read files
importCSV(path, zip_name = "NETN_Forest_20231106.zip")

plots <- joinLocEvent() |> filter(!ParkUnit %in% "ACAD") |> 
  select(Plot_Name, SampleYear, ParkUnit, X = xCoordinate, Y = yCoordinate, UTMZone = ZoneCode)

tree_den_spp <- joinTreeData(status = "live") |> filter(!ParkUnit %in% "ACAD") |> 
  group_by(Plot_Name, SampleYear, ScientificName) |> 
  summarize(treeden_ha = sum(num_stems, na.rm = T)*25, # conversion to stems/ha = 10000/400
            BA_m2ha = sum(BA_cm2, na.rm = T)/400) #cm2 to m2 cancels out, so just /400m2 plot.

tree_rich <- tree_den_spp |> group_by(Plot_Name, SampleYear) |> summarize(tree_rich = sum(!is.na(ScientificName)))

tree_den <- joinTreeData(status = "live") |> filter(!ParkUnit %in% "ACAD") |> 
  group_by(Plot_Name, SampleYear) |> 
  summarize(treeden_ha = sum(num_stems, na.rm = T)*25, # conversion to stems/ha = 10000/400
            BA_m2ha = sum(BA_cm2, na.rm = T)/400) #cm2 to m2 cancels out, so just /400m2 plot.

stand <- sumStrStage() |> filter(!ParkUnit %in% "ACAD") |> select(Plot_Name, SampleYear, Stage, pctBA_pole, pctBA_mature, pctBA_large) 

reg <- joinRegenData(units = "sq.m") |> filter(!ParkUnit %in% "ACAD") |> 
  select(Plot_Name, SampleYear, sap_den) |> 
  group_by(Plot_Name, SampleYear) |> 
  summarize(sap_den_m2 = sum(sap_den, na.rm = T))

shrub <- joinMicroShrubData() |> filter(!ParkUnit %in% "ACAD") |> 
  select(Plot_Name, SampleYear, shrub_avg_cov) |> 
  group_by(Plot_Name, SampleYear) |> 
  summarize(shrub_cov = sum(shrub_avg_cov)) 

# Combine data 
comb <- purrr::reduce(list(plots, tree_den, tree_rich, stand, reg, shrub), left_join, by = c("Plot_Name", "SampleYear"))
comb <- as_tibble(comb)

comb_sites <- comb %>% 
    select(Plot_Name, X, Y ) %>% 
    distinct() %>% 
    arrange()

# Write to file
write_rds(comb, paste0(path, "NETN_forest_data_2006-2023.rds"))
write_rds(comb_sites, paste0(path, "for_sites.rds"))
write_rds(tree_den_spp, paste0(path, "NETN_tree_dens_spp_2006-2023.rds"))

# Brief metadata for each column name
metadata <- data.frame(column = names(comb), 
                       description = c("Unique name of the plot", 
                                       "Year plot was sampled",
                                       "4-letter park code",
                                       "X coordinate in UTM NAD83",
                                       "Y coordinate in UTM NAD83",
                                       "UTM Zone- either 18N or 19N",
                                       "Density of live trees >=10cm DBH in stems/ha",
                                       "Basal area of live trees >=10cm DBH in m2/ha",
                                       "Number of species in the live tree strata per 400m2 plot",
                                       "Structural stage of tree stand",
                                       "percent BA in pole size (>=10 and < 26cm)",
                                       "percent BA in mature size (>=26 and < 45.9cm)",
                                       "percent BA in large size (>=45.9cm DBH)",
                                       "sapling density in stems/m2",
                                       "Average percent shrub cover. Note that we started collecting this in 2010, the start of cycle 2."
                                       ))

write.csv(metadata, paste0(path, "NETN_forest_metadata.csv"), row.names = F)

