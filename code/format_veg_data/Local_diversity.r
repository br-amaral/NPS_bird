# *********************************************************************************
# -------------------------------   Amazing Title   -------------------------------
# *********************************************************************************
# Code to ...
#
#
# Source ---------------------------------------------
#           - :
#           - :
#
# Input ----------------------------------------------
#           - :
#           - :
#
# Output ----------------------------------------------
#           - :
#           - :
#
# detach packages and clear workspace
if(!require(freshr)){install.packages('freshr')}
freshr::freshr()
#
# Load packages ---------------------------------------
library(conflicted)
library(tidyverse)
library(glue)
library(vegan)
library(plyr)
#
conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::mutate)
conflicts_prefer(dplyr::arrange)
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
FORSPS_SITE_PATH  <- "data/veg_kateaaron/NETN_tree_dens_spp_2006-2023.csv"

## read files
site_div <- FORSPS_SITE_PATH %>% read_csv()

tree_den_spp_a <- read_csv('data/veg_kateaaron/NETN_Forest_20231106/AdditionalSpecies_NETN.csv') %>% 
     select(Plot_Name, SampleYear, ScientificName)

tree_den_spp <- joinTreeData(status = "live") %>% filter(!ParkUnit %in% "ACAD") %>% 
  select(Plot_Name, SampleYear, ScientificName)  

tree_div <- rbind(tree_den_spp_a, tree_den_spp) %>% 
    mutate(park = substr(Plot_Name, 1, 4))  %>% 
    filter(park != "ACAD") %>%
    filter(park != "SAIR") %>% 
    arrange(park)

year3 <- tree_div$SampleYear %>% unique() %>% sort()
sites <- tree_div$Plot_Name %>% unique() %>% sort()
parks <- tree_div$park %>% unique() %>% sort()

# check which species I should remove (unknows, but keeping the ones that are just the genus):
tree_div <- tree_div %>% filter(str_sub(string = ScientificName, start = 1,end = 7) != "Unknown")

sps <- tree_div$ScientificName %>% unique() %>% sort()

# create empty vector for park and site values of S and J
site_div <- tree_div %>% select(Plot_Name, SampleYear) %>% distinct() %>% mutate(S = NA, J = NA)
park_div <- tree_div %>% select(park, SampleYear) %>% distinct() %>% mutate(S = NA, J = NA)

## For each site:
for(i in 1:nrow(site_div)){
    tree_loop <- tree_div %>% 
        filter(SampleYear == pull(site_div[i,2])) %>% 
        filter(Plot_Name == pull(site_div[i,1])) %>% 
        select(-c(SampleYear, park))

    tree_loopW <- pivot_wider(tree_loop, 
                            names_from = ScientificName, 
                            values_from = ScientificName,
                            values_fn = list(ScientificName = length))

    sum(tree_loopW[,-1], na.rm = TRUE) == nrow(tree_loop)
    tree_loopW[is.na(tree_loopW)] <- 0

    shannon <- ddply(tree_loopW,~Plot_Name,function(x) {
            data.frame(SHANNON=vegan::diversity(x[-1], index="shannon"))
    })

    J <- shannon[,2]/ log(ncol(tree_loopW)-1)

    site_div[i,3] <- shannon[,2]
    site_div[i,4] <- J

}

## For each park:
for(i in 1:nrow(park_div)){
    tree_loop <- tree_div %>% 
        filter(SampleYear == pull(park_div[i,2])) %>% 
        filter(park == pull(park_div[i,1])) %>% 
        select(-c(SampleYear, Plot_Name))

    tree_loopW <- pivot_wider(tree_loop, 
                            names_from = ScientificName, 
                            values_from = ScientificName,
                            values_fn = list(ScientificName = length))

    sum(tree_loopW[,-1], na.rm = TRUE) == nrow(tree_loop)
    tree_loopW[is.na(tree_loopW)] <- 0

    shannon <- ddply(tree_loopW,~park,function(x) {
            data.frame(SHANNON=vegan::diversity(x[-1], index="shannon"))
    })

    J <- shannon[,2]/ log(ncol(tree_loopW)-1)

    park_div[i,3] <- shannon[,2]
    park_div[i,4] <- J

}

write_rds(site_div, "data/out/site_div.rds")
write_rds(park_div, "data/out/park_div.rds")
