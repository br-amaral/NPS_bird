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
library(forestNETN)
#
conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::mutate)
conflicts_prefer(dplyr::arrange)
conflicts_prefer(dplyr::rename)
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
path <- glue("{getwd()}/data/veg_kateaaron") #"C:/NETN/collaborators/Bruna/"

## read files
importCSV(path, zip_name = "NETN_Forest_20231106.zip")
site_div <- FORSPS_SITE_PATH %>% read_csv()

tree_den_spp_a <- read_csv('data/veg_kateaaron/NETN_Forest_20231106/AdditionalSpecies_NETN.csv') %>% 
    select(Plot_Name, SampleYear, ScientificName)

tree_den_spp <- joinTreeData(status = "live") %>% 
    filter(!ParkUnit %in% "ACAD") %>% 
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

# remove years
site_div_m <- site_div  %>% 
                group_by(Plot_Name) %>%
                mutate(S_mean = mean(S, na.rm = TRUE),
                       J_mean = mean(J, na.rm = TRUE)) %>% 
                select(Plot_Name, S_mean, J_mean) %>%
                distinct()

park_div_m <- park_div %>% 
                group_by(park) %>%
                mutate(S_mean = mean(S, na.rm = TRUE),
                       J_mean = mean(J, na.rm = TRUE)) %>% 
                select(park, S_mean, J_mean) %>%
                distinct()

# join forest sites with bird sites
close_points_f <- read_rds(file = "data/out/close_points_f.rds")  %>% 
    select(for_sit, bird_sit) %>%
    rename(Point_name = bird_sit,
           Plot_Name = for_sit) %>% 
    distinct()

site_div2 <- left_join(close_points_f, site_div_m, by = "Plot_Name") %>% 
                select(-Plot_Name)

write_rds(site_div2, "data/out/site_div.rds")
write_rds(park_div, "data/out/park_div.rds")
