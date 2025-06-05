# PCA
library(tidyverse)
library(stats)  # for PCA
library(ggbiplot) #For graphing PCA's in ggplot style.

var_site_freq <- read_rds(file = "data/out/var_site_freq.rds")
var_park_freq <- read_rds(file = "data/out/var_park_freq.rds")
var_coun_freq <- read_rds(file = "data/out/var_coun_freq.rds")

# site 
site_data_full <- var_site_freq %>%
  filter(if_all(-c(Point_Name, park), ~ !is.na(.) & is.finite(.)))

site.pca <- prcomp(site_data_full %>% select(-Point_Name, -park), center = TRUE, scale. = TRUE)

ggbiplot(site.pca, groups = site_data_full$park, ellipse = TRUE) +
  scale_color_discrete(name = "park")
  
summary(site.pca)

site.pca$rotation

# park 
park_data_full <- var_park_freq %>%
  filter(if_all(-c( park), ~ !is.na(.) & is.finite(.)))

park.pca <- prcomp(park_data_full %>% select(-park), center = TRUE, scale. = TRUE)

ggbiplot(park.pca, groups = park_data_full$park) +
  scale_color_discrete(name = "park")
  
summary(park.pca)

park.pca$rotation

# coun
coun_data_full <- var_coun_freq %>%
  filter(if_all(-c( park), ~ !is.na(.) & is.finite(.)))

coun.pca <- prcomp(coun_data_full %>% select(-park), center = TRUE, scale. = TRUE)

ggbiplot(coun.pca, groups = coun_data_full$park) +
  scale_color_discrete(name = "park")
  
summary(coun.pca)

coun.pca$rotation
