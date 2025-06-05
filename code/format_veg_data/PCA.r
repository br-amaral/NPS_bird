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

## nMDS
library(tidyverse)
library(vegan)    # For nMDS
library(ggplot2)  # For plotting

var_site_freq <- read_rds(file = "data/out/var_site_freq.rds")
var_park_freq <- read_rds(file = "data/out/var_park_freq.rds")
var_coun_freq <- read_rds(file = "data/out/var_coun_freq.rds")

# site 
# Start from site_data_full, which still has park and Point_Name
site_data_numeric <- site_data_full %>% select(-Point_Name, -park)
nonzero_rows <- rowSums(site_data_numeric) != 0
site_data_numeric <- site_data_numeric[nonzero_rows, ]

# Run nMDS
site_nmds <- metaMDS(site_data_numeric, k = 2, trymax = 100)

#Stress < 0.05: Excellent representation. 
#Stress < 0.1: Good. 
#Stress < 0.2: Fair. 
#Stress approaching 0.3: Ordination might be arbitrary. 
#Stress = 0: May indicate an outlier or degenerate data, and NMDS might not be suitable. 

site_scores <- as.data.frame(scores(site_nmds)$sites)
site_scores$park <- site_data_full$park[nonzero_rows]

# Plot
ggplot(site_scores, aes(x = NMDS1, y = NMDS2, color = park, fill = park)) +
  geom_point(size = 3, alpha = 0.8) +
  stat_ellipse(geom = "polygon", alpha = 0.2, show.legend = FALSE) +
  theme_bw() +
  labs(title = "nMDS - Site Level") +
  scale_color_discrete(name = "park") +
  scale_fill_discrete(guide = "none")
