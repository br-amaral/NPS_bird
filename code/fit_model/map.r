
library(tidyverse)
library(knitr)   
library(broom)
library(stringr)
library(modelr)
library(forcats)
library(ggmap)
library(glue)
library(rgeos) # For gCentroid
library(ggplot2)
 
httpgd::hgd()
 
states <- map_data("state")
counties <- map_data("county")
 
# 'HOFR', 'Dutchess County', 'New York',
# 'MABI', 'Windsor County', 'Vermont',
# 'MIMA', 'Middlesex County', 'Massachusetts',
# 'MORR', 'Morris County', 'New Jersey',
# 'SAGA', 'Sullivan County', 'New Hampshire',
# 'SARA', 'Saratoga County', 'New York',
# 'VAMA', 'Dutchess County', 'New York',
# 'WEFA', 'Fairfield County', 'Connecticut'
 
# Corrected subset using the 'region' column
northeast <- subset(states, region %in% c("maine", "new york", "vermont",
                                        "massachusetts", "new jersey", "new hampshire",
                                        "connecticut", "rhode island"))
 
# Filter for Dutchess County in New York
# Filter for specific combinations of counties and states
counties_ne <- counties %>%
  filter((subregion == "dutchess" & region == "new york") |
         (subregion == "windsor" & region == "vermont") |
         (subregion == "middlesex" & region == "massachusetts") |
         (subregion == "morris" & region == "new jersey") |
         (subregion == "sullivan" & region == "new hampshire") |
         (subregion == "saratoga" & region == "new york") |
         (subregion == "fairfield" & region == "connecticut"))
 
park_bound <- read_rds("data/out/park_bound.rds")
 
parks <- readRDS(file = "data/src/key_park.rds") %>%
  dplyr::select(parks) %>%
  distinct() %>%
  pull()
 
parks <- parks[parks != "ACAD"]
parks <- parks[parks != "SAIR"]
parks <- parks[parks != "ELRO"]
 
for(i in 1:length(parks)){
  pb <- subset(park_bound, UNIT_CODE == parks[i])
  name3 <- glue("{parks[i]}_pb")
  assign(name3, pb)
}
 
# Initialize an empty data frame to store centroids
all_centroids <- data.frame(long = numeric(), lat = numeric(), park = character())
 
# Loop through each park to calculate centroids and store them
for (i in 1:length(parks)) {
  # Subset the park boundary
  pb <- subset(park_bound, UNIT_CODE == parks[i])
 
  # Calculate the centroid
  centroid <- rgeos::gCentroid(pb, byid = TRUE)
 
  # Convert the centroid to a data frame
  centroid_df <- as.data.frame(centroid@coords)
  colnames(centroid_df) <- c("long", "lat")
  centroid_df$park <- parks[i] # Add park name
 
  # Append to the main centroids data frame
  all_centroids <- rbind(all_centroids, centroid_df)
}
 
all_centroids <- all_centroids %>%
                    rename(Parks = park) %>%
                    arrange(Parks) %>%
                    distinct()
 
# Plot all parks and their centroids
ggplot(data = northeast, mapping = aes(x = long, y = lat, group = group)) +
  coord_fixed(1.3) +
  theme_bw() +
  theme(legend.title = element_text(hjust = 0.5, size = 16),
        legend.text = element_text(size = 11),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14)) +
  geom_polygon(color = "black", fill = "#D5D5DE") + # Background map
  geom_polygon(data = counties_ne,
               mapping = aes(x = long, y = lat, group = group),
               color = "black", fill = "#088A0F") + # Counties layer
geom_point(data = all_centroids,
           mapping = aes(x = long, y = lat, fill = Parks), # Use 'fill' for interior color
           inherit.aes = FALSE, # Prevent inheriting 'group' from ggplot()
           size = 3, shape = 21, color = "black") + # Shape 21 allows for outline and fill
  labs(x = "Longitude", y = "Latitude") +
  scale_color_viridis_d(name = "Parks") # Add a legend for parks
 
ggplot(data = states, mapping = aes(x = long, y = lat, group = group)) +
  coord_fixed(1.3) +
  theme_void() +
  geom_polygon(color = "#DFE6E6", fill = "#DFE6E6") +
  geom_polygon(data = northeast, mapping = aes(x = long, y = lat, group = group),
               color = "#D5D5DE", fill = "#D5D5DE")
 

library(tidyverse)
library(knitr)   
library(broom)
library(stringr)
library(modelr)
library(forcats)
library(ggmap)
library(glue)
library(rgeos) # For gCentroid
library(ggplot2)
 
httpgd::hgd()
 
states <- map_data("state")
counties <- map_data("county")
 
# 'HOFR', 'Dutchess County', 'New York',
# 'MABI', 'Windsor County', 'Vermont',
# 'MIMA', 'Middlesex County', 'Massachusetts',
# 'MORR', 'Morris County', 'New Jersey',
# 'SAGA', 'Sullivan County', 'New Hampshire',
# 'SARA', 'Saratoga County', 'New York',
# 'VAMA', 'Dutchess County', 'New York',
# 'WEFA', 'Fairfield County', 'Connecticut'
 
# Corrected subset using the 'region' column
northeast <- subset(states, region %in% c("maine", "new york", "vermont",
                                        "massachusetts", "new jersey", "new hampshire",
                                        "connecticut", "rhode island"))
 
# Filter for Dutchess County in New York
# Filter for specific combinations of counties and states
counties_ne <- counties %>%
  filter((subregion == "dutchess" & region == "new york") |
         (subregion == "windsor" & region == "vermont") |
         (subregion == "middlesex" & region == "massachusetts") |
         (subregion == "morris" & region == "new jersey") |
         (subregion == "sullivan" & region == "new hampshire") |
         (subregion == "saratoga" & region == "new york") |
         (subregion == "fairfield" & region == "connecticut"))
 
park_bound <- read_rds("data/out/park_bound.rds")
 
parks <- readRDS(file = "data/src/key_park.rds") %>%
  dplyr::select(parks) %>%
  distinct() %>%
  pull()
 
parks <- parks[parks != "ACAD"]
parks <- parks[parks != "SAIR"]
parks <- parks[parks != "ELRO"]
 
for(i in 1:length(parks)){
  pb <- subset(park_bound, UNIT_CODE == parks[i])
  name3 <- glue("{parks[i]}_pb")
  assign(name3, pb)
}
 
# Initialize an empty data frame to store centroids
all_centroids <- data.frame(long = numeric(), lat = numeric(), park = character())
 
# Loop through each park to calculate centroids and store them
for (i in 1:length(parks)) {
  # Subset the park boundary
  pb <- subset(park_bound, UNIT_CODE == parks[i])
 
  # Calculate the centroid
  centroid <- rgeos::gCentroid(pb, byid = TRUE)
 
  # Convert the centroid to a data frame
  centroid_df <- as.data.frame(centroid@coords)
  colnames(centroid_df) <- c("long", "lat")
  centroid_df$park <- parks[i] # Add park name
 
  # Append to the main centroids data frame
  all_centroids <- rbind(all_centroids, centroid_df)
}
 
all_centroids <- all_centroids %>%
                    arrange(lat) %>%
                    distinct()
 
# Plot all parks and their centroids
ggplot(data = northeast, mapping = aes(x = long, y = lat, group = group)) +
  coord_fixed(1.3) +
  theme_bw() +
  geom_polygon(color = "black", fill = "#D5D5DE") + # Background map
  geom_polygon(data = counties_ne,
               mapping = aes(x = long, y = lat, group = group),
               color = "black", fill = "#088A0F") + # Counties layer
geom_point(data = all_centroids,
           mapping = aes(x = long, y = lat, fill = park), # Use 'fill' for interior color
           inherit.aes = FALSE, # Prevent inheriting 'group' from ggplot()
           size = 3, shape = 21, color = "black") + # Shape 21 allows for outline and fill
  labs(title = "Map with Park Centroids", x = "Longitude", y = "Latitude") +
  scale_color_viridis_d(name = "Parks") # Add a legend for parks
 
ggplot(data = states, mapping = aes(x = long, y = lat, group = group)) +
  coord_fixed(1.3) +
  theme_void() +
  geom_polygon(color = "#DFE6E6", fill = "#DFE6E6") +
  geom_polygon(data = northeast, mapping = aes(x = long, y = lat, group = group),
               color = "#D5D5DE", fill = "#D5D5DE")
 
