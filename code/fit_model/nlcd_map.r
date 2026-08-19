
library(tidyverse)
library(sf)
library(terra)
library(tidyterra)
library(FedData)   # for get_nlcd()
library(glue)
library(ggnewscale)
library(svglite)

# ---- 1. Study area boundary (used as template/crop extent for NLCD) ----
# Use your existing state/county polygons to build a bounding box for NLCD download.
states <- map_data("state")
counties <- map_data("county")

northeast <- subset(states, region %in% c("maine", "new york", "vermont",
                                           "massachusetts", "new jersey", "new hampshire",
                                           "connecticut", "rhode island"))

counties_ne <- counties %>%
  filter((subregion == "dutchess"   & region == "new york") |
         (subregion == "windsor"    & region == "vermont") |
         (subregion == "middlesex"  & region == "massachusetts") |
         (subregion == "morris"     & region == "new jersey") |
         (subregion == "sullivan"   & region == "new hampshire") |
         (subregion == "saratoga"   & region == "new york") |
         (subregion == "fairfield"  & region == "connecticut"))

# Convert county polygons to an sf object to use as the NLCD crop template
counties_sf <- counties_ne %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  group_by(subregion, region) %>%
  summarise(do_union = FALSE) %>%
  st_cast("POLYGON")

# Buffer slightly so the raster fully covers all park boundaries
study_extent <- counties_sf %>%
  st_union() %>%
  st_buffer(dist = 0.05) %>%  # ~5 km buffer in degrees; adjust as needed
  st_transform(5070)          # NLCD native CRS (Albers Conical Equal Area, EPSG:5070)

# ---- 2. Download and crop NLCD land cover ----
# year = most recent NLCD release matching or closest to your study period (2006-2023)
nlcd <- get_nlcd(
  template = study_extent,
  label    = "netn_study_area",
  year     = 2021,          # closest available NLCD year; adjust if using multiple years
  dataset  = "landcover"
)

# ---- 3. Reclassify NLCD into forest vs. non-forest (optional, simplifies legend) ----
# NLCD forest classes: 41 = Deciduous, 42 = Evergreen, 43 = Mixed Forest
forest_classes <- c(41, 42, 43)

nlcd_forest <- nlcd
# values(nlcd_forest) <- ifelse(values(nlcd) %in% forest_classes, "Forest", "Non-forest")

forest_classes <- c(41, 42, 43)

vals <- as.vector(values(nlcd))

vals_reclass <- dplyr::case_when(
  vals == 41 ~ "Deciduous Forest",
  vals == 42 ~ "Evergreen Forest",
  vals == 43 ~ "Mixed Forest",
  TRUE       ~ NA_character_
)

# 1. Reclassify raw NLCD codes into small integer codes
nlcd_forest <- classify(nlcd, rcl = matrix(c(
  41, 1,
  42, 2,
  43, 3
), ncol = 2, byrow = TRUE), others = NA)

# 2. Attach the category (levels) table -- must be a data.frame with
#    first column = integer codes, second column = labels
cats_df <- data.frame(
  ID = 1:3,
  Class = c("Deciduous Forest", "Evergreen Forest", "Mixed Forest")
)

levels(nlcd_forest) <- cats_df

# 3. Verify BEFORE reprojecting
cats(nlcd_forest)

# Reproject to match your original lat/long (EPSG:4326) plotting CRS
nlcd_forest_ll <- project(nlcd_forest, "EPSG:4326", method = "near")

# ---- 4. Park boundaries and centroids (using sf instead of deprecated rgeos) ----
park_bound <- read_rds("data/out/park_bound.rds") %>% st_as_sf()

parks <- readRDS(file = "data/src/key_park.rds") %>%
  dplyr::select(parks) %>%
  distinct() %>%
  pull()

parks <- parks[!parks %in% c("ACAD", "SAIR", "ELRO")]

park_bound_sub <- park_bound %>% filter(UNIT_CODE %in% parks)

all_centroids <- read_rds(file = "data/out/park_coord_points.rds")

# ---- 5. Final map: NLCD forest cover + county boundaries + park centroids ----
ggplot() +
  geom_polygon(data = northeast, aes(x = long, y = lat, group = group),
               color = "black", fill = "#D5D5DE", linewidth = 0.4) +
  geom_polygon(data = counties_ne, aes(x = long, y = lat, group = group),
               color = "black", fill = "#ffffffee", linewidth = 0.6) +
  geom_spatraster(data = nlcd_forest_ll, aes(fill = Class)) +
  scale_fill_manual(
    values = c(
    "Deciduous Forest" = "#fc4801",  # burnt orange (fall foliage)
    "Evergreen Forest"  = "#1B4D2E",  # deep conifer green
    "Mixed Forest"      = "#dec522"  # warm olive-brown, blending the two
    ),
    na.value = NA,
    name = "Forest type"
  ) +
  new_scale_fill() +  # start a fresh fill scale for the points
  geom_polygon(data = counties_ne, aes(x = long, y = lat, group = group),
               color = "black", fill = NA, linewidth = 0.6) +
  geom_point(data = all_centroids,
             aes(x = long, y = lat, fill = Parks),
             inherit.aes = FALSE, size = 3, shape = 21, color = "black") +
  scale_color_viridis_d(name = "Parks") + # Add a legend for parks
  coord_sf(xlim = range(counties_ne$long), ylim = range(counties_ne$lat)) +
  theme_bw() +
  labs(x = "Longitude", y = "Latitude")

ggsave("manus_figs/Final/figs4_nlcd.svg", width = 9, height = 7, dpi = 2000)
