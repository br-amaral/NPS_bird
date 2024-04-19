# *********************************************************************************
# -------------------------------   Amazing Title   -------------------------------
# *********************************************************************************
# Code to get gdb files from parks and classifi all bird and forest sites
#   according to vegetation type
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
library(sf)
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
PATH_PARK_GDB <- "data/veg_maps/mabigeodata/mabigeodata.gdb"
## read files
vegp_map <- st_read(PATH_PARK_GDB)

veg_layers <- st_layers(dsn = PATH_PARK_GDB)

vegp_map <- st_read(PATH_PARK_GDB, layer = "MABI_VegPolys")

plot(vegp_map)


st_intersection(vegp_map, xy)

st_transform(xy, crs = st_crs(vegp_map)) 

xy2 <- spTransform(xy, st_crs(vegp_map))
class(xy)
class(vegp_map)

vegp_map$geometry

# Convert the SpatialPointsDataFrame to an sf object
xy_sf <- st_as_sf(xy)

# Now you can apply the st_transform function
xy_transformed <- st_transform(xy_sf, crs = st_crs(vegp_map))

bird_point_veg <- st_intersection(xy_transformed, vegp_map)

plot(xy_transformed)

plot(bird_point_veg)

# Plot the first variable
plot(mtcars$mpg)

# Plot the second variable
plot(mtcars$cyl)

par(mfrow = c(1,1))
