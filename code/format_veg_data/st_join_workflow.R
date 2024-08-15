library(terra) # this package isn't needed but it's the best for raster layers, also helpful to call in vector data vect()
library(sf)
library(ggplot2)

# read in test shapefile
sa <- read_sf(dsn = "data/veg_maps/mimageodata/MIMA_Veg_shp/MIMA_Veg.shp")           

# converst shapefile to simple feature (sf) object
sa_sf <- st_as_sf(sa)

# generate random points within our simple feature object
pts <- st_sample(sa_sf, 100)

# show points over shapefile
ggplot() +
  geom_sf(sa_sf, mapping=aes(), fill = NA) +
  geom_sf(pts, mapping = aes()) # works

# convert pts to dataframe, since i assume this is how your XY coordinates are formatted
pts_df <- st_coordinates(pts) %>% 
  as.data.frame() %>% 
  dplyr::mutate(ID = 1:100) # made this to have an ID attached to each coordinate

# re-convert to sf object since that's how I'd interesect these with your other sf object 
# using lines 20 - 26 as examples of converting between dataframes and sf objects
pts_sf <- st_as_sf(pts_df, coords = c("X","Y"), crs = st_crs(sa_sf))

# intersect pts with south america sf object to figure out what country each point falls within
pts_sa <- st_join(pts_sf, sa_sf) #viola

