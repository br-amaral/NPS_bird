############### get_covariates.R ###############

# Script to get covariate data for the national parks on the NETN network. Get local data from NPS and
#   landscape data from rFIA
# Covariates
#  - tree diversity: Shannon diversity index, Shannon Equity index
#  - tree abundance: number of trees, total basal area
#  - shrub: shrub area?
#  - stand structure: percent coverage of different tree ages (?)

# Input files:
#               - data/site_coords.rds
#               - data/NETN-forest/Stand_Data.csv
#               - data/NETN-forest/forest_csvs/Microplot_Shrub_Data.csv
#               - data/NETN-forest/forest_csvs/Microplot_Characterization_Data.csv
#               - data/NETN-forest/shrub_import.rds
#               - data/FIA/out/div_fim.rds
#               - data/FIA/out/tpa_fim.rds
#               - 
#               - 
#               - 
#               - 
#               - 

# Output files:
#               - data/NETN-forest/tree_ba_import.rds
#               - data/NETN-forest/stand_import.rds
#               - data/FIA/out/div_fim_yr_import.rds
#               - data/FIA/out/div_fim_tot_import.rds
#               - data/FIA/out/bas_area_yr_import.rds
#               - data/FIA/out/bas_area_tot_import.rds
#               - 
#               - 
#               - 
#               - 
#               - 

# load packages ----------------------------------------------------------------------------------
library(glue)
library(tidyverse)
library(dplyr)
library(sp)
library(data.table)
library(rFIA)

# park site locations -----------------------------------------------------------------------------
site_park_loc <- read_rds("data/site_coords.rds") %>% 
   as_tibble()

get_utm <- function(x, y, zone, loc){
   points = SpatialPoints(cbind(x, y), proj4string = CRS("+proj=longlat +datum=WGS84"))
   points_utm = spTransform(points, CRS(paste0("+proj=utm +zone=",zone[1]," +ellps=WGS84")))
   if (loc == "x") {
      return(coordinates(points_utm)[,1])
   } else if (loc == "y") {
      return(coordinates(points_utm)[,2])
   }
}

site_park_loc <- site_park_loc %>% 
   mutate(zone2 = (floor((Longitude + 180)/6) %% 60) + 1, keep = "all"
   ) %>% 
   group_by(zone2) %>% 
   mutate(utm_x = get_utm(Longitude, Latitude, zone2, loc = "x"),
          utm_y = get_utm(Longitude, Latitude, zone2, loc = "y"))

plot(site_park_loc$utm_x, site_park_loc$utm_y)

# Local NPS variables -------------------------------------------------------------------------------------------------
## Basal area data ----------------------------------------------------------------------------------------------------
tree_ba <- read.csv(file = "data/NETN-forest/Tree_Data_site_by_species_live_BA.csv")

tree_ba$total_BA <- rowSums(tree_ba[,c(12:85)], na.rm = T)

site_park_loc3 <- site_park_loc %>% 
  rename(lon = utm_x,
         lat = utm_y) %>% 
  as_tibble() %>% 
  select(lon,lat, Point_Name) %>% 
  distinct()

lcl_tree_ba <- tree_ba %>% 
  rename(Admin_Unit_Code = Unit_Code,
         lon2 = X_Coord,
         lat2 = Y_Coord) %>% 
  select(lon2,lat2) %>% 
  distinct() %>% 
  mutate(closer_point = seq(1:nrow(.)))

#site_park_loc3 <- site_park_loc3[1,]
#lcl_tree_ba <- lcl_tree_ba[c(1:2),]

limsX <- c(min(c(site_park_loc3$lon, lcl_tree_ba$lon2), na.rm = T) - 100,
           max(c(site_park_loc3$lon, lcl_tree_ba$lon2), na.rm = T) + 100)

limsY <- c(min(c(site_park_loc3$lat, lcl_tree_ba$lat2), na.rm = T) - 100,
           max(c(site_park_loc3$lat, lcl_tree_ba$lat2), na.rm = T) + 100)

plot(site_park_loc3$lon, site_park_loc3$lat, xlim = limsX, ylim = limsY)
points(lcl_tree_ba$lon2, lcl_tree_ba$lat2, col = "darkgreen", pch = "*", cex = 1.7)

site_park_loc3$closer_point <- NA

for (ii in 1:nrow(site_park_loc3)) {
  
  dt <- data.table((site_park_loc3$lon[ii]-lcl_tree_ba$lon2)^2+(site_park_loc3$lat[ii]-lcl_tree_ba$lat2)^2)
  site_park_loc3$closer_point[ii] <- which.min(dt$V1)
  
}

site_park_loc4 <- left_join(site_park_loc3, lcl_tree_ba, by = "closer_point")

segments(site_park_loc4$lon, site_park_loc4$lat, 
         site_park_loc4$lon2, site_park_loc4$lat2,
         col = "blue")

lcl_tree_ba3 <- tree_ba %>% 
  rename(lon2 = X_Coord,
         lat2 = Y_Coord) 

years_ba <- tree_ba %>% select(Year) %>% pull() %>%  unique() %>% sort()

tree_ba_tab <- expand_grid(site_park_loc4, years_ba) %>% 
  rename(Year = years_ba) %>% 
  left_join(., lcl_tree_ba3, by = c("lon2", "lat2", "Year")) %>% 
  relocate(Point_Name, lon, lat, Year, total_BA)

write_rds(tree_ba_tab, file = "data/NETN-forest/tree_ba_import.rds")

## Tree per acre data -------------------------------------------------------------------------------------------------
tree_den <- read.csv(file = "data/NETN-forest/Tree_Data_site_by_species_live_density.csv")

tree_den$total_den <- rowSums(tree_den[,c(12:85)], na.rm = T)

site_park_loc3 <- site_park_loc %>% 
  rename(lon = utm_x,
         lat = utm_y) %>% 
  as_tibble() %>% 
  select(lon,lat, Point_Name) %>% 
  distinct()

lcl_tree_den <- tree_den %>% 
  rename(Admin_Unit_Code = Unit_Code,
         lon2 = X_Coord,
         lat2 = Y_Coord) %>% 
  select(lon2,lat2) %>% 
  distinct() %>% 
  mutate(closer_point = seq(1:nrow(.)))

# site_park_loc3 <- site_park_loc3[1,]
# lcl_tree_den <- lcl_tree_den[c(1:2),]

limsX <- c(min(c(site_park_loc3$lon, lcl_tree_den$lon2), na.rm = T) - 100,
           max(c(site_park_loc3$lon, lcl_tree_den$lon2), na.rm = T) + 100)

limsY <- c(min(c(site_park_loc3$lat, lcl_tree_den$lat2), na.rm = T) - 100,
           max(c(site_park_loc3$lat, lcl_tree_den$lat2), na.rm = T) + 100)

plot(site_park_loc3$lon, site_park_loc3$lat, xlim = limsX, ylim = limsY)
points(lcl_tree_den$lon2, lcl_tree_den$lat2, col = "darkgreen", pch = "*", cex = 1.7)

site_park_loc3$closer_point <- NA

for (ii in 1:nrow(site_park_loc3)) {
  
  dt <- data.table((site_park_loc3$lon[ii]-lcl_tree_den$lon2)^2+(site_park_loc3$lat[ii]-lcl_tree_den$lat2)^2)
  site_park_loc3$closer_point[ii] <- which.min(dt$V1)
  
}

site_park_loc4 <- left_join(site_park_loc3, lcl_tree_den, by = "closer_point")

segments(site_park_loc4$lon, site_park_loc4$lat, 
         site_park_loc4$lon2, site_park_loc4$lat2,
         col = "blue")

lcl_tree_den3 <- tree_den %>% 
  rename(lon2 = X_Coord,
         lat2 = Y_Coord) 

years_den <- tree_den %>% select(Year) %>% pull() %>%  unique() %>% sort()

tree_den_tab <- expand_grid(site_park_loc4, years_den) %>% 
  rename(Year = years_den) %>% 
  left_join(., lcl_tree_den3, by = c("lon2", "lat2", "Year")) %>% 
  relocate(Point_Name, lon, lat, Year, total_den)

write_rds(tree_den_tab, file = "data/NETN-forest/tree_den_import.rds")



## Stand structure ----------------------------------------------------------------------------------------------------

lcl_stand <- read.csv("data/NETN-forest/Stand_Data.csv") %>% 
   as_tibble() %>% 
   filter(Year == 2018)

str(lcl_stand)

plot(site_park_loc$utm_x, site_park_loc$utm_y)
points(lcl_stand$X_Coord, lcl_stand$Y_Coord, col = "red")

## distance to bird points

site_park_loc2 <- site_park_loc %>% 
   rename(lon = utm_x,
          lat = utm_y) %>% 
   as_tibble() %>% 
   select(lon,lat, Point_Name) %>% 
   distinct()

lcl_stand2 <- lcl_stand %>% 
   rename(Admin_Unit_Code = Unit_Code,
          lon2 = X_Coord,
          lat2 = Y_Coord) %>% 
   select(lon2,lat2) %>% 
   distinct() %>% 
   mutate(closer_point = seq(1:nrow(.)))

limsX <- c(min(c(site_park_loc2$lon, lcl_stand2$lon2), na.rm = T) - 100,
           max(c(site_park_loc2$lon, lcl_stand2$lon2), na.rm = T) + 100)

limsY <- c(min(c(site_park_loc2$lat, lcl_stand2$lat2), na.rm = T) - 100,
           max(c(site_park_loc2$lat, lcl_stand2$lat2), na.rm = T) + 100)
   

#site_park_loc2 <- site_park_loc2[1,]
#lcl_stand2 <- lcl_stand2[c(1:2,9,13),]

plot(site_park_loc2$lon, site_park_loc2$lat, xlim = limsX, ylim = limsY)
points(lcl_stand2$lon2, lcl_stand2$lat2, col = "darkgreen", pch = "*", cex = 1.7)

site_park_loc2$closer_point <- NA

for (ii in 1:nrow(site_park_loc2)) {
   
   dt <- data.table((site_park_loc2$lon[ii]-lcl_stand2$lon2)^2+(site_park_loc2$lat[ii]-lcl_stand2$lat2)^2)
   site_park_loc2$closer_point[ii] <- which.min(dt$V1)
   
   }

lcl_stand2 <- lcl_stand2 %>% mutate(closer_point = seq(1:nrow(.)))

site_park_loc3 <- left_join(site_park_loc2, lcl_stand2, by = "closer_point")

segments(site_park_loc3$lon, site_park_loc3$lat, 
         site_park_loc3$lon2, site_park_loc3$lat2,
         col = "blue")

lcl_stand3 <- lcl_stand %>% 
   rename(lon2 = X_Coord,
          lat2 = Y_Coord) 

stand_struc_tab <- site_park_loc3 %>% 
   left_join(., lcl_stand3, by = c("lon2", "lat2"))

write_rds(stand_struc_tab, file = "data/NETN-forest/stand_import.rds")

## Shrub data -----------------------------------------------------------------------------------

lcl_shrub <- read.csv("data/NETN-forest/forest_csvs/Microplot_Shrub_Data.csv") %>% 
   as_tibble() 
# taxonomic serial number 

microplotkey <- read.csv("data/NETN-forest/forest_csvs/Microplot_Characterization_Data.csv") %>% 
   as_tibble() 

plot_locs <- read.csv("data/NETN-forest/Stand_Data.csv") %>% 
   as_tibble() %>% 
   select(Location_ID, Event_ID,
          Plot_Name,
          X_Coord, Y_Coord) %>% 
   left_join(., microplotkey, by = "Event_ID")

lcl_shrub2 <- lcl_shrub %>% 
   left_join(., plot_locs, by = "Microplot_Characterization_Data_ID") %>% 
   mutate(plotmp_ID = paste0(Plot_Name, sep = "_", Microplot_Name)) %>% 
   rename(lon2 = X_Coord,
          lat2 = Y_Coord)
# three microplots per plot
site_park_loc2$closer_point <- NA
# site_park_loc2 <- site_park_loc2 %>% filter(grepl('ACAD', Point_Name))

limsX <- c(min(c(site_park_loc2$lon, lcl_shrub2$lon2), na.rm = T) - 100,
           max(c(site_park_loc2$lon, lcl_shrub2$lon2), na.rm = T) + 100)

limsY <- c(min(c(site_park_loc2$lat, lcl_shrub2$lat2), na.rm = T) - 100,
           max(c(site_park_loc2$lat, lcl_shrub2$lat2), na.rm = T) + 100)

plot(site_park_loc2$lon, site_park_loc2$lat, xlim = limsX, ylim = limsY)
points(lcl_shrub2$lon2, lcl_shrub2$lat2, col = "darkgreen", pch = "*", cex = 1.7)

for (ii in 1:nrow(site_park_loc2)) {
   
   dt <- data.table((site_park_loc2$lon[ii]-lcl_shrub2$lon2)^2+(site_park_loc2$lat[ii]-lcl_shrub2$lat2)^2)
   site_park_loc2$closer_point[ii] <- which.min(dt$V1)
   
}

lcl_shrub2 <- lcl_shrub2 %>% mutate(closer_point = seq(1:nrow(.)))

lcl_shrub23 <- left_join(site_park_loc2, lcl_shrub2, by = "closer_point")

segments(lcl_shrub23$lon, lcl_shrub23$lat, 
         lcl_shrub23$lon2, lcl_shrub23$lat2,
         col = "blue")

write_rds(lcl_shrub2, file = "data/NETN-forest/shrub_import.rds")

## Tree diversity data -----------------------------------------------------------------------------------
## forest_csvs/Trees.csv - tree data per plot (tsn is species I think)
## forest_csvs/Plants.csv - has the tree code names

tree_tab <- read.csv("data/NETN-forest/forest_csvs/Trees.csv") %>% 
  as_tibble() %>% 
  filter(as.numeric(substring(TimeStamp,1,4)) > 2001)

length(unique(tree_tab$Location_ID))  # :(

tree_ba <- read.csv(file = "data/NETN-forest/Tree_Data_site_by_species_live_BA.csv")


### mean Shannon's Equitability Index, alpha (stand) level ----------------------------------------------------------------

### mean Species Richness, alpha (stand) level ----------------------------------------------------------------


## Tree_Data_site_by_species_live_BA.csv - basal area of trees by species
## Tree_Data_site_by_species_live_density.csv - density of trees by species

## try a pca with src/NER_BIRDS_NLCD_20190522.csv
## park area is here: src/park_info.csv


# Park level variables from NPS -----------------------------------------------------------------------------


# County level FIA data -----------------------------------------------------
## Tree diversity data ----------------------------------------------------------------
div_fim <- read_rds(file = "data/FIA/out/div_fim.rds")
head(div_fim)

### mean Shannon's Equitability Index, alpha (stand) level ----------------------------------------------------------------
div_fim$Eh_a
div_fim$Eh_a_SE
### mean Species Richness, alpha (stand) level ----------------------------------------------------------------
div_fim$S_a
div_fim$S_a_SE

div_fim_yr <- div_fim

div_fim_tot <- div_fim %>% 
  group_by(park) %>% 
  summarise(H_g_mean = mean(H_g, na.rm = T),
            Eh_a_mean = mean(Eh_a, na.rm = T),
            S_a_mean = mean(S_a, na.rm = T),
            Eh_a_SE_mean = mean(Eh_a_SE, na.rm = T),
            S_a_SE_mean = mean(S_a_SE, na.rm = T)
            )

write_rds(div_fim_yr, file = "data/FIA/out/div_fim_yr_import.rds")
write_rds(div_fim_tot, file = "data/FIA/out/div_fim_tot_import.rds")

## Basal area data ---------------------------------------------------------------
bas_area <- read_rds(file = "data/FIA/out/tpa_fim.rds") %>% 
  select("YEAR","BAA","BAA_SE","park")

bas_area_yr <- bas_area

bas_area_tot <- bas_area %>% 
  group_by(park) %>% 
  summarise(BAA_mean = mean(BAA, na.rm = T),
            BAA_SE_mean = mean(BAA_SE, na.rm = T)
  )

write_rds(bas_area_yr, file = "data/FIA/out/bas_area_yr_import.rds")
write_rds(bas_area_tot, file = "data/FIA/out/bas_area_tot_import.rds")

## Tree per acre data ---------------------------------------------------------------
# mean trees per acre
tree_acre <- read_rds(file = "data/FIA/out/tpa_fim.rds") %>% 
  select("YEAR","TPA","TPA_SE","park")

tree_acre_yr <- tree_acre

tree_acre_tot <- tree_acre %>% 
  group_by(park) %>% 
  summarise(TPA_mean = mean(TPA, na.rm = T),
            TPA_SE_mean = mean(TPA_SE, na.rm = T)
  )

write_rds(tree_acre_yr, file = "data/FIA/out/tree_acre_yr_import.rds")
write_rds(tree_acre_tot, file = "data/FIA/out/tree_acre_tot_import.rds")

## Stand structure data -------------------------------------------------------------
stastr_tab <- read_rds(file = "data/FIA/out/stand_struct_fim.rds")

stastr_tab %>% 
  ggplot(aes(x = park, y = COVER_PCT)) +
  geom_boxplot(fill="#BF87B3") +
  theme_bw() +
  facet_wrap(~STAGE) +
  theme(axis.text.x = element_text(angle = 90))

stastr_tab %>% 
  ggplot(aes(x = park, y = COVER_PCT)) +
  geom_boxplot(fill="#BF87B3") +
  theme_bw() +
  facet_wrap(~STAGE, scales = "free_y") +
  theme(axis.text.x = element_text(angle = 90))

late <- stastr_tab %>% 
  filter(STAGE == "LATE") %>% 
  select(YEAR, park, COVER_PCT) %>% 
  rename(late = COVER_PCT)

matu <- stastr_tab %>% 
  filter(STAGE == "MATURE") %>% 
  select(YEAR, park, COVER_PCT) %>% 
  rename(matu = COVER_PCT)

mosa <- stastr_tab %>% 
  filter(STAGE == "MOSAIC") %>% 
  select(YEAR, park, COVER_PCT) %>% 
  rename(mosa = COVER_PCT)

pole <- stastr_tab %>% 
  filter(STAGE == "POLE") %>% 
  select(YEAR, park, COVER_PCT) %>% 
  rename(pole = COVER_PCT)

str_key <- stastr_tab %>% select(YEAR, park) %>% 
  distinct()

str_cor <- str_key %>% 
  left_join(., late) %>% 
  left_join(., matu) %>% 
  left_join(., mosa) %>% 
  left_join(., pole)

head(str_cor)

cor(str_cor[,3:6] %>% scale(), use="pairwise.complete.obs") %>% 
  corrplot(method = 'square', order = 'alphabet', addCoef.col = 'black', 
           cl.pos = 'n', col = COL2('BrBG'), type = 'lower', number.cex=0.5)

cor(str_cor[,3:5] %>% scale(), use="pairwise.complete.obs") %>% 
  pairs()

reg <- function(x, y, col) abline(lm(y~x), col=col) 

panel.lm <- function (x, y, col = par("col"), bg = NA, pch = par("pch"), 
                      cex = 1, col.smooth = "red", span = 2/3, iter = 3, ...)  {
  points(x, y, pch = pch, col = col, bg = bg, cex = cex)
  ok <- is.finite(x) & is.finite(y)
  if (any(ok)) reg(x[ok], y[ok], col.smooth)
}

pairs(str_cor[,3:6], panel = panel.lm,
      cex = 1, pch = 19,  cex.labels = 2, 
      font.labels = 2, lower.panel = panel.cor)

write_rds(str_cor, file = "data/FIA/out/stand_struc_import.rds")

## Shrub data :( --------------------------------------------------------------------------

