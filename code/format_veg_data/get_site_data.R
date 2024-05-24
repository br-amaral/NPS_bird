#? *********************************************************************************
#? ------------------------------   get_site_data.R   ------------------------------
#? *********************************************************************************
#! Code to ...
#
#
#! Source ---------------------------------------------
#           - :
#           - :
#
#! Input ----------------------------------------------
#           - data/out/NETNtib.rds :
#           - data/src/key_park.rds :
#           - data/veg_kateaaron/NETN_forest_data_2006-2023.rds :
#           - data/veg_kateaaron/NETN_tree_dens_spp_2006-2023.rds :
#           - data/veg_kateaaron/for_sites.rds : 
#           - data/out/key_bsite.rds : 
#           - data/out/key_fsite.rds :
#
#! Output ----------------------------------------------
#           - data/out/close_points_f.rds : tibble with combinations of forest and bird sites, and the distances between them
#           - data/out/park_site.rds :
#           - data/out/for_sit_coord.rds :
#           - data/out/bird_site_coords.rds :
#           - data/out/site_covs.rds : output file with the covariate values
#           - data/out/for_sit2.rds :
#           - data/out/close_points_fcovs.rds :
#
# detach packages and clear workspace
if(!require(freshr)){install.packages('freshr')}
freshr::freshr()
#
#! Load packages ---------------------------------------
library(conflicted)
library(tidyverse)
library(glue)
library(sp)
#library(rgdal)
library(sf)
library(reshape2)
library(ggplot2)
library(ggh4x)
library("MetBrewer")

conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::filter)
# conflicts_prefer(scales::alpha)
#
#! Make functions --------------------------------------
colanmes <- colnmaes <- colnames
lenght <- length
`%!in%` <- Negate(`%in%`)

Modes <- function(x) {
  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  if(length(ux[tab == max(tab)]) > 1) {
    sample(ux[tab == max(tab)], 1)
    } else {
      ux[tab == max(tab)]}
}

#! Import data -----------------------------------------
## file paths
BIRD_SITE_PATH    <- "data/out/NETNtib.rds"
PARK_SITE_PATH    <- "data/src/key_park.rds"
FORCOVS_SITE_PATH <- "data/veg_kateaaron/NETN_forest_data_2006-2023.rds"
FORSPS_SITE_PATH  <- "data/veg_kateaaron/NETN_tree_dens_spp_2006-2023.rds"
FOR_SITE_PATH     <- "data/veg_kateaaron/for_sites.rds"
BIRD_FOR_PATH     <- "data/out/key_bsite.rds"
FOR_FOR_PATH      <- "data/out/key_fsite.rds"
## read files
bird_sit      <- read_rds(file = BIRD_SITE_PATH)
parks         <- read_rds(file = PARK_SITE_PATH) 
for_sit       <- read_rds(file = FORCOVS_SITE_PATH)
fordiv_sit    <- read_rds(file = FORSPS_SITE_PATH)
for_sit_coord <- read_rds(file = FOR_SITE_PATH)
key_bsite     <- read_rds(file = BIRD_FOR_PATH)
key_fsite     <- read_rds(file = FOR_FOR_PATH)

#! get coordinates from the bird plots ------------------------------
parks <- parks %>% 
  dplyr::select(parks) %>% 
  distinct() %>% 
  pull()

parks <- sort(parks)


for(ii in 1:length(bird_sit$points)){
  coord_loop <- 
    bird_sit$points[ii][[1]] %>% 
      select(Admin_Unit_Code,
            NETN_Point_Name,
            Point_Name,
            Latitude,
            Longitude,
            UTM_ZONE)
  if(ii == 1) {
    park_site <- coord_loop
  } else {
    park_site <- rbind(park_site, coord_loop)
  }
}

nrow(park_site)

## write_rds "data/out/park_site.rds"
write_rds(park_site, file = "data/out/park_site.rds")

# get coordinates from bird plots
bird_sit_coord <- park_site %>% 
  select(Point_Name,
         Latitude,
         Longitude, 
         UTM_ZONE) %>% 
  rename(bird_sit = Point_Name,
         lat = Latitude,
         lon = Longitude,
         UTMZone = UTM_ZONE) 

#! get coordinates from the forest plots ------------------------------
for_sit_coord <- for_sit_coord %>% 
  rename(for_sit = Plot_Name,
         latutm = Y,
         lonutm = X) %>% 
  relocate(for_sit, latutm, lonutm, UTMZone) %>% 
  mutate(UTMZone = substr(UTMZone, 1 , 2))

colnmaes(for_sit_coord); colnmaes(bird_sit_coord)
write_rds(for_sit_coord, file = "data/out/for_sit_coord.rds")

par(mfrow = c(1,2))
plot(for_sit_coord$lonutm, for_sit_coord$latutm)
plot(bird_sit_coord$lon, bird_sit_coord$lat)

#? convert all bird coordinates to UTM to get distances in meters --------------------
xy <- data.frame(ID = 1:nrow(bird_sit_coord), 
                 X = bird_sit_coord$lon, 
                 Y = bird_sit_coord$lat)
coordinates(xy) <- c("X", "Y")
proj4string(xy) <- CRS("+proj=longlat +datum=WGS84")

write_rds(xy, file = "data/out/bird_site_coords.rds")

for(ii in 1:nrow(xy)){
  band <- as.numeric(bird_sit_coord$UTMZone[ii])
  y <- spTransform(xy[ii,], CRS(glue("+proj=utm +zone={band} +datum=WGS84 +units=m")))
  y2 <- as(y, "SpatialPoints") %>% 
        as_tibble()
  y2$UTMZone <- band
  
  if(ii == 1) {
    utm_bir <- y2
  } else {
    utm_bir <- rbind(utm_bir, y2)
  }
  rm(y)
}

colnames(utm_bir) <- c("lonutm", "latutm", "UTMZone")
table(bird_sit_coord$UTMZone == utm_bir$UTMZone)

bird_sit_coord <- cbind(bird_sit_coord, utm_bir[,1:2])

park_plot_nam <- bird_sit_coord %>% 
              mutate(park = substr(bird_sit, 1 , 4)) %>% 
              group_by(park) %>% 
              filter(row_number()==1) %>%
              ungroup()

ggplot() +
  geom_point(aes(x = bird_sit_coord$lonutm, 
                 y = bird_sit_coord$latutm),
             size = 2,
             color = "red") +
  geom_point(aes(x = for_sit_coord$lonutm, 
                 y = for_sit_coord$latutm), 
             color = "darkgreen") +
  #geom_text(aes(x= park_plot_nam$lonutm, y =park_plot_nam$latutm),
  #          label = park_plot_nam$park, size = 3, vjust = -1.3) +
  theme_bw() +
  xlim(697200,700200) + ylim(4833300,4835000)

# great - it all alligns very well; now let's get the closest 3 sites

#? Connect forest sites and bird sites -----------------------------------
# there is no SAIR site level data
bird_sit_coord2 <- bird_sit_coord %>% 
    as_tibble() %>% 
    mutate(park = substr(bird_sit, 1 , 4)) %>%
    filter(park != "SAIR") %>% 
    select(-park)

# link forest types with sites
bird_sit_coord2 <- left_join(bird_sit_coord2, 
                             key_bsite %>% 
                                rename(bird_sit = Point_Name,
                                       b_for = MapUnit_ID) %>% 
                                select(bird_sit, b_for),
                             by = "bird_sit") %>% 
                   filter(!is.na(b_for))

for_sit_coord2 <- left_join(for_sit_coord  %>% 
                              mutate(park = substr(for_sit, 1, 4)), #%>% 
                              #filter(park != "ACAD") %>% 
                              #filter(park != "SAIR"), 
                           key_fsite %>% 
                              rename(for_sit = ID,
                                     f_for = MapUnit_ID) %>% 
                              select(for_sit, f_for, geometry),
                           by = "for_sit") %>% 
                   filter(!is.na(f_for))

# make sure parks patch with forest sites only within the same park
for_sit_coord_minusrova <- for_sit_coord2 %>% 
  filter(park != "ROVA")

for_sit_coord_rova <- for_sit_coord2 %>% 
  filter(park == "ROVA") %>% 
  select(-park)

for_sit_coord_elro <- for_sit_coord_rova %>% 
  mutate(park = "ELRO")
for_sit_coord_vama <- for_sit_coord_rova %>% 
  mutate(park = "VAMA")
for_sit_coord_hofr <- for_sit_coord_rova %>% 
  mutate(park = "HOFR")

for_sit_coord3 <- rbind(for_sit_coord_minusrova, 
                        for_sit_coord_elro, 
                        for_sit_coord_vama, 
                        for_sit_coord_hofr)

#bird_sit_coord2 <- bird_sit_coord2[1:11,]

# get neighbors
for (ii in 1:nrow(bird_sit_coord2)) {

  band <- as.numeric(bird_sit_coord2$UTMZone[ii])
  y <- spTransform(xy[ii,], CRS(glue("+proj=utm +zone={band} +datum=WGS84 +units=m")))

  bird <- st_as_sf(bird_sit_coord2[ii,5:6], 
                   coords=c("lonutm", "latutm"), 
                   crs = CRS(proj4string(y)))

  plop <- substr(bird_sit_coord2$bird_sit[ii], 1, 4)

  for_sit_coord4 <- for_sit_coord3 %>% 
                      filter(park == plop)
  
  print(bird_sit_coord2$bird_sit[ii])

  for(jj in 1:nrow(for_sit_coord4)) {
    
    if((bird_sit_coord2$b_for[ii] == for_sit_coord4$f_for[jj]) == TRUE){
    
      print(for_sit_coord4$for_sit[jj])
      print(jj)

      band2 <- as.numeric(for_sit_coord4$UTMZone[jj])
      x <- spTransform(xy[ii,], CRS(glue("+proj=utm +zone={band2} +datum=WGS84 +units=m")))

      fore <- st_as_sf(for_sit_coord4[,2:3], 
                    coords=c("lonutm", "latutm"), 
                    crs = CRS(proj4string(x)))

      distances <- st_distance(bird, fore[jj,], by_element = TRUE)
  
      distances2 <- cbind(as.numeric(distances), for_sit_coord4$for_sit[jj]) %>% 
          as_tibble() %>% 
          rename(dist = V1, 
                for_sit = V2) %>% 
          mutate(dist = as.numeric(dist),
                for_b = as.character(bird_sit_coord2$b_for[ii]),
                for_f = as.character(for_sit_coord4$f_for[jj]),
                bird_sit = bird_sit_coord2$bird_sit[ii])
    
      if("dist1" %!in% ls()) {
          dist1 <- distances2
        } else {
          dist1 <- rbind(dist1, distances2)
        } 
    } 
  }

  dist_small <- dist1 %>% 
                  arrange(dist) %>% 
                  filter(dist <= 2000) %>% # diameter of the home range area of birds plus some slack if it is not circular 
                  arrange(bird_sit, dist)
  
  table(dist_small$bird_sit)

# ERROR: NOT REALLY AN ERROR, just choosing how many neighbours
#!!ERROR: the problem is here! do it by site!!!!!!!!!!!
  close_points <- dist_small %>%
                    group_by(bird_sit) %>%
                    slice(1:5) %>% # TODO: 
                    ungroup()

  table(close_points$bird_sit)

  if(ii == 1) {
    close_points_f <- close_points
    } else {
      close_points_f <- rbind(close_points_f, close_points)
    }
  print(ii)
}

# connects forest and bird sites
close_points_f <- close_points_f %>% 
                    arrange(bird_sit, dist) %>% 
                    distinct()

# write_rds data/out/close_points_f.rds
write_rds(close_points_f, file = "data/out/close_points_f1.rds")

# plot close points by park
## join coordinates
close_points_f2 <- close_points_f %>% 
  left_join(for_sit_coord2 %>% 
              rename(latutmf = latutm, lonutmf = lonutm) %>%
              select(for_sit, latutmf, lonutmf),
            by = "for_sit") %>% 
  left_join(bird_sit_coord2 %>% 
              rename(latutmb = latutm, lonutmb = lonutm) %>%
              select(bird_sit, latutmb, lonutmb),
            by = "bird_sit") 

for(ii in 1:lenght(parks)){
  (plop <- parks[ii])
  close_points_f3 <- close_points_f2 %>% 
                      filter(substr(bird_sit, 1, 4) == plop)

  p2 <- 
  ggplot(close_points_f3) +
    geom_segment(aes(x = lonutmb, y = latutmb, 
                     xend = lonutmf, yend = latutmf, 
                     colour = bird_sit)) +
    geom_point(aes(x = lonutmb, 
                   y = latutmb,
                   colour = bird_sit),
              size = 3) +
    geom_point(aes(x = lonutmf, 
                   y = latutmf),
              size = 3,
              color = "#186A3B",
              shape = 15) +
    #geom_text(aes(x= park_plot_nam$lonutm, y =park_plot_nam$latutm),
    #          label = park_plot_nam$park, size = 3, vjust = -1.3) +
    theme_bw()
    #)
  print(p2)
  #library(plotly)
  #ggplotly(p2)
}

table(close_points_f$bird_sit) %>% sort()

table(for_sit$SampleYear) %>% max()

#? get means for all years ----------------------------------------------
## mean for all years
for_sit2 <- for_sit %>% 
  #filter(SampleYear == 2022) %>% 
  group_by(Plot_Name) %>% 
  mutate(treeden_haM = mean(treeden_ha, na.rm = T),
         BA_m2haM = mean(BA_m2ha, na.rm = T),
         tree_richM = mean(tree_rich, na.rm = T),
         StageM = Modes(Stage),
         pctBA_poleM = mean(pctBA_pole, na.rm = T),
         pctBA_matureM = mean(pctBA_mature, na.rm = T),
         pctBA_largeM = mean(pctBA_large, na.rm = T),
         sap_den_m2M = mean(sap_den_m2, na.rm = T),
         shrub_covM = mean(shrub_cov, na.rm = T),
         X_for = X,      
         Y_for = Y,
         UTMZone_for = UTMZone,
         for_sit = Plot_Name)  %>% 
  ungroup() %>% 
  select(for_sit, ParkUnit, X_for, Y_for, UTMZone_for,
         treeden_haM, BA_m2haM, tree_richM, StageM, pctBA_poleM, 
         pctBA_matureM, pctBA_largeM, sap_den_m2M, shrub_covM) %>% 
  distinct()
  
close_points_f2 <- left_join(close_points_f, for_sit2, by = "for_sit") %>% 
    group_by(bird_sit) %>%
  mutate(treeden_haM = mean(treeden_haM, na.rm = T),
         BA_m2haM = mean(BA_m2haM, na.rm = T),
         tree_richM = mean(tree_richM, na.rm = T),
         StageM = Modes(StageM),
         pctBA_poleM = mean(pctBA_poleM, na.rm = T),
         pctBA_matureM = mean(pctBA_matureM, na.rm = T),
         pctBA_largeM = mean(pctBA_largeM, na.rm = T),
         sap_den_m2M = mean(sap_den_m2M, na.rm = T),
         shrub_covM = mean(shrub_covM, na.rm = T))  %>% 
  ungroup() %>% 
  mutate(park = substr(bird_sit, 1, 4)) %>%
  select(bird_sit, park,
         treeden_haM, BA_m2haM, tree_richM, StageM, 
         pctBA_poleM, pctBA_matureM, pctBA_largeM, 
         sap_den_m2M, 
         shrub_covM) %>% 
  distinct() %>% 
  rename(ParkUnit = park,
         siteDEN = treeden_haM, siteBA = BA_m2haM, 
         siteRICH = tree_richM, siteSTA = StageM,
         siteBA_pole = pctBA_poleM, siteBA_mature = pctBA_matureM, siteBA_large = pctBA_largeM,
         siteSAPden = sap_den_m2M, 
         siteSHRUden = shrub_covM)

#! Output files ----------------------------------------------
write_rds(for_sit2, file = "data/out/for_sit2.rds")
write_rds(close_points_f2, file = "data/out/site_covs.rds")






# creating correlation matrix
corr_mat <- round(cor(close_points_f2[,c(3:5,7:11)], use="complete.obs"),2)

# reduce the size of correlation matrix
melted_corr_mat <- melt(corr_mat) %>% 
    mutate(cov = substr(Var1,5,6))

# plotting the correlation heatmap
ggplot(data = melted_corr_mat, aes(x=Var1, y=Var2, 
								fill=value)) + 
geom_tile(color = "white")+
 scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
   midpoint = 0, limit = c(-1,1), space = "Lab", 
   name="Correlation \n") +
   theme_minimal()+ 
 theme(axis.text.x = element_text(vjust = 1, angle = 90),
       axis.title.x = element_blank(),       # Change x axis title only
       axis.title.y = element_blank() )+
 geom_text(aes(Var1, Var2, label = value), 
		color = "black", 
        size = 4)  

#! Variation plot for site ----------------------------------------------
ggplot(close_points_f2, aes(x=park, y=BA_m2haM, fill=park)) +
  geom_boxplot() +
  geom_jitter(position=position_jitter(0.2), alpha = 0.5) +
  coord_flip() +
  theme_bw() +
  theme(legend.position="none",
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "Site Scale",
       y =" \n  Basal area of live trees \n(>=10cm DBH in m2/ha)")


#! park level covs ----------------------------------------------
park_covs <- for_sit  %>% 
              mutate(park = substr(Plot_Name, 1, 4))  %>% 
              group_by(park) %>% 
              mutate(treeden_haM = mean(treeden_ha, na.rm = T),
                    BA_m2haM = mean(BA_m2ha, na.rm = T),
                    tree_richM = mean(tree_rich, na.rm = T),
                    StageM = Modes(Stage),
                    pctBA_poleM = mean(pctBA_pole, na.rm = T),
                    pctBA_matureM = mean(pctBA_mature, na.rm = T),
                    pctBA_largeM = mean(pctBA_large, na.rm = T),
                    sap_den_m2M = mean(sap_den_m2, na.rm = T),
                    shrub_covM = mean(shrub_cov, na.rm = T))  %>% 
              ungroup() %>% 
              select(park,
                    treeden_haM, BA_m2haM, tree_richM, StageM, pctBA_poleM, 
                    pctBA_matureM, pctBA_largeM, sap_den_m2M, shrub_covM) %>% 
              distinct()

#! Variation plot for park ----------------------------------------------
for_sit %>% 
  mutate(park = substr(Plot_Name, 1, 4))  %>% 
  #filter(park %!in% c("ELRO", "HOFR", "ROVA", "VAMA")) %>%  
  ggplot(aes(x=park, y=BA_m2ha, fill=park))  +
    geom_boxplot() +
    geom_jitter(position=position_jitter(0.2), alpha = 0.3) +
    coord_flip() +
    theme_bw() +
    theme(legend.position="none",
          axis.title.y = element_blank(),
          plot.title = element_text(hjust = 0.5)) +
    labs(title = "Park Scale",
        y =" \n  Basal area of live trees \n(>=10cm DBH in m2/ha)") +
    scale_fill_manual(values = met.brewer("Morgenstern")) +
    stat_summary(colour = "red", size = 0.75)

#! Correlation plots --------------------------------------------------
## basal area ---------------------------------------------------------
county_covs <- read_rds(file = "data/FIA/out/tpa_fim.rds") %>% 
  filter(park %!in% c("ELRO", "HOFR", "ROVA", "VAMA"))  %>% 
  group_by(park) %>% 
  mutate(BA_coun = mean(BAA, na.rm = T))  %>% 
  ungroup() %>% 
  select(park,
         BA_coun) %>% 
  distinct()

park_covs

all_scales_covs <- left_join(county_covs, 
                             park_covs %>% rename(BA_park = BA_m2haM) %>% select(park, BA_park),
                             by = "park")

all_scales_covs <- left_join(close_points_f2 %>% 
                                rename(BA_site = BA_m2haM) %>% 
                                select(park, BA_site),
                             all_scales_covs,
                             by = "park") %>% 
                             distinct()
all_scales_covs <- all_scales_covs  %>% 
                     relocate(park, BA_site, BA_park, BA_coun)

# creating correlation matrix
corr_mat <- round(cor(all_scales_covs[,2:4], use="complete.obs"),2)

# reduce the size of correlation matrix
melted_corr_mat <- melt(corr_mat) %>% 
    mutate(cov = substr(Var1,5,6))

# plotting the correlation heatmap
ggplot(data = melted_corr_mat, aes(x=Var1, y=Var2, 
								fill=value)) + 
geom_tile(color = "white")+
 scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
   midpoint = 0, limit = c(-1,1), space = "Lab", 
   name="Correlation \n") +
   theme_minimal()+ 
 theme(axis.text.x = element_text(vjust = 1, angle = 90),
       axis.title.x = element_blank(),       # Change x axis title only
       axis.title.y = element_blank() )+
 geom_text(aes(Var1, Var2, label = value), 
		color = "black", 
        size = 4)  

library("PerformanceAnalytics")
chart.Correlation(all_scales_covs[,2:4], histogram=TRUE, pch=19)

## density ---------------------------------------------------------
county_covs <- read_rds(file = "data/FIA/out/tpa_fim.rds") %>% 
  filter(park %!in% c("ELRO", "HOFR", "ROVA", "VAMA"))  %>% 
  group_by(park) %>% 
  # change from per acre to hectare
  mutate(TPH_coun = mean(TPA*2.471, na.rm = T))  %>% 
  ungroup() %>% 
  select(park,
         TPH_coun) %>% 
  distinct()

park_covs

all_scales_covs <- left_join(county_covs, 
                             park_covs %>% 
                                rename(TPH_park = treeden_haM) %>% 
                                select(park, TPH_park),
                             by = "park")

all_scales_covs <- left_join(close_points_f2 %>% 
                                rename(TPH_site = treeden_haM) %>% 
                                select(park, TPH_site),
                             all_scales_covs,
                             by = "park") %>% 
                             distinct()

all_scales_covs <- all_scales_covs  %>% 
                     relocate(park, TPH_site, TPH_park, TPH_coun)

# creating correlation matrix
corr_mat <- round(cor(all_scales_covs[,2:4], use="complete.obs"),2)

# reduce the size of correlation matrix
melted_corr_mat <- melt(corr_mat) %>% 
    mutate(cov = substr(Var1,5,6))

# plotting the correlation heatmap
ggplot(data = melted_corr_mat, aes(x=Var1, y=Var2, 
								fill=value)) + 
geom_tile(color = "white")+
 scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
   midpoint = 0, limit = c(-1,1), space = "Lab", 
   name="Correlation \n") +
   theme_minimal()+ 
 theme(axis.text.x = element_text(vjust = 1, angle = 90),
       axis.title.x = element_blank(),       # Change x axis title only
       axis.title.y = element_blank() )+
 geom_text(aes(Var1, Var2, label = value), 
		color = "black", 
        size = 4)  

chart.Correlation(all_scales_covs[,2:4], histogram=TRUE, pch=19)
