#! *********************************************************************************
#! ------------------------------   veg_maps_park.R   ------------------------------
#! *********************************************************************************
# Code to get gdb files from parks and classifi all bird and forest sites
#   according to vegetation type. Also add which ones are in the same forest patch
#
#! Source ---------------------------------------------
#           - :
#           - :
#
#! Input ----------------------------------------------
#           - data/veg_maps/ :
#           - data/src/key_park.rds :
#           - data/out/park_site.rds :
#           - data/out/bird_site_coords.rds :
#           - data/out/for_sit_coord.rds :
#           - :
#
#! Output ----------------------------------------------
#           - data/out/key_fsite.rds : 
#           - data/out/key_bsite.rds :
#
#? detach packages and clear workspace
freshr::freshr()
#
#! Load packages ---------------------------------------
library(conflicted)
library(tidyverse)
library(glue)
library(sf)
library(readxl)
library(DT)
#library(rgdal)
#
conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::filter)
# conflicts_prefer(scales::alpha)

#! Make functions --------------------------------------
colanmes <- colnames
lenght <- length
`%!in%` <- Negate(`%in%`)

#! Source code -----------------------------------------

#! Import data -----------------------------------------
##? file paths
PATH_PARK_GDB   <- "data/veg_maps/"
PARK_KEY_PATH   <- "data/src/key_park.rds"
PARK_SITE_PATH  <- "data/out/park_site.rds"
BIRD_SITE_COORD <- "data/out/bird_site_coords.rds"
FOR_SITE_COORD  <- "data/out/for_sit_coord.rds"
FOR_CATE_PATH   <- "data/out/tab_veg3_EDITED_AW.xlsx"

##? read files
parks <- read_rds(file = PARK_KEY_PATH) 
park_site <- read_rds(file = PARK_SITE_PATH) 
xy <- read_rds(file = BIRD_SITE_COORD)
xy2 <- read_rds(file = FOR_SITE_COORD)
for_cats <- read_xlsx(FOR_CATE_PATH)

##? format files
parks <- parks %>% 
  dplyr::select(parks) %>% 
  distinct() %>% 
  pull()

parks_remove <- c("ACAD", "ELRO", "SAIR")

parks <- parks[which(parks %!in% parks_remove)]

xy_sf <- st_as_sf(xy)

# loop to load all park files
park_folder <- list.files(path = "data/veg_maps/")
folder_names <- substr(park_folder, 1, 4)

# get layer names
# scdl <- st_layers(dsn = PATH_PARK_LOOP)
# scdl$name

parks_ana <- c()

for(ii in 1:lenght(parks)){
    (park_loop <- tolower(parks[ii]))
    if(park_loop %in% folder_names) {

        if(park_loop == "mima"){ 

        vegp_map <- read_sf(dsn = "data/veg_maps/mimageodata/MIMA_Veg_shp/MIMA_Veg.shp") 

        assign(glue("{park_loop}_vegmap"), vegp_map)
        
        parks_ana <- c(parks_ana, park_loop)

    } else { 

        PATH_PARK_LOOP <- glue("{PATH_PARK_GDB}{park_loop}geodata/{park_loop}geodata.gdb")
        
        # get layer names
        # scdl <- st_layers(dsn = PATH_PARK_LOOP)
        # scdl$name

        vegp_map <- sf::st_read(PATH_PARK_LOOP, layer = glue("{toupper(park_loop)}_VegPolys"))

        assign(glue("{park_loop}_vegmap"), vegp_map)
        
        parks_ana <- c(parks_ana, park_loop)
        }
    }
    
    if(park_loop %in% c("elro", "hofr", "vama")) {

        PATH_PARK_LOOP <- glue("{PATH_PARK_GDB}rovageodata/rovageodata.gdb")
        
        vegp_map <- sf::st_read(PATH_PARK_LOOP, layer = glue("ROVA_VegPolys"))

        assign(glue("{park_loop}_vegmap"), vegp_map)

        parks_ana <- c(parks_ana, park_loop)

    }
}

#! get park X and Y coordinates ----------------------------------
xy$park <- park_site$Admin_Unit_Code
xy$Point_Name <- park_site$Point_Name
xy$UTM_ZONE <- park_site$UTM_ZONE

str(xy)

# Convert the SpatialPointsDataFrame to an sf object
xy_sf <- st_as_sf(xy)

parks_ana <- sort(parks_ana)

for(ii in 1:lenght(parks_ana)){
    (park_loop <- tolower(parks_ana[ii]))

    xy_sf_loop <- xy_sf %>% filter(park == toupper(park_loop))

    get_parkloop_veg <- get(glue("{park_loop}_vegmap"))

    xy_transformed <- st_transform(xy_sf_loop, crs = st_crs(get_parkloop_veg))

    bird_point_veg <- st_intersection(xy_transformed, get_parkloop_veg)

    # plot(xy_transformed)

    # plot(get_parkloop_veg)

    # plot(bird_point_veg)

    # dim(xy_transformed)

    # dim(bird_point_veg)

    # get_parkloop_veg$MapUnit_Name %>% unique()

    # bird_point_veg$MapUnit_Name %>% unique()

    assign(glue("{park_loop}_birdsite_vegmap"), bird_point_veg)

    if(park_loop == "mima") {
        bird_point_veg <- bird_point_veg  %>% 
            mutate(MapUnit_ID = GROUP_CODE,
                   MapUnit_Name = GROUP_NAME)
    }
    
    key_bsite_l <- bird_point_veg %>% 
        select(park, Point_Name, UTM_ZONE, 
            MapUnit_ID,  MapUnit_Name) %>% 
            as_tibble()

    if(ii == 1){key_bsite <- key_bsite_l
        } else {
            key_bsite <- rbind(key_bsite, key_bsite_l)
        }

    print(park_loop)
    print(nrow(key_bsite_l))
    rm(key_bsite_l)
    rm(bird_point_veg)
    rm(park_loop)

}

dim(key_bsite)

# OMG great! now do the same (SpatialPointsDataFrame to an sf object) for forest plots!
xy3 <- data.frame(ID = xy2$for_sit, 
                  park = substr(xy2$for_sit, 1, 4),
                  X =  xy2$lonutm, 
                  Y =  xy2$latutm,
                  zone = as.numeric(xy2$UTMZone))

xy4 <- bcmaps::utm_convert(xy3, easting = "X", northing = "Y",
                   zone = "zone", crs = "WGS84")

projcrs <- "+proj=utm +datum=WGS84 +units=m"
xy_sf2 <- st_as_sf(xy4)

# load forest plots, add rova in the list and rova veg_map
parks_ana2 <- c(parks_ana[which(parks_ana %in% tolower(unique(xy3$park)))],
                "rova")
PATH_PARK_LOOP <- glue("{PATH_PARK_GDB}rovageodata/rovageodata.gdb")
rova_vegmap <- sf::st_read(PATH_PARK_LOOP, 
                           layer = glue("ROVA_VegPolys"))

for(ii in 1:lenght(parks_ana2)){
    (park_loop <- tolower(parks_ana2[ii]))

    # Now you can apply the st_transform function
    xy_sf_loop2 <- xy_sf2 %>% filter(park == toupper(park_loop))
    #st_crs(xy_sf_loop2) <- CRS("+proj=longlat +datum=WGS84")

    get_parkloop_veg2 <- get(glue("{park_loop}_vegmap"))

    xy_transformed2 <- sf::st_transform(xy_sf_loop2, 
                                        crs = st_crs(get_parkloop_veg2))

    # st_crs(xy_transformed2) == st_crs(xy_sf_loop2)
    # st_crs(xy_transformed2) == st_crs(get_parkloop_veg2)

    for_point_veg <- st_intersection(xy_transformed2, get_parkloop_veg2)

    # plot(xy_transformed2)    ## points
    # st_crs(xy_transformed2)

    # plot(xy_sf_loop2)
    # st_crs(xy_sf_loop2)

    # plot(get_parkloop_veg2)  ## polygons
    # st_crs(get_parkloop_veg2)

    # plot(for_point_veg)

    # dim(xy_transformed2)

    # dim(for_point_veg)

    # get_parkloop_veg$MapUnit_Name %>% unique()

    # bird_point_veg$MapUnit_Name %>% unique()

    assign(glue("{park_loop}_forsite_vegmap"), for_point_veg)
    
  if(park_loop == "mima") {
        for_point_veg <- for_point_veg %>% 
            mutate(MapUnit_ID = GROUP_CODE,
                   MapUnit_Name = GROUP_NAME)
    }

    key_fsite_l <- for_point_veg %>% 
        select(park, ID, 
               X, Y, zone, 
               MapUnit_ID) %>% 
               as_tibble()

    if(ii == 1){key_fsite <- key_fsite_l
        } else {
            key_fsite <- rbind(key_fsite, key_fsite_l)
        }
    print(park_loop)
    print(nrow(key_fsite_l))
    rm(key_fsite_l)
    rm(for_point_veg)
    rm(park_loop)
}

key_fsite %>% tail()

# write_rds(key_fsite, file = "data/out/key_fsite.rds")
# write_rds(key_bsite, file = "data/out/key_bsite.rds")




FOR_COOR_PATH <- "data/out/for_sit_coord.rds"
for_plots <- read_rds(FOR_COOR_PATH) %>% 
    mutate(park = substr(for_sit, 1, 4)) %>% 
    filter(park %!in% parks_remove) %>% 
    arrange(park)


get_parkloop_veg2$Poly_ID       # polygon ID
get_parkloop_veg2$MapUnit_Name  # forest type of shape

str(get_parkloop_veg2)

if(park_loop == "mima") {
    get_parkloop_veg2 <- get_parkloop_veg2 %>% 
        mutate(MapUnit_ID = GROUP_CODE,
               MapUnit_Name = GROUP_NAME,
               Poly_ID = OBJECTID)
}

length(unique(get_parkloop_veg2$MapUnit_Name))    # 22 forest types
length(unique(get_parkloop_veg2$Poly_ID))         # 86 polygons
length(unique(get_parkloop_veg2$MapUnit_ID))      # 21 

ggplot(data = get_parkloop_veg2) +
    geom_sf(aes(fill = MapUnit_Name)) +
    geom_sf(data = xy_sf  %>% filter(park == "ROVA")) +
    #theme(legend.position = "bottom")
    theme(legend.position = "none")


for_cats1 <- for_cats  %>% 
                select("MapUnit_ID", "Cover_Type")

for_cats2 <- for_cats  %>% 
                select("MapUnit_Name", "Cover_Type")

get_parkloop_veg3 <- left_join(get_parkloop_veg2, for_cats1, by = c("MapUnit_ID")) 

table(is.na(get_parkloop_veg3$Cover_Type))

for(jj in 1:nrow(get_parkloop_veg3)){
    # if(get_parkloop_veg3$MapUnit_Name[jj] == "Mixed Conifer Plantation"){ 
    #     get_parkloop_veg3$Cover_Type[jj] <- "Mixed Conifer Plantation"
    #     }

    # if(is.na(get_parkloop_veg3$Cover_Type[jj])){
    #     get_parkloop_veg3$Cover_Type[jj] <- get_parkloop_veg3$MapUnit_Name[jj]
    #     } 

    if(is.na(get_parkloop_veg3$Cover_Type[jj])){
        if((nrow(for_cats2[which(for_cats2$MapUnit_Name == get_parkloop_veg3$MapUnit_Name[jj]),2][1])) != 0){
             get_parkloop_veg3$Cover_Type[jj] <- pull(for_cats2[which(for_cats2$MapUnit_Name == get_parkloop_veg3$MapUnit_Name[jj]),2][1])
        }  
    }
}

get_parkloop_veg3$Cover_Type[1] %>% is.na()

table(is.na(get_parkloop_veg3$Cover_Type))

col_map <- 3
if(park_loop == "mima") {col_map <- 13}

get_parkloop_veg3[which(is.na(get_parkloop_veg3$Cover_Type)),col_map]

ggplot(data = get_parkloop_veg3) +
    geom_sf(aes(fill = Cover_Type)) +
    geom_sf(data = xy_sf  %>% filter(park == "MIMA")) +
    theme(legend.position = "bottom") #+
    #theme(legend.position = "none") +
    #ylim(c(4757860, 4765371))

ggplot(data = get_parkloop_veg3) +
    geom_sf(aes(fill = MapUnit_Name)) +
    geom_sf(data = xy_sf  %>% filter(park == "MIMA")) +
    theme(legend.position = "bottom")
    #theme(legend.position = "none")


datatable(get_parkloop_veg3  %>% filter(is.na(Cover_Type)) )


get_parkloop_veg3  %>% filter(is.na(Cover_Type)) %>% select(MapUnit_Name) %>% distinct()

#!TODO: missing: 

# MABI: 
mabi_miss_veg <- c("Mixed Conifer Plantation")      #!TODO:
x <- mabi_vegmap
for_plots_sf <- st_as_sf(for_plots, 
                            coords = c("lonutm", "latutm"), 
                            crs = st_crs(x))

ggplot(data = mabi_vegmap %>% filter(MapUnit_Name %in% mabi_miss_veg)) +
    geom_sf(data = mabi_vegmap) +
    geom_sf(aes(fill = MapUnit_Name)) +
    geom_sf(data = for_plots_sf %>% filter(park == "MABI"), color = "black", shape = 21, size = 3, fill = "red") +
    geom_sf(data = xy_sf %>% filter(park == "MABI"), color = "black", shape = 24, size = 3, fill = "black") +
    theme(legend.position = "bottom",
          plot.title = element_text(hjust = 0.5, size = 22)) +
    ggtitle("MABI") 

# MORR: 
morr_miss_veg <- c(
    "Hemlock - Red Oak - Mixed Hardwood Forest"    #!TODO:
    #"Northeastern Modified Successional Forest"
    )    

ggplot(data = morr_vegmap %>% filter(MapUnit_Name %in% morr_miss_veg)) +
    geom_sf(data = morr_vegmap) +
    geom_sf(aes(fill = MapUnit_Name)) +
    geom_sf(data = for_plots_sf %>% filter(park == "MORR"), color = "black", shape = 21, size = 3, fill = "red") +
    geom_sf(data = xy_sf %>% filter(park == "MORR"), color = "black", shape = 24, size = 3, fill = "black") + 
    theme_bw() +
    theme(legend.position = "bottom",
          plot.title = element_text(hjust = 0.5, size = 22)) +
    ggtitle("MORR") +
    ylim(4510000, 4514856) +
    xlim(536800, 541250) 

# SAGA: 
saga_miss_veg <- "White Pine Successional Forest"   #!TODO:

ggplot(data = saga_vegmap %>% filter(MapUnit_Name %in% saga_miss_veg)) +
    geom_sf(data = saga_vegmap) +
    geom_sf(aes(fill = MapUnit_Name)) +
    geom_sf(data = for_plots_sf %>% filter(park == "SAGA"), color = "black", shape = 21, size = 3, fill = "red") +
    geom_sf(data = xy_sf %>% filter(park == "SAGA"), color = "black", shape = 24, size = 3, fill = "black") + 
    theme_bw() +
    theme(legend.position = "bottom",
          plot.title = element_text(hjust = 0.5, size = 22)) +
    ggtitle("SAGA") +

# SARA: 
sara_miss_veg <- c(
        #"Beech - Maple Glaciated Forest" ,                #!TODO: not key
        #"Boxelder Floodplain Forest" ,                    #!TODO: not key
        #"Northeastern Modified Successional Forest",      #!TODO: not key
        #"Silver Maple - Elm Floodplain Forest",           #!TODO: not key
        #"Silver Maple Floodplain Levee Forest" ,          #!TODO: not key
        #"Swamp White Oak Floodplain Forest",              #!TODO: not key
        #"Terrace Hardwood Floodplain Forest",             #!TODO: not key
        "White Pine Plantation",                          #!TODO:
        "White Pine Successional Forest")                 #!TODO:

ggplot(data = sara_vegmap %>% filter(MapUnit_Name %in% sara_miss_veg)) +
    geom_sf(data = sara_vegmap) +
    geom_sf(aes(fill = MapUnit_Name)) +
    geom_sf(data = for_plots_sf %>% filter(park == "SARA"), color = "black", shape = 21, size = 3, fill = "red") +
    geom_sf(data = xy_sf %>% filter(park == "SARA"), color = "black", shape = 24, size = 3, fill = "black") + 
    theme_bw() +
    theme(legend.position = "bottom",
          plot.title = element_text(hjust = 0.5, size = 22)) +
    ggtitle("SARA") +
    ylim(c(4757860, 4765000)) 

# wefa:
wefa_miss_veg <- c(
    #"Interrupted Fern Variant of Semi-rich Northern Hardwood Forest",                                   
    #"Interrupted Fern Variant of Southern New England / Northern Piedmont Red Maple Seepage Swamp",     
    "Lower New England Red Maple - Blackgum Swamp",                                                     #!TODO: maybe overlaps, maybe keep
    #"Mountain Laurel Variant of Northeastern Dry Oak - Hickory Forest",                                 
    "Northeastern Dry Oak - Hickory Forest",                                                            #!TODO: overlap forest plot
    "Northeastern Dry Oak - Hickory Forest and Lower New England Slope Chestnut Oak Forest complex",    #!TODO: overlap forest plot
    "Northeastern Modified Successional Forest"                                                         #!TODO: does not overlap forest plot
    #"Striped Maple Variant of Northeastern Dry Oak - Hickory Forest"
    )                                   

ggplot(data = wefa_vegmap %>% filter(MapUnit_Name %in% wefa_miss_veg)) +
    geom_sf(data = wefa_vegmap) +
    geom_sf(aes(fill = MapUnit_Name)) +
    geom_sf(data = for_plots_sf %>% filter(park == "WEFA"), color = "black", shape = 21, size = 3, fill = "red") +
    geom_sf(data = xy_sf %>% filter(park == "WEFA"), color = "black", shape = 24, size = 3, fill = "black") + 
    theme_bw() +
    theme(#legend.position = "bottom",
          plot.title = element_text(hjust = 0.5, size = 22)) +
    ggtitle("WEFA") 

# rova:
rova_miss_veg <- c(
   "Eastern White Pine Successional Forest",
   "Hardwood Plantation",
   "Hemlock - Hardwood Swamp",
   "Hemlock / White Pine - Red Oak - Mixed Hardwood Forest",
   "Mixed Pine Conifer Plantation",
   "Northeastern Modified Successional Forest",
   "Northeastern Pin Oak - Swamp White Oak Forest",
   "Red Maple - Blackgum Basin Swamp",
   "Red Maple / Tussock Sedge Wooded Marsh",
   "Red Oak - Heath Woodland / Rocky Summit",
   "White Pine Plantation")

hofr_miss_veg <- c(
   "Eastern White Pine Successional Forest",   # not overlapping forest plot
   "Hardwood Plantation",                      # not overlapping forest plot
   "White Pine Plantation")                    # not overlapping forest plot

ggplot(data = rova_vegmap %>% filter(MapUnit_Name %in% hofr_miss_veg)) +
    geom_sf(data = rova_vegmap) +
    geom_sf(aes(fill = MapUnit_Name)) +
    geom_sf(data = for_plots_sf %>% filter(park == "ROVA"), color = "black", shape = 21, size = 3, fill = "red") +
    geom_sf(data = xy_sf %>% filter(park %in% c("HOFR")), color = "black", shape = 24, size = 3, fill = "black") + 
    theme_bw() +
    theme(legend.position = "bottom",
          plot.title = element_text(hjust = 0.5, size = 22)) +
    ggtitle("HOFR") +
    xlim(c(587400, 589000)) +
    ylim(c(4622900, 4625200))

vama_miss_veg <- c(
    "Mixed Pine Conifer Plantation",                           # not overlapping forest plot
    "Hemlock / White Pine - Red Oak - Mixed Hardwood Forest"   # not overlapping forest plot
)
ggplot(data = rova_vegmap %>% filter(MapUnit_Name %in% rova_miss_veg)) +
    geom_sf(data = rova_vegmap) +
    geom_sf(aes(fill = MapUnit_Name)) +
    geom_sf(data = for_plots_sf %>% filter(park == "ROVA"), color = "black", shape = 21, size = 3, fill = "red") +
    geom_sf(data = xy_sf %>% filter(park %in% c("VAMA")), color = "black", shape = 24, size = 3, fill = "black") + 
    theme_bw() +
    theme(legend.position = "bottom",
          plot.title = element_text(hjust = 0.5, size = 22)) +
    ggtitle("VAMA") +
    xlim(c(587400, 588400)) +
    ylim(c(4626700, 4628909))
    
# mima:

xm <- mima_vegmap
for_plots_sfm <- st_as_sf(for_plots, 
                            coords = c("lonutm", "latutm"), 
                            crs = st_crs(xm))
mima_miss_veg <- c(
    #"Silver Maple - Green Ash - Black Ash Floodplain Forest",      
    "Northern Conifer & Hardwood Acidic Swamp",                     #!TODO: mixed keep - overlap with veg plot
    "Northern & Central Native Ruderal Forest",                     #!TODO: mixed keep - overlap with veg plot
    "Northeastern Oak - Hickory Forest & Woodland",                 #!TODO: mixed keep - overlap with veg plot (some oak + hickory or oak + pine)
    #"North Central Flatwoods & Swamp Forest",                      
    #"Laurentian & Acadian Hardwood Forest",                        
    "Appalachian & Allegheny Northern Hardwood - Conifer Forest")   #!TODO: mixed maybe - overlap with veg plot

ggplot(data = mima_vegmap %>% filter(GROUP_NAME %in% mima_miss_veg)) +
    geom_sf(data = mima_vegmap) +
    geom_sf(aes(fill = GROUP_NAME)) +
    geom_sf(data = for_plots_sfm %>% filter(park == "MIMA"), color = "black", shape = 21, size = 3, fill = "red") +
    geom_sf(data = xy_sf %>% filter(park %in% c("MIMA")), color = "black", shape = 24, size = 3, fill = "black") + 
    theme_bw() +
    theme(legend.position = "bottom",
          plot.title = element_text(hjust = 0.5, size = 22)) +
    ggtitle("MIMA") +
    xlim(c(308500, 314468))

miss_veg <- c(mima_miss_veg,
              rova_miss_veg,
              wefa_miss_veg,
              sara_miss_veg,
              saga_miss_veg,
              morr_miss_veg,
              mabi_miss_veg) %>% 
            unique() %>% 
            as_tibble() %>% 
            rename(MapUnit_Name = value) %>% 
            arrange(MapUnit_Name)

write_csv(miss_veg, file = "data/out/vegclass_miss.csv")
