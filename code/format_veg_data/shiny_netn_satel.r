#? *********************************************************************************
#? ------------------------------   shiny_veg_types   ------------------------------
#? *********************************************************************************
#
#! Code to plot park vegetation types, bird sites, and forest plots, and classify them
#!    and density of trees of each of the three groups. This code will help us understand
#!    whether our environmental covariates are all matching our expectations and representing
#!    the forest properly 
#
#! Source ---------------------------------------------
#           - format_veg_data/veg_maps_park.R : get park shape files with vegetation types and classify each as conifer, hardwood, mixed, or not forest
#           - format_veg_data/get_conhar_baden.R : gets all forest plots and calculates what percentage and value of density and BA is conifer and hardwood
#
#! Input ----------------------------------------------
#           - :
#           - :
#
#! Output ----------------------------------------------
#           - :
#           - :

#! Package library and versions -------------------------
#  Created a library repo?
#  (  )yes  (  )no
#  renv::init()

# Load an existing library?
#  renv::restore()

# Installed new packages?
#  renv::snapshot()

# detach packages and clear workspace
freshr::freshr()

#! Load packages ---------------------------------------
library(tidyverse)
library(conflicted)
library(glue)
library(shiny)
library(ggplot2)
library(sf)
library(dplyr)
library(plotly)
library(DT)
library(ggnewscale)  ## package for multiple scales

conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::filter)
# conflicts_prefer(scales::alpha)

#! Make functions --------------------------------------
colanmes <- colnames
lenght <- length
`%!in%` <- Negate(`%in%`)

radi_dist <- 250

#! Source code -----------------------------------------
#? veggie maps -----------------------------------------
# get park shape files with vegetation types and classify each as conifer, hardwood, mixed, or not forest
source("/Users/bamaral/Documents/GitHub/NPS_bird_copy/code/format_veg_data/veg_maps_park.R")

keep_objects <- c("for_plots_sf", "for_plots_sfm", "xy_sf", 
                  "mabi_vegmap2", "morr_vegmap2", "saga_vegmap2", "sara_vegmap2",
                  "wefa_vegmap2", "rova_vegmap2", "mima_vegmap2", "keep_objects", "radi_dist")

rm(list = setdiff(ls(), keep_objects))

#! NETN Bird covariate data ----------------------------------------------
source('/Users/bamaral/Documents/GitHub/NPS_bird_copy/code/format_veg_data/get_site_data_rad.R')

print(glue("\n\n\n\n\n\n radius distance is {radi_dist} \n\n\n\n\n\n"))

neighbor <- close_points_f2 %>% 
                mutate(park = substr(bird_sit,1,4))

bird_sit_covs <- bird_sit_covs2 %>%
  # Remove "_wei" suffix from all column names
  rename_with(~str_remove(.x, "_wei$"))

bird_sit_covs2 <- bird_sit_covs %>% 
                      mutate(park = substr(bird_sit, 1, 4)) %>% 
                      filter(park %!in% c("ACAD", "ELRO", "SAIR")) %>%
                      rename(Point_Name = bird_sit) 

xy_sf <- left_join(xy_sf, bird_sit_covs2, by = c("Point_Name", "park")) %>% 
                      filter(park %!in% c("ACAD", "ELRO", "SAIR"))

keep_objects2 <- c(keep_objects, "radi_dist", "neighbor", "bird_sit_covs2")
rm(list = setdiff(ls(), keep_objects2))

colanmes <- colnames
lenght <- length
`%!in%` <- Negate(`%in%`)

#! Import data -----------------------------------------
## file paths
COV_FOR_PLY <- "data/out/for_plot_covs.rds"              ## covariate values for each forest plot
SATE_BIR_COV <- "data/conifer_final_aaron.rds"           ## covariate values for bird sites using satelite data
PARK_BOUN_PATH <- "data/out/park_plot_lims.rds"          ## geographic limits to plot parks
SATE_FOR_COV <- "data/NETNForestPlot_Conifer_BA.csv"     ## covariate variables for forest plots using satelite data

## read files
park_bounds <- read_rds(PARK_BOUN_PATH)

#! NETN Forest covariate data ----------------------------------------------
for_plots_covs <- read_rds(file = COV_FOR_PLY) %>% 
                      rename(for_sit = Plot_Name) %>% 
                      select(-UTMZone) %>% 
                      mutate(type = as.character(NA))

for(ii in 1:nrow(for_plots_covs)){

    if(for_plots_covs$BA_m2ha_Conifer[ii] > for_plots_covs$BA_m2ha_Hardwood[ii]) {
        for_plots_covs$type[ii] <- "Conifer"
    } else { for_plots_covs$type[ii] <- "Hardwood" }
}

#safety copy
for_plots_sf1 <- for_plots_sf

# join the forest plot spatial data (location) with the forest covariate data
for_plots_sf <- for_plots_sf %>%
      rename(for_sit = for_sit) %>% 
      left_join(.,for_plots_covs, by = "for_sit")

for_plots_sfm <- for_plots_sfm %>%
      left_join(.,for_plots_covs, by = "for_sit")  # mima has different crs

# rename for horf and vama
for_plots_sfh <- for_plots_sf  %>% filter(park == "ROVA"); for_plots_sfh$park <- "HOFR"   # hofr
for_plots_sfv <- for_plots_sf  %>% filter(park == "ROVA"); for_plots_sfv$park <- "VAMA"   # vama

#! Satelite bird covariate data ----------------------------------------------
bird_sat_cov <- read_rds(file = SATE_BIR_COV)  %>% 
              mutate(Point_Name = paste0(substr(PT_CODE, 1, 4), substr(PT_CODE, 6, 7), substr(PT_CODE, 9, 10))) %>% 
              select(Point_Name, BA_SUM) %>% 
              mutate(basal_m2 = BA_SUM * 0.092903,                      ## feet to m2 in one acre
                     basal_m2_per_ha1 = basal_m2 / (pi * 250^2 / 10000), ## conversion of 250m radius area in square feet/acre to ha
                     ## ORRRR
                     basal_m2_per_ha2 = BA_SUM * 0.229568,   ## feet in 250m radius to m2 in ha
                     park = substr(Point_Name, 1,4 )) 

# Join with xy_sf to get spatial geometry - bird plots
bird_sat_cov_sf <- left_join(xy_sf %>% select(ID, park, Point_Name, UTM_ZONE), 
                             bird_sat_cov, 
                             by = c("Point_Name", "park")) %>%
              filter(!is.na(basal_m2_per_ha1))  # Only keep records that have Aaron's data

hist(as_tibble(bird_sat_cov_sf) %>% pull(basal_m2_per_ha1)) 

#! Satelite forest covariate data ----------------------------------------------
for_sat_cov <- read_csv(SATE_FOR_COV) %>%
  mutate(plot_num = as.numeric(str_extract(Plot_Numbe, "\\d+")),  # Extract numbers
         for_sit = paste0(substr(Unit_ID, 1, 4), "-", str_pad(plot_num, width = 3, side = "left", pad = "0")))  %>% 
         mutate(Merged_Conifer_BA_20092 = Merged_Conifer_BA_2009 * (10000 / (pi * 250^2))) # conversion of 250m radius area to ha)

# get locations for the forest plots
for_sat_cov <- left_join(for_plots_sf1, for_sat_cov, by = "for_sit")

for_sat_cov %>% filter(Unit_ID == "MABI")  %>% ggplot() + geom_point(aes(x = X_Coord, y = Y_Coord))

#? make dataset to go into the shiny app
## xy_sf:  bird NETN covariate data
## for_plots_sf{}: forest NETN covariate data
## bird_sat_cov:   bird SATELITE covariate data  
## for_sat_cov:    forest SATELITE covariate data

# forest
comp_for_plot <- st_join(for_plots_sf %>% select("for_sit", "BA_m2ha_Conifer"), 
                         for_sat_cov %>% select("Merged_Conifer_BA_2009"))  %>% 
                    select("for_sit", "BA_m2ha_Conifer", "Merged_Conifer_BA_2009") %>% 
                    rename(netn_con_ba = BA_m2ha_Conifer,
                           sate_con_ba = Merged_Conifer_BA_2009) %>% 
                    mutate(park = substr(for_sit, 1, 4))

t.test(as_tibble(comp_for_plot) %>% pull(sate_con_ba), 
           as_tibble(comp_for_plot) %>% pull(netn_con_ba))

par(mfrow = c(1,1))
ggplot(comp_for_plot) +
    geom_point(aes(x = sate_con_ba, y = netn_con_ba, col = park)) +
    geom_smooth(aes(x = sate_con_ba, y = netn_con_ba), method = "lm") +
    theme_bw()

ggplot(comp_for_plot) +
    geom_point(aes(x = sate_con_ba, y = netn_con_ba, col = park)) +
    geom_smooth(aes(x = sate_con_ba, y = netn_con_ba), method = "lm") +
    theme_bw()+
    facet_wrap(~park, scales = "free")

par(mfrow = c(1,2))
comp_for_plot %>% pull(sate_con_ba) %>% hist(col = "lightblue", main = "sate_con_ba")
comp_for_plot %>% pull(netn_con_ba) %>% hist(col = "lightgreen", main = "netn_con_ba")
par(mfrow = c(1,1))

# Bird
comp_bir_plot <- st_join(xy_sf %>% select("Point_Name", "BA_m2ha_Conifer"), 
                         bird_sat_cov_sf %>% select("basal_m2_per_ha1"))  %>% 
                    select("Point_Name", "BA_m2ha_Conifer", "basal_m2_per_ha1") %>% 
                    rename(netn_con_ba = BA_m2ha_Conifer,
                           sate_con_ba = basal_m2_per_ha1) %>% 
                    mutate(park = substr(Point_Name, 1, 4))

t.test(as_tibble(comp_bir_plot) %>% pull(sate_con_ba), 
           as_tibble(comp_bir_plot) %>% pull(netn_con_ba))

par(mfrow = c(1,1))
ggplot(comp_bir_plot) +
    geom_point(aes(x = sate_con_ba, y = netn_con_ba, col = park)) +
    geom_smooth(aes(x = sate_con_ba, y = netn_con_ba), method = "lm") +
    theme_bw()

ggplot(comp_bir_plot) +
    geom_point(aes(x = sate_con_ba, y = netn_con_ba, col = park)) +
    geom_smooth(aes(x = sate_con_ba, y = netn_con_ba), method = "lm") +
    theme_bw()+
    facet_wrap(~park, scales = "free")

par(mfrow = c(1,2))
comp_bir_plot %>% pull(sate_con_ba) %>% hist(col = "lightblue", main = "sate_con_ba")
comp_bir_plot %>% pull(netn_con_ba) %>% hist(col = "lightgreen", main = "netn_con_ba")
par(mfrow = c(1,1))

par(mfrow = c(2,2))
comp_bir_plot %>% pull(sate_con_ba) %>% hist(col = "lightblue", main = "satelite bird")
comp_bir_plot %>% pull(netn_con_ba) %>% hist(col = "lightgreen", main = "netn bird")
comp_for_plot %>% pull(sate_con_ba) %>% hist(col = "lightblue", main = "satelite forest")
comp_for_plot %>% pull(netn_con_ba) %>% hist(col = "lightgreen", main = "netn forest")
par(mfrow = c(1,1))

## xy_sf:  bird NETN covariate data
## for_plots_sf{}: forest NETN covariate data
## bird_sat_cov_sf:   bird SATELITE covariate data  
## for_sat_cov:    forest SATELITE covariate data

park_list <- list(
  "MABI" = list(map = mabi_vegmap2, 
                park_lim = park_bounds %>% filter(park == "MABI"), 
                for_plots = for_plots_sf %>% filter(ParkUnit == "MABI"), 
                xy = xy_sf %>% filter(park == "MABI"), 
                neighbor = neighbor %>% filter(park == "MABI"),
                for_sat_cov = for_sat_cov %>% filter(park == "MABI"),
                bird_sat_covi_cov = bird_sat_cov_sf %>% filter(park == "MABI")),
  "MORR" = list(map = morr_vegmap2, 
                park_lim = park_bounds %>% filter(park == "MORR"), 
                for_plots = for_plots_sf %>% filter(ParkUnit == "MORR"),  
                xy = xy_sf %>% filter(park == "MORR"), 
                neighbor = neighbor %>% filter(park == "MORR"),
                for_sat_cov = for_sat_cov %>% filter(park == "MORR"),
                bird_sat_covi_cov = bird_sat_cov_sf %>% filter(park == "MORR")),
  "SAGA" = list(map = saga_vegmap2,
                park_lim = park_bounds %>% filter(park == "SAGA"), 
                for_plots = for_plots_sf %>% filter(ParkUnit == "SAGA"),    
                xy = xy_sf %>% filter(park == "SAGA"), 
                neighbor = neighbor %>% filter(park == "SAGA"),
                for_sat_cov = for_sat_cov %>% filter(park == "SAGA"),
                bird_sat_covi_cov = bird_sat_cov_sf %>% filter(park == "SAGA")),
  "SARA" = list(map = sara_vegmap2, 
                park_lim = park_bounds %>% filter(park == "SARA"),  
                for_plots = for_plots_sf %>% filter(ParkUnit == "SARA"),  
                xy = xy_sf %>% filter(park == "SARA"), 
                neighbor = neighbor %>% filter(park == "SARA"),
                for_sat_cov = for_sat_cov %>% filter(park == "SARA"),
                bird_sat_covi_cov = bird_sat_cov_sf %>% filter(park == "SARA")),
  "WEFA" = list(map = wefa_vegmap2, 
                park_lim = park_bounds %>% filter(park == "WEFA"), 
                for_plots = for_plots_sf %>% filter(ParkUnit == "WEFA"),  
                xy = xy_sf %>% filter(park == "WEFA"), 
                neighbor = neighbor %>% filter(park == "WEFA"),
                for_sat_cov = for_sat_cov %>% filter(park == "WEFA"),
                bird_sat_covi_cov = bird_sat_cov_sf %>% filter(park == "WEFA")),
  "HOFR" = list(map = rova_vegmap2, 
                park_lim = park_bounds %>% filter(park == "HOFR"), 
                for_plots = for_plots_sfh %>% filter(ParkUnit == "HOFR"),  
                xy = xy_sf %>% filter(park == "HOFR"), 
                neighbor = neighbor %>% filter(park == "HOFR"),
                for_sat_cov = for_sat_cov %>% filter(park == "HOFR"),
                bird_sat_covi_cov = bird_sat_cov_sf %>% filter(park == "HOFR")),
  "VAMA" = list(map = rova_vegmap2, 
                park_lim = park_bounds %>% filter(park == "VAMA"), 
                for_plots = for_plots_sfv %>% filter(ParkUnit == "VAMA"),  
                xy = xy_sf %>% filter(park == "VAMA"), 
                neighbor = neighbor %>% filter(park == "VAMA"),
                for_sat_cov = for_sat_cov %>% filter(park == "VAMA"),
                bird_sat_covi_cov = bird_sat_cov_sf %>% filter(park == "VAMA")),
  "MIMA" = list(map = mima_vegmap2, 
                park_lim = park_bounds %>% filter(park == "MIMA"), 
                for_plots = for_plots_sfm %>% filter(ParkUnit == "MIMA"),  
                xy = xy_sf %>% filter(park == "MIMA"), 
                neighbor = neighbor %>% filter(park == "MIMA"),
                for_sat_cov = for_sat_cov %>% filter(park == "MIMA"),
                bird_sat_covi_cov = bird_sat_cov_sf %>% filter(park == "MIMA"))
)

#? veggie types to only forest or not-forest (Cover_Type2)
for(kk in 1:lenght(park_list)){
    
    park_list[[kk]]$map$Cover_Type2 <- NA

    for(ll in 1:nrow(park_list[[kk]]$map)){
      current_type <- park_list[[kk]]$map$Cover_Type[ll]
      
      if(!is.na(current_type)) {
        if(current_type == "Not forest") {
          park_list[[kk]]$map$Cover_Type2[ll] <- "Not forest"
        } else {
          park_list[[kk]]$map$Cover_Type2[ll] <- "Forest"
        }
      }
      # If it's NA, Cover_Type2 remains NA
    }
}

write_rds(park_list, file = "data/out/park_list.rds")

all_cover_types <- unique(unlist(lapply(park_list, function(x) unique(x$map$Cover_Type))))
palette <- c("#a0a0a0", "#68c568", "#3a78dc", "#c98b19", "#dcdada")
cover_type_colors <- setNames(palette[seq_along(all_cover_types)], all_cover_types)

for_nofor <- unique(unlist(lapply(park_list, function(x) unique(x$map$Cover_Type2))))
palette2 <- c("#a0a0a0", "#68c568")
for_nofor_colors <- setNames(palette2[seq_along(for_nofor)], for_nofor)

plot_id <- unique(unlist(lapply(park_list, function(x) unique(x$close_points$bird_sit))))
plot_id_park <- substr(plot_id, 1,4) 
#plot_id <- cbind(plot_id, plot_id_park) %>% as_tibble()

# Create a large color palette with enough colors for all bird sites
# Using a combination of different color palettes to get enough distinct colors
plot_palette <- c(
  # Primary colors (12)
  "#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00", "#ffff33", 
  "#a65628", "#f781bf", "#999999", "#66c2a5", "#fc8d62", "#8da0cb",
  # Additional vibrant colors (12)
  "#1f78b4", "#33a02c", "#e31a1c", "#ff7f00", "#cab2d6", "#6a3d9a",
  "#b15928", "#fb9a99", "#a6cee3", "#b2df8a", "#fdbf6f", "#ffff99",
  # More distinct colors (12)  
  "#8dd3c7", "#ffffb3", "#bebada", "#fb8072", "#80b1d3", "#fdb462",
  "#b3de69", "#fccde5", "#d9d9d9", "#bc80bd", "#ccebc5", "#ffed6f",
  # Even more colors (12)
  "#a50026", "#d73027", "#f46d43", "#fdae61", "#fee08b", "#ffffbf",
  "#e6f598", "#abd9e9", "#74add1", "#4575b4", "#313695", "#006837"
)

# Ensure we have enough colors by repeating if needed
total_bird_sites <- length(plot_id[!is.na(plot_id)])
if(length(plot_palette) < total_bird_sites) {
  plot_palette <- rep(plot_palette, ceiling(total_bird_sites / length(plot_palette)))
}

plot_id_colors <- setNames(plot_palette[1:total_bird_sites], plot_id[!is.na(plot_id)])

# Better map names:
# vegmap -> bird_netn_map (Bird sites with NETN data)
# vegmap3 -> bird_satellite_map (Bird sites with satellite data)
# vegmap2 -> forest_netn_map (Forest plots with NETN data)
# vegmap32 -> forest_satellite_map (Forest plots with satellite data)
# + bird_difference_map (Difference between NETN and satellite for bird sites)
# + forest_difference_map (Difference between NETN and satellite for forest plots)

ui <- fluidPage(
  titlePanel(glue("NPS Park Bird Sites ({radi_dist} radius) and Forest Plots")),
  sidebarLayout(
    sidebarPanel(
      selectInput("park", "Choose a Park:", choices = names(park_list), selected = "MABI")
    ),
    mainPanel(
      h3("Bird Sites"),
      plotOutput("bird_netn_map", height = "500px"),
      plotOutput("bird_satellite_map", height = "500px"),
      plotOutput("bird_difference_map", height = "500px"),
      plotlyOutput("bird_comparison_plot", height = "500px"),
      
      h3("Forest Plots"),
      plotOutput("forest_netn_map", height = "500px"),
      plotOutput("forest_satellite_map", height = "500px"),
      plotOutput("forest_difference_map", height = "500px"),
      plotlyOutput("forest_comparison_plot", height = "500px")
    )
  )
)

server <- function(input, output, session) {
  
  # Bird Sites - NETN Data
  output$bird_netn_map <- renderPlot({
    park_data <- park_list[[input$park]]
    
    plot_points <- park_data$for_plots 
    current_bird_sits <- unique(park_data$xy$Point_Name)
    current_colors <- setNames(plot_palette[1:length(current_bird_sits)], current_bird_sits)
    
    bird_values_sat <- park_data$bird_sat_covi_cov %>%
                  filter(park == input$park) %>% 
                  pull(basal_m2_per_ha1)
    bird_values_netn <- park_data$xy$BA_m2ha_Conifer
    combined_range <- range(c(bird_values_sat, bird_values_netn), na.rm = TRUE)

    p <- 
      ggplot(data = park_data$map) +
        geom_sf(aes(fill = Cover_Type2)) +
        scale_fill_manual(values = for_nofor_colors, na.value = "grey80") +
        ggnewscale::new_scale_fill() +
        geom_segment(data = park_data$neighbor,
                      aes(x = lonutmb, y = latutmb, 
                          xend = lonutmf, yend = latutmf, 
                          colour = bird_sit)) +
        scale_color_manual(values = current_colors, guide = "none") + 
        geom_sf(data = plot_points, size = 2, color = "black") +       
        geom_sf(data = park_data$xy, 
                aes(fill = BA_m2ha_Conifer), 
                shape = 21, size = 7, stroke = 0.5) +
        scale_fill_viridis_c(name = "Conifer BA", 
                            option = "plasma",
                            na.value = "grey50",
                            limits = combined_range) +
        theme_bw() +
        theme(legend.position = "right",
              legend.text = element_text(size = 8),
              legend.title = element_text(size = 9),
              plot.title = element_text(hjust = 0.5, size = 22)) +
        labs(title = glue("Bird Sites - NETN Data - {input$park}")) +
        scale_x_continuous(limits = c(pull(park_data$park_lim[1,2]), pull(park_data$park_lim[1,3]))) +
        scale_y_continuous(limits = c(pull(park_data$park_lim[1,4]), pull(park_data$park_lim[1,5])))
      
    print(p)
  })
  
  # Bird Sites - Satellite Data
  output$bird_satellite_map <- renderPlot({
    park_data <- park_list[[input$park]]
    current_bird_sits <- unique(park_data$xy$Point_Name)
    current_colors <- setNames(plot_palette[1:length(current_bird_sits)], current_bird_sits)
    plot_points <- park_data$for_plots

    bird_values_sat <- park_data$bird_sat_covi_cov %>%
                  filter(park == input$park) %>% 
                  pull(basal_m2_per_ha1)
    bird_values_netn <- park_data$xy$BA_m2ha_Conifer
    combined_range <- range(c(bird_values_sat, bird_values_netn), na.rm = TRUE)

    r <- 
      ggplot(data = park_data$map) +
        geom_sf(aes(fill = Cover_Type2)) +
        scale_fill_manual(values = for_nofor_colors, na.value = "grey80") +
        ggnewscale::new_scale_fill() +
        geom_segment(data = park_data$neighbor,
                      aes(x = lonutmb, y = latutmb, 
                          xend = lonutmf, yend = latutmf, 
                          colour = bird_sit)) +
        scale_color_manual(values = current_colors, guide = "none") + 
        geom_sf(data = plot_points, size = 3, color = "black") +        
        geom_sf(data = park_data$bird_sat_covi_cov %>%
                  filter(park == input$park), 
                aes(fill = basal_m2_per_ha1), 
                shape = 21, size = 7, stroke = 1) +
        scale_fill_viridis_c(name = "Conifer BA", 
                            option = "plasma", 
                            na.value = "grey50",
                            limits = combined_range) +
        theme_bw() +
        theme(legend.position = "right",
              legend.text = element_text(size = 8),
              legend.title = element_text(size = 9),
              plot.title = element_text(hjust = 0.5, size = 22)) +
        labs(title = glue("Bird Sites - Satellite Data - {input$park}")) +
        scale_x_continuous(limits = c(pull(park_data$park_lim[1,2]), pull(park_data$park_lim[1,3]))) +
        scale_y_continuous(limits = c(pull(park_data$park_lim[1,4]), pull(park_data$park_lim[1,5])))

    print(r)
  })

  # Bird Sites - Difference Map (NETN - Satellite)
  output$bird_difference_map <- renderPlot({
    park_data <- park_list[[input$park]]
    current_bird_sits <- unique(park_data$xy$Point_Name)
    current_colors <- setNames(plot_palette[1:length(current_bird_sits)], current_bird_sits)
    plot_points <- park_data$for_plots

    # Calculate differences
    bird_netn <- park_data$xy %>% select(Point_Name, BA_m2ha_Conifer)
    bird_sat <- park_data$bird_sat_covi_cov %>% 
                filter(park == input$park) %>% 
                select(Point_Name, basal_m2_per_ha1)
    
    bird_diff <- st_join(bird_netn, bird_sat) %>%
                 mutate(difference = BA_m2ha_Conifer - basal_m2_per_ha1)
    
    diff_range <- range(bird_diff$difference, na.rm = TRUE)

    d1 <- 
      ggplot(data = park_data$map) +
        geom_sf(aes(fill = Cover_Type2)) +
        scale_fill_manual(values = for_nofor_colors, na.value = "grey80") +
        ggnewscale::new_scale_fill() +
        geom_segment(data = park_data$neighbor,
                      aes(x = lonutmb, y = latutmb, 
                          xend = lonutmf, yend = latutmf, 
                          colour = bird_sit)) +
        scale_color_manual(values = current_colors, guide = "none") + 
        geom_sf(data = plot_points, size = 3, color = "black") +        
        geom_sf(data = bird_diff, 
                aes(fill = difference), 
                shape = 21, size = 7, stroke = 1) +
        scale_fill_gradient2(name = "Difference\n(NETN - Satellite)", 
                            low = "red", mid = "white", high = "blue",
                            midpoint = 0,
                            na.value = "grey50",
                            limits = diff_range) +
        theme_bw() +
        theme(legend.position = "right",
              legend.text = element_text(size = 8),
              legend.title = element_text(size = 9),
              plot.title = element_text(hjust = 0.5, size = 22)) +
        labs(title = glue("Bird Sites - Difference (NETN - Satellite) - {input$park}")) +
        scale_x_continuous(limits = c(pull(park_data$park_lim[1,2]), pull(park_data$park_lim[1,3]))) +
        scale_y_continuous(limits = c(pull(park_data$park_lim[1,4]), pull(park_data$park_lim[1,5])))

    print(d1)
  })

  # Bird Comparison Plot
  output$bird_comparison_plot <- renderPlotly({
    park_data <- park_list[[input$park]]
    
    comp_net_sat2 <- comp_bir_plot %>%
      filter(park == input$park)
    
    max_sca <- ceiling(max(comp_net_sat2 %>% as_tibble() %>% select(netn_con_ba, sate_con_ba), na.rm = T) / 10) * 10
    min_sca <- floor(min(comp_net_sat2 %>% as_tibble() %>% select(netn_con_ba, sate_con_ba), na.rm = T) / 10) * 10

    q <- ggplot(data = comp_net_sat2) +
      geom_point(aes(x = netn_con_ba, y = sate_con_ba, 
                     text = paste0("Bird Point: ", Point_Name, "<br>",
                                  "NETN BA: ", round(netn_con_ba, 2), " m²/ha<br>",
                                  "Satelite BA: ", round(sate_con_ba, 2), " m²/ha")), 
                 size = 2) +
      xlim(min_sca, max_sca) +
      ylim(min_sca, max_sca) +
      geom_smooth(aes(x = netn_con_ba, y = sate_con_ba), method = "lm", se = FALSE) +
      geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed", linewidth = 1) +
      labs(x = "NETN BA Estimates (m²/ha)", y = "Satelite BA Estimates (m²/ha)",
           title = glue("Bird Sites Comparison - {input$park}")) +
      theme_bw() +
      theme(legend.position = "right",
            legend.text = element_text(size = 12),
            plot.title = element_text(hjust = 0.5, size = 16))
    
    ggplotly(q, tooltip = "text")
  })

  # Forest Plots - NETN Data
  output$forest_netn_map <- renderPlot({
    park_data <- park_list[[input$park]]
    
    plot_points <- park_data$for_plots 
    current_bird_sits <- unique(park_data$xy$Point_Name)
    current_colors <- setNames(plot_palette[1:length(current_bird_sits)], current_bird_sits)

    for_values_sat <- park_data$for_sat_cov %>%
                  pull(Merged_Conifer_BA_2009)
    for_values_netn <- plot_points$BA_m2ha_Conifer
    combined_range <- range(c(for_values_sat, for_values_netn), na.rm = TRUE)

    p2 <- 
      ggplot(data = park_data$map) +
        geom_sf(aes(fill = Cover_Type2)) +
        scale_fill_manual(values = for_nofor_colors, na.value = "grey80") +
        ggnewscale::new_scale_fill() +
        geom_segment(data = park_data$neighbor,
                      aes(x = lonutmb, y = latutmb, 
                          xend = lonutmf, yend = latutmf, 
                          colour = bird_sit)) +
        scale_color_manual(values = current_colors, guide = "none") + 
        geom_sf(data = park_data$xy, size = 3, color = "black") +       
        geom_sf(data = plot_points, 
                aes(fill = BA_m2ha_Conifer),  
                shape = 23, size = 6, stroke = 1, color = "black") +  
        scale_fill_viridis_c(name = "Conifer BA", 
                            option = "plasma", 
                            na.value = "grey50",
                            limits = combined_range) +
        theme_bw() +
        theme(legend.position = "right",
              legend.text = element_text(size = 8),
              legend.title = element_text(size = 9),
              plot.title = element_text(hjust = 0.5, size = 22)) +
        labs(title = glue("Forest Plots - NETN Data - {input$park}")) +
        scale_x_continuous(limits = c(pull(park_data$park_lim[1,2]), pull(park_data$park_lim[1,3]))) +
        scale_y_continuous(limits = c(pull(park_data$park_lim[1,4]), pull(park_data$park_lim[1,5])))
      
    print(p2)
  })
  
  # Forest Plots - Satellite Data
  output$forest_satellite_map <- renderPlot({
    park_data <- park_list[[input$park]]
    plot_points <- park_data$for_plots 
    current_bird_sits <- unique(park_data$xy$Point_Name)
    current_colors <- setNames(plot_palette[1:length(current_bird_sits)], current_bird_sits)

    for_values_sat <- park_data$for_sat_cov %>%
                  pull(Merged_Conifer_BA_2009)
    for_values_netn <- plot_points$BA_m2ha_Conifer
    combined_range <- range(c(for_values_sat, for_values_netn), na.rm = TRUE)
    
    r2 <- 
      ggplot(data = park_data$map) +
        geom_sf(aes(fill = Cover_Type2)) +
        scale_fill_manual(values = for_nofor_colors, na.value = "grey80") +
        ggnewscale::new_scale_fill() +
        geom_segment(data = park_data$neighbor,
                      aes(x = lonutmb, y = latutmb, 
                          xend = lonutmf, yend = latutmf, 
                          colour = bird_sit)) +
        scale_color_manual(values = current_colors, guide = "none") + 
        geom_sf(data = park_data$xy, size = 3, color = "black") +              
        geom_sf(data = park_data$for_sat_cov %>%
                  filter(park == input$park), 
                aes(fill = Merged_Conifer_BA_2009), 
                shape = 23, size = 6, stroke = 1, color = "black") +
        scale_fill_viridis_c(name = "Conifer BA", 
                            option = "plasma", 
                            na.value = "grey50",
                            limits = combined_range) +
        theme_bw() +
        theme(legend.position = "right",
              legend.text = element_text(size = 8),
              legend.title = element_text(size = 9),
              plot.title = element_text(hjust = 0.5, size = 22)) +
        labs(title = glue("Forest Plots - Satellite Data - {input$park}")) +
        scale_x_continuous(limits = c(pull(park_data$park_lim[1,2]), pull(park_data$park_lim[1,3]))) +
        scale_y_continuous(limits = c(pull(park_data$park_lim[1,4]), pull(park_data$park_lim[1,5])))
      
    print(r2)
  })

  # Forest Plots - Difference Map (NETN - Satellite)
  output$forest_difference_map <- renderPlot({
    park_data <- park_list[[input$park]]
    current_bird_sits <- unique(park_data$xy$Point_Name)
    current_colors <- setNames(plot_palette[1:length(current_bird_sits)], current_bird_sits)
    
    # Calculate differences
    forest_netn <- park_data$for_plots %>% select(for_sit, BA_m2ha_Conifer)
    forest_sat <- park_data$for_sat_cov %>% 
                  filter(park == input$park) %>% 
                  select(for_sit, Merged_Conifer_BA_2009)
    
    forest_diff <- st_join(forest_netn, forest_sat) %>%
                   mutate(difference = BA_m2ha_Conifer - Merged_Conifer_BA_2009)
    
    diff_range <- range(forest_diff$difference, na.rm = TRUE)

    d2 <- 
      ggplot(data = park_data$map) +
        geom_sf(aes(fill = Cover_Type2)) +
        scale_fill_manual(values = for_nofor_colors, na.value = "grey80") +
        ggnewscale::new_scale_fill() +
        geom_segment(data = park_data$neighbor,
                      aes(x = lonutmb, y = latutmb, 
                          xend = lonutmf, yend = latutmf, 
                          colour = bird_sit)) +
        scale_color_manual(values = current_colors, guide = "none") + 
        geom_sf(data = park_data$xy, size = 3, color = "black") +              
        geom_sf(data = forest_diff, 
                aes(fill = difference), 
                shape = 23, size = 6, stroke = 1, color = "black") +
        scale_fill_gradient2(name = "Difference\n(NETN - Satellite)", 
                            low = "red", mid = "white", high = "blue",
                            midpoint = 0,
                            na.value = "grey50",
                            limits = diff_range) +
        theme_bw() +
        theme(legend.position = "right",
              legend.text = element_text(size = 8),
              legend.title = element_text(size = 9),
              plot.title = element_text(hjust = 0.5, size = 22)) +
        labs(title = glue("Forest Plots - Difference (NETN - Satellite) - {input$park}")) +
        scale_x_continuous(limits = c(pull(park_data$park_lim[1,2]), pull(park_data$park_lim[1,3]))) +
        scale_y_continuous(limits = c(pull(park_data$park_lim[1,4]), pull(park_data$park_lim[1,5])))

    print(d2)
  })

  # Forest Comparison Plot
  output$forest_comparison_plot <- renderPlotly({
    park_data <- park_list[[input$park]]
    
    comp_net_sat2 <- comp_for_plot %>%
      filter(park == input$park)
    
    max_sca <- ceiling(max(comp_net_sat2 %>% as_tibble() %>% select(netn_con_ba, sate_con_ba), na.rm = T) / 10) * 10
    min_sca <- floor(min(comp_net_sat2 %>% as_tibble() %>% select(netn_con_ba, sate_con_ba), na.rm = T) / 10) * 10

    q2 <- ggplot(data = comp_net_sat2) +
      geom_point(aes(x = netn_con_ba, y = sate_con_ba, 
                     text = paste0("Forest Plot: ", for_sit, "<br>",
                                  "NETN BA: ", round(netn_con_ba, 2), " m²/ha<br>",
                                  "Satelite BA: ", round(sate_con_ba, 2), " m²/ha")), 
                 size = 2) +
      xlim(min_sca, max_sca) +
      ylim(min_sca, max_sca) +
      geom_smooth(aes(x = netn_con_ba, y = sate_con_ba), method = "lm", se = FALSE) +
      geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed", linewidth = 1) +
      labs(x = "NETN BA Estimates (m²/ha)", y = "Satelite BA Estimates (m²/ha)", 
           title = glue("Forest Plots Comparison - {input$park}")) +
      theme_bw() +
      theme(legend.position = "right",
            legend.text = element_text(size = 12),
            plot.title = element_text(hjust = 0.5, size = 16))
    
    ggplotly(q2, tooltip = "text")
  })
}

shinyApp(ui, server)
## ( x ) send bird sites values to aaron
## (   ) check park errors
## (   ) make a single scale for bottom plots
## ( x ) add park limits/boundaries for plots
## ( x ) sensitivity analysis - how many neighbours I get and how estimates changes as the radius gets bigger
## ( x ) classify everything as forest and not forest
## ( x ) connect bird and forest sites
## ( x ) remove the red circles and triangles - same symbol
## ( x ) get percentage hardwood and conifer for bird sites
## ( x ) weight that percentage according to distance to point
## ( x ) create same map with surface heat map
## ( x ) compare with aarons estimates
## ( x ) get some measure of stand structure (how much vegetation in each strata)
## ( x ) get same covariates as birds (close_points_etc) for the forest sites
## ( x ) gradient of color for the covariates for each bird site and plot (e.g. ba 0.2 is blue and 0.8 is green)


