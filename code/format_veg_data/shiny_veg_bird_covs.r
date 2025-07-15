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

#! NETN Bird ----------------------------------------------
source('/Users/bamaral/Documents/GitHub/NPS_bird_copy/code/format_veg_data/get_site_data_rad.R')

print(glue("\n\n\n\n\n\n radius distance is {radi_dist} \n\n\n\n\n\n"))

neighbor <- close_points_f2%>% 
                mutate(park = substr(bird_sit,1,4))

bird_sit_covs <- bird_sit_covs2%>%
  # Remove "_wei" suffix from all column names
  rename_with(~str_remove(.x, "_wei$"))

keep_objects2 <- c(keep_objects, "radi_dist", "neighbor", "bird_sit_covs")
rm(list = setdiff(ls(), keep_objects2))

colanmes <- colnames
lenght <- length
`%!in%` <- Negate(`%in%`)
#? forest covariates -----------------------------------
# gets all forest plots and calculates what percentage and value of density and BA is conifer and hardwood
#source("/Users/bamaral/Documents/GitHub/NPS_bird_copy/code/format_veg_data/get_conhar_baden.R")

#! Import data -----------------------------------------
## file paths
COV_FOR_PLY <- "data/out/for_plot_covs.rds"
AAR_FOR_COV <- "data/conifer_final_aaron.rds"
SATE_BIR_COV <- "data/conifer_final_aaron.rds"
PARK_BOUN_PATH <- "data/out/park_plot_lims.rds"

## read files
park_bounds <- read_rds(PARK_BOUN_PATH)

#! Satelite Forest ----------------------------------------------
aa_covs_for <- read_rds(AAR_FOR_COV) 

#! NETN Forest ----------------------------------------------
for_plots_covs <- read_rds(file = COV_FOR_PLY) %>% 
                      rename(for_sit = Plot_Name) %>% 
                      select(-UTMZone) %>% 
                      mutate(type = as.character(NA))

for(ii in 1:nrow(for_plots_covs)){

    if(for_plots_covs$BA_m2ha_Conifer[ii] > for_plots_covs$BA_m2ha_Hardwood[ii]) {
        for_plots_covs$type[ii] <- "Conifer"
    } else { for_plots_covs$type[ii] <- "Hardwood" }
}

for_plots_sf1 <- for_plots_sf

# Prepare forest plot data
for_plots_sf <- for_plots_sf %>%
      rename(for_sit = for_sit) %>% 
      left_join(.,for_plots_covs, by = "for_sit")

# shiny for parks, forest type_plot, and new covariates
for_plots_sfh <- for_plots_sf  %>% filter(park == "ROVA"); for_plots_sfh$park <- "HOFR"   # hofr
for_plots_sfv <- for_plots_sf  %>% filter(park == "ROVA"); for_plots_sfv$park <- "VAMA"   # vama
for_plots_sfm <- for_plots_sfm %>%
      left_join(.,for_plots_covs, by = "for_sit")  # mima

bird_sit_covs2 <- bird_sit_covs %>% 
                      mutate(park = substr(bird_sit, 1, 4)) %>% 
                      filter(park %!in% c("ACAD", "ELRO", "SAIR")) %>%
                      rename(Point_Name = bird_sit) 

#! Satelite Bird ----------------------------------------------
aar_con <- read_rds(file = SATE_BIR_COV)  %>% 
              mutate(Point_Name = paste0(substr(PT_CODE, 1, 4), substr(PT_CODE, 6, 7), substr(PT_CODE, 9, 10))) %>% 
              select(Point_Name, BA_SUM) %>% 
              mutate(BA_SUM = BA_SUM * (10000 / (pi * 250^2)), # conversion of 250m raius area to ha
                     park = substr(Point_Name, 1,4 )) 

# Join with xy_sf to get spatial geometry - bird plots
aar_con_sf <- left_join(xy_sf, aar_con, by = c("Point_Name", "park")) %>%
              filter(!is.na(BA_SUM))  # Only keep records that have Aaron's data

hist(as_tibble(aar_con) %>% pull(BA_SUM)) 

# forest sdatelite plots?
aaron2 <- read_csv("data/NETNForestPlot_Conifer_BA.csv") %>%
  mutate(plot_num = as.numeric(str_extract(Plot_Numbe, "\\d+")),  # Extract numbers
         for_sit = paste0(substr(Unit_ID, 1, 4), "-", str_pad(plot_num, width = 3, side = "left", pad = "0")))  %>% 
         mutate(Merged_Conifer_BA_2009 = Merged_Conifer_BA_2009 * (10000 / (pi * 250^2))) # conversion of 250m raius area to ha)

comp_for_plot <- full_join(for_plots_sf, aaron2, by = "for_sit")  %>% 
                    select("for_sit","UTMZone","park","geometry","ParkUnit","X","Y" , "BA_m2ha_Conifer", "Merged_Conifer_BA_2009")

t.test(as_tibble(comp_for_plot) %>% pull(Merged_Conifer_BA_2009), 
           as_tibble(comp_for_plot) %>% pull(BA_m2ha_Conifer))

par(mfrow = c(1,1))
ggplot(comp_for_plot) +
    geom_point(aes(x = Merged_Conifer_BA_2009, y = BA_m2ha_Conifer)) +
    geom_smooth(aes(x = Merged_Conifer_BA_2009, y = BA_m2ha_Conifer), method = "lm") +
    theme_bw()

aaron2 <- left_join(for_plots_sf1, aaron2, by = "for_sit")

aaron2 %>% filter(Unit_ID == "MABI")  %>% ggplot() + geom_point(aes(x = X_Coord, y = Y_Coord))

xy_sf <- left_join(xy_sf, bird_sit_covs2, by = c("Point_Name", "park")) %>% 
                      filter(park %!in% c("ACAD", "ELRO", "SAIR"))

comp_net_sat <- full_join(as_tibble(xy_sf), as_tibble(aar_con_sf), by = c( "ID","park","Point_Name", "UTM_ZONE","geometry" ))

par(mfrow = c(1,2))
hist(as_tibble(comp_net_sat) %>% pull(BA_SUM)) 
hist(as_tibble(comp_net_sat) %>% pull(BA_m2ha_Conifer)) 

ggplot() + 
  geom_sf(data = aar_con_sf %>% filter(park == "MABI"), color = "blue") + 
  geom_sf(data = xy_sf %>% filter(park == "MABI"), color = "red", size = 2, shape = 21)

t.test(as_tibble(comp_net_sat) %>% pull(BA_SUM), 
           as_tibble(comp_net_sat) %>% pull(BA_m2ha_Conifer))

par(mfrow = c(1,1))
ggplot(comp_net_sat) +
    geom_point(aes(x = BA_SUM, y = BA_m2ha_Conifer)) +
    geom_smooth(aes(x = BA_SUM, y = BA_m2ha_Conifer), method = "lm") +
    theme_bw()

comp_net_sat1 <- 
  comp_net_sat %>% 
        arrange(Point_Name) %>% 
        as_tibble() %>% 
        mutate(difference = BA_SUM - BA_m2ha_Conifer)

#? make dataset to go into the shiny app
park_list <- list(
  "MABI" = list(map = mabi_vegmap2, 
                park_lim = park_bounds %>% filter(park == "MABI"), 
                for_plots = for_plots_sf %>% filter(ParkUnit == "MABI"), 
                xy = xy_sf %>% filter(park == "MABI"), 
                neighbor = neighbor%>% filter(park == "MABI"),
                aar_coni_cov = aar_con_sf %>% filter(park == "MABI")),
  "MORR" = list(map = morr_vegmap2, 
                park_lim = park_bounds %>% filter(park == "MORR"), 
                for_plots = for_plots_sf %>% filter(ParkUnit == "MORR"),  
                xy = xy_sf %>% filter(park == "MORR"), 
                neighbor = neighbor %>% filter(park == "MORR"),
                aar_coni_cov = aar_con_sf %>% filter(park == "MORR")),
  "SAGA" = list(map = saga_vegmap2,
                park_lim = park_bounds %>% filter(park == "SAGA"), 
                for_plots = for_plots_sf %>% filter(ParkUnit == "SAGA"),    
                xy = xy_sf %>% filter(park == "SAGA"), 
                neighbor = neighbor %>% filter(park == "SAGA"),
                aar_coni_cov = aar_con_sf %>% filter(park == "SAGA")),
  "SARA" = list(map = sara_vegmap2, 
                park_lim = park_bounds %>% filter(park == "SARA"),  
                for_plots = for_plots_sf %>% filter(ParkUnit == "SARA"),  
                xy = xy_sf %>% filter(park == "SARA"), 
                neighbor = neighbor %>% filter(park == "SARA"),
                aar_coni_cov = aar_con_sf %>% filter(park == "SARA")),
  "WEFA" = list(map = wefa_vegmap2, 
                park_lim = park_bounds %>% filter(park == "WEFA"), 
                for_plots = for_plots_sf %>% filter(ParkUnit == "WEFA"),  
                xy = xy_sf %>% filter(park == "WEFA"), 
                neighbor = neighbor %>% filter(park == "WEFA"),
                aar_coni_cov = aar_con_sf %>% filter(park == "WEFA")),
  "HOFR" = list(map = rova_vegmap2, 
                park_lim = park_bounds %>% filter(park == "HOFR"), 
                for_plots = for_plots_sfh %>% filter(ParkUnit == "HOFR"),  
                xy = xy_sf %>% filter(park == "HOFR"), 
                neighbor = neighbor %>% filter(park == "HOFR"),
                aar_coni_cov = aar_con_sf %>% filter(park == "HOFR")),
  "VAMA" = list(map = rova_vegmap2, 
                park_lim = park_bounds %>% filter(park == "VAMA"), 
                for_plots = for_plots_sfv %>% filter(ParkUnit == "VAMA"),  
                xy = xy_sf %>% filter(park == "VAMA"), 
                neighbor = neighbor %>% filter(park == "VAMA"),
                aar_coni_cov = aar_con_sf %>% filter(park == "VAMA")),
  "MIMA" = list(map = mima_vegmap2, 
                park_lim = park_bounds %>% filter(park == "MIMA"), 
                for_plots = for_plots_sfm %>% filter(ParkUnit == "MIMA"),  
                xy = xy_sf %>% filter(park == "MIMA"), 
                neighbor = neighbor %>% filter(park == "MIMA"),
                aar_coni_cov = aar_con_sf %>% filter(park == "MIMA"))
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

ui <- fluidPage(
  titlePanel(glue("NPS Park Bird Sites ({radi_dist} radius) and Forest Plots")),
  sidebarLayout(
    sidebarPanel(
      selectInput("park", "Choose a Park:", choices = names(park_list), selected = "MABI")
    ),
    mainPanel(
      plotOutput("vegmap", height = "500px"),
      plotOutput("vegmap3", height = "500px"),
      plotlyOutput("complot", height = "500px"),
      plotOutput("vegmap2", height = "500px"),
      plotOutput("vegmap32", height = "500px"),
      plotlyOutput("complot2", height = "500px"),
      ),
    )
  )

# park_data <- park_list[["MABI"]]

server <- function(input, output, session) {
  output$vegmap <- renderPlot({
    park_data <- park_list[[input$park]]
    
    plot_points <- park_data$for_plots 
    
    current_bird_sits <- unique(park_data$xy$Point_Name)
    current_colors <- setNames(plot_palette[1:length(current_bird_sits)], current_bird_sits)
    
    bird_values_sat <- park_data$aar_coni_cov %>%
                  filter(park == input$park) %>% 
                  pull(BA_SUM)
    bird_values_netn <- park_data$xy$BA_m2ha_Conifer
    combined_range <- range(c(bird_values_sat, bird_values_netn), na.rm = TRUE)

    p <- 
      ggplot(data = park_data$map) +
        geom_sf(aes(fill = Cover_Type2, 
                    text = paste("MapUnit:", MapUnit_Name, "<br>Cover Type:", Cover_Type2))) +
        scale_fill_manual(values = for_nofor_colors, na.value = "grey80") +
        
        # Add new scale for the continuous fill
        ggnewscale::new_scale_fill() +
        
        geom_segment(data = park_data$neighbor,
                      aes(x = lonutmb, y = latutmb, 
                          xend = lonutmf, yend = latutmf, 
                          colour = bird_sit)) +
        scale_color_manual(values = current_colors) + 
        
        geom_sf(data = plot_points,
                size = 2, 
                color = "black",
                aes(text = paste0("Plot: ", for_sit, "<br>",
                                 "Conifer Den: ", treeden_ha_Conifer, "<br>",
                                 "Conifer BA: ", BA_m2ha_Conifer))) +       
        
        geom_sf(data = park_data$xy, 
                aes(fill = BA_m2ha_Conifer, 
                    color = Point_Name,
                    text = paste0("Bird Site: ", Point_Name, "<br>",
                                 "Conifer Den: ", treeden_ha_Conifer, "<br>",
                                 "Conifer BA: ", BA_m2ha_Conifer)), 
                shape = 21, size = 7, stroke = 0.5) +  # Use shape 21 for fill + color
        guides(color = "none") +
        scale_fill_viridis_c(name = "Conifer BA", 
                            option = "plasma",
                            na.value = "grey50",
                            limits = combined_range) +
        theme_bw() +
        theme(legend.position = "right",
              legend.text = element_text(size = 8),
              legend.title = element_text(size = 9),
              plot.title = element_text(hjust = 0.5, size = 22)) +
        labs(title = glue("Bird Site NETN -{ input$park} {radi_dist} radius")) +
        scale_x_continuous(limits = c(pull(park_data$park_lim[1,2]), pull(park_data$park_lim[1,3]))) +
        scale_y_continuous(limits = c(pull(park_data$park_lim[1,4]), pull(park_data$park_lim[1,5])))
      
    print(p)
  })
  
  output$vegmap3 <- renderPlot({
    park_data <- park_list[[input$park]]
    current_bird_sits <- unique(park_data$xy$Point_Name)
    current_colors <- setNames(plot_palette[1:length(current_bird_sits)], current_bird_sits)

    plot_points <- park_data$for_plots

    bird_values_sat <- park_data$aar_coni_cov %>%
                  filter(park == input$park) %>% 
                  pull(BA_SUM)
    bird_values_netn <- park_data$xy$BA_m2ha_Conifer
    combined_range <- range(c(bird_values_sat, bird_values_netn), na.rm = TRUE)

    r <- 
      ggplot(data = park_data$map) +
        geom_sf(aes(fill = Cover_Type2)) +  # Remove text aesthetic
        scale_fill_manual(values = for_nofor_colors, na.value = "grey80") +
        
        # Add new scale for continuous fill
        ggnewscale::new_scale_fill() +
        
        geom_segment(data = park_data$neighbor,
                      aes(x = lonutmb, y = latutmb, 
                          xend = lonutmf, yend = latutmf, 
                          colour = bird_sit)) +
        scale_color_manual(values = current_colors, guide = "none") + 
        
        geom_sf(data = plot_points,
                size = 3, 
                color = "black") +        
        
        geom_sf(data = park_data$aar_coni_cov %>%
                  filter(park == input$park), 
                aes(fill = BA_SUM,  color = Point_Name),  # Only use fill, not color
                shape = 21, size = 7, stroke = 1) +  # Use shape 21 for fill
        guides(color = "none") +

        scale_fill_viridis_c(name = "Conifer BA", 
                            option = "plasma", 
                            na.value = "grey50",
                            limits = combined_range) +
        theme_bw() +
        theme(legend.position = "right",
              legend.text = element_text(size = 8),
              legend.title = element_text(size = 9),
              plot.title = element_text(hjust = 0.5, size = 22)) +
        labs(title = paste("Bird Site Satellite -", input$park)) +
        scale_x_continuous(limits = c(pull(park_data$park_lim[1,2]), pull(park_data$park_lim[1,3]))) +
        scale_y_continuous(limits = c(pull(park_data$park_lim[1,4]), pull(park_data$park_lim[1,5])))

    print(r)
  })

  output$complot <- renderPlotly({
     park_data <- park_list[[input$park]]
    
    # Join percent conifer BA to plot points for annotation
    comp_net_sat2 <- comp_net_sat1 %>%
      filter(park == input$park)
    # Create base plot without text aesthetic to avoid warnings

    max_sca <- ceiling(max(comp_net_sat2 %>% select(BA_m2ha_Conifer, BA_SUM), na.rm = T) / 10) * 10
    min_sca <- floor(min(comp_net_sat2 %>% select(BA_m2ha_Conifer, BA_SUM), na.rm = T) / 10) * 10

    q <- ggplot(data = comp_net_sat2) +
      geom_point(aes(x = BA_m2ha_Conifer, y = BA_SUM, 
                     text = paste0("Bird Point: ", Point_Name, "<br>",
                                  "NETN BA: ", round(BA_m2ha_Conifer, 2), " m²/ha<br>",
                                  "Satelite BA: ", round(BA_SUM, 2), " m²/ha")), 
                 size = 2) +
      xlim(min_sca, max_sca) +
      ylim(min_sca, max_sca) +
      geom_smooth(aes(x = BA_m2ha_Conifer, y = BA_SUM), method = "lm", se = FALSE) +
      geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed", size = 1) +
      labs(x = "NETN BA Estimates (m²/ha)", y = "Satelite BA Estimates (m²/ha)") +
      theme_bw() +
      theme(legend.position = "right",
            legend.text = element_text(size = 12),
            plot.title = element_text(hjust = 0.5, size = 16))
    
    # Convert to plotly with tooltip
    ggplotly(q, tooltip = "text")

  })

  output$vegmap2 <- renderPlot({
    park_data <- park_list[[input$park]]
    
    plot_points <- park_data$for_plots 
    
    current_for_sits <- unique(plot_points$for_sit)
    current_colors_for <- setNames(plot_palette[1:length(current_for_sits)], current_for_sits)
    current_bird_sits <- unique(park_data$xy$Point_Name)
    current_colors <- setNames(plot_palette[1:length(current_bird_sits)], current_bird_sits)

    for_values_sat <- aaron2 %>%
                  filter(park == input$park) %>% 
                  pull(Merged_Conifer_BA_2009)
    for_values_netn <- plot_points$BA_m2ha_Conifer
    combined_range <- range(c(for_values_sat, for_values_netn), na.rm = TRUE)

    p2 <- 
      ggplot(data = park_data$map) +
        geom_sf(aes(fill = Cover_Type2)) +  # Remove text aesthetic
        scale_fill_manual(values = for_nofor_colors, na.value = "grey80") +
        
        # Add new scale for continuous fill
        ggnewscale::new_scale_fill() +
        
        geom_segment(data = park_data$neighbor,
                      aes(x = lonutmb, y = latutmb, 
                          xend = lonutmf, yend = latutmf, 
                          colour = bird_sit)) +
        scale_color_manual(values = current_colors, guide = "none") + 
        
        geom_sf(data = park_data$xy,
                size = 3, 
                color = "black") +       
        
        geom_sf(data = plot_points %>%
                  filter(park == input$park), 
                aes(fill = BA_m2ha_Conifer),  # Only use fill
                shape = 21, size = 7, stroke = 1, color = "black") +  # Use shape 21
        
        scale_fill_viridis_c(name = "Conifer BA", 
                            option = "plasma", 
                            na.value = "grey50",
                            limits = combined_range) +
        theme_bw() +
        theme(legend.position = "right",
              legend.text = element_text(size = 8),
              legend.title = element_text(size = 9),
              plot.title = element_text(hjust = 0.5, size = 22)) +
        labs(title = paste("Forest Plot NETN -", input$park))  +
        scale_x_continuous(limits = c(pull(park_data$park_lim[1,2]), pull(park_data$park_lim[1,3]))) +
        scale_y_continuous(limits = c(pull(park_data$park_lim[1,4]), pull(park_data$park_lim[1,5])))
      
    print(p2)
  })
  
  #
  output$vegmap32 <- renderPlot({
    park_data <- park_list[[input$park]]

    plot_points <- park_data$for_plots 

    current_bird_sits <- unique(park_data$xy$Point_Name)
    current_colors <- setNames(plot_palette[1:length(current_bird_sits)], current_bird_sits)

    for_values_sat <- aaron2 %>%
                  filter(Unit_ID == "MABI") %>%  # Using fixed filter like your code
                  pull(Merged_Conifer_BA_2009)
    for_values_netn <- plot_points$BA_m2ha_Conifer
    combined_range <- range(c(for_values_sat, for_values_netn), na.rm = TRUE)
    
    r2 <- 
      ggplot(data = park_data$map) +
        geom_sf(aes(fill = Cover_Type2)) +  # Remove text aesthetic
        scale_fill_manual(values = for_nofor_colors, na.value = "grey80") +
        
        # Add new scale for continuous fill
        ggnewscale::new_scale_fill() +
        
        geom_segment(data = park_data$neighbor,
                      aes(x = lonutmb, y = latutmb, 
                          xend = lonutmf, yend = latutmf, 
                          colour = bird_sit)) +
        scale_color_manual(values = current_colors, guide = "none") + 
        
        geom_sf(data = park_data$xy,
                size = 3, 
                color = "black") +              
        
        geom_sf(data = aaron2 %>%
                  filter(Unit_ID == "MABI"), 
                aes(fill = Merged_Conifer_BA_2009),  # Only use fill
                shape = 21, size = 7, stroke = 1, color = "black") +  # Use shape 21
        
        scale_fill_viridis_c(name = "Conifer BA", 
                            option = "plasma", 
                            na.value = "grey50",
                            limits = combined_range) +
        theme_bw() +
        theme(legend.position = "right",
              legend.text = element_text(size = 8),
              legend.title = element_text(size = 9),
              plot.title = element_text(hjust = 0.5, size = 22)) +
        labs(title = paste("Forest Plot Satellite -", input$park)) +
        scale_x_continuous(limits = c(pull(park_data$park_lim[1,2]), pull(park_data$park_lim[1,3]))) +
        scale_y_continuous(limits = c(pull(park_data$park_lim[1,4]), pull(park_data$park_lim[1,5])))
      
    print(r2)
  })

  output$complot2 <- renderPlotly({
     park_data <- park_list[[input$park]]
    
    # Join percent conifer BA to plot points for annotation
    comp_net_sat2 <- comp_for_plot %>%
      filter(park == input$park)
    # Create base plot without text aesthetic to avoid warnings

    max_sca <- ceiling(max(comp_net_sat2 %>% as_tibble() %>% select(BA_m2ha_Conifer, Merged_Conifer_BA_2009), na.rm = T) / 10) * 10
    min_sca <- floor(min(comp_net_sat2 %>% as_tibble() %>% select(BA_m2ha_Conifer, Merged_Conifer_BA_2009), na.rm = T) / 10) * 10

    q2 <- ggplot(data = comp_net_sat2) +
      geom_point(aes(x = BA_m2ha_Conifer, y = Merged_Conifer_BA_2009, 
                     text = paste0("Bird Point: ", for_sit, "<br>",
                                  "NETN BA: ", round(BA_m2ha_Conifer, 2), " m²/ha<br>",
                                  "Satelite BA: ", round(Merged_Conifer_BA_2009, 2), " m²/ha")), 
                 size = 2) +
      xlim(min_sca, max_sca) +
      ylim(min_sca, max_sca) +
      geom_smooth(aes(x = BA_m2ha_Conifer, y = Merged_Conifer_BA_2009), method = "lm", se = FALSE) +
      geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed", size = 1) +
      labs(x = "NETN BA Estimates (m²/ha)", y = "SateliteBA Estimates (m²/ha)", 
           title = paste("Forest Plot BA Estimates -", input$park)) +
      theme_bw() +
      theme(legend.position = "right",
            legend.text = element_text(size = 12),
            plot.title = element_text(hjust = 0.5, size = 16))
    
    # Convert to plotly with tooltip
    ggplotly(q2, tooltip = "text")

  })
}

shinyApp(ui, server)
## ( x ) send bird sites values to aaron
## (   ) check park errors
## (   ) make a single scale for bottom plots
##! (   ) add park limits/boundaries for plots
## (   ) sensitivity analysis - how many neighbours I get and how estimates changes as the radius gets bigger
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


