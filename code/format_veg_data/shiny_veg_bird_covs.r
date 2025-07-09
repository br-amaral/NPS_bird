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

#! Source code -----------------------------------------
#? veggie maps -----------------------------------------
# get park shape files with vegetation types and classify each as conifer, hardwood, mixed, or not forest
source("/Users/bamaral/Documents/GitHub/NPS_bird_copy/code/format_veg_data/veg_maps_park.R")

#? forest covariates -----------------------------------
# gets all forest plots and calculates what percentage and value of density and BA is conifer and hardwood
source("/Users/bamaral/Documents/GitHub/NPS_bird_copy/code/format_veg_data/get_conhar_baden.R")

#! Import data -----------------------------------------
## file paths

## read files
#? bird site covariates
radi_dist <- 250
close_points_f2 <- read_rds(file = glue("data/out/site_covs_fornofor_{radi_dist}m.rds"))

# shiny for parks, forest type_plot, and new covariates
for_plots_sf <- for_plots_sf  %>% rename(PlotID = for_sit)
for_plots_sfh <- for_plots_sf  %>% filter(park == "ROVA"); for_plots_sfh$park <- "HOFR"   # hofr
for_plots_sfv <- for_plots_sf  %>% filter(park == "ROVA"); for_plots_sfv$park <- "VAMA"   # vama
for_plots_sfm <- for_plots_sfm  %>% rename(PlotID = for_sit)                              # mima

tre_cov3_key <- expand.grid(sort(unique(tre_cov3$PlotID)), c('Hardwood', 'Conifer'))  %>% 
                    as_tibble() %>% 
                    rename(PlotID = Var1,
                           type = Var2) %>% 
                    mutate(PlotID = as.character(PlotID),
                           type = as.character(type)) %>% 
                    arrange(PlotID)

tre_cov4 <- left_join(tre_cov3_key, tre_cov3, by = c('PlotID', 'type')) %>% 
                mutate(ParkUnit = substr(PlotID, 1, 4),
                       BA_m2ha = ifelse(is.na(BA_m2ha), 0, BA_m2ha),
                       density = ifelse(is.na(density), 0, density),
                       type_plot = ifelse(is.na(type_plot), 
                                          ifelse(type == 'Conifer', 'Hardwood', 'Conifer'), 
                                          type_plot))

tre_cov3percent_con <- tre_cov4  %>% 
  group_by(PlotID) %>% 
  mutate(
    tot_ba = sum(BA_m2ha),
    tot_den = sum(density)
  ) %>% 
  arrange(PlotID, type_plot)  %>% 
  mutate(
    per_ba = BA_m2ha / tot_ba,
    per_den = density / tot_den
  ) %>% 
  filter(#type_plot == "Conifer",
         type == "Conifer") %>% 
  ungroup()

# datatable(tre_cov3percent_con)

## get neighbours to calculate % of conifer and hardwood for each bird point
neighbor <- read_rds(file = "data/out/close_points_dist.rds") %>% 
                      select(dist, for_sit, bird_sit)

neighbor2 <- left_join(neighbor %>% 
                          rename(PlotID = for_sit), 
                       tre_cov3percent_con %>% 
                          select(PlotID, ParkUnit, BA_m2ha, density, tot_ba, tot_den, per_ba, per_den),
                       by = "PlotID")  %>% 
                  arrange(bird_sit) %>%
                  select(-ParkUnit) %>% 
                  mutate(BA_m2ha = ifelse(BA_m2ha == 0, BA_m2ha + 0.001, BA_m2ha)) %>% 
                  group_by(bird_sit) %>% 
                  mutate(BA_m2ha_m   = mean(BA_m2ha, na.rm = T),
                         density_m   = mean(density, na.rm = T), 
                         tot_ba_m    = mean(tot_ba,  na.rm = T), 
                         tot_den_m   = mean(tot_den, na.rm = T), 
                         per_ba_m    = mean(per_ba,  na.rm = T), 
                         per_den_m   = mean(per_den, na.rm = T),
                         BA_m2ha_wei = (sum(BA_m2ha * 1/dist))/sum(1/dist),
                         density_wei = (sum(density * 1/dist))/sum(1/dist), 
                         tot_ba_wei  = (sum(tot_ba *  1/dist))/sum(1/dist), 
                         tot_den_wei = (sum(tot_den * 1/dist))/sum(1/dist),
                         per_ba_wei  = (sum(per_ba *  1/dist))/sum(1/dist),
                         per_den_wei = (sum(per_den * 1/dist))/sum(1/dist)) %>% 
                  ungroup() %>% 
                  select(-PlotID, -dist, -BA_m2ha, -density, -tot_ba, -tot_den, -per_ba, -per_den) %>%
                  distinct() %>% 
                  mutate(type = "Conifer")

xy_sf2 <- xy_sf %>% 
            filter(park %!in% c("ACAD", "ELRO", "SAIR")) %>% 
            left_join(., 
                      neighbor2 %>% 
                        rename(Point_Name = bird_sit), 
                      by = c('Point_Name')) %>% 
            mutate(BApercent_type = ifelse(per_ba_m > 0.5, 'Conifer', 'Hardwood')) %>%
            distinct()

#? aaron data
aar_con <- read_rds(file = "data/conifer_final_aaron.rds")  %>% 
              mutate(Point_Name = paste0(substr(PT_CODE, 1, 4), substr(PT_CODE, 6, 7), substr(PT_CODE, 9, 10))) %>% 
              select(Point_Name, BA_SUM)  

aar_con <- left_join(xy_sf, aar_con, by = "Point_Name") %>% 
              mutate(BA_sum_ha = BA_SUM/19.63) %>%  ## The area of a circle with a 250-meter radius is approximately 19.63 hectares.
              rename(ParkUnit = park)

par(mfrow = c(1,3))
hist(as_tibble(aar_con) %>% pull(BA_sum_ha)) 
hist(as_tibble(xy_sf2) %>% pull(tot_ba_m)) 
hist(as_tibble(xy_sf2) %>% pull(tot_ba_wei))

t.test(as_tibble(aar_con) %>% pull(BA_sum_ha), 
           as_tibble(xy_sf2) %>% pull(tot_ba_wei))

par(mfrow = c(1,1))
plot(as_tibble(xy_sf2) %>% pull(tot_ba_m), as_tibble(xy_sf2) %>% pull(tot_ba_wei))

left_join(as_tibble(aar_con) %>% select(Point_Name, BA_sum_ha),
          as_tibble(xy_sf2) %>% select(Point_Name, tot_ba_m),
          by = "Point_Name") %>% 
      arrange(Point_Name) %>% 
      as_tibble() %>% 
      mutate(difference = tot_ba_m - BA_sum_ha)  %>% 
      pull(difference) %>% 
      hist()

park_list <- list(
  "MABI" = list(map = mabi_vegmap2, for_plots = for_plots_sf,  xy = xy_sf2, 
                close_points = close_points_f2 %>% filter(ParkUnit == "MABI"),
                aar_coni_cov = aar_con %>% filter(ParkUnit == "MABI")),
  "MORR" = list(map = morr_vegmap2, for_plots = for_plots_sf,  xy = xy_sf2, 
                close_points = close_points_f2 %>% filter(ParkUnit == "MORR"),
                aar_coni_cov = aar_con %>% filter(ParkUnit == "MORR")),
  "SAGA" = list(map = saga_vegmap2, for_plots = for_plots_sf,  xy = xy_sf2, 
                close_points = close_points_f2 %>% filter(ParkUnit == "SAGA"),
                aar_coni_cov = aar_con %>% filter(ParkUnit == "SAGA")),
  "SARA" = list(map = sara_vegmap2, for_plots = for_plots_sf,  xy = xy_sf2, 
                close_points = close_points_f2 %>% filter(ParkUnit == "SARA"),
                aar_coni_cov = aar_con %>% filter(ParkUnit == "SARA")),
  "WEFA" = list(map = wefa_vegmap2, for_plots = for_plots_sf,  xy = xy_sf2, 
                close_points = close_points_f2 %>% filter(ParkUnit == "WEFA"),
                aar_coni_cov = aar_con %>% filter(ParkUnit == "WEFA")),
  "HOFR" = list(map = rova_vegmap2, for_plots = for_plots_sfh, xy = xy_sf2, 
                close_points = close_points_f2 %>% filter(ParkUnit == "HOFR"),
                aar_coni_cov = aar_con %>% filter(ParkUnit == "HOFR")),
  "VAMA" = list(map = rova_vegmap2, for_plots = for_plots_sfv, xy = xy_sf2, 
                close_points = close_points_f2 %>% filter(ParkUnit == "VAMA"),
                aar_coni_cov = aar_con %>% filter(ParkUnit == "VAMA")),
  "MIMA" = list(map = mima_vegmap2, for_plots = for_plots_sfm, xy = xy_sf2, 
                close_points = close_points_f2 %>% filter(ParkUnit == "MIMA"),
                aar_coni_cov = aar_con %>% filter(ParkUnit == "MIMA"))
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
  titlePanel("NPS Park Vegetation Maps"),
  sidebarLayout(
    sidebarPanel(
      selectInput("park", "Choose a Park:", choices = names(park_list), selected = "MABI")
    ),
    mainPanel(
      plotlyOutput("vegmap", height = "500px"),
      plotOutput("vegmap2", height = "500px"),
      plotOutput("vegmap3", height = "500px"),
      plotlyOutput("complot", height = "500px"),
      ),
    )
  )


## test code
# park_data <- park_list[["MABI"]]
# plot_points <- park_data$for_plots %>%
#   left_join(tre_cov4, by = "PlotID") %>%
#   left_join(tre_cov3percent_con %>% 
#   filter(ParkUnit == "MABI"), by = "PlotID", suffix = c("", "_con")) %>% 
#   filter(ParkUnit == "MABI")

server <- function(input, output, session) {
  output$vegmap <- renderPlotly({
    park_data <- park_list[[input$park]]
    
    # Join percent conifer BA to plot points for annotation
    plot_points <- park_data$for_plots %>%
      left_join(tre_cov4, by = "PlotID") %>%
      left_join(tre_cov3percent_con %>% 
      filter(ParkUnit == input$park), by = "PlotID", suffix = c("", "_con")) %>% 
      filter(ParkUnit == input$park)
    
    # Create park-specific color palette for current park's bird_sit values
    current_bird_sits <- unique(park_data$close_points$bird_sit)
    current_colors <- setNames(plot_palette[1:length(current_bird_sits)], current_bird_sits)
    
    p <- 
      ggplot(data = park_data$map) +
        geom_sf(aes(fill = Cover_Type2, text = paste("MapUnit:", MapUnit_Name, "<br>Cover Type:", Cover_Type2))) +
        scale_fill_manual(values = for_nofor_colors, na.value = "grey80") +
        geom_segment(data = park_data$close_points,
                      aes(x = lonutmb, y = latutmb, 
                          xend = lonutmf, yend = latutmf, 
                          colour = bird_sit)) +
        scale_color_manual(values = current_colors) + 
        geom_sf(
          data = plot_points,
          size = 3, 
          color = "black",
          aes(text = paste0(
                "Plot: ", PlotID, "<br>",
                "Type: ", type_plot, "<br>",
                "Percent Conifer Den: ", round(per_den, 2), "<br>",
                "Percent Conifer BA: ", round(per_ba, 2)
            ))) +       
        geom_sf(data = park_data$xy %>%
                  filter(park == input$park), 
                  #filter(park == "MABI"), 
                aes(color = Point_Name,
                text = paste0(
                "Bird Site: ", Point_Name, "<br>",
                "Type: ", BApercent_type, "<br>",
                "Percent Conifer Den: ", round(per_den_wei, 2), "<br>",
                "Percent Conifer BA: ", round(per_ba_wei, 2)
            )),
                shape = 18, size = 4) +
        scale_color_manual(values = current_colors, na.value = "#615e5e") + 
        theme_bw() +
        theme(legend.position = "none",
              legend.text = element_text(size = 8),
              legend.title = element_text(size = 9),
              plot.title = element_text(hjust = 0.5, size = 22)) +
        ggtitle(input$park)
      
    ggplotly(p, tooltip = "text")
  })

  output$vegmap2 <- renderPlot({
    park_data <- park_list[[input$park]]
    
    # Join percent conifer BA to plot points for annotation
    plot_points <- park_data$for_plots %>%
      left_join(tre_cov4, by = "PlotID") %>%
      left_join(tre_cov3percent_con %>% 
      filter(ParkUnit == input$park), by = "PlotID", suffix = c("", "_con")) %>% 
      filter(ParkUnit == input$park)
    
    ggplot(data = park_data$map) +
      geom_sf(aes(fill = Cover_Type2)) +
      scale_fill_manual(values = for_nofor_colors, na.value = "grey80") +
      geom_segment(data = park_data$close_points,
                    aes(x = lonutmb, y = latutmb, 
                        xend = lonutmf, yend = latutmf), 
                        colour = "black", linewidth = 1) +
      ggnewscale::new_scale_fill() +
      geom_sf(
        data = plot_points,
        shape = 22,
        size = 10, 
        stroke = 1,
        color = "red",
        aes(fill = BA_m2ha)) +
      geom_sf(data = park_data$xy %>%
                filter(park == input$park), 
          aes(fill = BA_m2ha_wei),
          shape = 21, 
          size = 10,
          stroke = 1,
          color = "black") +
      scale_fill_viridis_c(name = "BA (m²/ha)", option = "plasma") +
      theme_bw() +
      theme(legend.position = "right",
            legend.text = element_text(size = 10),
            legend.title = element_text(size = 12),
            plot.title = element_text(hjust = 0.5, size = 16)) +
      ggtitle(paste("Basal Area Map -", input$park))
  })

  output$vegmap3 <- renderPlot({
    park_data <- park_list[[input$park]]
        
    ggplot(data = park_data$map) +
      geom_sf(aes(fill = Cover_Type2)) +
      scale_fill_manual(values = for_nofor_colors, na.value = "grey80") +
      ggnewscale::new_scale_fill() +
      geom_sf(
        data =  park_data$aar_coni_cov,
        shape = 21,
        size = 10, 
        stroke = 1,
        color = "black",
        aes(fill = BA_sum_ha)) +
      scale_fill_viridis_c(name = "BA (m²/ha)", option = "plasma") +
      theme_bw() +
      theme(legend.position = "right",
            legend.text = element_text(size = 10),
            legend.title = element_text(size = 12),
            plot.title = element_text(hjust = 0.5, size = 16)) +
      ggtitle(paste("Basal Area Map GIS-", input$park))
  })

  output$complot <- renderPlotly({
    park_data <- park_list[[input$park]]
        
    comp <- left_join(park_data$aar_coni_cov %>% select(Point_Name, BA_sum_ha),
                      as_tibble(park_data$xy) %>% filter(park == input$park) %>% select(Point_Name, tot_ba_m),
                      by = "Point_Name") %>% 
                arrange(Point_Name)  %>% 
                as_tibble()

    max_sca <- ceiling(max(comp %>% select(BA_sum_ha, tot_ba_m), na.rm = T) / 10) * 10
    min_sca <- floor(min(comp %>% select(BA_sum_ha, tot_ba_m), na.rm = T) / 10) * 10

    # Create base plot without text aesthetic to avoid warnings
    q <- ggplot(data = comp) +
      geom_point(aes(x = tot_ba_m, y = BA_sum_ha, 
                     text = paste0("Bird Point: ", Point_Name, "<br>",
                                  "Your BA: ", round(tot_ba_m, 2), " m²/ha<br>",
                                  "Aaron's BA: ", round(BA_sum_ha, 2), " m²/ha")), 
                 size = 2) +
      geom_smooth(aes(x = tot_ba_m, y = BA_sum_ha), method = "lm", se = FALSE) +
      geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed", size = 1) +
      xlim(min_sca, max_sca) +
      ylim(min_sca, max_sca) +
      labs(x = "For Plot BA Estimates (m²/ha)", y = "Aaron's BA Estimates (m²/ha)", 
           title = paste("Comparison of BA Estimates -", input$park)) +
      theme_bw() +
      theme(legend.position = "right",
            legend.text = element_text(size = 12),
            plot.title = element_text(hjust = 0.5, size = 16))
    
    # Convert to plotly with tooltip
    ggplotly(q, tooltip = "text")

  })
}

shinyApp(ui, server)
## send bird sites values to aaron

## ( x ) classify everything as forest and not forest
## ( x ) connect bird and forest sites
## ( x ) remove the red circles and triangles - same symbol
## ( x ) get percentage hardwood and conifer for bird sites
## ( x ) weight that percentage according to distance to point
## ( x ) create same map with surface heat map
## (   ) sensitivity analysis - how many neighbours I get and how estimates changes as the radius gets bigger
## ( x ) compare with aarons estimates


## (   ) get some measure of stand structure (how much vegetation in each strata)
## (   ) get same covariates as birds (close_points_etc) for the forest sites
## (   ) gradient of color for the covariates for each bird site and plot (e.g. ba 0.2 is blue and 0.8 is green)
## (   ) HOFR and VAMA error 


