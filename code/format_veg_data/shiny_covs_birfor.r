#? *********************************************************************************
#? ------------------------------   shiny_veg_types   ------------------------------
#? *********************************************************************************
#
#! Code to vizualize all covariates in the model in the bird and forest in the same map, and how they vary
#!  within the park, and between the parks
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
library(ggnewscale)

conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::filter)
# conflicts_prefer(scales::alpha)

#! Source code -----------------------------------------
# get park shape files with vegetation types and classify each as conifer, hardwood, mixed, or not forest
source("/Users/bamaral/Documents/GitHub/NPS_bird_copy/code/format_veg_data/veg_maps_park.R")
# keep only relevant files
keep_objects <- c("for_plots_sf", "for_plots_sfm", "xy_sf", 
                  "mabi_vegmap2", "morr_vegmap2", "saga_vegmap2", "sara_vegmap2",
                  "wefa_vegmap2", "rova_vegmap2", "mima_vegmap2", "keep_objects")

rm(list = setdiff(ls(), keep_objects))

radi_dist <- 250
source('/Users/bamaral/Documents/GitHub/NPS_bird_copy/code/format_veg_data/get_site_data_rad.R')

keep_objects2 <- c(keep_objects, radi_dist)

#! Make functions --------------------------------------
colanmes <- colnames
lenght <- length
`%!in%` <- Negate(`%in%`)

#! Import data -----------------------------------------

## file paths
COV_FOR_PLY <- "data/out/for_plot_covs.rds"
COV_BRD_SIT <- glue("data/out/site_covs_fornofor_{radi_dist}m.rds")
#AAR_BIR_COV <- 
AAR_FOR_COV <- "data/conifer_final_aaron.rds"
NEI_PATH <- glue("data/out/neighbor_fornofor_{radi_dist}m.rds")
PARK_BOUN_PATH <- "data/out/park_plot_lims.rds"          ## geographic limits to plot parks

## read files
# get neighbors
neighbor <- read_rds(NEI_PATH) %>% 
                mutate(park = substr(bird_sit,1,4))

# park boundaries
park_bounds <- read_rds(PARK_BOUN_PATH)

# get info on site and plot level for bird sites and forest plots
bird_sit_covs  <- read_rds(file = COV_BRD_SIT) %>%
  # Remove "_wei" suffix from all column names
  rename_with(~str_remove(.x, "_wei$"))

aa_covs_for <- read_rds(AAR_FOR_COV) 

for_plots_covs <- read_rds(file = COV_FOR_PLY) %>% 
                      rename(for_sit = Plot_Name) %>% 
                      select(-UTMZone) %>% 
                      mutate(type = as.character(NA))

for(ii in 1:nrow(for_plots_covs)){

    if(for_plots_covs$BA_m2ha_Conifer[ii] > for_plots_covs$BA_m2ha_Hardwood[ii]) {
        for_plots_covs$type[ii] <- "Conifer"
    } else { for_plots_covs$type[ii] <- "Hardwood" }
}

# shiny for parks, forest type_plot, and new covariates
for_plots_sfh <- for_plots_sf  %>% filter(park == "ROVA"); for_plots_sfh$park <- "HOFR"   # hofr
for_plots_sfv <- for_plots_sf  %>% filter(park == "ROVA"); for_plots_sfv$park <- "VAMA"   # vama
for_plots_sfm <- for_plots_sfm                                                            # mima

bird_sit_covs2 <- bird_sit_covs %>% 
                      mutate(park = substr(bird_sit, 1, 4)) %>% 
                      filter(park %!in% c("ACAD", "ELRO", "SAIR")) %>%
                      rename(Point_Name = bird_sit) 

xy_sf <- left_join(xy_sf, bird_sit_covs2, by = c("Point_Name", "park")) %>% 
                      filter(park %!in% c("ACAD", "ELRO", "SAIR"))
park_list <- list(
  "MABI" = list(map = mabi_vegmap2, for_plots = for_plots_sf,  xy = xy_sf, neighbor = neighbor, park_lim = park_bounds %>% filter(park == "MABI")),
  "MORR" = list(map = morr_vegmap2, for_plots = for_plots_sf,  xy = xy_sf, neighbor = neighbor, park_lim = park_bounds %>% filter(park == "MORR")),
  "SAGA" = list(map = saga_vegmap2, for_plots = for_plots_sf,  xy = xy_sf, neighbor = neighbor, park_lim = park_bounds %>% filter(park == "SAGA")),
  "SARA" = list(map = sara_vegmap2, for_plots = for_plots_sf,  xy = xy_sf, neighbor = neighbor, park_lim = park_bounds %>% filter(park == "SARA")),
  "WEFA" = list(map = wefa_vegmap2, for_plots = for_plots_sf,  xy = xy_sf, neighbor = neighbor, park_lim = park_bounds %>% filter(park == "WEFA")),
  "HOFR" = list(map = rova_vegmap2, for_plots = for_plots_sfh, xy = xy_sf, neighbor = neighbor, park_lim = park_bounds %>% filter(park == "HOFR")),
  "VAMA" = list(map = rova_vegmap2, for_plots = for_plots_sfv, xy = xy_sf, neighbor = neighbor, park_lim = park_bounds %>% filter(park == "VAMA")),
  "MIMA" = list(map = mima_vegmap2, for_plots = for_plots_sfm, xy = xy_sf, neighbor = neighbor, park_lim = park_bounds %>% filter(park == "MIMA"))
)

all_cover_types <- unique(unlist(lapply(park_list, function(x) unique(x$map$Cover_Type))))
palette <- c("#605d5d", "#1e7b1e", "#3a78dc", "#c98b19", "#dcdada")
cover_type_colors <- setNames(palette[seq_along(all_cover_types)], all_cover_types)

ui <- fluidPage(
  titlePanel("NPS Park Vegetation Maps"),
  sidebarLayout(
    sidebarPanel(
      selectInput("park", "Choose a Park:", choices = names(park_list), selected = "MABI"),
      selectInput("variable", "Choose Variable:", 
                  choices = c("BA_m2ha", 
                              "BA_m2ha_Conifer", "BA_m2ha_Hardwood","BA_m2ha_perc_con",
                              "BA_m2ha_pole", "BA_m2ha_mature", "BA_m2ha_large",
                              "treeden_ha", 
                              "treeden_ha_Conifer", "treeden_ha_Hardwood",
                              "treeden_ha_pole", "treeden_ha_mature", "treeden_ha_large", 
                              "shrub_cov_nat", "shrub_cov_nonat", 
                              "seed_den_m2", "sap_den_m2", "regen_den_m2", "cwd"), 
                  selected = "BA_m2ha")
    ),
    mainPanel(
      plotOutput("vegmap", height = "500px"),
      fluidRow(
        column(
          width = 6,
          h4(textOutput("plot_title")),
          plotlyOutput("variable_plot", height = "400px")
        ),
        column(
          width = 6,
          h4(textOutput("bird_plot_title")),
          plotlyOutput("bird_variable_plot", height = "400px")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Create a reactive expression for combined_range so it's accessible everywhere
  combined_range <- reactive({
    park_data <- park_list[[input$park]]
    
    # Prepare forest plot data
    forest_points <- park_data$for_plots %>%
      left_join(for_plots_covs, by = "for_sit") %>%
      filter(park == input$park)
    
    # Prepare bird site data  
    bird_points <- park_data$xy %>%
      filter(park == input$park)
    
    # Get the combined range for shared scale
    forest_values <- if(input$variable %in% colnames(forest_points)) forest_points[[input$variable]] else numeric(0)
    bird_values <- if(input$variable %in% colnames(bird_points)) bird_points[[input$variable]] else numeric(0)
    
    range(c(forest_values, bird_values), na.rm = TRUE)
  })
  
  output$vegmap <- renderPlot({
    park_data <- park_list[[input$park]]
    
    # Prepare forest plot data
    forest_points <- park_data$for_plots %>%
      left_join(for_plots_covs, by = "for_sit") %>%
      filter(park == input$park)
    
    # Prepare bird site data  
    bird_points <- park_data$xy %>%
      filter(park == input$park)
    
    # Use the reactive combined_range
    range_values <- combined_range()
    
    p <- ggplot(data = park_data$map) +
      geom_sf(aes(fill = Cover_Type), alpha = 0.4) +
      scale_fill_manual(values = cover_type_colors, na.value = "grey80", name = "Cover Type") +
      ggnewscale::new_scale_fill() +
      # Add forest plots with variable coloring
      geom_segment(data = park_data$neighbor %>% filter(park == input$park), 
                   aes(x = lonutmb, y = latutmb, 
                       xend = lonutmf, yend = latutmf, 
                       colour = bird_sit)) +
      geom_sf(
        data = forest_points,
        color = "black", 
        size = 6, 
        shape = 23,
        stroke = 1,
        aes(fill = !!sym(input$variable))
      ) +
      # Add bird sites with variable coloring
      geom_sf(
        data = bird_points,
        size = 7, 
        shape = 21,
        stroke = 1,
        aes(fill = !!sym(input$variable), color = Point_Name)
      ) +
      # Single shared scale for both forest plots and bird sites with combined range
      scale_fill_viridis_c(
        name = paste(input$variable), 
        option = "plasma", 
        na.value = "grey50",
        limits = range_values
      ) +
      guides(color = "none") +  # Remove color legend but keep fill legend
      theme_bw() +
      theme(legend.position = "bottom",
            legend.text = element_text(size = 8),
            legend.title = element_text(size = 9),
            plot.title = element_text(hjust = 0.5, size = 22),
            axis.text = element_text(size = 6),
            axis.title = element_text(size = 8)) +
      ggtitle(paste(input$park, "- Forest Plots (diamonds) & Bird Sites (circles)")) +
      scale_x_continuous(limits = c(pull(park_data$park_lim[1,2])-150, pull(park_data$park_lim[1,3])+150)) +
      scale_y_continuous(limits = c(pull(park_data$park_lim[1,4])-150, pull(park_data$park_lim[1,5])+150))
    
    print(p)
  })

  output$plot_title <- renderText({
    paste(input$variable, "by Forest Plot")
  })

  output$variable_plot <- renderPlotly({
    # Handle all variables as single variables (no conifer/hardwood splitting)
    park_data <- park_list[[input$park]]

    df <- for_plots_covs %>% 
      select(-ParkUnit) %>% 
      left_join(., park_data$for_plots, by = "for_sit") %>%
      filter(park == input$park) %>% 
      filter(X >= (pull(park_data$park_lim[1, "xmin"])-200),
             X <= (pull(park_data$park_lim[1, "xmax"])+200),
             Y >= (pull(park_data$park_lim[1, "ymin"])-200),
             Y <= (pull(park_data$park_lim[1, "ymax"])+200)) %>% 
      arrange(for_sit)  

    # Use the reactive combined_range
    range_values <- combined_range()

    p <- ggplot(df, aes(x = for_sit, y = !!sym(input$variable))) +
      geom_point(aes(text = paste0(
                    "Plot: ", for_sit, "<br>",
                    input$variable, ": ", round(!!sym(input$variable), 2)
                  )), size = 2, color = "#1e7b1e") +
      labs(x = "Plot", y = input$variable, title = "Forest Plot Data") +
      theme_bw() +
      ylim(range_values) +  # Use reactive range
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
            legend.text = element_text(size = 8),
            legend.title = element_text(size = 9),
            plot.title = element_text(hjust = 0.5))
    
    ggplotly(p, tooltip = "text")
  })

  output$bird_plot_title <- renderText({
    paste(input$variable, "by Bird Site")
  })

  output$bird_variable_plot <- renderPlotly({
    # Get bird site data
    df <- bird_sit_covs2 %>% 
      filter(park == input$park) %>% 
      arrange(Point_Name)

    # Check if the variable exists in the data
    if(input$variable %in% colnames(df)) {
      # Use the reactive combined_range
      range_values <- combined_range()
      
      p <- ggplot(df, aes(x = Point_Name, y = !!sym(input$variable))) +
        geom_point(aes(text = paste0(
                      "Site: ", Point_Name, "<br>",
                      input$variable, ": ", round(!!sym(input$variable), 2)
                    )), size = 2, color = "#3a78dc") +
        labs(x = "Bird Site", y = input$variable, title = "Bird Site Data") +
        theme_bw() +
        ylim(range_values) +  # Use reactive range
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
              legend.text = element_text(size = 8),
              legend.title = element_text(size = 9),
              plot.title = element_text(hjust = 0.5))
      
      ggplotly(p, tooltip = "text")
    } else {
      # If variable doesn't exist, show empty plot with message
      ggplot() + 
        annotate("text", x = 0.5, y = 0.5, label = paste("Variable", input$variable, "not found"), size = 5) +
        theme_void()
    }
  })  
}

shinyApp(ui, server)

## add variation between parks and counties - plots with all parks highlighting the current, for forest and bird.
## (   ) make a single scale for bottom plots
