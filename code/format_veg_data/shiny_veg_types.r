#? *********************************************************************************
#? ------------------------------   shiny_veg_types   ------------------------------
#? *********************************************************************************
#
#! Code to plot park vegetation types, bird sites, and forest plots, and classify them
#!    according to conifer, mixed, hardwood and not forest, as well as average BA and
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
                  "wefa_vegmap2", "rova_vegmap2", "mima_vegmap2")

rm(list = setdiff(ls(), keep_objects))

#! Make functions --------------------------------------
colanmes <- colnames
lenght <- length
`%!in%` <- Negate(`%in%`)

#! Import data -----------------------------------------
radi_dist <- 250

## file paths
COV_FOR_PLY <- "data/out/for_plot_covs.rds"
COV_BRD_SIT <- glue("data/out/site_covs_fornofor_{radi_dist}m.rds")
#AAR_BIR_COV <- 
AAR_FOR_COV <- "data/conifer_final_aaron.rds"

## read files
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

xy_sf <- left_join(xy_sf, bird_sit_covs2, by = c("Point_Name", "park"))
park_list <- list(
  "MABI" = list(map = mabi_vegmap2, for_plots = for_plots_sf, xy = xy_sf),
  "MORR" = list(map = morr_vegmap2, for_plots = for_plots_sf, xy = xy_sf),
  "SAGA" = list(map = saga_vegmap2, for_plots = for_plots_sf, xy = xy_sf),
  "SARA" = list(map = sara_vegmap2, for_plots = for_plots_sf, xy = xy_sf),
  "WEFA" = list(map = wefa_vegmap2, for_plots = for_plots_sf, xy = xy_sf),
  "HOFR" = list(map = rova_vegmap2, for_plots = for_plots_sfh, xy = xy_sf),
  "VAMA" = list(map = rova_vegmap2, for_plots = for_plots_sfv, xy = xy_sf),
  "MIMA" = list(map = mima_vegmap2, for_plots = for_plots_sfm, xy = xy_sf)
)

write_rds(park_list, file = "data/out/park_list.rds")

all_cover_types <- unique(unlist(lapply(park_list, function(x) unique(x$map$Cover_Type))))
palette <- c("#605d5d", "#1e7b1e", "#3a78dc", "#c98b19", "#dcdada")
cover_type_colors <- setNames(palette[seq_along(all_cover_types)], all_cover_types)

ui <- fluidPage(
  titlePanel("NPS Park Vegetation Maps"),
  sidebarLayout(
    sidebarPanel(
      selectInput("park", "Choose a Park:", choices = names(park_list), selected = "MABI"),
      selectInput("variable", "Choose Variable:", 
                  choices = c("treeden_ha", "BA_m2ha", "shrub_cov_nat", "shrub_cov_nonat", 
                             "treeden_ha_Conifer", "treeden_ha_Hardwood", "BA_m2ha_Conifer", 
                             "BA_m2ha_Hardwood", "seed_den_m2", "sap_den_m2", "regen_den_m2", 
                             "Stage", "pctBA_pole", "pctBA_mature", "pctBA_large", 
                             "treeden_ha_large", "treeden_ha_mature", "treeden_ha_pole", 
                             "BA_m2ha_large", "BA_m2ha_mature", "BA_m2ha_pole", "cwd"), 
                  selected = "BA_m2ha")
    ),
    mainPanel(
      plotlyOutput("vegmap", height = "500px"),
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
  
  output$vegmap <- renderPlot({
    park_data <- park_list[[input$park]]
    
    # Prepare forest plot data
    forest_points <- park_data$for_plots %>%
      left_join(.,for_plots_covs, by = "for_sit") %>%
      filter(park == input$park)
    
    # Prepare bird site data  
    bird_points <- park_data$xy %>%
      filter(park == input$park)
    
    # Get the combined range for shared scale
    forest_values <- forest_points[[input$variable]]
    bird_values <- if(input$variable %in% colnames(bird_points)) bird_points[[input$variable]] else numeric(0)
    combined_range <- range(c(forest_values, bird_values), na.rm = TRUE)
    
    p <- ggplot(data = park_data$map) +
      geom_sf(aes(fill = Cover_Type)) +
      scale_fill_manual(values = cover_type_colors, na.value = "grey80", name = "Cover Type") +
      ggnewscale::new_scale_fill() +
      # Add forest plots with variable coloring
      geom_sf(
        data = forest_points,
        color = "black", 
        size = 3, 
        shape = 21,
        stroke = 0.5,
        aes(fill = !!sym(input$variable),
        text = paste0(
                    "Plot: ", for_sit, "<br>",
                    input$variable, ": ", round(!!sym(input$variable), 2)
                  ))
      ) +
      # Add bird sites with variable coloring
      geom_sf(
        data = bird_points,
        color = "black", 
        size = 2.5, 
        shape = 23,
        stroke = 0.5,
        aes(fill = !!sym(input$variable),
        text = paste0(
                    "Plot: ", Point_Name, "<br>",
                    input$variable, ": ", round(!!sym(input$variable), 2)
                  ))
      ) +
      # Single shared scale for both forest plots and bird sites with combined range
      scale_fill_viridis_c(
        name = paste(input$variable), 
        option = "plasma", 
        na.value = "grey50",
        limits = combined_range
      ) +
      theme_bw() +
      theme(legend.position = "bottom",
            legend.text = element_text(size = 8),
            legend.title = element_text(size = 9),
            plot.title = element_text(hjust = 0.5, size = 22),
            axis.text = element_text(size = 6),
            axis.title = element_text(size = 8)) +
      ggtitle(paste(input$park, "- Forest Plots (circles) & Bird Sites (diamonds)"))
    
    #print(p)
        ggplotly(p, tooltip = "text")

  })

  output$plot_title <- renderText({
    paste(input$variable, "by Forest Plot")
  })

  output$variable_plot <- renderPlotly({
    # Handle all variables as single variables (no conifer/hardwood splitting)
    df <- for_plots_covs %>% 
      filter(ParkUnit == input$park) %>% 
      arrange(for_sit)

    p <- ggplot(df, aes(x = for_sit, y = !!sym(input$variable))) +
      geom_point(aes(text = paste0(
                    "Plot: ", for_sit, "<br>",
                    input$variable, ": ", round(!!sym(input$variable), 2)
                  )), size = 2, color = "#1e7b1e") +
      labs(x = "Plot", y = input$variable, title = "Forest Plot Data") +
      theme_bw() +
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
      p <- ggplot(df, aes(x = Point_Name, y = !!sym(input$variable))) +
        geom_point(aes(text = paste0(
                      "Site: ", Point_Name, "<br>",
                      input$variable, ": ", round(!!sym(input$variable), 2)
                    )), size = 2, color = "#3a78dc") +
        labs(x = "Bird Site", y = input$variable, title = "Bird Site Data") +
        theme_bw() +
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

## add neighbour conection
## two tabs, second comparing satelite vs netn