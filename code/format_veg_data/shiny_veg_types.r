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

conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::filter)
# conflicts_prefer(scales::alpha)

#! Make functions --------------------------------------
colanmes <- colnames
lenght <- length
`%!in%` <- Negate(`%in%`)

#! Source code -----------------------------------------
# get park shape files with vegetation types and classify each as conifer, hardwood, mixed, or not forest
source("/Users/bamaral/Documents/GitHub/NPS_bird_copy/code/format_veg_data/veg_maps_park.R")
# gets all forest plots and calculates what percentage and value of density and BA is conifer and hardwood
source("/Users/bamaral/Documents/GitHub/NPS_bird_copy/code/format_veg_data/get_conhar_baden.R")

#! Import data -----------------------------------------
## file paths

## read files

# shiny for parks, forest type_plot, and new covariates
for_plots_sf <- for_plots_sf  %>% rename(PlotID = for_sit)
for_plots_sfh <- for_plots_sf  %>% filter(park == "ROVA"); for_plots_sfh$park <- "HOFR"   # hofr
for_plots_sfv <- for_plots_sf  %>% filter(park == "ROVA"); for_plots_sfv$park <- "VAMA"   # vama
for_plots_sfm <- for_plots_sfm  %>% rename(PlotID = for_sit)                              # mima

tre_cov3 <- tre_cov3  %>% mutate(ParkUnit = substr(PlotID, 1, 4))

tre_cov3percent_con <- tre_cov3  %>% 
  group_by(PlotID) %>% 
  mutate(
    tot_ba = sum(BA_m2ha),
    tot_den = sum(density)
  ) %>% 
  mutate(
    per_ba = BA_m2ha / tot_ba,
    per_den = density / tot_den
  ) %>% 
  filter(type_plot == "Conifer")

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

all_cover_types <- unique(unlist(lapply(park_list, function(x) unique(x$map$Cover_Type))))
palette <- c("#605d5d", "#1e7b1e", "#3a78dc", "#c98b19", "#dcdada")
cover_type_colors <- setNames(palette[seq_along(all_cover_types)], all_cover_types)

ui <- fluidPage(
  titlePanel("NPS Park Vegetation Maps"),
  sidebarLayout(
    sidebarPanel(
      selectInput("park", "Choose a Park:", choices = names(park_list), selected = "MABI")
    ),
    mainPanel(
      plotlyOutput("vegmap", height = "500px"),
      fluidRow(
        column(
          width = 6,
          h4("Percent Conifer BA by Plot"),
          plotlyOutput("per_ba_plot", height = "200px")
        ),
        column(
          width = 6,
          h4("Percent Conifer Density by Plot"),
          plotlyOutput("per_den_plot", height = "200px")
        )
      ),
      fluidRow(
        column(
          width = 6,
          h4("BA_m2ha by Plot"),
          plotlyOutput("ba_plot", height = "250px")
        ),
        column(
          width = 6,
          h4("Density by Plot"),
          plotlyOutput("density_plot", height = "250px")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  output$vegmap <- renderPlotly({
    park_data <- park_list[[input$park]]
    # Join percent conifer BA to plot points for annotation
    plot_points <- park_data$for_plots %>%
      left_join(tre_cov3, by = "PlotID") %>%
      left_join(tre_cov3percent_con %>% 
      filter(ParkUnit == input$park), by = "PlotID", suffix = c("", "_con")) %>% 
      filter(ParkUnit == input$park)
    p <- ggplot(data = park_data$map) +
      geom_sf(aes(fill = Cover_Type, text = paste("MapUnit:", MapUnit_Name, "<br>Cover Type:", Cover_Type))) +
      scale_fill_manual(values = cover_type_colors, na.value = "grey80") +
      geom_sf(
        data = plot_points,
        aes(
          text = paste0(
            "Plot: ", PlotID, "<br>",
            "Type: ", type_plot, "<br>",
            "Percent Conifer Den: ", round(per_den, 2), "<br>",
            "Percent Conifer BA: ", round(per_ba, 2)
          )
        ),
        color = "black", shape = 21, size = 3, fill = "red"
      ) +
      geom_sf(data = park_data$xy %>%
                filter(park == input$park), color = "black", shape = 24, size = 3, fill = "black") +
      theme_bw() +
      theme(legend.position = "bottom",
            plot.title = element_text(hjust = 0.5, size = 22)) +
      ggtitle(input$park)
    ggplotly(p, tooltip = "text")
  })

  output$per_ba_plot <- renderPlotly({
    df <- tre_cov3percent_con %>% filter(ParkUnit == input$park) %>% arrange(PlotID)
    p <- ggplot(df) +
      geom_point(aes(x = PlotID, y = per_ba, col = type_plot, text = paste0(
        "Plot: ", PlotID, "<br>",
        "Type: ", type_plot, "<br>",
        "Percent BA: ", round(per_ba, 2)
      )), size = 2) +
      labs(x = "Plot", y = "Percent Conifer BA", color = "Type") +
      theme_bw() +
      scale_color_manual(values = c("Conifer" = "#1e7b1e")) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    ggplotly(p, tooltip = "text")
  })

  output$per_den_plot <- renderPlotly({
    df <- tre_cov3percent_con %>% filter(ParkUnit == input$park) %>% arrange(PlotID)
    p <- ggplot(df) +
      geom_point(aes(x = PlotID, y = per_den, col = type_plot, text = paste0(
        "Plot: ", PlotID, "<br>",
        "Type: ", type_plot, "<br>",
        "Percent Density: ", round(per_den, 2)
      )), size = 2) +
      labs(x = "Plot", y = "Percent Conifer Density", color = "Type") +
      theme_bw() +
      scale_color_manual(values = c("Conifer" = "#1e7b1e")) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    ggplotly(p, tooltip = "text")
  })

  output$ba_plot <- renderPlotly({
    df <- tre_cov3 %>% filter(ParkUnit == input$park) %>% arrange(PlotID)
    p <- ggplot(df) +
      geom_point(aes(x = PlotID, y = BA_m2ha, col = type_plot, text = paste0(
        "Plot: ", PlotID, "<br>",
        "Type: ", type_plot, "<br>",
        "BA_m2ha: ", round(BA_m2ha, 2)
      )), size = 2) +
      labs(x = "Plot", y = "BA (m²/ha)", color = "Type") +
      theme_bw() +
      scale_color_manual(values = c("Conifer" = "#1e7b1e", "Hardwood" = "#c98b19")) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    ggplotly(p, tooltip = "text")
  })

  output$density_plot <- renderPlotly({
    df <- tre_cov3 %>% filter(ParkUnit == input$park) %>% arrange(PlotID)
    p <- ggplot(df) +
      geom_point(aes(x = PlotID, y = density, col = type_plot, text = paste0(
        "Plot: ", PlotID, "<br>",
        "Type: ", type_plot, "<br>",
        "Density: ", density
      )), size = 2) +
      labs(x = "Plot", y = "Density", color = "Type") +
      theme_bw() +
      scale_color_manual(values = c("Conifer" = "#1e7b1e", "Hardwood" = "#c98b19")) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    ggplotly(p, tooltip = "text")
  })
}

shinyApp(ui, server)



