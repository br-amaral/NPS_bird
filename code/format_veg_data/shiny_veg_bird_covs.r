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

tre_cov3percent_con <- tre_cov3  %>% 
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
         type == "Conifer")
## TODO: where did ROVA-023 went? if it is all hardwood is getting NA - FIX

# datatable(tre_cov3percent_con)

## get neighbours to calculate % of conifer and hardwood for each bird point
neighbor <- read_rds(file = glue("data/out/neighbor_fornofor_{radi_dist}m.rds"))

neighbor2 <- left_join(neighbor %>% 
                          rename(PlotID = for_sit), 
                       tre_cov3percent_con %>% 
                          select(PlotID, ParkUnit, tot_ba, tot_den, per_ba, per_den),
                        by = "PlotID")  %>% 
                       arrange(bird_sit)


## if it is hardwood, put 0% conifer
## color the points according to type_plot

park_list <- list(
  "MABI" = list(map = mabi_vegmap2, for_plots = for_plots_sf, xy = xy_sf, close_points = close_points_f2 %>% filter(ParkUnit == "MABI")),
  "MORR" = list(map = morr_vegmap2, for_plots = for_plots_sf, xy = xy_sf, close_points = close_points_f2 %>% filter(ParkUnit == "MORR")),
  "SAGA" = list(map = saga_vegmap2, for_plots = for_plots_sf, xy = xy_sf, close_points = close_points_f2 %>% filter(ParkUnit == "SAGA")),
  "SARA" = list(map = sara_vegmap2, for_plots = for_plots_sf, xy = xy_sf, close_points = close_points_f2 %>% filter(ParkUnit == "SARA")),
  "WEFA" = list(map = wefa_vegmap2, for_plots = for_plots_sf, xy = xy_sf, close_points = close_points_f2 %>% filter(ParkUnit == "WEFA")),
  "HOFR" = list(map = rova_vegmap2, for_plots = for_plots_sfh, xy = xy_sf, close_points = close_points_f2 %>% filter(ParkUnit == "HOFR")),
  "VAMA" = list(map = rova_vegmap2, for_plots = for_plots_sfv, xy = xy_sf, close_points = close_points_f2 %>% filter(ParkUnit == "VAMA")),
  "MIMA" = list(map = mima_vegmap2, for_plots = for_plots_sfm, xy = xy_sf, close_points = close_points_f2 %>% filter(ParkUnit == "MIMA"))
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
palette <- c("#605d5d", "#1e7b1e", "#3a78dc", "#c98b19", "#dcdada")
cover_type_colors <- setNames(palette[seq_along(all_cover_types)], all_cover_types)

for_nofor <- unique(unlist(lapply(park_list, function(x) unique(x$map$Cover_Type2))))
palette2 <- c("#605d5d", "#1e7b1e")
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


## test code
# park_data <- park_list[["MABI"]]
# plot_points <- park_data$for_plots %>%
#   left_join(tre_cov3, by = "PlotID") %>%
#   left_join(tre_cov3percent_con %>% 
#   filter(ParkUnit == "MABI"), by = "PlotID", suffix = c("", "_con")) %>% 
#   filter(ParkUnit == "MABI")


server <- function(input, output, session) {
  output$vegmap <- renderPlotly({
    park_data <- park_list[[input$park]]
    
    # Join percent conifer BA to plot points for annotation
    plot_points <- park_data$for_plots %>%
      left_join(tre_cov3, by = "PlotID") %>%
      left_join(tre_cov3percent_con %>% 
      filter(ParkUnit == input$park), by = "PlotID", suffix = c("", "_con")) %>% 
      filter(ParkUnit == input$park)
    
    # Create park-specific color palette for current park's bird_sit values
    current_bird_sits <- unique(park_data$close_points$bird_sit)
    current_colors <- setNames(plot_palette[1:length(current_bird_sits)], current_bird_sits)
    
    p <- ggplot(data = park_data$map) +
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
              aes(color = Point_Name),
              shape = 18, size = 3) +
      scale_color_manual(values = current_colors) + 
      theme_bw() +
      theme(legend.position = "none",
            legend.text = element_text(size = 8),
            legend.title = element_text(size = 9),
            plot.title = element_text(hjust = 0.5, size = 22)) +
      ggtitle(input$park)
    
    ggplotly(p, tooltip = "text")
  })

  output$per_ba_plot <- renderPlotly({
    df <- tre_cov3percent_con %>% filter(ParkUnit == input$park) %>% arrange(PlotID)
    p <- ggplot(df) +
      geom_point(aes(x = PlotID, y = per_ba, col = type_plot, shape = type_plot, text = paste0(
        "Plot: ", PlotID, "<br>",
        "Type: ", type_plot, "<br>",
        "Percent BA: ", round(per_ba, 2)
      )), size = 2) +
      labs(x = "Plot", y = "Percent Conifer BA", color = "Type") +
      theme_bw() +
      scale_color_manual(values = c("Conifer" = "#1e7b1e", "Hardwood" = "#c98b19")) +
      scale_shape_manual(values = c("Conifer" = 16, "Hardwood" = 17)) + 
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
            legend.text = element_text(size = 8),
            legend.title = element_text(size = 9))
    ggplotly(p, tooltip = "text")
  })

  output$per_den_plot <- renderPlotly({
    df <- tre_cov3percent_con %>% filter(ParkUnit == input$park) %>% arrange(PlotID)
    p <- ggplot(df) +
      geom_point(aes(x = PlotID, y = per_den, col = type_plot, shape = type_plot, text = paste0(
        "Plot: ", PlotID, "<br>",
        "Type: ", type_plot, "<br>",
        "Percent Density: ", round(per_den, 2)
      )), size = 2) +
      labs(x = "Plot", y = "Percent Conifer Density", color = "Type") +
      theme_bw() +
      scale_color_manual(values = c("Conifer" = "#1e7b1e", "Hardwood" = "#c98b19")) +
      scale_shape_manual(values = c("Conifer" = 16, "Hardwood" = 17)) + 
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
            legend.text = element_text(size = 8),
            legend.title = element_text(size = 9))
    ggplotly(p, tooltip = "text")
  })

  output$ba_plot <- renderPlotly({
    df <- tre_cov3 %>% filter(ParkUnit == input$park) %>% arrange(PlotID)
    p <- ggplot(df) +
      geom_point(aes(x = PlotID, y = BA_m2ha, 
                     col = type, 
                     shape = type_plot,
                     text = paste0(
        "Plot: ", PlotID, "<br>",
        "Type: ", type_plot, "<br>",
        "BA_m2ha: ", round(BA_m2ha, 2)
      )), size = 2) +
      labs(x = "Plot", y = "BA (m²/ha)", color = "Type", shape = "Plot Type") +
      theme_bw() +
      scale_color_manual(values = c("Conifer" = "#1e7b1e", "Hardwood" = "#c98b19")) +
      scale_shape_manual(values = c("Conifer" = 16, "Hardwood" = 17)) +  # 16=circle, 17=triangle
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
            legend.text = element_text(size = 8),
            legend.title = element_text(size = 9))
    ggplotly(p, tooltip = "text")
  })

  output$density_plot <- renderPlotly({
    df <- tre_cov3 %>% filter(ParkUnit == input$park) %>% arrange(PlotID)
    p <- ggplot(df) +
      geom_point(aes(x = PlotID, y = density, 
                     col = type, 
                     shape = type_plot,
                     text = paste0(
        "Plot: ", PlotID, "<br>",
        "Type: ", type_plot, "<br>",
        "Density: ", density
      )), size = 2) +
      labs(x = "Plot", y = "Density", color = "Type", shape = "Plot Type") +
      theme_bw() +
      scale_color_manual(values = c("Conifer" = "#1e7b1e", "Hardwood" = "#c98b19")) +
      scale_shape_manual(values = c("Conifer" = 16, "Hardwood" = 17)) +  # 16=circle, 17=triangle
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
            legend.text = element_text(size = 8),
            legend.title = element_text(size = 9))
    ggplotly(p, tooltip = "text")
  })

   output$plot <- renderPlotly({
    covar <- input$cov_type
    site_col <- paste0("site", covar)
    df <- var_site_freq %>% ungroup() %>% arrange(.data[[site_col]]) %>% mutate(tnrow = row_number())
    limits <- c(0, round(max(df[[site_col]], na.rm = TRUE) + 2))
    p <- ggplot(
      df,
      aes(y = .data[[site_col]], color = park, x = tnrow,
          text = paste("Park:", park, "<br>Point:", Point_Name))) +
      geom_jitter(height = 0.1, width = 0.1) +
      scale_color_discrete() +
      theme_bw() +
      labs(title = paste("Average SITE", site_col)) + 
      theme(plot.title = element_text(hjust = 0.5),
            axis.title.x = element_blank(),
            legend.text = element_text(size = 12),
            legend.title = element_blank())
    ggplotly(p, tooltip = "text")
  })
  
  output$plot2 <- renderPlotly({
    covar <- input$cov_type
    park_col <- paste0("park", covar)
    df <- var_park_freq %>% ungroup() %>% arrange(.data[[park_col]]) %>% mutate(tnrow = row_number())
    limits <- c(0, round(max(df[[park_col]], na.rm = TRUE) + 2))
    p2 <- ggplot(
      df,
      aes(x = tnrow, y = .data[[park_col]], color = park, text = paste("Park:", park))) +
      geom_point(size = 3) +
      theme_bw() +
      labs(title = paste("Average PARK", park_col)) + 
      theme(plot.title = element_text(hjust = 0.5),
            axis.title.x = element_blank(),
            legend.text = element_text(size = 12),
            legend.title = element_blank()) 
    ggplotly(p2, tooltip = "text")
  })
}

shinyApp(ui, server)
## send bird sites values to aaron

## ( x ) classify everything as forest and not forest
## ( x ) connect bird and forest sites
## ( x ) remove the red circles and triangles - same symbol
## (   ) get percentage hardwood and conifer for bird sites
## (   ) sensitivity analysis - how many neighbours I get and how estimates changes as the radius gets bigger
## (   ) compare with aarons estimates


## (   ) get some measure of stand structure (how much vegetation in each strata)
## (   ) get same covariates as birds (close_points_etc) for the forest sites
## (   ) toggle for each covariate
## (   ) gradient of color for the covariates for each bird site and plot (e.g. ba 0.2 is blue and 0.8 is green)
## (   ) HOFR and VAMA error 
