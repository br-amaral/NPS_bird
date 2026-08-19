library(shiny)
library(plotly)
library(ggplot2)
library(dplyr)
library(reshape2)
library(conflicted)
library(tidyverse)
library(glue)
library(psych)
library(AHMbook)
library(ggplot2)
library(shiny)
library(plotly)

conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::filter)
conflicts_prefer(AHMbook::standardize)



#? Averages used in the model ------------------------------------------------
var_site_freq <- read_rds("data/out/var_site_freq.rds")
var_park_freq <- read_rds("data/out/var_park_freq.rds")
var_coun_freq <- read_rds("data/out/var_coun_freq.rds")

#? Annual variation from the forest plots rotation design ---------------------
site_covs_nei_grp_500m_yr
park_covs_yr
coun_covs_yr


# Map display names to column names for each level
covariate_choices <- c(
  "Shrub Density" = "SHRUden",
  "Density" = "DEN",
  "Basal Area" = "BA",
  "Richness" = "RICH",
  "Pole BA" = "BA_pole",
  "Large BA" = "BA_large"
)

ui <- fluidPage(
  fluidRow(
    column(12, selectInput("cov_type", "Covariate", choices = covariate_choices, selected = "SHRUden"))
  ),
  plotlyOutput("plot"),
  fluidRow(
    column(6, plotlyOutput("plot2", height = "700px")),
    column(6, plotlyOutput("plot3", height = "700px"))
  )
)

server_shrub <- function(input, output) {
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
  
  output$plot3 <- renderPlotly({
    covar <- input$cov_type
    coun_col <- paste0("coun", covar)
    df <- var_coun_freq %>% ungroup() %>% arrange(.data[[coun_col]]) %>% mutate(tnrow = row_number())
    limits <- c(0, round(max(df[[coun_col]], na.rm = TRUE) + 2))
    p3 <- ggplot(
      df,
      aes(x = tnrow, y = .data[[coun_col]], color = park, text = paste("Park:", park))) +
      geom_point(size = 3) +
      theme_bw() +
      labs(title = paste("Average COUNTY", coun_col)) + 
      theme(plot.title = element_text(hjust = 0.5),
            axis.title.x = element_blank(),
            legend.text = element_text(size = 12),
            legend.title = element_blank()) 
    ggplotly(p3, tooltip = "text")
  })
}

shinyApp(ui = ui, server = server_shrub)

## test

var_site_freq2 <- var_site_freq  %>% 
        ungroup() %>% 
        distinct() %>% 
        arrange(siteSHRUden) %>% 
        select(Point_Name, park, siteSHRUden) %>%
        mutate(tnrow = row_number()) 


ggplot(data = var_site_freq2) +
      geom_point(aes(y = siteSHRUden, color = park, x = tnrow))+
      scale_color_discrete() +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5),
            axis.title.x = element_blank(),
            legend.text = element_text(size = 12),
            legend.title = element_blank())

var_park_freq2 <- var_park_freq  %>% 
        ungroup() %>% 
        distinct() %>% 
        arrange(parkSHRUden) %>% 
        select(park, parkSHRUden) %>%
        mutate(tnrow = row_number()) 

ggplot(data = var_park_freq2) +
      geom_point(aes(y = parkSHRUden, color = park, x = tnrow))+
      scale_color_discrete() +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5),
            axis.title.x = element_blank(),
            legend.text = element_text(size = 12),
            legend.title = element_blank())

var_coun_freq2 <- var_coun_freq  %>% 
        ungroup() %>% 
        distinct() %>% 
        arrange(counSHRUden) %>% 
        select(park, counSHRUden) %>%
        mutate(tnrow = row_number()) 

ggplot(data = var_coun_freq2) +
      geom_point(aes(y = counSHRUden, color = park, x = tnrow))+
      scale_color_discrete() +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5),
            axis.title.x = element_blank(),
            legend.text = element_text(size = 12),
            legend.title = element_blank())

## richness for county
## pole BA county
## large BA county