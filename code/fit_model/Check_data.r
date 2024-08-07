#? *********************************************************************************
#? -------------------------------   Check_data.R   -------------------------------
#? *********************************************************************************
#
#! Code to check the data that is being given to the model
#
#! Source ---------------------------------------------
#           - :
#           - :
#
#! Input ----------------------------------------------
#           - data/ana_file/data_{sps_name}_{park_name}.rds : data used to run the model
#           - :
#
#! Output ----------------------------------------------
#           - :
#           - :

# Print script file name
context <- rstudioapi::getSourceEditorContext()
cat("\n", "\n", "\n", 'Current script: ', basename(context[[2]]), "\n", "\n", "\n", "\n")

#! Package library and versions -------------------------
#  Created a library repo?
#  (  )yes  (  )no
#  renv::init()

# Load an existing library?
#  renv::restore()

# Installed new packages?
#  renv::snapshot()

# detach packages and clear workspace
#if(!require(freshr)){install.packages('freshr')}
#freshr::freshr()

#! Load packages ---------------------------------------
library(conflicted)
library(tidyverse)
library(glue)

conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::filter)
# conflicts_prefer(scales::alpha)

#! Make functions --------------------------------------
colanmes <- colnames
lenght <- length
`%!in%` <- Negate(`%in%`)

#! Settings --------------------------------------------
sps_name <- "RBWO"
park_name <- "parks"

#! Source code -----------------------------------------

#! Import data -----------------------------------------
## file paths
SPS_ANA_DATA <- glue("data/ana_file/data_{sps_name}_{park_name}.rds")

## read files
dat <- read_rds(SPS_ANA_DATA)

glimpse(dat)
names(dat)

# check detection variables
## date
dat2xb <- dat$Xb %>% 
              mutate(year = as.factor(dat$y[,6]),
                     intervalkey = as.factor(dat$y[,5]),
                     park = as.factor(dat$y[,2])) %>% 
                     filter(intervalkey == 1) %>% 
              arrange(date_jul_s)
              
ggplot(dat2xb) + 
       geom_point(aes(x = seq(1, nrow(dat2xb),1), 
                      y = date_jul_s,
                      col = year),
                  #alpha = 0.5,
                  size = 0.4) +
               theme_bw() +
               theme(#legend.position = "none",
               strip.text = element_blank()
               ) +
               facet_wrap(~ park, ncol = 4, scales = "free_x")

dat2xb %>% 
    ggplot() + 
       geom_histogram(aes(x = date_jul_s, fill = park)) +
       facet_wrap(~ year, ncol = 4) +
       theme_bw()
#TODO: maybe park 7 has an outlier - 2010 to 2012

dat2xb %>% 
    ggplot() + 
       geom_histogram(aes(x = date_jul_s, fill = park)) +
       theme_bw()

dat2xb %>% 
    ggplot() + 
       geom_histogram(aes(x = date_jul_s, fill = year)) +
       theme_bw()

## time
dat2xa <- dat$Xa %>% 
              mutate(year = as.factor(dat$y[,6]),
                     intervalkey = as.factor(dat$y[,5]),
                     park = as.factor(dat$y[,2])) %>% 
                     filter(intervalkey == 1) %>% 
              arrange(time_jul_s)
              
ggplot(dat2xa) + 
       geom_point(aes(x = seq(1, nrow(dat2xa),1), 
                      y = time_jul_s,
                      col = year),
                  #alpha = 0.5,
                  size = 0.4) +
               theme_bw() +
               theme(legend.position = "none",
               strip.text = element_blank()
               ) +
               facet_wrap(~ park, ncol = 4, scales = "free_x")
#TODO: park 3 has outliers on time in 2012 or 2011!!! 
dat2xa %>% 
    ggplot() + 
       geom_histogram(aes(x = time_jul_s, fill = park)) +
       facet_wrap(~ year, ncol = 4) +
       theme_bw()

dat2xa %>% 
    ggplot() + 
       geom_histogram(aes(x = time_jul_s, fill = park)) +
       theme_bw()

dat2xa %>% 
    ggplot() + 
       geom_histogram(aes(x = time_jul_s, fill = year)) +
       theme_bw()

# basal area
dat2x1 <- dat$X1 %>% 
              mutate(year = as.factor(dat$y[,6]),
                     intervalkey = as.factor(dat$y[,5]),
                     park = as.factor(dat$y[,2]),
                     sitekey = as.factor(dat$y[,3])) %>% 
                     filter(intervalkey == 1) 

ggplot(dat2x1) + 
       geom_point(aes(x = sitekey, 
                      y = siteBA_s,
                      col = year),
                  #alpha = 0.5,
                  size = 0.4) +
               theme_bw() +
               theme(legend.position = "none",
               strip.text = element_blank()
               ) +
               facet_wrap(~ park, ncol = 4, scales = "free_x")
# TODO: park 6 has different years
ggplot(dat2x1) + 
       geom_point(aes(x = park, 
                      y = parkBA_s,
                      col = year),
                  #alpha = 0.5,
                  size = 0.4) +
               theme_bw() +
               theme(legend.position = "none",
               strip.text = element_blank()
               )

ggplot(dat2x1) + 
       geom_point(aes(x = park, 
                      y = counBA_s,
                      col = year),
                  #alpha = 0.5,
                  size = 0.4) +
               theme_bw() +
               theme(legend.position = "none",
               strip.text = element_blank()
               ) 




               
#TODO: 
dat2xa %>% 
    ggplot() + 
       geom_histogram(aes(x = time_jul_s, fill = park)) +
       facet_wrap(~ year, ncol = 4) +
       theme_bw()

dat2xa %>% 
    ggplot() + 
       geom_histogram(aes(x = time_jul_s, fill = park)) +
       theme_bw()

dat2xa %>% 
    ggplot() + 
       geom_histogram(aes(x = time_jul_s, fill = year)) +
       theme_bw()
