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
hg <- httpgd::hgd()

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
sps_name <- "BHVI"
park_name <- "parks"

#! Source code -----------------------------------------

#! Import data -----------------------------------------
## file paths
file_name <- "BLBW_step1_jagsdata_2025_09_15"
SPS_ANA_DATA <- glue("data/ana_file/{file_name}.rds")

## read files
dat <- read_rds(SPS_ANA_DATA)

glimpse(dat)
names(dat)

# park size
## date
dat2xb <- dat$Xp %>% 
              mutate(year = as.factor(dat$y[,6]),
                     intervalkey = as.factor(dat$y[,5]),
                     park = as.factor(dat$y[,2])) %>% 
                     filter(intervalkey == 1) %>% 
              arrange(date_jul_s)
              
ggplot() + 
       geom_point(aes(x = dat$Xp, 
                      y = dat$y2[,1]),

                  #alpha = 0.5,
                  size = 0.4) +
               theme_bw() +
               theme(#legend.position = "none",
               strip.text = element_blank()
               ) 

psize <- cbind(dat$Xp, dat$y2[,1], glue("{dat$y2[,2]}_{dat$y2[,3]}"))  %>% as_tibble()

psize %>% filter(V2 == 1) %>% pull(V1) %>% table()
psize %>% filter(V2 == 0) %>% pull(V1) %>% table()

psize %>%
  ggplot(aes(x = V1, y = V2, col = as.factor(V3))) +
  geom_jitter(height = 0.1, alpha = 0.6, size = 0.8) +
  labs(x = "Park Size (scaled)", y = "Detection (0/1)", 
       title = "Detection vs Park Size") +
  theme_bw()

dat2xb %>% 
    ggplot() + 
       geom_histogram(aes(x = date_jul_s, fill = park)) +
       facet_wrap(~ year, ncol = 4) +
       theme_bw()

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
       filter(park == 7,
       year == 2010)
-3.235437752 # 138

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
#TODO: park 3 has outliers on time in 2012!!! 
#TODO: park 6 has outliers on time in 2010!!! 

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

dat2x1 %>% mutate(year = as.character(year)) %>% 
           filter(year %in% c('2006','2007','2008','2009','2010',
                              '2011','2012','2013','2014','2015',
                              '2016','2017','2018','2019','2020')) %>% 
       ggplot() + 
              geom_point(aes(#x = jitter(as.numeric(sitekey),1.2), 
                            x = as.numeric(sitekey),
                            y = siteBA_s,
                            col = year),
                     #alpha = 0.5,
                     size = 0.4) +
                     theme_bw() +
                     theme(#legend.position = "none",
                     strip.text = element_blank()
                     ) +
                     facet_wrap(~ park, ncol = 1, scales = "free_x")

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

## check for correlation between vars
## check covariate values - proper place?
## go back in time MORE.