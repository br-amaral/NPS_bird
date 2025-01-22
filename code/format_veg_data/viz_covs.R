# *********************************************************************************
# -------------------------------   Amazing Title   -------------------------------
# *********************************************************************************
# Code to vizualize NPS environmental covariates, and check for correlations
#  between them and between the scales
#
# Source ---------------------------------------------
#           - :
#           - :
#
# Input ----------------------------------------------
#           - :
#           - :
#
# Output ----------------------------------------------
#           - :
#           - :
#
# detach packages and clear workspace
if(!require(freshr)){install.packages('freshr')}
freshr::freshr()
#
# Load packages ---------------------------------------
library(conflicted)
library(tidyverse)
library(glue)
library(reshape2)
library(ggplot2)
library(ggh4x)

conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::filter)
# conflicts_prefer(scales::alpha)
#
# Make functions --------------------------------------
colanmes <- colnames
lenght <- length
`%!in%` <- Negate(`%in%`)
#
# Source code -----------------------------------------
#
# Import data -----------------------------------------
## file paths
XDAT_PATH <- "data/X10.rds"

## read files
X <- read_rds(file = XDAT_PATH)

X2 <- X %>% 
    distinct() %>% 
    select(siteBA, parkBA, counBA, 
           siteDEN, parkDEN, counDEN)

X2 <- X %>% 
    distinct() %>% 
    select(park, parkBA, counBA) %>% 
    distinct()

# creating correlation matrix
corr_mat <- round(cor(X2),2)

# reduce the size of correlation matrix
melted_corr_mat <- melt(corr_mat) %>% 
    mutate(cov = substr(Var1,5,6))

# plotting the correlation heatmap
ggplot(data = melted_corr_mat, aes(x=Var1, y=Var2, 
								fill=value)) + 
geom_tile(color = "white")+
 scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
   midpoint = 0, limit = c(-1,1), space = "Lab", 
   name="Correlation") +
   theme_minimal()+ 
 theme(axis.text.x = element_text(vjust = 1, 
    size = 12, hjust = 0.5))+
 geom_text(aes(Var1, Var2, label = value), 
		color = "black", 
        size = 4)  
 

 ggplot(melted_corr_mat, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  geom_hline(yintercept = 0:6 + 0.5, color = "white") +
   scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
   midpoint = 0, limit = c(-1,1), space = "Lab", 
   name="Correlation") +
   theme_minimal()+
   facet_grid2(.~cov, 
              scales = "free_x", space = "free_x", 
              switch = "x")+
  theme_bw(base_size = 16) +
  theme(axis.title.x     = element_blank(),
        axis.title.y     = element_blank(),
        axis.ticks.x     = element_blank(),
        panel.spacing.x  = unit(0, "mm"),
        panel.background = element_rect(fill = NA, color = "black", size = 1),
        panel.grid       = element_blank()) +
  geom_text(aes(Var1, Var2, label = value), 
		color = "black", 
        size = 4)  

library(ggplot2)
library(viridis)

X2 <- X10 %>% dplyr::filter(park != "ACAD") %>% 
  select(park, Year, siteDEN_s, parkDEN_s, counDEN_s) %>% 
  mutate(siteDEN_s = as.numeric(siteDEN_s), 
         parkDEN_s = as.numeric(parkDEN_s), 
         counDEN_s = as.numeric(counDEN_s))

X1 <- X10 %>% dplyr::filter(park != "ACAD") %>% 
  select(park, Year, siteBA_s, parkBA_s, counBA_s) %>% 
  mutate(siteBA_s = as.numeric(siteBA_s), 
         parkBA_s = as.numeric(parkBA_s), 
         counBA_s = as.numeric(counBA_s))

ggplot(X1 %>% select(park, Year, siteBA_s) %>% distinct(), 
  aes(Year, park, fill= siteBA_s)) + 
    geom_tile() +
  scale_fill_viridis(discrete=FALSE) +
    theme_bw()

ggplot(X1 %>% select(park, Year, parkBA_s) %>% distinct(), 
  aes(Year, park, fill= parkBA_s)) + 
    geom_tile() +
  scale_fill_viridis(discrete=FALSE) +
    theme_bw()

ggplot(X1 %>% select(park, Year, counBA_s) %>% distinct(), 
  aes(Year, park, fill= counBA_s)) + 
    geom_tile() +
  scale_fill_viridis(discrete=FALSE) +
    theme_bw()

ggplot(X2 %>% select(park, Year, siteDEN_s) %>% distinct(), 
  aes(Year, park, fill= siteDEN_s)) + 
    geom_tile() +
  scale_fill_viridis(discrete=FALSE) +
    theme_bw()

ggplot(X2 %>% select(park, Year, parkDEN_s) %>% distinct(), 
  aes(Year, park, fill= parkDEN_s)) + 
    geom_tile() +
  scale_fill_viridis(discrete=FALSE) +
    theme_bw()

ggplot(X2 %>% select(park, Year, counDEN_s) %>% distinct(), 
  aes(Year, park, fill= counDEN_s)) + 
    geom_tile() +
  scale_fill_viridis(discrete=FALSE) +
    theme_bw()
