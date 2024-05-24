library(reshape2)
library(conflicted)
library(tidyverse)
library(glue)
library(psych)

conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::filter)

X5 <- read_rds(file = "data/X5.rds")

colnames(X5)

# creating correlation matrix
corr_mat <- round(cor(X5[,c(6:8, 10:17,19:39)], use="complete.obs"),2)

# reduce the size of correlation matrix
melted_corr_mat <- melt(corr_mat) %>% 
    mutate(cov = substr(Var1,5,6))

# plotting the correlation heatmap
ggplot(data = melted_corr_mat, aes(x=Var1, y=Var2, 
								fill=value)) + 
geom_tile(color = "white")+
 scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
   midpoint = 0, limit = c(-1,1), space = "Lab", 
   name="Correlation \n") +
   theme_minimal()+ 
 theme(axis.text.x = element_text(vjust = 1, angle = 90),
       axis.title.x = element_blank(),       # Change x axis title only
       axis.title.y = element_blank() )+
 geom_text(aes(Var1, Var2, label = value), 
		color = "black", 
        size = 4)  

melted_corr_mat %>% 
  filter(abs(value) < 0.5) %>% 
  arrange(desc(value))

#! Correlation plot for site ----------------------------------------------
corr_mat <- round(cor(X5[,c(6:8, 10:14)], use="complete.obs"),2)

melted_corr_mat <- melt(corr_mat) %>% 
    mutate(cov = substr(Var1,5,6))

# plotting the correlation heatmap
ggplot(data = melted_corr_mat, aes(x=Var1, y=Var2, 
								fill=value)) + 
geom_tile(color = "white")+
 scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
   midpoint = 0, limit = c(-1,1), space = "Lab", 
   name="Correlation \n") +
   theme_minimal()+ 
 theme(axis.text.x = element_text(vjust = 1, angle = 90),
       axis.title.x = element_blank(),       # Change x axis title only
       axis.title.y = element_blank() )+
 geom_text(aes(Var1, Var2, label = value), 
		color = "black", 
        size = 4)  

melted_corr_mat %>% 
  filter(abs(value) < 0.5) %>% 
  arrange(desc(value))

#! Correlation plot for parks ----------------------------------------------
corr_mat <- round(cor(X5[,c(15:17,19:23)], use="complete.obs"),2)

# reduce the size of correlation matrix
melted_corr_mat <- melt(corr_mat) %>% 
    mutate(cov = substr(Var1,5,6))

# plotting the correlation heatmap
ggplot(data = melted_corr_mat, aes(x=Var1, y=Var2, 
								fill=value)) + 
geom_tile(color = "white")+
 scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
   midpoint = 0, limit = c(-1,1), space = "Lab", 
   name="Correlation \n") +
   theme_minimal()+ 
 theme(axis.text.x = element_text(vjust = 1, angle = 90),
       axis.title.x = element_blank(),       # Change x axis title only
       axis.title.y = element_blank() )+
 geom_text(aes(Var1, Var2, label = value), 
		color = "black", 
        size = 4)  

melted_corr_mat %>% 
  filter(abs(value) < 0.5) %>% 
  arrange(desc(value))

#! Correlation plot for county ----------------------------------------------
corr_mat <- round(cor(X5[,c(24:39)], use="complete.obs"),2)

# reduce the size of correlation matrix
melted_corr_mat <- melt(corr_mat) %>% 
    mutate(cov = substr(Var1,5,6))

# plotting the correlation heatmap
ggplot(data = melted_corr_mat, aes(x=Var1, y=Var2, 
								fill=value)) + 
geom_tile(color = "white")+
 scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
   midpoint = 0, limit = c(-1,1), space = "Lab", 
   name="Correlation \n") +
   theme_minimal()+ 
 theme(axis.text.x = element_text(vjust = 1, angle = 90),
       axis.title.x = element_blank(),       # Change x axis title only
       axis.title.y = element_blank() )+
 geom_text(aes(Var1, Var2, label = value), 
		color = "black", 
        size = 4)  

melted_corr_mat %>% 
  filter(abs(value) < 0.5) %>% 
  arrange(desc(value))

# Same, but only for basal area, density and shrub ----------------

# site
tab1 <- X5[,c(6:7,14)] %>% 
          distinct() %>% 
          filter(!is.na(siteDEN))
          
pairs.panels(tab1,
             smooth = TRUE,         # If TRUE, draws loess smooths
             scale = TRUE,          # If TRUE, scales the correlation text font
             density = TRUE,        # If TRUE, adds density plots and histograms
             ellipses = FALSE,      # If TRUE, draws ellipses
             method = "spearman",   # Correlation method ("spearman", "pearson" or "kendall")
             lm = TRUE,             # If TRUE, plots linear fit rather than the LOESS (smoothed) fit
             cor = TRUE,            # If TRUE, reports correlations
             jiggle = FALSE,        # If TRUE, data points are jittered
             hist.col = "magenta",  # Histograms color
             stars = TRUE,          # If TRUE, adds significance level with stars
             ci = TRUE)             # If TRUE, adds confidence intervals


# park
tab2 <- X5[,c(15:16,23)] %>% 
          distinct() %>% 
          filter(!is.na(parkDEN))
pairs.panels(tab2,
             smooth = TRUE,         # If TRUE, draws loess smooths
             scale = TRUE,          # If TRUE, scales the correlation text font
             density = TRUE,        # If TRUE, adds density plots and histograms
             ellipses = FALSE,      # If TRUE, draws ellipses
             method = "spearman",   # Correlation method ("spearman", "pearson" or "kendall")
             lm = TRUE,             # If TRUE, plots linear fit rather than the LOESS (smoothed) fit
             cor = TRUE,            # If TRUE, reports correlations
             jiggle = FALSE,        # If TRUE, data points are jittered
             hist.col = "magenta",  # Histograms color
             stars = TRUE,          # If TRUE, adds significance level with stars
             ci = TRUE)             # If TRUE, adds confidence intervals

# county
tab3 <- X5[,c(24:25,39)] %>% 
          distinct() %>% 
          filter(!is.na(counDEN))
pairs.panels(tab3,
             smooth = TRUE,         # If TRUE, draws loess smooths
             scale = TRUE,          # If TRUE, scales the correlation text font
             density = TRUE,        # If TRUE, adds density plots and histograms
             ellipses = FALSE,      # If TRUE, draws ellipses
             method = "spearman",   # Correlation method ("spearman", "pearson" or "kendall")
             lm = TRUE,             # If TRUE, plots linear fit rather than the LOESS (smoothed) fit
             cor = TRUE,            # If TRUE, reports correlations
             jiggle = FALSE,        # If TRUE, data points are jittered
             hist.col = "magenta",  # Histograms color
             stars = TRUE,          # If TRUE, adds significance level with stars
             ci = TRUE)             # If TRUE, adds confidence intervals







#! Variation plot for park ----------------------------------------------
for_sit %>% 
  mutate(park = substr(Plot_Name, 1, 4))  %>% 
  #filter(park %!in% c("ELRO", "HOFR", "ROVA", "VAMA")) %>%  
  ggplot(aes(x=park, y=BA_m2ha, fill=park))  +
    geom_boxplot() +
    geom_jitter(position=position_jitter(0.2), alpha = 0.3) +
    coord_flip() +
    theme_bw() +
    theme(legend.position="none",
          axis.title.y = element_blank(),
          plot.title = element_text(hjust = 0.5)) +
    labs(title = "Park Scale",
        y =" \n  Basal area of live trees \n(>=10cm DBH in m2/ha)") +
    scale_fill_manual(values = met.brewer("Morgenstern")) +
    stat_summary(colour = "red", size = 0.75)

#! Correlation plots --------------------------------------------------
## basal area ---------------------------------------------------------
county_covs <- read_rds(file = "data/FIA/out/tpa_fim.rds") %>% 
  filter(park %!in% c("ELRO", "HOFR", "ROVA", "VAMA"))  %>% 
  group_by(park) %>% 
  mutate(BA_coun = mean(BAA, na.rm = T))  %>% 
  ungroup() %>% 
  select(park,
         BA_coun) %>% 
  distinct()

park_covs

all_scales_covs <- left_join(county_covs, 
                             park_covs %>% rename(BA_park = BA_m2haM) %>% select(park, BA_park),
                             by = "park")

all_scales_covs <- left_join(close_points_f2 %>% 
                                rename(BA_site = BA_m2haM) %>% 
                                select(park, BA_site),
                             all_scales_covs,
                             by = "park") %>% 
                             distinct()
all_scales_covs <- all_scales_covs  %>% 
                     relocate(park, BA_site, BA_park, BA_coun)

# creating correlation matrix
corr_mat <- round(cor(all_scales_covs[,2:4], use="complete.obs"),2)

# reduce the size of correlation matrix
melted_corr_mat <- melt(corr_mat) %>% 
    mutate(cov = substr(Var1,5,6))

# plotting the correlation heatmap
ggplot(data = melted_corr_mat, aes(x=Var1, y=Var2, 
								fill=value)) + 
geom_tile(color = "white")+
 scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
   midpoint = 0, limit = c(-1,1), space = "Lab", 
   name="Correlation \n") +
   theme_minimal()+ 
 theme(axis.text.x = element_text(vjust = 1, angle = 90),
       axis.title.x = element_blank(),       # Change x axis title only
       axis.title.y = element_blank() )+
 geom_text(aes(Var1, Var2, label = value), 
		color = "black", 
        size = 4)  

library("PerformanceAnalytics")
chart.Correlation(all_scales_covs[,2:4], histogram=TRUE, pch=19)

## density ---------------------------------------------------------
county_covs <- read_rds(file = "data/FIA/out/tpa_fim.rds") %>% 
  filter(park %!in% c("ELRO", "HOFR", "ROVA", "VAMA"))  %>% 
  group_by(park) %>% 
  # change from per acre to hectare
  mutate(TPH_coun = mean(TPA*2.471, na.rm = T))  %>% 
  ungroup() %>% 
  select(park,
         TPH_coun) %>% 
  distinct()

park_covs

all_scales_covs <- left_join(county_covs, 
                             park_covs %>% 
                                rename(TPH_park = treeden_haM) %>% 
                                select(park, TPH_park),
                             by = "park")

all_scales_covs <- left_join(close_points_f2 %>% 
                                rename(TPH_site = treeden_haM) %>% 
                                select(park, TPH_site),
                             all_scales_covs,
                             by = "park") %>% 
                             distinct()

all_scales_covs <- all_scales_covs  %>% 
                     relocate(park, TPH_site, TPH_park, TPH_coun)

# creating correlation matrix
corr_mat <- round(cor(all_scales_covs[,2:4], use="complete.obs"),2)

# reduce the size of correlation matrix
melted_corr_mat <- melt(corr_mat) %>% 
    mutate(cov = substr(Var1,5,6))

# plotting the correlation heatmap
ggplot(data = melted_corr_mat, aes(x=Var1, y=Var2, 
								fill=value)) + 
geom_tile(color = "white")+
 scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
   midpoint = 0, limit = c(-1,1), space = "Lab", 
   name="Correlation \n") +
   theme_minimal()+ 
 theme(axis.text.x = element_text(vjust = 1, angle = 90),
       axis.title.x = element_blank(),       # Change x axis title only
       axis.title.y = element_blank() )+
 geom_text(aes(Var1, Var2, label = value), 
		color = "black", 
        size = 4)  

chart.Correlation(all_scales_covs[,2:4], histogram=TRUE, pch=19)
