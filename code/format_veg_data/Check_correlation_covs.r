library(reshape2)
library(conflicted)
library(tidyverse)
library(glue)
library(psych)
library(AHMbook)
library(ggplot2)

conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::filter)
conflicts_prefer(AHMbook::standardize)

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

## group by year and site
sit_yr <- X5[,c(3:4,6:8, 10:14)] %>% 
  group_by(Year, Point_Name) %>% 
  summarise_all(mean, na.rm = T) %>% 
  ungroup() %>% 
  filter(!is.na(siteDEN))

corr_mat <- round(cor(sit_yr[,c(3:10)], use="complete.obs"),2)

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

#! Variation plot for site ----------------------------------------------
var_site <- X5[,c(3:4, 6:8, 10:14)] %>% 
  distinct() %>% 
  rowwise() %>%
  mutate(rem = sum(siteDEN, siteBA, siteRICH, 
                   siteBA_pole, siteBA_mature, siteBA_large,
                   siteSAPden, siteSHRUden,
                   na.rm = T)) %>%
  filter(rem != 0) %>% 
  select(-rem)

var_site2 <- var_site 
var_site2$siteDEN <- standardize(var_site2$siteDEN)
var_site2$siteBA <- standardize(var_site2$siteBA)
var_site2$siteRICH <- standardize(var_site2$siteRICH)
var_site2$siteBA_pole <- standardize(var_site2$siteBA_pole)
var_site2$siteBA_mature <- standardize(var_site2$siteBA_mature)
var_site2$siteBA_large <- standardize(var_site2$siteBA_large)
var_site2$siteSAPden <- standardize(var_site2$siteSAPden)
var_site2$siteSHRUden <- standardize(var_site2$siteSHRUden)

long_var_site <- pivot_longer(var_site2, 
                                cols = c(3:10), 
                                names_to = "Var", 
                                values_to = "Value")
long_var_site_mean <- long_var_site %>% 
  group_by(Point_Name, Var) %>% 
  summarise(Value = mean(Value, na.rm = T)) %>% 
  ungroup() %>% 
  #! TODO: attention 2021 is the mean 
  mutate(Year = as.numeric(2021)) %>% 
  relocate(Year)

long_var_site2 <- rbind(long_var_site_mean, long_var_site)
# plotting variation plot
Vars <- c('siteDEN', 'siteBA', 'siteRICH', 
          'siteBA_pole', 'siteBA_mature', 'siteBA_large',
          'siteSAPden', 'siteSHRUden')
hist(long_var_site2$Value)
hist(long_var_site2$Year)

for(i in 1:length(Vars)){
  Year <- long_var_site2 %>% 
            filter(Var == Vars[i]) %>% 
            pull(Year)
  values <- long_var_site2 %>% 
            filter(Var == Vars[i]) %>% 
            pull(Value)
  park2 <- long_var_site2 %>% 
            filter(Var == Vars[i]) %>% 
            mutate(park = substr(Point_Name, 1, 4)) %>% 
            select(park) 
  park1 <- long_var_site2 %>% 
            filter(Var == Vars[i]) %>% 
            select(-Year) %>%
            select(-Value) %>%
            distinct() %>% 
            mutate(park = substr(Point_Name, 1, 4)) %>% 
            select(park) 
  park3 <- long_var_site2 %>% 
            filter(Var == Vars[i]) %>% 
            select(-Year) %>%
            select(-Value) %>%
            distinct() %>%
            mutate(park = substr(Point_Name, 1, 4)) %>% 
            group_by(park) %>%
            slice_head(n = 1) %>%
            ungroup() %>% 
            select(Point_Name, park)
  park3 <- left_join(as_tibble(park1), park3, by = "park") %>% 
            pull(Point_Name)
  park3 <- cumsum(as.numeric(table(park3))) 

  park2 <- park2 %>% pull(park)
  
  p <- ggplot(data = long_var_site2 %>% 
                        filter(Var == Vars[i]) %>% 
                        mutate(park = substr(Point_Name, 1, 4)),
              aes(x=Year, y=Point_Name, 
                            fill=Value)) + 
              #facet_wrap(~park, scales = "free") +
              geom_tile(
                aes(color="black"#as.factor(park2)#, width=0.4, height=0.7
                ), linewidth=0.1
                ) +
              scale_fill_gradientn(colours = c("red","orange", "pink", "blue", "black"), 
                                  name="SD from mean \n",
                                  limits=c(min(values),max(values))) +
              theme_bw() + 
              geom_hline(yintercept = c(as.numeric(park3) + 0.5, 0.5)) +
              geom_vline(xintercept = 2020.5) +
              geom_vline(xintercept = 2021.5) +
              scale_x_continuous("Year", labels = as.character(Year), breaks = Year) +
              theme(axis.text.x = element_text(vjust = 1, 
                    angle = 90, size = 6),
                    axis.text.y = element_text(size = 6),
                    axis.title.x = element_blank(),       # Change x axis title only
                    axis.title.y = element_blank()
                    ) +
              ggtitle(glue("Variation of {Vars[i]}")) 
  print(p)
}

## loop 2 facet
for(i in 1:length(Vars)){
  Year <- long_var_site2 %>% 
            filter(Var == Vars[i]) %>% 
            pull(Year)
  values <- long_var_site2 %>% 
            filter(Var == Vars[i]) %>% 
            pull(Value)
  park2 <- long_var_site2 %>% 
            filter(Var == Vars[i]) %>% 
            mutate(park = substr(Point_Name, 1, 4)) %>% 
            select(park) 
  park1 <- long_var_site2 %>% 
            filter(Var == Vars[i]) %>% 
            select(-Year) %>%
            select(-Value) %>%
            distinct() %>% 
            mutate(park = substr(Point_Name, 1, 4)) %>% 
            select(park) 
  park3 <- long_var_site2 %>% 
            filter(Var == Vars[i]) %>% 
            select(-Year) %>%
            select(-Value) %>%
            distinct() %>%
            mutate(park = substr(Point_Name, 1, 4)) %>% 
            group_by(park) %>%
            slice_head(n = 1) %>%
            ungroup() %>% 
            select(Point_Name, park)
  park3 <- left_join(as_tibble(park1), park3, by = "park") %>% 
            pull(Point_Name)
  park3 <- cumsum(as.numeric(table(park3))) 

  park2 <- park2 %>% pull(park)
  
  p <- ggplot(data = long_var_site2 %>% 
                        filter(Var == Vars[i]) %>% 
                        mutate(park = substr(Point_Name, 1, 4)),
              aes(x=Year, y=Point_Name, 
                            fill=Value)) + 
              facet_wrap(~park, scales = "free") +
              geom_tile(
                aes(color="black"#as.factor(park2)#, width=0.4, height=0.7
                ), linewidth=0.1
                ) +
              scale_fill_gradientn(colours = c("red","orange", "pink", "blue", "black"), 
                                  name="SD from mean \n",
                                  #limits=c(min(values),max(values))
                                  ) +
              theme_bw() + 
              geom_hline(yintercept = c(as.numeric(park3) + 0.5, 0.5)) +
              scale_x_continuous("Year", labels = as.character(Year), breaks = Year) +
              theme(axis.text.x = element_text(vjust = 1, 
                    angle = 90, size = 6),
                    axis.text.y = element_text(size = 6),
                    axis.title.x = element_blank(),       # Change x axis title only
                    axis.title.y = element_blank()
                    ) +
              ggtitle(glue("Variation of {Vars[i]}")) 
  print(p)
}

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

pairs.panels(X5[,c(24:29)],
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
