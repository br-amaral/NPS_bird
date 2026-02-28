freshr::freshr()
library(reshape2)
library(tidyverse)

XDAT_PATH <- "data/X.rds"
X10 <- read_rds(file = XDAT_PATH)

X_corr <- X10 %>% 
            filter(interval_n == 1) %>% 
            dplyr::select(Point_Name,
                          BA_m2ha_Conifer_coun,
                          BA_m2ha_Conifer_park,
                          BA_m2ha_Conifer_site,
                          BA_m2ha_coun,
                          BA_m2ha_park,
                          BA_m2ha_site,
                          BA_m2ha_large_coun,
                          BA_m2ha_large_park,
                          BA_m2ha_large_site,
                          shrub_cov_coun,
                          shrub_avg_cov_park,
                          shrub_avg_cov_site,
                          treeden_ha_coun,
                          treeden_ha_park,
                          treeden_ha_site
                          ) %>% 
            mutate(#AOU_code = sps_loop2,
                   park = substr(Point_Name,1,4)) %>% 
            distinct()

write_rds(X_corr, file = "data/X_corr.rds")

dim(X_corr)
(corr_mat <- round(cor(X_corr[,c(2:16)], use="complete.obs"),2))

#? plot different ones here if needed
#? reduce the size of correlation matrix and plot
melted_corr_mat <- melt(corr_mat) 

melted_corr_mat  %>%  mutate(cor = abs(value)) %>% arrange(-cor)
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

# Correlation matrices for the different scales



X_corr_site <- X_corr %>% 
                  select(ends_with("_site")) %>% 
                  cor(., use="complete.obs") %>% 
                  round(.,2) %>% 
                  melt()

ggplot(data = X_corr_site, aes(x=Var1, y=Var2, 
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

X_corr_park <- X_corr %>% 
                  select(ends_with("_park")) %>% 
                  cor(., use="complete.obs") %>% 
                  round(.,2) %>% 
                  melt()

ggplot(data = X_corr_park, aes(x=Var1, y=Var2, 
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

#? COUNTY ------------------------------------------------------------------
nice_labs <- c(
  aBA_m2ha_coun = "Basal area",
  bBA_m2ha_Conifer_coun = "Conifer basal area",
  cBA_m2ha_large_coun = "Late succes.\nbasal area",
  dshrub_cov_coun = "Shrub cover",
  etreeden_ha_coun = "Tree density"
)
ord <- names(nice_labs)

# correlation matrix
X_corr_coun_num <- X_corr %>%
  dplyr::select(dplyr::ends_with("_coun")) %>%
  dplyr::select(where(is.numeric)) %>%
  distinct() %>%
  rename(aBA_m2ha_coun = BA_m2ha_coun,
         bBA_m2ha_Conifer_coun = BA_m2ha_Conifer_coun,
         cBA_m2ha_large_coun = BA_m2ha_large_coun,
         dshrub_cov_coun = shrub_cov_coun,
         etreeden_ha_coun = treeden_ha_coun) 

# full matrix (no lower/upper.tri masking)
corr_coun_mat <- X_corr_coun_num %>%
  cor(use = "pairwise.complete.obs", method = "pearson") %>%
  round(2)

X_corr_coun <- reshape2::melt(corr_coun_mat, na.rm = TRUE) %>%
  dplyr::mutate(
    i = match(as.character(Var1), ord),
    j = match(as.character(Var2), ord)
  ) %>%
  dplyr::filter(i > j) %>%   # keep one triangle only
  dplyr::mutate(
    Var1 = factor(Var1, levels = ord),
    Var2 = factor(Var2, levels = rev(ord))
  )
  
ggplot(data = X_corr_coun, aes(x=Var1, y=Var2, 
								fill=value)) + 
    geom_tile(color = "white")+
    scale_fill_gradient2(low = "steelblue", high = "darkred", mid = "white", 
      midpoint = 0, limit = c(-1,1), space = "Lab", 
      name="Correlation \n") +
    theme(axis.title.x     = element_blank(),       # Change x axis title only
          axis.title.y     = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "white", color = NA),
          plot.background  = element_rect(fill = "white", color = NA),
          panel.border     = element_rect(color = "black", fill = NA, linewidth = 0.8),
          axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5)) +
    geom_text(aes(Var1, Var2, label = value), 
        color = "black", 
            size = 4) +
    scale_x_discrete(limits = rev(ord), labels = nice_labs, drop = FALSE) +
    scale_y_discrete(limits = (ord), labels = nice_labs[rev(ord)], drop = FALSE)    

#-------------------------------------------------------------------------

X_corr2 <- X10 %>% 
            filter(interval_n == 1) %>% 
            dplyr::select(Point_Name,
                          BA_m2ha_perc_con_site,
                          BA_m2ha_perc_con_park,
                          BA_m2ha_perc_con_site,
                          BA_m2ha_coun,
                          BA_m2ha_park,
                          BA_m2ha_site,
                          BA_m2ha_large_coun,
                          BA_m2ha_large_park,
                          BA_m2ha_large_site,
                          shrub_cov_coun,
                          shrub_avg_cov_park,
                          shrub_avg_cov_site,
                          treeden_ha_coun,
                          treeden_ha_park,
                          treeden_ha_site
                          ) %>% 
            mutate(#AOU_code = sps_loop2,
                   park = substr(Point_Name,1,4))

dim(X_corr2)
(corr_mat2 <- round(cor(X_corr2[,c(2:15)], use="complete.obs"),2))

#? plot different ones here if needed
#? reduce the size of correlation matrix and plot
melted_corr_mat2 <- melt(corr_mat2) 

# plotting the correlation heatmap
ggplot(data = melted_corr_mat2, aes(x=Var1, y=Var2, 
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


# creating correlation matrices

#? first, check the correlation of the mature, pole, and late forest types 
#?  within the same scale, and between the forest types
(corr_mat_pole <- round(cor(X_corr[,c(6,7,8)], use="complete.obs"),2))
(corr_mat_matu <- round(cor(X_corr[,c(14,15,16)], use="complete.obs"),2))
(corr_mat_late <- round(cor(X_corr[,c(22,23,24)], use="complete.obs"),2))

#! ALL: definetely cannot be added together. 
#! GCFL_b0yes_late: definetely cannot be added together. 
# Note that large is strongly negatively correlated with pole and mature
# mature and pole are not bad except for park level

#? check between each variables, the correlation between the SCALES: 0.75 thres
(corr_mat_DEN <- round(cor(X_corr[,c(2,10,18)], use="complete.obs"),2))

(corr_mat_BA <- round(cor(X_corr[,c(3,11,19)], use="complete.obs"),2))

(corr_mat_H_g <- round(cor(X_corr[,c(4,12,20)], use="complete.obs"),2))

(corr_mat_Eh_g <- round(cor(X_corr[,c(5,13,21)], use="complete.obs"),2))

(corr_mat_BApole <- round(cor(X_corr[,c(6,14,22)], use="complete.obs"),2))

(corr_mat_BAmatu <- round(cor(X_corr[,c(7,15,23)], use="complete.obs"),2))

(corr_mat_BAlate <- round(cor(X_corr[,c(8,16,24)], use="complete.obs"),2))

(corr_mat_SHRU <- round(cor(X_corr[,c(9,17,25)], use="complete.obs"),2))

#? plot different ones here if needed
#? reduce the size of correlation matrix and plot
melted_corr_mat <- melt(corr_mat_SHRU) %>% 
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
  filter(abs(value) < 0.75) %>% 
  arrange(desc(value))

#! all good for GCFL with 0.75

#! GCFL_b0yes_late: definetely cannot be added together. 
# Note that large is strongly negatively correlated with pole and mature
# mature and pole are not bad except for park level

#? check between each variables, the correlation between the VARIABLES at same scale: 0.5 thres
(corr_mat_SITE <- round(cor(X_corr[,c(2:9)], use="complete.obs"),2))

(corr_mat_PARK <- round(cor(X_corr[,c(10:17)], use="complete.obs"),2))

(corr_mat_COUN <- round(cor(X_corr[,c(18:25)], use="complete.obs"),2))

#? reduce the size of correlation matrix and plot
melted_corr_mat <- melt(corr_mat_PARK) %>% 
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

#! ALL county rm EH_g, forest age BA, TRY H_g

#! GCFL site good with 0.5! on the forest age BA are cor
#! GCFL park has shrub, BApole, BA, DEN
(corr_mat_PARK <- round(cor(X_corr[,c(10:11,14,17)], use="complete.obs"),2))
#! GCFL county has shrub(0.72 with DEN), Eh_g, BA, DEN
(corr_mat_COUN <- round(cor(X_corr[,c(18:19,21,25)], use="complete.obs"),2))

cbind(colnames(X_corr), seq(1,ncol(X_corr),1))

# variation
var_site <- X_corr[,c(#1,29,
                    2,3,4,9)] 

  table(is.na(var_site))

var_site2 <- X_corr[,c(1,29,
                    2,3,4,9)]  %>% 
    pivot_longer(cols = c("siteDEN", "siteBA", "siteH_g", "siteSHRUden"),
                names_to = "site",
                values_to = "value",
                values_drop_na = TRUE)

ggplot(var_site2) +
  geom_boxplot(aes(x = site, y = value)) +
  theme_bw() +
  facet_wrap(~park, scales = "free_y", ncol = 1) +
  coord_flip()

## compare scales ------------------------------------------------------
X1000 <- read_rds(file = "data/X_1000.rds")
X500 <- read_rds(file = "data/X_500.rds")
X1000for <- read_rds(file = "data/X_1000nei.rds")

X_corr <- X10 %>% 
            filter(interval_n == 1) %>% 
            dplyr::select(Point_Name,
                siteDEN, siteBA,
                siteH_g, siteEh_g,
                siteBA_pole, siteBA_mature, siteBA_large,
                siteSHRUden,
                parkDEN, parkBA, 
                parkH_g, parkEh_g,
                parkBA_pole, parkBA_mature, parkBA_large,   
                parkSHRUden, 
                counDEN, counBA, 
                counH_g, counEh_g, ## https://rdrr.io/cran/rFIA/man/diversity.html
                counPER_pole, counPER_matu, counPER_late,
                counSHRUden,
                area,
                EventDate2, StartTime2) %>% 
            rename(date_jul = EventDate2,
                time_jul = StartTime2) %>% 
            mutate(#AOU_code = sps_loop2,
                    park = substr(Point_Name,1,4))
