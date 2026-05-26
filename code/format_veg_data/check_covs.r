library(reshape2)
library(tidyverse)
library(ggbiplot)
library(glue)
library(DT)
# library(psych)
# library(AHMbook)
# library(ggplot2)

comb <- read_rds(file = "data/out/for_plot_covs.rds")



# creating correlation matrix
corr_mat <- round(cor(comb[,c(6:18, 20:29)], use="complete.obs"),2)

# reduce the size of correlation matrix
melted_corr_mat <- melt(corr_mat) %>%
  # Convert factor levels to numeric for comparison
  mutate(
    Var1_num = as.numeric(Var1),
    Var2_num = as.numeric(Var2)
  ) %>%
  # Keep only upper triangle (excluding diagonal)
  filter(Var1_num < Var2_num) %>%
  select(-Var1_num, -Var2_num)

# plotting the correlation heatmap
ggplot(data = melted_corr_mat %>% as_tibble() %>% arrange(Var1,Var2) #%>% filter(abs(value) <= 0.5)
      ,aes(x=Var1, y=Var2, fill=value)) + 
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


melted_corr_mat %>% 
  filter(abs(value) > 0.5) %>% 
  arrange(desc(value))

# PCA 

site_data_full <- comb[,c(2, 6:16, 18:27)] %>%
  filter(complete.cases(.))

site_data_full1 <- site_data_full  %>% select(-ParkUnit)

site.pca <- prcomp(site_data_full1, center = TRUE, scale. = TRUE)

ggbiplot(site.pca, groups = site_data_full$ParkUnit, ellipse = TRUE) +
  scale_color_discrete(name = "ParkUnit") +
  theme_bw()
  
summary(site.pca)

site.pca$rotation



### MORR BA conifer is all zero

close_points_f2 <- read_rds(file = glue("data/out/neighbor_fornofor_{radi_dist}m.rds"))

close_points_f2 %>% mutate(park = substr(bird_sit, 1, 4)) %>% filter(park == "MORR") %>% rename(for_sit = for_plt) %>% 
                    left_join(., for_plots_sf, by = "for_sit") %>% 
                    datatable()

neighbor %>% datatable()
