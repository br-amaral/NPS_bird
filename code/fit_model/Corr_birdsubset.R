#? *********************************************************************************
#? -------------------------------   Amazing Title   -------------------------------
#? *********************************************************************************
#
#! Code to ...
#
#! Source ---------------------------------------------
#           - :
#           - :
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
library(reshape2)
library(ggplot2)
library(patchwork)  

# httpgd::hgd()
conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::filter)

#! Make functions --------------------------------------
colanmes <- colnames
lenght <- length
`%!in%` <- Negate(`%in%`)

#! Get working directory for figures --------------------------------------
if(substr(getwd(), 1, 3) == "/Us") {direc <- "local"} else {direc <- "hpc" ; httpgd::hgd_browse()}

#! Source code -----------------------------------------

#! Import data -----------------------------------------
## file paths
XDAT_PATH <- "data/X.rds"

## read files
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

axis_labels_size <- 22
title_size <- 36
corr_labels_size <- 7
legend_label_size <- 20
legend_title_size <- 25

#? Stand -------------------------------------------------------------------------------
nice_labs_site <- c(
  aBA_m2ha_site= "Basal area",
  bBA_m2ha_Conifer_site = "Conifer\nbasal area",
  cBA_m2ha_large_site = "Late succes.\nbasal area",
  dshrub_cov_site = "Shrub cover",
  etreeden_ha_site = "Tree density"
)
ord_site <- names(nice_labs_site)

# correlation matrix
X_corr_site_num <- X_corr %>%
  dplyr::select(dplyr::ends_with("_site")) %>%
  dplyr::select(where(is.numeric)) %>%
  distinct() %>%
  rename(aBA_m2ha_site = BA_m2ha_site,
         bBA_m2ha_Conifer_site = BA_m2ha_Conifer_site,
         cBA_m2ha_large_site = BA_m2ha_large_site,
         dshrub_cov_site = shrub_avg_cov_site,
         etreeden_ha_site = treeden_ha_site) 

# full matrix (no lower/upper.tri masking)
corr_site_mat <- X_corr_site_num %>%
  cor(use = "pairwise.complete.obs", method = "spearman") %>%
  round(2)

X_corr_site <- reshape2::melt(corr_site_mat, na.rm = TRUE) %>%
  dplyr::mutate(
    i = match(as.character(Var1), ord_site),
    j = match(as.character(Var2), ord_site)
  ) %>%
  dplyr::filter(i > j) %>%   # keep one triangle only
  dplyr::mutate(
    Var1 = factor(Var1, levels = ord_site),
    Var2 = factor(Var2, levels = rev(ord_site))
  )
  
p1 <- ggplot(data = X_corr_site, aes(x=Var1, y=Var2, 
								fill=value)) + 
    geom_tile(color = "white")+
    scale_fill_gradient2(low = "steelblue", high = "darkred", mid = "white", 
      midpoint = 0, limit = c(-1,1), space = "Lab", 
      name="Correlation\n(spearman)\n") +
    theme(axis.title.x     = element_blank(),       # Change x axis title only
          axis.title.y     = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "white", color = NA),
          plot.background  = element_rect(fill = "white", color = NA),
          panel.border     = element_rect(color = "black", fill = NA, linewidth = 0.8),
          axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5, size = axis_labels_size),
          axis.text.y = element_text(size = axis_labels_size),
          legend.title.align = 0.5,
          legend.title = element_text(size = legend_title_size, face = "bold"),
          legend.text  = element_text(size = legend_label_size),
          plot.title   = element_text(
            hjust    = 0.5,      # horizontally centered
            size     = title_size,       
            margin   = margin(t = 10, b = 10),
            vjust    = -8         # just inside the plot area
    )) +
    geom_text(aes(Var1, Var2, label = value), 
              color = "black", 
              size = corr_labels_size) +
    scale_x_discrete(limits = rev(ord_site), labels = nice_labs_site, drop = FALSE) +
    scale_y_discrete(limits = (ord_site), labels = nice_labs_site[rev(ord_site)], drop = FALSE) +
    labs(title = "Stand") +
    guides(color = guide_legend(title = "Correlation\n (spearman)"))

#? Park -------------------------------------------------------------------------------
nice_labs_park <- c(
  aBA_m2ha_park= "Basal area",
  bBA_m2ha_Conifer_park = "Conifer\nbasal area",
  cBA_m2ha_large_park = "Late succes.\nbasal area",
  dshrub_cov_park = "Shrub cover",
  etreeden_ha_park = "Tree density"
)
ord_park <- names(nice_labs_park)

# correlation matrix
X_corr_park_num <- X_corr %>%
  dplyr::select(dplyr::ends_with("_park")) %>%
  dplyr::select(where(is.numeric)) %>%
  distinct() %>%
  rename(aBA_m2ha_park = BA_m2ha_park,
         bBA_m2ha_Conifer_park = BA_m2ha_Conifer_park,
         cBA_m2ha_large_park = BA_m2ha_large_park,
         dshrub_cov_park = shrub_avg_cov_park,
         etreeden_ha_park = treeden_ha_park) 

# full matrix (no lower/upper.tri masking)
corr_park_mat <- X_corr_park_num %>%
  cor(use = "pairwise.complete.obs", method = "spearman") %>%
  round(2)

X_corr_park <- reshape2::melt(corr_park_mat, na.rm = TRUE) %>%
  dplyr::mutate(
    i = match(as.character(Var1), ord_park),
    j = match(as.character(Var2), ord_park)
  ) %>%
  dplyr::filter(i > j) %>%   # keep one triangle only
  dplyr::mutate(
    Var1 = factor(Var1, levels = ord_park),
    Var2 = factor(Var2, levels = rev(ord_park))
  )
  
p2 <- ggplot(data = X_corr_park, aes(x=Var1, y=Var2, 
						 fill=value)) + 
    geom_tile(color = "white")+
    scale_fill_gradient2(low = "steelblue", high = "darkred", mid = "white", 
                         midpoint = 0, limit = c(-1,1), space = "Lab", 
                         name="Correlation\n(spearman)\n") +
    theme(axis.title.x     = element_blank(),       # Change x axis title only
          axis.title.y     = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "white", color = NA),
          plot.background  = element_rect(fill = "white", color = NA),
          panel.border     = element_rect(color = "black", fill = NA, linewidth = 0.8),
          axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5, size = axis_labels_size),
          axis.text.y = element_text(size = axis_labels_size),
          legend.title.align = 0.5,
          legend.title = element_text(size = legend_title_size, face = "bold"),
          legend.text  = element_text(size = legend_label_size),
          plot.title   = element_text(
              hjust    = 0.5,      # horizontally centered
              size     = title_size,       
              margin   = margin(t = 10, b = 10),
              vjust    = -8         # just inside the plot area
    )) +
    geom_text(aes(Var1, Var2, label = value), 
              color = "black", 
              size = corr_labels_size) +
    scale_x_discrete(limits = rev(ord_park), labels = nice_labs_park, drop = FALSE) +
    scale_y_discrete(limits = (ord_park), labels = nice_labs_park[rev(ord_park)], drop = FALSE) +
    labs(title = "Park")

#? regional ------------------------------------------------------------------
nice_labs_coun <- c(
  aBA_m2ha_coun = "Basal area",
  bBA_m2ha_Conifer_coun = "Conifer\nbasal area",
  cBA_m2ha_large_coun = "Late succes.\nbasal area",
  dshrub_cov_coun = "Shrub cover",
  etreeden_ha_coun = "Tree density"
)
ord_coun <- names(nice_labs_coun)

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
  cor(use = "pairwise.complete.obs", method = "spearman") %>%
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
  
p3 <- ggplot(data = X_corr_coun, aes(x=Var1, y=Var2, 
								fill=value)) + 
    geom_tile(color = "white")+
    scale_fill_gradient2(low = "steelblue", high = "darkred", mid = "white", 
                         midpoint = 0, limit = c(-1,1), space = "Lab", 
                         name = "Correlation\n(spearman)\n") +
    theme(axis.title.x       = element_blank(),       # Change x axis title only
          axis.title.y       = element_blank(),
          panel.grid.major   = element_blank(),
          panel.grid.minor   = element_blank(),
          panel.background   = element_rect(fill = "white", color = NA),
          plot.background    = element_rect(fill = "white", color = NA),
          panel.border       = element_rect(color = "black", fill = NA, linewidth = 0.8),
          axis.text.x        = element_text(angle = 0, vjust = 0.5, hjust = 0.5, size = axis_labels_size),
          axis.text.y        = element_text(size = axis_labels_size),
          legend.title.align = 0.5,
          legend.title       = element_text(size = legend_title_size, face = "bold"),
          legend.text        = element_text(size = legend_label_size),
          # legend.key.height = unit(3.2, "cm"),   # Makes colorbar longer (default ~1cm)
          # legend.key.width  = unit(1, "cm"),  # Makes colorbar thicker (default ~0.5cm)
          # legend.justification = "center",    # Centers entire legend block
          # legend.box.just = "center",          # Centers legend box in space
          plot.title         = element_text(
              hjust          = 0.5,       # horizontally centered
              size           = title_size,       
              margin         = margin(t = 10, b = 10),
              vjust          = -8         # just inside the plot area
      )) +
    geom_text(aes(Var1, Var2, label = value), 
              color = "black", 
              size = corr_labels_size) +
    scale_x_discrete(limits = rev(ord_coun), labels = nice_labs_coun, drop = FALSE) +
    scale_y_discrete(limits = (ord_coun), labels = nice_labs_coun[rev(ord_coun)], drop = FALSE) +
    labs(title = "Regional") + 
    guides(fill = guide_colorbar(
           title.hjust = 0.5,              # Centers title exactly over bar
           title.vjust = 1,                 # Pulls title closer to bar
           barheight = unit(9, "cm"),       # Your larger size
           barwidth  = unit(1.5, "cm")
    ))

# put all figures together in one panel -------------------------------------------------------------------------
# Hide legend on first two, keep on third
p1_noleg <- p1 + theme(legend.position = "none")
p2_noleg <- p2 + theme(legend.position = "none", axis.text.y  = element_blank(), axis.ticks.y = element_blank())
p3_leg   <- p3 + theme(axis.text.y  = element_blank(), axis.ticks.y = element_blank())

# Arrange: 1 row, 3 columns; collect guides (auto-handles shared legend)
combined <- p1_noleg + p2_noleg + p3_leg + 
  plot_layout(ncol = 3, guides = "collect")

print(combined)

ggsave("figures/correlation.pdf", plot = combined, device = "pdf", width = 34, height = 11)

ggsave("figures/correlation.svg", plot = combined, device = "svg", width = 34, height = 11)

# Same covs accross scales -------------------------------------------------------------------------

axis_labels_size_covs <- 12
title_size_covs <- 14
corr_labels_size_covs <- 4
legend_label_size_covs <- 10
legend_title_size_covs <- 12

#? BA -------------------------------------------------------------------------------
nice_labs_BA <- c(
  aBA_m2ha_site= "Stand",
  bBA_m2ha_park = "Park",
  cBA_m2ha_coun = "Regional"
)
ord_BA <- names(nice_labs_BA)

# correlation matrix
X_corr_BA_num <- X_corr %>%
  dplyr::select(BA_m2ha_site, BA_m2ha_park, BA_m2ha_coun) %>%
  dplyr::select(where(is.numeric)) %>%
  distinct() %>%
  rename(aBA_m2ha_site = BA_m2ha_site,
         bBA_m2ha_park = BA_m2ha_park,
         cBA_m2ha_coun = BA_m2ha_coun,) 

# full matrix (no lower/upper.tri masking)
corr_BA_mat <- X_corr_BA_num %>%
  cor(use = "pairwise.complete.obs", method = "spearman") %>%
  round(2)

X_corr_BA <- reshape2::melt(corr_BA_mat, na.rm = TRUE) %>%
  dplyr::mutate(
    i = match(as.character(Var1), ord_BA),
    j = match(as.character(Var2), ord_BA)
  ) %>%
  dplyr::filter(i < j) %>%   # keep one triangle only
  dplyr::mutate(
    Var1 = factor(Var1, levels = ord_BA),
    Var2 = factor(Var2, levels = rev(ord_BA))
  )
  
pBA <- ggplot(data = X_corr_BA, aes(x=Var1, y=Var2, 
								fill=value)) + 
    geom_tile(color = "white")+
    scale_fill_gradient2(low = "steelblue", high = "darkred", mid = "white", 
      midpoint = 0, limit = c(-1,1), space = "Lab", 
      name="Correlation\n(spearman)\n") +
    theme(axis.title.x     = element_blank(),       # Change x axis title only
          axis.title.y     = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "white", color = NA),
          plot.background  = element_rect(fill = "white", color = NA),
          panel.border     = element_rect(color = "black", fill = NA, linewidth = 0.8),
          axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5, size = axis_labels_size_covs),
          axis.text.y = element_text(size = axis_labels_size_covs),
          legend.title.align = 0.5,
          legend.title = element_text(size = legend_title_size_covs, face = "bold"),
          legend.text  = element_text(size = legend_label_size_covs),
          plot.title   = element_text(
            hjust    = 0.5,      # horizontally centered
            size     = title_size_covs,       
            margin   = margin(t = 10, b = 10),
            vjust    = -13         # just inside the plot area
    )) +
    geom_text(aes(Var1, Var2, label = round(value,1)), 
              color = "black", 
              size = corr_labels_size_covs) +
    scale_y_discrete(limits = rev(ord_BA), labels = nice_labs_BA, drop = FALSE) +
    scale_x_discrete(limits = (ord_BA), labels = nice_labs_BA[rev(ord_BA)], drop = FALSE) +
    labs(title = "Tree Basal\nArea") +
    guides(color = guide_legend(title = "Correlation\n (spearman)"))

#? CON -------------------------------------------------------------------------------
nice_labs_CON <- c(
  aBA_m2ha_Conifer_site= "Stand",
  bBA_m2ha_Conifer_park = "Park",
  cBA_m2ha_Conifer_coun = "Regional"
)
ord_CON <- names(nice_labs_CON)

# correlation matrix
X_corr_CON_num <- X_corr %>%
  dplyr::select(BA_m2ha_Conifer_site, BA_m2ha_Conifer_park, BA_m2ha_Conifer_coun) %>%
  dplyr::select(where(is.numeric)) %>%
  distinct() %>%
  rename(aBA_m2ha_Conifer_site = BA_m2ha_Conifer_site,
         bBA_m2ha_Conifer_park = BA_m2ha_Conifer_park,
         cBA_m2ha_Conifer_coun = BA_m2ha_Conifer_coun,) 

# full matrix (no lower/upper.tri masking)
corr_CON_mat <- X_corr_CON_num %>%
  cor(use = "pairwise.complete.obs", method = "spearman") %>%
  round(2)

X_corr_CON <- reshape2::melt(corr_CON_mat, na.rm = TRUE) %>%
  dplyr::mutate(
    i = match(as.character(Var1), ord_CON),
    j = match(as.character(Var2), ord_CON)
  ) %>%
  dplyr::filter(i < j) %>%   # keep one triangle only
  dplyr::mutate(
    Var1 = factor(Var1, levels = ord_CON),
    Var2 = factor(Var2, levels = rev(ord_CON))
  )
  
pCON <- ggplot(data = X_corr_CON, aes(x=Var1, y=Var2, 
								fill=value)) + 
    geom_tile(color = "white")+
    scale_fill_gradient2(low = "steelblue", high = "darkred", mid = "white", 
      midpoint = 0, limit = c(-1,1), space = "Lab", 
      name="Correlation\n(spearman)\n") +
    theme(axis.title.x     = element_blank(),       # Change x axis title only
          axis.title.y     = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "white", color = NA),
          plot.background  = element_rect(fill = "white", color = NA),
          panel.border     = element_rect(color = "black", fill = NA, linewidth = 0.8),
          axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5, size = axis_labels_size_covs),
          axis.text.y = element_text(size = axis_labels_size_covs),
          legend.title.align = 0.5,
          legend.title = element_text(size = legend_title_size_covs, face = "bold"),
          legend.text  = element_text(size = legend_label_size_covs),
          plot.title   = element_text(
            hjust    = 0.5,      # horizontally centered
            size     = title_size_covs,       
            margin   = margin(t = 10, b = 10),
            vjust    = -13         # just inside the plot area
    )) +
    geom_text(aes(Var1, Var2, label = round(value,1)), 
              color = "black", 
              size = corr_labels_size_covs) +
    scale_y_discrete(limits = rev(ord_CON), labels = nice_labs_CON, drop = FALSE) +
    scale_x_discrete(limits = (ord_CON), labels = nice_labs_CON[rev(ord_CON)], drop = FALSE) +
    labs(title = "Coniferous Tree\nBasal Area") +
    guides(color = guide_legend(title = "Correlation\n (spearman)"))

#? LAT -------------------------------------------------------------------------------
nice_labs_LAT <- c(
  aBA_m2ha_large_site= "Stand",
  bBA_m2ha_large_park = "Park",
  cBA_m2ha_large_coun = "Regional"
)
ord_LAT <- names(nice_labs_LAT)

# correlation matrix
X_corr_LAT_num <- X_corr %>%
  dplyr::select(BA_m2ha_large_site, BA_m2ha_large_park, BA_m2ha_large_coun) %>%
  dplyr::select(where(is.numeric)) %>%
  distinct() %>%
  rename(aBA_m2ha_large_site = BA_m2ha_large_site,
         bBA_m2ha_large_park = BA_m2ha_large_park,
         cBA_m2ha_large_coun = BA_m2ha_large_coun,) 

# full matrix (no lower/upper.tri masking)
corr_LAT_mat <- X_corr_LAT_num %>%
  cor(use = "pairwise.complete.obs", method = "spearman") %>%
  round(2)

X_corr_LAT <- reshape2::melt(corr_LAT_mat, na.rm = TRUE) %>%
  dplyr::mutate(
    i = match(as.character(Var1), ord_LAT),
    j = match(as.character(Var2), ord_LAT)
  ) %>%
  dplyr::filter(i < j) %>%   # keep one triangle only
  dplyr::mutate(
    Var1 = factor(Var1, levels = ord_LAT),
    Var2 = factor(Var2, levels = rev(ord_LAT))
  )
  
pLAT <- ggplot(data = X_corr_LAT, aes(x=Var1, y=Var2, 
								fill=value)) + 
    geom_tile(color = "white")+
    scale_fill_gradient2(low = "steelblue", high = "darkred", mid = "white", 
      midpoint = 0, limit = c(-1,1), space = "Lab", 
      name="Correlation\n(spearman)\n") +
    theme(axis.title.x     = element_blank(),       # Change x axis title only
          axis.title.y     = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "white", color = NA),
          plot.background  = element_rect(fill = "white", color = NA),
          panel.border     = element_rect(color = "black", fill = NA, linewidth = 0.8),
          axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5, size = axis_labels_size_covs),
          axis.text.y = element_text(size = axis_labels_size_covs),
          legend.title.align = 0.5,
          legend.title = element_text(size = legend_title_size_covs, face = "bold"),
          legend.text  = element_text(size = legend_label_size_covs),
          plot.title   = element_text(
            hjust    = 0.5,      # horizontally centered
            size     = title_size_covs,       
            margin   = margin(t = 10, b = 10),
            vjust    = -13         # just inside the plot area
    )) +
    geom_text(aes(Var1, Var2, label = round(value,1)), 
              color = "black", 
              size = corr_labels_size_covs) +
    scale_y_discrete(limits = rev(ord_LAT), labels = nice_labs_LAT, drop = FALSE) +
    scale_x_discrete(limits = (ord_LAT), labels = nice_labs_LAT[rev(ord_LAT)], drop = FALSE) +
    labs(title = "Late Success.\nTree Basal Area") +
    guides(color = guide_legend(title = "Correlation\n (spearman)"))

#? SHR -------------------------------------------------------------------------------
nice_labs_SHR <- c(
  ashrub_avg_cov_site = "Stand",
  bshrub_avg_cov_park = "Park",
  cshrub_cov_coun = "Regional"
)
ord_SHR <- names(nice_labs_SHR)

# correlation matrix
X_corr_SHR_num <- X_corr %>%
  dplyr::select(shrub_avg_cov_site, shrub_avg_cov_park, shrub_cov_coun) %>%
  dplyr::select(where(is.numeric)) %>%
  distinct() %>%
  rename(ashrub_avg_cov_site = shrub_avg_cov_site,
         bshrub_avg_cov_park = shrub_avg_cov_park,
         cshrub_cov_coun = shrub_cov_coun,) 

# full matrix (no lower/upper.tri masking)
corr_SHR_mat <- X_corr_SHR_num %>%
  cor(use = "pairwise.complete.obs", method = "spearman") %>%
  round(2)

X_corr_SHR <- reshape2::melt(corr_SHR_mat, na.rm = TRUE) %>%
  dplyr::mutate(
    i = match(as.character(Var1), ord_SHR),
    j = match(as.character(Var2), ord_SHR)
  ) %>%
  dplyr::filter(i < j) %>%   # keep one triangle only
  dplyr::mutate(
    Var1 = factor(Var1, levels = ord_SHR),
    Var2 = factor(Var2, levels = rev(ord_SHR))
  )
  
pSHR <- ggplot(data = X_corr_SHR, aes(x=Var1, y=Var2, 
								fill=value)) + 
    geom_tile(color = "white")+
    scale_fill_gradient2(low = "steelblue", high = "darkred", mid = "white", 
      midpoint = 0, limit = c(-1,1), space = "Lab", 
      name="Correlation\n(spearman)\n") +
    theme(axis.title.x     = element_blank(),       # Change x axis title only
          axis.title.y     = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "white", color = NA),
          plot.background  = element_rect(fill = "white", color = NA),
          panel.border     = element_rect(color = "black", fill = NA, linewidth = 0.8),
          axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5, size = axis_labels_size_covs),
          axis.text.y = element_text(size = axis_labels_size_covs),
          legend.title.align = 0.5,
          legend.title = element_text(size = legend_title_size_covs, face = "bold"),
          legend.text  = element_text(size = legend_label_size_covs),
          plot.title   = element_text(
            hjust    = 0.5,      # horizontally centered
            size     = title_size_covs,       
            margin   = margin(t = 10, b = 10),
            vjust    = -1.5         # just inside the plot area
    )) +
    geom_text(aes(Var1, Var2, label = round(value,1)), 
              color = "black", 
              size = corr_labels_size_covs) +
    scale_y_discrete(limits = rev(ord_SHR), labels = nice_labs_SHR, drop = FALSE) +
    scale_x_discrete(limits = (ord_SHR), labels = nice_labs_SHR[rev(ord_SHR)], drop = FALSE) +
    labs(title = "Shrub Cover") +
    guides(color = guide_legend(title = "Correlation\n (spearman)"))

#? LAT -------------------------------------------------------------------------------
nice_labs_DEN <- c(
  atreeden_ha_site = "Stand",
  btreeden_ha_park = "Park",
  ctreeden_ha_coun = "Regional"
)
ord_DEN <- names(nice_labs_DEN)

# correlation matrix
X_corr_DEN_num <- X_corr %>%
  dplyr::select(treeden_ha_site, treeden_ha_park, treeden_ha_coun) %>%
  dplyr::select(where(is.numeric)) %>%
  distinct() %>%
  rename(atreeden_ha_site = treeden_ha_site,
         btreeden_ha_park = treeden_ha_park,
         ctreeden_ha_coun = treeden_ha_coun,) 

# full matrix (no lower/upper.tri masking)
corr_DEN_mat <- X_corr_DEN_num %>%
  cor(use = "pairwise.complete.obs", method = "spearman") %>%
  round(2)

X_corr_DEN <- reshape2::melt(corr_DEN_mat, na.rm = TRUE) %>%
  dplyr::mutate(
    i = match(as.character(Var1), ord_DEN),
    j = match(as.character(Var2), ord_DEN)
  ) %>%
  dplyr::filter(i < j) %>%   # keep one triangle only
  dplyr::mutate(
    Var1 = factor(Var1, levels = ord_DEN),
    Var2 = factor(Var2, levels = rev(ord_DEN))
  )
  
pDEN <- ggplot(data = X_corr_DEN, aes(x=Var1, y=Var2, 
								fill=value)) + 
    geom_tile(color = "white")+
    scale_fill_gradient2(low = "steelblue", high = "darkred", mid = "white", 
      midpoint = 0, limit = c(-1,1), space = "Lab", 
      name="Correlation\n(Spearman)\n") +
    theme(axis.title.x     = element_blank(),       # Change x axis title only
          axis.title.y     = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "white", color = NA),
          plot.background  = element_rect(fill = "white", color = NA),
          panel.border     = element_rect(color = "black", fill = NA, linewidth = 0.8),
          axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5, size = axis_labels_size_covs),
          axis.text.y = element_text(size = axis_labels_size_covs),
          legend.title.align = 0.5,
          legend.title = element_text(size = legend_title_size_covs, face = "bold"),
          legend.text  = element_text(size = legend_label_size_covs),
          plot.title   = element_text(
            hjust    = 0.5,      # horizontally centered
            size     = title_size_covs,       
            margin   = margin(t = 10, b = 10),
            vjust    = -8         # just inside the plot area
    )) +
    geom_text(aes(Var1, Var2, label = round(value,1)), 
              color = "black", 
              size = corr_labels_size_covs) +
    scale_y_discrete(limits = rev(ord_DEN), labels = nice_labs_DEN, drop = FALSE) +
    scale_x_discrete(limits = (ord_DEN), labels = nice_labs_DEN[rev(ord_DEN)], drop = FALSE) +
    labs(title = "Tree Density") +
    guides(color = guide_legend(title = "Correlation\n (Spearman)"))

# put all figures together in one panel -------------------------------------------------------------------------
# Hide legend on first two, keep on third
p1_cov <- pBA + theme(legend.position = "none", axis.text.x = element_blank())
p2_cov <- pCON + theme(legend.position = "none", axis.text.y = element_blank(),  axis.text.x = element_blank())
p3_cov   <- pLAT + theme(axis.text.x = element_blank(), legend.position = "none")
p4_cov <- pSHR + theme(legend.position = "none",  axis.text.y = element_blank())
p5_cov <- pDEN 

# Arrange: 1 row, 3 columns; collect guides (auto-handles shared legend)

combined2 <- p1_cov + p2_cov + p3_cov + p4_cov + p5_cov +
  plot_layout(
    ncol   = 2,
    guides = "collect")
print(combined2)

ggsave("figures/correlation2.pdf", plot = combined2, device = "pdf", width = 9, height = 11)
ggsave("figures/correlation2.svg", plot = combined2, device = "svg", width = 9, height = 11)

# END ---------
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

# Load X_corr from saved file instead of recreating
X_corr <- read_rds(file = "data/X_corr.rds")


