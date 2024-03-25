library(tidyverse)
library(glue)
library(abind)
library(corrplot)
rm(list = ls(all.names = TRUE))

# load files --------------------------
# keys
park_name <- c("ACAD", "ELRO", "HOFR", "MABI", "MIMA", "MORR", "SAGA", "SAIR", "SARA", "VAMA", "WEFA")
#park_name <- park_name[-1]
nsite_pk <- read_rds(file = "data/out/nsite_pk.rds")
#nsite_pk <- nsite_pk[-1]

years <- c(2004, 2006, 2008, 2011, 2013, 2016, 2019)

vars <- c("cai","coh","enn","ai",'pafrac',"nlsi",#"iji",
          'con','tca','te',"np","clu",'core',"area","areap")

# function to make plots of the variables at the park and site levels ------------
plots_covs_site <- function(varia,       # cai, coh, area, core ....
                            ext_b,       # park, site
                            ACAD)      { # is acadia included? T or F
  
  if (ext_b == "site") {
    buffers <- c(50, 150) #, 300)  ## distance is in meters!!!
  }
  
  if (ext_b == "park") {
    buffers <- c(100, 500, 1000, 2000)  ## distance is in meters!!!
  }
  
  buffers_n <- gsub("\\+", "", as.character(buffers))
  data_name <- glue("arr_{varia}_{ext_b}")
  
  arr_imp <- read_rds(file = glue("data/park_raster/{data_name}.rds"))
  
  assign("arr", arr_imp)
  
  # park level -------------------------  
  if(ext_b == "park") {
    
    # seven 7 is the last year of data for the environmental variables
    arr2 <- drop(arr)
    
    colnames(arr2) <- buffers_n
    
    arr2 <- arr2 %>% 
      as_tibble()  %>% 
      mutate(park = row.names(arr2))
    
    if(ACAD == F) {
      arr2 <- arr2 %>% 
        filter(park != "ACAD")
      }
    
    arr3 <- arr2
    
    if(varia != "clus" & varia != "areap") {  # cluster is an index
      arr3[,1:length(buffers)] <- scale(arr3[,1:length(buffers)]) %>% as.data.frame()
    }
    
    limits <- c(floor(min(arr3[,1:length(buffers)], na.rm = T)),
                ceiling(max(arr3[,1:length(buffers)], na.rm = T)))
    
    arr4 <- pivot_longer(arr3,
                       cols = 1:length(buffers),
                       names_to = c("buffer"),
                       values_to = "value")
    arr5 <- arr4[complete.cases(arr4),]
    arr5 <- arr5 %>% 
      mutate(buffer = factor(buffer, levels = buffers_n))
  
    plot1 <- 
      ggplot(arr5, aes(x = buffer, y = park, fill = value)) +
      geom_tile(color = "black") +
      geom_text(aes(label = round(value,2)), color = "black", size = 2) +
      theme_bw() +
      #theme(legend.position = "none") +
      xlab("Buffer scale") +
      ylab("Park") +
      scale_fill_gradientn(colors = hcl.colors(20, "RdYlGn"), limits=limits) +
      ggtitle(glue("{varia}")) +
      theme(plot.title = element_text(size=10, hjust = 0.5),
            axis.title = element_text(size = 9))
    
    plot2 <- ggplot(arr5, aes(x = buffer, y = value, color = park)) +
      geom_point() +
      geom_smooth(aes(y = value, x = buffer, group = park),
                  method = "lm", se = F) +
      theme_bw() +
      #theme(legend.position = "none") +
      xlab("Buffer scale") +
      ylab(glue("{varia}")) +
      ggtitle(glue("{varia}")) +
      coord_cartesian(ylim = limits) +
      theme(plot.title = element_text(size=10, hjust = 0.5)) +
      scale_color_viridis_d(option = "plasma")  
    
    name_p <- glue("{varia}_park_bufs")
    
    assign(glue("{name_p}_tab_out"), arr5, envir = .GlobalEnv)
    assign(glue("{name_p}_plt1_out"), plot1, envir = .GlobalEnv)
    assign(glue("{name_p}_plt2_out"), plot2, envir = .GlobalEnv)
  
  }
  
  # site level ---------------------
  
  if(ext_b == "site") {
    
    # seven 7 is the last year of data for the environmental variables
    # keep only year 7 (dim two) - get rid of dim 2
    arr2 <- drop(arr)
    
    arr2 <- abind(arr2, array(NA, replace(dim(arr2), 2, 1)), along = 2)
    
    colnames(arr2) <- c(buffers_n,"park")
    
    ### differences between parks (mean of sites across parks) ---------
    arr3bp <- apply(arr2, c(1,2), mean, na.rm = T)
    rnam <- row.names(arr3bp)
    arr3bp <- as_tibble(arr3bp)
    arr3bp$park <- rnam
    
    if(ACAD == F) {
      arr3bp <- arr3bp %>% 
        filter(park != "ACAD")
    }
    
    arr4bp <- pivot_longer(arr3bp,
                           cols = 1:length(buffers),
                           names_to = c("buffer"),
                           values_to = "value")
    
    arr5bp <- arr4bp[complete.cases(arr4bp),]
    arr5bp <- arr5bp %>% 
      mutate(buffer = factor(buffer, levels = buffers_n))
    
    if(varia != "clus" & varia != "areap") {  # cluster is an index, not scale!
      arr5bp <- arr5bp %>% 
        mutate(value = scale(value))
    }
    
    limits <- c(floor(min(arr5bp$value, na.rm = T)),
                ceiling(max(arr5bp$value, na.rm = T)))
    
    plot1 <- 
      ggplot(arr5bp, aes(x = buffer, y = park, fill = value)) +
      geom_tile(color = "black") +
      geom_text(aes(label = round(value,2)), color = "black", size = 2) +
      theme_bw() +
      #theme(legend.position = "none") +
      xlab("Buffer scale") +
      ylab("Park") +
      scale_fill_gradientn(colors = hcl.colors(20, "RdYlGn"), limits=limits) +
      ggtitle(glue("average {varia} across sites")) +
      theme(plot.title = element_text(size=10, hjust = 0.5),
            axis.title = element_text(size = 9))
    
    plot2 <- 
      ggplot(arr5bp, aes(x = buffer, y = value, color = park)) +
      geom_point() +
      geom_smooth(aes(y = value, x = buffer, group = park),
                  method = "lm", se = F) +
      theme_bw() +
      theme(legend.position = "none") +
      xlab("Buffer scale") +
      ylab(glue("{varia}")) +
      ggtitle(glue("average {varia} across sites")) +
      coord_cartesian(ylim = limits) +
      theme(plot.title = element_text(size=10, hjust = 0.5)) +
      scale_color_viridis_d(option = "plasma")  
    
    name_p <- glue("{varia}_sites_bufs")
    
    assign(glue("{name_p}_tab_out"), arr5bp, envir = .GlobalEnv)
    assign(glue("{name_p}_plt1_out"), plot1, envir = .GlobalEnv)
    assign(glue("{name_p}_plt2_out"), plot2, envir = .GlobalEnv)
    
    ## differences within parks (variation between sites) -------------
    # one plot per buffer size
    
    for(ii in 1:(ncol(arr2)-1)){
      
      arr_l1 <- arr2[,ii,]
      
      rnam <- row.names(arr_l1)
      
      arr_l2 <- arr_l1 %>% 
        as_tibble() %>% 
        mutate(park = rnam)
      
      if(ACAD == F) {
        arr_l2 <- arr_l2 %>% 
          filter(park != "ACAD")
      }
      arr_l3 <- arr_l2
      
      # remove colums (sites) with all NA - in the case of removing Acadia from analisys
      arr_l4 <- arr_l3[,colSums(is.na(arr_l3)) != ncol(arr_l3)]
      
      arr_l5 <- pivot_longer(arr_l4,
                             cols = 1:(ncol(arr_l4)-1),
                             names_to = c("sites"),
                             values_to = "value") %>% 
        mutate(sites = as.numeric(sites))
      
      arr_l6 <- arr_l5[complete.cases(arr_l5),]

      if(varia != "clus" & varia != "areap") {  # cluster is an index, not scale!
        arr_l6 <- arr_l6 %>% 
          mutate(value = scale(value))
      }
      
      limits <- c(floor(min(arr_l6$value, na.rm = T)),
                  ceiling(max(arr_l6$value, na.rm = T)))
      
      name_p <- glue("{varia}_sitesall_{buffers[ii]}buf")
      
      assign(glue("{name_p}_tab_out"), arr_l6, envir = .GlobalEnv)
      
      plot3 <- 
        ggplot(arr_l6, aes(x = sites, y = park, fill = value)) +
        geom_tile(color = "black") +
        geom_text(aes(label = round(value,2)), color = "black", size = 2, angle = 90) +
        theme_bw() +
        #theme(legend.position = "none") +
        xlab("sites") +
        ylab("Park") +
        scale_fill_gradientn(colors = hcl.colors(20, "RdYlGn"), limits=limits) +
        ggtitle(glue("{name_p}")) +
        theme(plot.title = element_text(size=10, hjust = 0.5),
              axis.title = element_text(size = 9))
      
      assign(glue("{name_p}_out"), plot3, envir = .GlobalEnv)
    }
    
    # this plot makes sense across sites within the same park
    arr_l_all <- get(glue("{varia}_sitesall_{buffers[1]}buf_tab_out")) %>% 
      mutate(buffer = buffers[1])
    
    for(ii in 2:(ncol(arr2)-1)){
      bind <- get(glue("{varia}_sitesall_{buffers[ii]}buf_tab_out")) %>% 
        mutate(buffer = buffers[ii])
      
      arr_l_all <- rbind(arr_l_all, bind)
      
    }
  
    limits <- c(floor(min(arr_l_all$value, na.rm = T)),
                ceiling(max(arr_l_all$value, na.rm = T)))
    
    arr_l_all2 <- arr_l_all %>% 
      mutate(park_buf = glue("{park}_{buffer}"))
    
    name_p <- glue("{varia}_sitesalline_bufs")
    
    plot1 <- 
      ggplot(arr_l_all2, aes(x = sites, y = park_buf, fill = value)) +
      geom_tile(color = "black") +
      geom_text(aes(label = round(value,2)), color = "black", size = 2, angle = 90) +
      theme_bw() +
      #theme(legend.position = "none") +
      xlab("sites") +
      ylab("Park") +
      scale_fill_gradientn(colors = hcl.colors(20, "RdYlGn"), limits=limits) +
      ggtitle(glue("{varia}_{ext_b}_all")) +
      theme(plot.title = element_text(size=10, hjust = 0.5),
            axis.title = element_text(size = 9))
    
    plot2 <- 
      ggplot(arr_l_all2, aes(x = buffer, y = value, color = park)) +
      geom_point() +
      geom_smooth(aes(y = value, x = buffer), #color = park,
                  method = "lm", se = F) +
      theme_bw() +
      theme(legend.position = "none",
            strip.background=element_rect(colour="black",
                                           fill="#BF87B3")) +
      xlab("Buffer") +
      ylab(glue("{varia}")) +
      ggtitle(glue("{varia} all sites")) +
      coord_cartesian(ylim = limits) +
      theme(plot.title = element_text(size=10, hjust = 0.5)) +
      facet_wrap(~park) +
      scale_color_viridis_d(option = "plasma")    

    assign(glue("{name_p}_tab_out"), arr_l_all2, envir = .GlobalEnv)
    assign(glue("{name_p}_plt1_out"), plot1, envir = .GlobalEnv)
    assign(glue("{name_p}_plt2_out"), plot2, envir = .GlobalEnv)
  }
}

# function to make plots of the variables at the park and site levels ------------
plots_covs_site(varia = "areap",       # cai, coh, area, core ....
                ext_b = "park",        # park, site
                ACAD = T)              # is acadia included? T or F

areap_park_bufs_plt1_out
areap_park_bufs_plt2_out
areap_park_bufs_tab_out

# same but across the sites
plots_covs_site(varia = "areap",       # cai, coh, area, core ....
                ext_b = "site",        # park, site
                ACAD = T)              # is acadia included? T or F

areap_sites_bufs_plt1_out
areap_sites_bufs_plt2_out
areap_sites_bufs_tab_out

areap_sitesall_150buf_out
areap_sitesall_50buf_out
areap_sitesall_150buf_tab_out
areap_sitesall_50buf_tab_out

areap_sitesalline_bufs_plt1_out
areap_sitesalline_bufs_plt2_out
areap_sitesalline_bufs_tab_out

# focus on variation between parks for now
for (kk in 1:length(vars)){
  
  plots_covs_site(varia = vars[kk],       # cai, coh, area, core ....
                  ext_b = "park",       # park, site
                  ACAD = T)  
  
}

# view all the plots:
## check core vs ncore
core_park_bufs_plt1_out
core_park_bufs_plt2_out
plots_covs_site(varia = "core",       # cai, coh, area, core ....
                ext_b = "park",       # park, site
                ACAD = F)  

np_park_bufs_plt1_out
np_park_bufs_plt2_out
plots_covs_site(varia = "np",       # cai, coh, area, core ....
                ext_b = "park",       # park, site
                ACAD = F)  


con_park_bufs_plt1_out
con_park_bufs_plt2_out


ai_park_bufs_plt1_out
ai_park_bufs_plt2_out
# correlation between the buffer levels within the same variables
# CORRELATED! 
for (kk in 1:length(vars)){
  
  tab <- get(glue("{vars[kk]}_park_bufs_tab_out"))
  
  tab1 <- tab %>% pivot_wider(names_from = buffer, values_from = value)
  
  name1 <- glue("wide_{vars[kk]}_parks")

  assign(name1,tab1)
  
  colour_set <- colorRampPalette(colors = c("#f4ff4d", "#c7d123", "#acb515", "#81890b", "#656c06"))
  tab1_cor <- cor(tab1[,-1])
  
  corrplot(tab1_cor, tl.col = "black", bg = "gray", tl.srt = 35, 
           title = glue("n\n\n\n\n{name1}"),
           addCoef.col = "black", type = "lower",
           col = colour_set(100))
}

# variation of the variables within the same buffer level between parks
for (kk in 1:length(vars)){
  print(get(glue("{vars[kk]}_park_bufs_plt1_out")))
}

# correlation for variables between parks within the same buffer level

for(kk in 1:length(vars)){
  tabx2 <- get(glue("wide_{vars[kk]}_parks"))
  
  if (ext_b == "site") {
    buffers <- c(50, 150) #, 300)  ## distance is in meters!!!
  }
  
  if (ext_b == "park") {
    buffers <- c(100, 500, 1000, 2000)  ## distance is in meters!!!
  }
  
  for(ii in 1:(length(buffers))){
    tab_ll <- tabx2[,c(1,ii+1)]
    name_ll <- glue({"{vars[kk]}"})
    
    name_tab <- glue("buff{buffers[ii]}")
    
    if(kk == 1) {
      colnames(tab_ll)[2] <- name_ll
      tab_fim <- tab_ll
    }
    
    if(kk > 1){
      colnames(tab_ll)[2] <- name_ll
      tab_fim <- left_join(get(name_tab), tab_ll)
    }
    assign(name_tab, tab_fim)
  }
  
}

buffers_ah <- c(100, 500, 1000, 2000)
for (kk in 1:length(buffers_ah)){
  
  name1 <- glue("buff{buffers_ah[kk]}")
  
  tab1 <- get(name1)
  
  colour_set <- colorRampPalette(colors = c("#f4ff4d", "#c7d123", "#acb515", "#81890b", "#656c06"))
  tab1_cor <- cor(tab1[,-1],  use = "complete.obs")
  
  corrplot(tab1_cor, tl.col = "black", bg = "gray", tl.srt = 15, 
                   title = glue("n\n\n\n\n{name1}"),
                   addCoef.col = "black", type = "lower",
                   col = colour_set(100),
                   number.cex=0.75)
  
  #assign(glue("{name1}_plt"), crpl)
}
buff100_plt
buff500_plt
buff1000_plt
buff2000_plt

# identify variables that would benefit from removing acadia/ correcting for area
# area
# core
# np
# te
# tca

# SITE
# variation of the variables at the site scale - too small - local for variables
vars_site <- vars[-c(5,11)]
for (kk in 1:length(vars_site)){
  
  plots_covs_site(varia = vars_site[kk],       # cai, coh, area, core ....
                  ext_b = "site",       # park, site
                  ACAD = T)  
  
}
## between buffers - not only the variation is small, but a lot of the metrics cannot be properly calculated
for (kk in 1:length(vars_site)){
  
  print(get(glue("{vars_site[kk]}_sitesalline_bufs_plt1_out")))
  print(get(glue("{vars_site[kk]}_sitesalline_bufs_plt2_out")))
  
}


# plot the maps for forest area ------------------
# plot the buffers for each site with their values to compare

# Choose buffer extends and if the buffer is around the park area or the sites
# ext_b <- "park"    ;     ext_b <- "site"


plot_park_map <- function(i,      # park
                          j){     # year
  
  buffers_s <- c(50, 150) #, 300)  ## distance is in meters!!!
  buffers_p <- c(100, 500, 1000, 2000)  ## distance is in meters!!!
  
  buffers_ns <- gsub("\\+", "", as.character(buffers_s))
  buffers_np <- gsub("\\+", "", as.character(buffers_p))
  
  int2 <- read_rds(file = glue("data/park_raster/{park_name[i]}/{park_name[i]}{years[j]}_land_buf1000_park_int2.rds"))
  int3 <- read_rds(file = glue("data/park_raster/{park_name[i]}/{park_name[i]}{years[j]}_land_buf2000_park_int2.rds"))

  psit_sf <- read_rds(file = glue("data/park_raster/{park_name[i]}/{park_name[i]}_site.rds")) 
<<<<<<< HEAD
  pb <- read_rds(file = glue("data/park_raster/{park_name[i]}_pb.rds"))
=======
  pb <- read_rds(file = glue("data/park_raster/{park_name[i]}/{park_name[i]}_pb.rds"))
>>>>>>> parent of 51ca3bf (organizing folder structure)
  buf1 <- read_rds(file = glue("data/park_raster/{park_name[i]}/{park_name[i]}_buf100.rds"))
  #buf2 <- read_rds(file = glue("data/park_raster/{park_name[i]}/{park_name[i]}_buf250.rds"))
  buf3 <- read_rds(file = glue("data/park_raster/{park_name[i]}/{park_name[i]}_buf500.rds"))
  #buf4 <- read_rds(file = glue("data/park_raster/{park_name[i]}/{parks[i]}_buf750.rds"))
  buf5 <- read_rds(file = glue("data/park_raster/{park_name[i]}/{park_name[i]}_buf1000.rds"))
  buf6 <- read_rds(file = glue("data/park_raster/{park_name[i]}/{park_name[i]}_buf2000.rds"))
  
  int_poly <- rasterToPolygons(int2)
  
  if(park_name[i] != "ACAD" & park_name[i] != "SARA"){
    plot(int3, legend=FALSE, main = glue("{park_name[i]} map"))
  }
  
  if(park_name[i] == "ACAD"){
    plot(int3, legend=FALSE, main = glue("{park_name[i]} map"),
         xlim = c(-68.45, -68), ylim = c(42.95, 44.45))
  }
  
  if(park_name[i] == "SARA"){
    plot(int3, legend=FALSE, main = glue("{park_name[i]} map"),
         xlim = c(-73.67, -73.57), ylim = c(42.91, 43.04))
  }
  
  #plot(int2, legend=FALSE, add = T)
  plot(pb, add = T, lwd=3)
<<<<<<< HEAD
  psit_sf %>% plot(add = T, cex = 3, pch = 16, col = "purple")
=======
  psit_sf %>% plot(add = T, cex = 0.5, pch = 16, col = "blue")
>>>>>>> parent of 51ca3bf (organizing folder structure)
  
  plot(buf1, add = T, border = "red")
  #plot(buf2,  add = T, border = "red")
  plot(buf3,  add = T, border = "red")
  #plot(buf4,  add = T, border = "red")
  plot(buf5,  add = T, border = "red")
  plot(buf6,  add = T, border = "red")
  
  for(ii in 1:nsite_pk[i]){
    for(jj in 1:length(buffers_ns)){
      
      s2 <- as.character(ii)
      if(nchar(s2) < 2){s2 <- glue("0{s2}")}
      buf_s <- read_rds(file = glue("data/park_raster/{park_name[i]}/{park_name[i]}_buf{buffers_ns[jj]}_site{s2}.rds"))
      plot(buf_s,  add = T, border = "red")
      
    }
  }
}

plot_park_map(1,7)
plot_park_map(2,7)
plot_park_map(3,7)
plot_park_map(4,7)
plot_park_map(5,7)
plot_park_map(6,7)
plot_park_map(7,7)
plot_park_map(8,7)
plot_park_map(9,7)
plot_park_map(10,7)

# have a table of which species were detected in which park
library(coda)
ncovs <- 1
for_int <- "T"          # forest interior birds

source("code/create_data_files.R")

yyy4 <- apply(yyy3, c(1,2,3,4), sum, na.rm = TRUE)

custom_mean_gt_zero <- function(x) {
  mean(x > 0, na.rm = TRUE)
}

# Apply the custom function to columns 1, 2, and 3
yyy5 <- apply(yyy4, c(1,2,3), custom_mean_gt_zero)
dim(yyy5)

library(reshape2)
df <- melt(yyy5) 
colnames(df) <- c("AOU_Code", "Admin_Unit_Code", "site_n", "occn")

acad <- 
ggplot(df %>% filter(Admin_Unit_Code == "ACAD", occn > 0), 
       aes(y = as.factor(site_n), x = AOU_Code, fill = occn)) +
  geom_tile(color = "black") +
  #geom_text(aes(label = round(occn,2)), color = "black", size = 2, angle = 90) +
  theme_bw() +
  #theme(legend.position = "none") +
  ylab("sites") +
  xlab("Species") +
  scale_fill_viridis_c(option = "plasma") +  
  ggtitle("Naive occ prob in ACAD") +
  theme(plot.title = element_text(size=10, hjust = 0.5),
        axis.title = element_text(size = 9),
        axis.text.x = element_text( size = 7, angle = 90),
        axis.text.y = element_text( size = 5)) +
  labs(fill = "Occ Prob \n")
acad

mabi <- 
  ggplot(df %>% filter(Admin_Unit_Code == "MABI", occn > 0), 
         aes(y = as.factor(site_n), x = AOU_Code, fill = occn)) +
  geom_tile(color = "black") +
  #geom_text(aes(label = round(occn,2)), color = "black", size = 2, angle = 90) +
  theme_bw() +
  #theme(legend.position = "none") +
  ylab("sites") +
  xlab("Species") +
  scale_fill_viridis_c(option = "plasma") +  
  ggtitle("Naive occ prob in MABI") +
  theme(plot.title = element_text(size=10, hjust = 0.5),
        axis.title = element_text(size = 9),
        axis.text.x = element_text( size = 7, angle = 90),
        axis.text.y = element_text( size = 5)) +
  labs(fill = "Occ Prob \n")
mabi

elro <- 
  ggplot(df %>% filter(Admin_Unit_Code == "ELRO", occn > 0), 
         aes(y = as.factor(site_n), x = AOU_Code, fill = occn)) +
  geom_tile(color = "black") +
  #geom_text(aes(label = round(occn,2)), color = "black", size = 2, angle = 90) +
  theme_bw() +
  #theme(legend.position = "none") +
  ylab("sites") +
  xlab("Species") +
  scale_fill_viridis_c(option = "plasma") +  
  ggtitle("Naive occ prob in ELRO") +
  theme(plot.title = element_text(size=10, hjust = 0.5),
        axis.title = element_text(size = 9),
        axis.text.x = element_text( size = 7, angle = 90),
        axis.text.y = element_text( size = 5)) +
  labs(fill = "Occ Prob \n")
elro

hofr <- 
  ggplot(df %>% filter(Admin_Unit_Code == "HOFR", occn > 0), 
         aes(y = as.factor(site_n), x = AOU_Code, fill = occn)) +
  geom_tile(color = "black") +
  #geom_text(aes(label = round(occn,2)), color = "black", size = 2, angle = 90) +
  theme_bw() +
  #theme(legend.position = "none") +
  ylab("sites") +
  xlab("Species") +
  scale_fill_viridis_c(option = "plasma") +  
  ggtitle("Naive occ prob in HOFR") +
  theme(plot.title = element_text(size=10, hjust = 0.5),
        axis.title = element_text(size = 9),
        axis.text.x = element_text( size = 7, angle = 90),
        axis.text.y = element_text( size = 5)) +
  labs(fill = "Occ Prob \n")
hofr




for_int <- "F"          # all birds

source("code/2_format_data.R")

y3 <- 
  y1 %>% 
  select(AOU_Code, Admin_Unit_Code) %>% 
  table() %>% 
  as_tibble() %>% 
  filter(n > 0) 


# use that yyy3 table to make this - naive occ prob
a <- 
ggplot(y3, aes(x = AOU_Code, y = Admin_Unit_Code)) +
  geom_tile(fill = "#BF87B3") +
  theme_bw() +
  #theme(legend.position = "none") +
  ylab("Parks") +
  xlab("Species") +
  ggtitle("Species in each park") +
  theme(plot.title = element_text(size=10, hjust = 0.5),
        axis.title = element_text(size = 9),
        axis.text.x = element_text(angle = 90, size = 7))

a

# plot the number of sps per site and naive occ

# get vegetation data from inside the park and make some plots

# check correlations between all of these things

# get some humble non-correlated variables and put it in a simple model


# tree dataset
# look at it!

# vegetation data for inside of the park (check correlations and variation between parks (and sites?))

# compare inside and outside of parks! are the local forest variables correlated with the satellite ones?


### break species by park SITE - last plots, sps specific
#### it is HARD to compare diferent numbers of sites and visits with the numbers i have
#### parks still have a small sample size - eventually we need to add other parks to have a bigger sample size (all eatsren US)
#### point/site level, park, regional (FIA - county level)