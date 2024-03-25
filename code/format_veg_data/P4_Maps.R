# *****************************************
# -----------      P4_Maps      -----------
# *****************************************
# Script to 
#
# input:    - :
#           - :
# output:   - :
#           - :


# .rs.restartR()
detach()
rm(list = ls(all.names = TRUE))

library(tidyverse)
library(glue)
library(raster)


parks <- readRDS(file = "data/src/key_park.rds") %>% 
  dplyr::select(parks) %>% 
  distinct() %>% 
  pull()

parks <- sort(parks)

parks <- parks[-1]

years <- c(2004, 2006, 2008, 2011, 2013, 2016, 2019)

ext_b <- "park"

# Choose buffer extends and if the buffer is around the park area or the sites
# ext_b <- "park"    ;     ext_b <- "site"
if (ext_b == "site") {
  buffers <- c(50, 150, 300)  ## distance is in meters!!!
}

if (ext_b == "park") {
  buffers <- c(100, 500, 1000, 2000)  ## distance is in meters!!!
}

buffers_n <- gsub("\\+", "", as.character(buffers))

plot_park_map <- function(i,   # park
                          j){  # year
  int2 <- read_rds(file = glue("data/park_raster/{parks[i]}/{parks[i]}{years[j]}_land_buf1000_park_int2.rds"))
  int3 <- read_rds(file = glue("data/park_raster/{parks[i]}/{parks[i]}{years[j]}_land_buf2000_park.rds"))
  int3[int3 < 41] <- 0 # make everything below 41 zero
  int3[int3 > 43] <- 0 # make everything above 43 zero
  int3[int3 == 41 | int3 == 42 | int3 == 43] <- 1
  
  psit_sf <- read_rds(file = glue("data/park_raster/{parks[i]}/{parks[i]}_site.rds")) 
  pb <- read_rds(file = glue("data/park_raster/{parks[i]}/{parks[i]}_pb.rds"))
  buf1 <- read_rds(file = glue("data/park_raster/{parks[i]}/{parks[i]}_buf100.rds"))
  #buf2 <- read_rds(file = glue("data/park_raster/{parks[i]}/{parks[i]}_buf250.rds"))
  buf3 <- read_rds(file = glue("data/park_raster/{parks[i]}/{parks[i]}_buf500.rds"))
  #buf4 <- read_rds(file = glue("data/park_raster/{parks[i]}/{parks[i]}_buf750.rds"))
  buf5 <- read_rds(file = glue("data/park_raster/{parks[i]}/{parks[i]}_buf1000.rds"))
  buf6 <- read_rds(file = glue("data/park_raster/{parks[i]}/{parks[i]}_buf2000.rds"))
  
  int_poly <- rasterToPolygons(int2)
  
  plot(int3, legend=FALSE, main = glue("{parks[i]} map"))
  plot(int2, legend=FALSE, add = T)
  plot(pb, add = T, lwd=3)
  psit_sf %>% plot(add = T, cex = 0.7, pch = 16, col = "blue")
  
  plot(buf1, add = T, border = "red")
  #plot(buf2,  add = T, border = "red")
  plot(buf3,  add = T, border = "red")
  #plot(buf4,  add = T, border = "red")
  plot(buf5,  add = T, border = "red")
  plot(buf6,  add = T, border = "red")
  
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
#plot_park_map(11,7)


# forest area
arr_var <- read_rds(file = glue("data/out/arr_area_park.rds"))
arr_var <- read_rds(file = glue("data/out/arr_clu_park.rds"))
arr_var <- read_rds(file = glue("data/out/arr_core_park.rds"))



# last year of data for the environmental variables (recent_dat)
# c(1,4,6) are the parks that are declining, standardize MABI, ACAD and MORR ONLY between themselves, ignore other parks data
recent_dat <- dim(arr_var)[2]

arr_var1 <- arr_var[,recent_dat,] 

arr_var2 <- arr_var1 %>% 
  #AHMbook::standardize() %>% 
  as_tibble() %>% 
  mutate(park = parks)

plt_var <- pivot_longer(arr_var2, 
                         !park,
                         names_to = "buf", 
                         values_to = "var") %>% 
  mutate(buf = as.numeric(buf))

plt_var %>% 
  ggplot() +
  geom_line(aes(x = buf, y = var, col = park)) +
  geom_point(aes(x = buf, y = var, col = park)) +
  geom_hline(yintercept=0, linetype="dashed", 
             color = "black", size=1) +
  facet_wrap(~park#, scales = "free"
             ) +
  theme_bw() +
  theme(legend.position = NULL) 

## plot correlations
  
# ggplot attempt of map
r.spdf <- as(int2, "SpatialPixelsDataFrame")
r.df <- as.data.frame(r.spdf) %>% fortify()

colors <- c("white", "springgreen3")

ggplot(r.df, aes(x=x, y=y)) + 
  #geom_tile(aes(fill = factor(Layer_1))) + 
  scale_fill_manual(values=colors) +
  coord_equal() +
  geom_polygon(data = pb, 
               aes(long, lat, group=factor(group)), 
               colour='black', 
               fill= NA) +
  theme_bw() 

ggplot() +
  geom_stars(sf_as_st(MABI_buf1000) )















