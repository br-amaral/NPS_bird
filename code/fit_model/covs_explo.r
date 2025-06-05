freshr::freshr()
library(reshape2)
library(tidyverse)

httpgd::hgd()

XDAT_PATH <- "data/X.rds"
X10 <- read_rds(file = XDAT_PATH)

X_res <- X10 %>% 
            filter(interval_n == 1) %>% 
            dplyr::select(park, Point_Name,
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
                area) %>% 
            distinct()

# write_rds(X_res, file = "data/X_res.rds")
# X_res <- read_rds(file = "data/X_res.rds")

X_shrub <- X_res  %>% 
    select(park, Point_Name,
            siteSHRUden, parkSHRUden, counSHRUden) 
            
X_shrub_park <- X_shrub %>% 
    select(park, Point_Name, siteSHRUden, parkSHRUden) %>% 
    arrange(parkSHRUden)

park_order <- X_shrub_park %>% arrange(parkSHRUden) %>% select(park) %>% distinct() %>% pull()
            
X_shrub_park <- X_shrub_park  %>% 
                    mutate(park = factor(park, levels = park_order))
ggplot(X_shrub_park) +
    #geom_point(aes(x = park, y = siteSHRUden), col = "black") +
    geom_point(aes(x = park, y = parkSHRUden, col = park)) +
    theme_bw()

    #geom_density(aes(y = siteSHRUden))

 X_shrub %>% 
    select(park, siteSHRUden)  %>% filter(park %in% c("VAMA", "HOFR"))  %>% distinct() %>% view()
