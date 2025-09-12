## ask kate


shrub <- joinMicroShrubData() %>%  
            as_tibble() %>% 
            filter(Shrub == 1) %>%         #should I include vines?
            filter(is.na(shrub_avg_cov)) 