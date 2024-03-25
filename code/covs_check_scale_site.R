library(ggplot2)
library("GGally")
library(reshape)
library(tidyverse)
library(glue)
library(ggpubr)
library(lme4)
library(lmerTest)
library(psych)

park_name <- c("ACAD", "ELRO", "HOFR", "MABI", "MIMA", "MORR", "SAGA", "SAIR", "SARA", "VAMA", "WEFA")
park_name <- park_name[-1]
nsite_pk <- read_rds(file = "data/out/nsite_pk.rds")
nsite_pk <- nsite_pk[-1]
# 11 parks, 7 years, 5 buffers (100, 250, 500, 750, 1000), 59 total sites (varies by park)
# forest area
arr_area_site1 <- read_rds(file = glue("data/out/arr_area_site.rds"))

# number of core areas
arr_core_site1 <- read_rds(file = glue("data/out/arr_core_site.rds"))

# cluster index
arr_clus_site1 <- read_rds(file = glue("data/out/arr_clu_site.rds"))



# filter only the 3 parks declining
park_name <- c("ACAD", "MABI", "MORR")
nsite_pk <- nsite_pk[c(1,4,6)]

arr_area_site1 <- arr_area_site1[c(1,4,6),,,]

arr_core_site1 <- arr_core_site1[c(1,4,6),,,]

arr_clus_site1 <- arr_clus_site1[c(1,4,6),,,]

# seven 7 is the last year of data for the environmental variables
recent_dat <- dim(arr_area_site1)[2]
arr_area_site_b <- arr_area_site1[,recent_dat,,] %>% AHMbook::standardize()
arr_core_site_b <- arr_core_site1[,recent_dat,,] %>% AHMbook::standardize()
arr_clus_site_b <- arr_clus_site1[,recent_dat,,]

# divide by area
arr_area_site <- arr_area_site1[,recent_dat,,] 
arr_core_site <- arr_core_site1[,recent_dat,,] 
arr_clus_site <- arr_clus_site1[,recent_dat,,]

arr_area_site_b <- arr_area_site
arr_core_site_b <- arr_core_site

a1 <- (pi * (100)^2)/10000  # area in he divided by radius in meters
a2 <- (pi * (250)^2)/10000
a3 <- (pi * (500)^2)/10000
a4 <- (pi * (750)^2)/10000
a5 <- (pi * (1000)^2)/10000

for(i in 1:(dim(arr_area_site)[3])){
  arr_area_site_b1 <- arr_area_site[,,i]
  arr_core_site_b1 <- arr_core_site[,,i]
  
  for(j in 1:(dim(arr_area_site)[1])) {
    arr_area_site_b1[j,] <-  arr_area_site_b1[j,]/c(a1, a2, a3, a4, a5)
    arr_core_site_b1[j,] <-  arr_core_site_b1[j,]/c(a1, a2, a3, a4, a5)
  }
  
  arr_area_site_b[,,i] <- arr_area_site_b1
  arr_core_site_b[,,i] <- arr_core_site_b1
}

arr_area_site_b <- arr_area_site_b %>% AHMbook::standardize()
arr_core_site_b <- arr_core_site_b %>% AHMbook::standardize()

# back to formating
colnames(arr_area_site) <- colnames(arr_clus_site) <- 
  colnames(arr_core_site) <- c(1,2,3,4,5)

df_area_sdF <- matrix(NA, ncol = 4, nrow = 0)
colnames(df_area_sdF) <- c("buf","site","value","park" )
df_area_sdF <- as_tibble(df_area_sdF) %>% 
  mutate(site = as.numeric(site)) 
df_area_sdT <- df_core_sdF <- df_core_sdT <- df_clus_sdF <- df_area_sdF

plots_covs_site <- function(var, ar_sd) {
  
  for(p in 1:length(park_name)){
    
    ### area T -------------
    if (var == "area" & ar_sd == "T") {
      limits <- c(floor(min(arr_area_site_b, na.rm = T)),
                  ceiling(max(arr_area_site_b, na.rm = T)))
      
      df <- arr_area_site_b[p,,1:nsite_pk[p]] %>% 
        as_tibble() %>% 
        mutate(buf = seq(1,5,1))
      df <- pivot_longer(df,
                         cols = 1:nsite_pk[p],
                         names_to = c("site"),
                         values_to = "value")
      
      df <- df %>% 
        mutate(value = round(value, 2)) %>% 
        mutate(site = as.numeric(site)) %>% 
        mutate(park = park_name[p])
      
      df_area_sdT <<- rbind(df_area_sdT, df)
      
    } 
    
    ### area F -------------
    if (var == "area" & ar_sd == "F") {
      limits <- c(floor(min(arr_area_site, na.rm = T)), ceiling(max(arr_area_site, na.rm = T)))
      
      df <- arr_area_site[p,,1:nsite_pk[p]] %>% 
        as_tibble() %>% 
        mutate(buf = seq(1,5,1))
      df <- pivot_longer(df,
                         cols = 1:nsite_pk[p],
                         names_to = c("site"),
                         values_to = "value")
      
      df <- df %>% 
        mutate(value = round(value, 2)) %>% 
        mutate(park = park_name[p]) %>% 
        mutate(site = as.numeric(site))
      
      df_area_sdF <<- rbind(df_area_sdF, df)
    }
    
    ### core T -------------
    if (var == "core" & ar_sd == "T") {
      limits <- c(floor(min(arr_core_site_b, na.rm = T)), ceiling(max(arr_core_site_b, na.rm = T)))
      
      df <- arr_core_site_b[p,,1:nsite_pk[p]] %>% 
        as_tibble() %>% 
        mutate(buf = seq(1,5,1))
      df <- pivot_longer(df,
                         cols = 1:nsite_pk[p],
                         names_to = c("site"),
                         values_to = "value")
      
      df <- df %>% 
        mutate(value = round(value, 2)) %>% 
        mutate(park = park_name[p])%>% 
        mutate(site = as.numeric(site))
      
      df_core_sdT <<- rbind(df_core_sdT, df)
      
    }
    
    ### core F -------------
    if (var == "core" & ar_sd == "F") {
      limits <- c(floor(min(arr_core_site, na.rm = T)), ceiling(max(arr_core_site, na.rm = T)))
      
      df <- arr_core_site[p,,1:nsite_pk[p]] %>% 
        as_tibble() %>% 
        mutate(buf = seq(1,5,1))
      df <- pivot_longer(df,
                         cols = 1:nsite_pk[p],
                         names_to = c("site"),
                         values_to = "value")
      
      df <- df %>% 
        mutate(value = round(value, 2)) %>% 
        mutate(park = park_name[p])%>% 
        mutate(site = as.numeric(site))
      
      df_core_sdF <<- rbind(df_core_sdF, df)
      
    }
    
    ### clustering F -------------
    if (var == "clus" & ar_sd == "F") {
      limits <- c(floor(min(arr_clus_site, na.rm = T)), ceiling(max(arr_clus_site[is.finite(arr_clus_site)], na.rm = T)))
      
      df <- arr_clus_site[p,,1:nsite_pk[p]] %>% 
        as_tibble() %>% 
        mutate(buf = seq(1,5,1))
      df <- pivot_longer(df,
                         cols = 1:nsite_pk[p],
                         names_to = c("site"),
                         values_to = "value") %>% 
        mutate(park = park_name[p])%>% 
        mutate(site = as.numeric(site))
      
      df_clus_sdF <<- rbind(df_clus_sdF, df)
    }
    
    plot1 <- ggplot(df, aes(x = buf, y = site, fill = value)) +
      geom_tile(color = "black") +
      geom_text(aes(label = value), color = "black", size = 2) +
      theme_bw() +
      #theme(legend.position = "none") +
      xlab("Buffer scale") +
      ylab("Site number") +
      scale_fill_gradientn(colors = hcl.colors(20, "RdYlGn"), limits=limits) +
      ggtitle(glue("{park_name[p]}")) +
      theme(plot.title = element_text(size=10, hjust = 0.5),
            axis.title = element_text(size = 9))
    
    plot2 <- ggplot(df, aes(x = buf, y = value, color = site)) +
      geom_point() +
      geom_smooth(aes(y = value, x = buf, group = site),
                  method = "lm", se = F) +
      theme_bw() +
      theme(legend.position = "none") +
      xlab("Buffer scale") +
      ylab("Forest cover by site") +
      ggtitle(glue("{park_name[p]}")) +
      ylim(limits) +
      theme(plot.title = element_text(size=10, hjust = 0.5))
    
    assign(glue("{park_name[p]}_plt1_{var}_sd{ar_sd}"), plot1, envir = parent.frame())
    assign(glue("{park_name[p]}_plt2_{var}_sd{ar_sd}"), plot2, envir = parent.frame())
    
  }
}

plots_covs_site(var = "area", ar_sd = "T")	
plots_covs_site(var = "core", ar_sd = "T")	
plots_covs_site(var = "clus", ar_sd = "F")	

## area divided by buffer --------
(plt1_area_sdT <- 
  ggarrange(ncol = 6,
    nrow = 2,
    ACAD_plt1_area_sdT,
    ELRO_plt1_area_sdT,
    HOFR_plt1_area_sdT,
    MABI_plt1_area_sdT,
    MIMA_plt1_area_sdT, ##
    MORR_plt1_area_sdT,
    SAGA_plt1_area_sdT,
    SAIR_plt1_area_sdT, ## 
    SARA_plt1_area_sdT, ##
    VAMA_plt1_area_sdT,
    WEFA_plt1_area_sdT,
    legend = "none"))
write_rds(plt1_area_sdT, file = "figures/buf_covs/plt1_area_sdT.rds")

plt2_area_sdT <- 
  ggarrange(ncol = 6,
    nrow = 2,
    ACAD_plt2_area_sdT,
    ELRO_plt2_area_sdT,
    HOFR_plt2_area_sdT,
    MABI_plt2_area_sdT,
    MIMA_plt2_area_sdT, ##
    MORR_plt2_area_sdT,
    SAGA_plt2_area_sdT,
    SAIR_plt2_area_sdT, ## 
    SARA_plt2_area_sdT, ##
    VAMA_plt2_area_sdT,
    WEFA_plt2_area_sdT,
    legend = "none")
write_rds(plt2_area_sdT, file = "figures/buf_covs/plt2_area_sdT.rds")
plt2_area_sdT

## core divided by buffer --------
(plt1_core_sdT <- 
  ggarrange(ncol = 6,
    nrow = 2,
    ACAD_plt1_core_sdT,
    ELRO_plt1_core_sdT,
    HOFR_plt1_core_sdT,
    MABI_plt1_core_sdT,
    MIMA_plt1_core_sdT, ##
    MORR_plt1_core_sdT,
    SAGA_plt1_core_sdT,
    SAIR_plt1_core_sdT, ## 
    SARA_plt1_core_sdT, ##
    VAMA_plt1_core_sdT,
    WEFA_plt1_core_sdT,
    legend = "none"))
write_rds(plt1_core_sdT, file = "figures/buf_covs/plt1_core_sdT.rds")

(plt2_core_sdT <- 
  ggarrange(ncol = 6,
    nrow = 2,
    ACAD_plt2_core_sdT,
    ELRO_plt2_core_sdT,
    HOFR_plt2_core_sdT,
    MABI_plt2_core_sdT,
    MIMA_plt2_core_sdT, ##
    MORR_plt2_core_sdT,
    SAGA_plt2_core_sdT,
    SAIR_plt2_core_sdT, ## 
    SARA_plt2_core_sdT, ##
    VAMA_plt2_core_sdT,
    WEFA_plt2_core_sdT,
    legend = "none"))
write_rds(plt2_core_sdT, file = "figures/buf_covs/plt2_core_sdT.rds")

## clus divided by buffer --------
(plt1_clus_sdF <- 
   ggarrange(ncol = 6,
             nrow = 2,
             ACAD_plt1_clus_sdF,
             ELRO_plt1_clus_sdF,
             HOFR_plt1_clus_sdF,
             MABI_plt1_clus_sdF,
             MIMA_plt1_clus_sdF, ##
             MORR_plt1_clus_sdF,
             SAGA_plt1_clus_sdF,
             SAIR_plt1_clus_sdF, ## 
             SARA_plt1_clus_sdF, ##
             VAMA_plt1_clus_sdF,
             WEFA_plt1_clus_sdF,
             legend = "none"))
write_rds(plt1_clus_sdF, file = "figures/buf_covs/plt1_clus_sdF.rds")

(plt2_clus_sdF <- 
    ggarrange(ncol = 6,
              nrow = 2,
              ACAD_plt2_clus_sdF,
              ELRO_plt2_clus_sdF,
              HOFR_plt2_clus_sdF,
              MABI_plt2_clus_sdF,
              MIMA_plt2_clus_sdF, ##
              MORR_plt2_clus_sdF,
              SAGA_plt2_clus_sdF,
              SAIR_plt2_clus_sdF, ## 
              SARA_plt2_clus_sdF, ##
              VAMA_plt2_clus_sdF,
              WEFA_plt2_clus_sdF,
              legend = "none"))
write_rds(plt2_clus_sdF, file = "figures/buf_covs/plt2_clus_sdF.rds")





# Correlation between variables ------------------
# for each variable, site and buffer extent (cell in plot), how correlated are they in the same park?
df_sdT <- left_join(df_area_sdT %>% dplyr::rename(area = value),
                    df_core_sdT %>% dplyr::rename(core = value),
                    by = c("buf", "site", "park")) %>% 
  relocate("buf", "site", "park", "area", "core")

ggpairs(df_sdT[,4:5]) + theme_bw()



## park level

### area
arr_area_park <- read_rds(file = "data/out/arr_area_park.rds")
arr_clus_park <- read_rds(file = "data/out/arr_clu_park.rds")
arr_core_park <- read_rds(file = "data/out/arr_core_park.rds")
arr_cai_park <- read_rds(file = "data/out/arr_cai_park.rds")
arr_coh_park <- read_rds(file = "data/out/arr_coh_park.rds")
# arr_enn_park <- read_rds(file = "data/out/arr_enn_park.rds")
arr_ai_park <- read_rds(file = "data/out/arr_ai_park.rds")
arr_pafrac_park <- read_rds(file = "data/out/arr_pafrac_park.rds")
arr_nlsi_park <- read_rds(file = "data/out/arr_nlsi_park.rds")
# arr_iji_park <- read_rds(file = "data/out/arr_iji_park.rds")
arr_con_park <- read_rds(file = "data/out/arr_con_park.rds")
arr_tca_park <- read_rds(file = "data/out/arr_tca_park.rds")
arr_te_park <- read_rds(file = "data/out/arr_te_park.rds")
arr_np_park <- read_rds(file = "data/out/arr_np_park.rds")






# last year of data for the environmental variables (recent_dat)
# c(1,4,6) are the parks that are declining, standardize MABI, ACAD and MORR ONLY between themselves, ignore other parks data
recent_dat <- dim(arr_area_park)[2]

arr_area_park1 <- arr_area_park[,recent_dat,] 
arr_core_park1 <- arr_core_park[,recent_dat,] 
arr_clus_park1 <- arr_clus_park[,recent_dat,]
arr_cai_park1 <- arr_cai_park[,recent_dat,]
arr_coh_park1 <- arr_coh_park[,recent_dat,]
arr_ai_park1 <- arr_ai_park[,recent_dat,]
arr_pafrac_park1 <- arr_pafrac_park[,recent_dat,]
arr_nlsi_park1 <- arr_nlsi_park[,recent_dat,]
arr_con_park1 <- arr_con_park[,recent_dat,]
arr_tca_park1 <- arr_tca_park[,recent_dat,]
arr_te_park1 <- arr_te_park[,recent_dat,]
arr_np_park1 <- arr_np_park[,recent_dat,]

arr_area_park2 <- arr_area_park1 %>% AHMbook::standardize()
arr_core_park2 <- arr_core_park1 %>% AHMbook::standardize()
arr_clus_park2 <- arr_clus_park1
arr_cai_park2 <- arr_cai_park1
arr_coh_park2 <- arr_coh_park1
arr_ai_park2 <- arr_ai_park1
arr_pafrac_park2 <- arr_pafrac_park1
arr_nlsi_park2 <- arr_nlsi_park1
arr_con_park2 <- arr_con_park1
arr_tca_park2 <- arr_tca_park1
arr_te_park2 <- arr_te_park1
arr_np_park2 <- arr_np_park1

# back to formating
colnames(arr_area_park2) <- 
  colnames(arr_clus_park2) <- 
  colnames(arr_core_park2) <- 
  colnames(arr_cai_park2) <-
  colnames(arr_coh_park2) <-
  colnames(arr_ai_park2) <-
  colnames(arr_pafrac_park2) <-
  colnames(arr_nlsi_park2) <-
  colnames(arr_con_park2) <-
  colnames(arr_tca_park2) <-
  colnames(arr_te_park2) <-
  colnames(arr_np_park2) <- c(1,2,3,4,5)

df_sdT <- matrix(NA, ncol = 3, nrow = 0)
colnames(df_sdT) <- c("park","buf","value" )
df_sdT <- as_tibble(df_sdT) 


plots_covs_park <- function(var) {
  
  if (var == "area") {arr_park2 <- arr_area_park2}
  if (var == "core") {arr_park2 <- arr_core_park2}
  if (var == "clus") {arr_park2 <- arr_clus_park2}
  if (var == "cai") {arr_park2 <- arr_cai_park2}
  if (var == "coh") {arr_park2 <- arr_coh_park2}
  if (var == "ai") {arr_park2 <- arr_ai_park2}
  if (var == "pafrac") {arr_park2 <- arr_pafrac_park2}
  if (var == "nlsi") {arr_park2 <- arr_nlsi_park2}
  if (var == "con") {arr_park2 <- arr_con_park2}
  if (var == "tca") {arr_park2 <- arr_tca_park2}
  if (var == "te") {arr_park2 <- arr_te_park2}
  if (var == "np") {arr_park2 <- arr_np_park2}
  
    
    limits <- c(floor(min(arr_park2, na.rm = T)),
                ceiling(max(arr_park2, na.rm = T)))
    
    arr_park2 <- as_tibble(arr_park2)
    
    arr_park2$park <- park_name
    
    df <- arr_park2 %>% 
      as_tibble() %>% 
      pivot_longer(!park, values_to = "value", names_to = "buf") %>% 
      mutate(buf = as.numeric(buf),
             value = round(value, 2))
    
    df_sdT <<- rbind(df_sdT, df)
    
    plot1 <- ggplot(df, aes(x = buf, y = park, fill = value)) +
      geom_tile(color = "black") +
      geom_text(aes(label = value), color = "black", size = 2) +
      theme_bw() +
      #theme(legend.position = "none") +
      xlab("Buffer scale") +
      scale_fill_gradientn(colors = hcl.colors(20, "RdYlGn")#, limits=limits
                           ) +
      ggtitle(glue("{var}")) +
      theme(plot.title = element_text(size=10, hjust = 0.5),
            axis.title = element_text(size = 9))
    
    plot2 <- ggplot(df, aes(x = buf, y = value, color = park)) +
      geom_point() +
      geom_smooth(aes(y = value, x = buf, group = park),
                  method = "lm", se = F) +
      theme_bw() +
      theme(legend.position = "none") +
      xlab("Buffer scale") +
      ylab("Forest cover by park") +
      ggtitle(glue("{var}")) +
      ylim(limits) +
      theme(plot.title = element_text(size=10, hjust = 0.5))
    
    assign(glue("plt1_{var}"), plot1, envir = parent.frame())
    assign(glue("plt2_{var}"), plot2, envir = parent.frame())
    
}

plots_covs_park(var = "area") ; plt1_area
plots_covs_park(var = "core") ; plt1_core
plots_covs_park(var = "clus") ; plt1_clus
plots_covs_park(var = "cai") ; plt1_cai
plots_covs_park(var = "coh") ; plt1_coh
plots_covs_park(var = "ai") ; plt1_ai
plots_covs_park(var = "pafrac") ; plt1_pafrac
plots_covs_park(var = "nlsi") ; plt1_nlsi
plots_covs_park(var = "con") ; plt1_con
plots_covs_park(var = "tca") ; plt1_tca
plots_covs_park(var = "te") ; plt1_te
plots_covs_park(var = "np") ; plt1_np


######## # add 2000 -----------------------------
arr_area_site1 <- arr_area_park #read_rds(file = glue("data/park_raster/arr_area_park_3.rds"))

arr_area_site1 <- arr_area_site

dim(arr_area_site1)

arr_area_site_b <- arr_area_site1[-1,,] 
dim(arr_area_site_b)
arr_area_site_b <- as.data.frame(arr_area_site_b)

arr_area_site_c <- arr_area_site_b %>% scale() %>% as_tibble()

plots_covs_site <- function(var, ar_sd) {
  

    ### area T -------------
      limits <- c(floor(min(arr_area_site_c, na.rm = T)),
                  ceiling(max(arr_area_site_c, na.rm = T)))
      
      df <- arr_area_site_c %>% 
        mutate(park = parks)
      
      colnames(df) <- c("100", "1000", "2000", "park")
      
      df <- pivot_longer(df,
                         cols = 1:ncol(arr_area_site_c),
                         names_to = c("buf"),
                         values_to = "value")
      
      df <- df %>% 
        mutate(value = round(value, 2)) %>% 
        mutate(buf = as.numeric(buf))
    
   
    plot1 <- ggplot(df, aes(x = buf, y = park, fill = value)) +
      geom_tile(color = "black") +
      geom_text(aes(label = value), color = "black", size = 2) +
      theme_bw() +
      #theme(legend.position = "none") +
      xlab("Buffer scale") +
      scale_fill_gradientn(colors = hcl.colors(20, "RdYlGn"), limits=limits) +
      #ggtitle(glue("{park_name[p]}")) +
      theme(plot.title = element_text(size=10, hjust = 0.5),
            axis.title = element_text(size = 9))
    
    plot2 <- ggplot(df, aes(x = buf, y = value, color = park)) +
      geom_point() +
      geom_smooth(aes(y = value, x = buf, group = park),
                  method = "lm", se = F) +
      theme_bw() +
      theme(legend.position = "none") +
      xlab("Buffer scale") +
      ylab("Forest cover by site") +
      ylim(limits) +
      theme(plot.title = element_text(size=10, hjust = 0.5))
    
    #assign(glue("{park_name[p]}_plt1_{var}_sd{ar_sd}"), plot1, envir = parent.frame())
    #assign(glue("{park_name[p]}_plt2_{var}_sd{ar_sd}"), plot2, envir = parent.frame())
    
   return(plot1)
}

plots_covs_site(var = "area", ar_sd = "T")	

parks <- parks[-1]
nsite_pk <- nsite_pk[-1]


arr_area_site1 <- arr_area_site

dim(arr_area_site1)

arr_area_site_b <- adrop(arr_area_site1[,1,,,drop=FALSE],drop=2)

dim(arr_area_site_b)

arr_area_site_b <- arr_area_site_b[-1,,]

arr_area_site_b <- arr_area_site_b %>% AHMbook::standardize()

df_area_sdT <- matrix(NA, ncol = 4, nrow = 0)
colnames(df_area_sdT) <- c("buf","site","value","park" )
df_area_sdT <- as_tibble(df_area_sdT) %>% 
  mutate(site = as.numeric(site)) 

plots_covs_site <- function(var, ar_sd) {
  
  
  for(p in 1:length(parks)){
    
    ### area T -------------
      limits <- c(floor(min(arr_area_site_b, na.rm = T)),
                  ceiling(max(arr_area_site_b, na.rm = T)))
      
      df <- arr_area_site_b[p,,1:nsite_pk[p]] %>% 
        as_tibble() %>% 
        mutate(buf = seq(1,3,1))
      df <- pivot_longer(df,
                         cols = 1:nsite_pk[p],
                         names_to = c("site"),
                         values_to = "value")
      
      df <- df %>% 
        mutate(value = round(value, 2)) %>% 
        mutate(site = as.numeric(site)) %>% 
        mutate(park = parks[p])
      
      df_area_sdT <- rbind(df_area_sdT, df)
      
    } 
    
  
  plot1 <- ggplot(df_area_sdT, aes(x = buf, y = site, fill = value)) +
    geom_tile(color = "black") +
    geom_text(aes(label = value), color = "black", size = 2) +
    theme_bw() +
    #theme(legend.position = "none") +
    xlab("Buffer scale") +
    scale_fill_gradientn(colors = hcl.colors(20, "RdYlGn"), limits=limits) +
    #ggtitle(glue("{park_name[p]}")) +
    theme(plot.title = element_text(size=10, hjust = 0.5),
          axis.title = element_text(size = 9)) +
    facet_wrap(~park)
  
  plot2 <- ggplot(df, aes(x = buf, y = value, color = park)) +
    geom_point() +
    geom_smooth(aes(y = value, x = buf, group = park),
                method = "lm", se = F) +
    theme_bw() +
    theme(legend.position = "none") +
    xlab("Buffer scale") +
    ylab("Forest cover by site") +
    ylim(limits) +
    theme(plot.title = element_text(size=10, hjust = 0.5))
  
  #assign(glue("{park_name[p]}_plt1_{var}_sd{ar_sd}"), plot1, envir = parent.frame())
  #assign(glue("{park_name[p]}_plt2_{var}_sd{ar_sd}"), plot2, envir = parent.frame())
  
  return(plot1)
}

plots_covs_site(var = "area", ar_sd = "T")	



