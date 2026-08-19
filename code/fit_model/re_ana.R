#? *********************************************************************************
#? ---------------------------   re_ana.R   ---------------------------
#? *********************************************************************************
#
#! Code to reanalize the data - either more its, of post hoc

#! Load packages ---------------------------------------
#library(conflicted)
freshr::freshr()
library(tidyverse)
library(glue)

#conflicts_prefer(dplyr::select)
#conflicts_prefer(dplyr::filter)
# conflicts_prefer(scales::alpha)

#! Make functions --------------------------------------
colanmes <- colnames
lenght <- length
`%!in%` <- Negate(`%in%`)

#! MCMC settings ---------------------------------------
niterations <- 20000
nburnin <- 10000
nchains <- 5
nthin <- 5

#niterations <- 10 ; nburnin <- 5 ; nchains <- 1 ; nthin <- 1

#! Source code and Import data -----------------------------------------
## file paths

## read files
master_tab <- read_csv("data/out/re_ana_info.csv")

for (i in 1:16){ #nrow(master_tab)){
    (sps_loop <- master_tab$sps[i])
    if(grepl("BA",  master_tab$par_names[i])){BA <- 1} else{BA <- 0}
    if(grepl("DEN", master_tab$par_names[i])){DEN <- 1}else{DEN <- 0}
    if(grepl("SHR", master_tab$par_names[i])){SHR <- 1}else{SHR <- 0}
    if(grepl("DIV", master_tab$par_names[i])){DIV <- 1}else{DIV <- 0}    
    if(grepl("EAR", master_tab$par_names[i])){EAR <- 1}else{EAR <- 0}
    if(grepl("MID", master_tab$par_names[i])){MID <- 1}else{MID <- 0}    
    if(grepl("LAT", master_tab$par_names[i])){LAT <- 1}else{LAT <- 0}
    if(grepl("CAN", master_tab$par_names[i])){CAN <- 1}else{CAN <- 0}
    if(grepl("DEB", master_tab$par_names[i])){DEB <- 1}else{DEB <- 0}
    
    pars_mod_name <- master_tab$par_names[i] %>% strsplit(., "_") %>% unlist() %>% gsub(" ", "", .) 
    pars_sca_mod <- master_tab$scales[i] %>% strsplit(., ",") %>% unlist() %>% gsub(" ", "", .) %>% as.numeric()
    pars_mod <- master_tab$pars[i] %>% strsplit(., ",") %>% unlist() %>% gsub(" ", "", .) %>% as.numeric()
    mod_name2 <- master_tab$model[i]

    cov_key <- c(BA, DEN, SHR, DIV, EAR, MID, LAT, CAN, DEB)
    print(sps_loop)
    print(cov_key)
    if(grepl('y', master_tab$post[i])){
        file_name <- master_tab$file[i]
        source('code/fit_model/post_hoc2.r')
    }
    if(grepl('n', master_tab$post[i])){
        if(grepl('y', master_tab$yr[i])){source("code/fit_model/back2d_covs_scales_2min_spscov_yr.R")}
        if(grepl('n', master_tab$yr[i])){source("code/fit_model/back2d_covs_scales_2min_spscov.R")}
    }
}

cat(paste('\n ************************************** \n \n \n 
        ---------------- all DONE Lol ----------------', 
        '\n\n \n ************************************** \n'))
