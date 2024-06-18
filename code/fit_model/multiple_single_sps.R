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

# Print script file name
context <- rstudioapi::getSourceEditorContext()
cat("\n", "\n", "\n", 'Current script: ', basename(context[[2]]), "\n", "\n", "\n", "\n")

#! Package library and versions -------------------------
#  Created a library repo?
#  (  )yes  (  )no
#  renv::init()

# Load an existing library?
#  renv::status()
#  renv::restore()

# Installed new packages?
#  renv::snapshot()

# detach packages and clear workspace
if(!require(freshr)){install.packages('freshr')}
freshr::freshr()

#! Load packages ---------------------------------------
library(conflicted)
library(tidyverse)
library(glue)

conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::filter)
# conflicts_prefer(scales::alpha)

#! Make functions --------------------------------------
colanmes <- colnames
lenght <- length
`%!in%` <- Negate(`%in%`)


#! Source code and Import data -----------------------------------------
## file paths

## read files
#sps_list <- read_rds("data/src/guilds.rds")  %>% 
#                filter(Response_Guild == "InteriorForestObligate") %>% 
#                select(AOU_Code) %>% 
#                distinct() %>% 
#                pull()

#sps_list <- sps_list[-1]
sps_list1 <- c("GCFL", "AMGO", "DOWO", "NOCA", "SCTA", "SOSP", "GRCA", "RBWO", "COYE", "WOTH", "RWBL",
                "WBNU", "BTNW", "EAWP", "BCCH", "BLJA", "TUTI", "AMRO", "REVI", "OVEN", "BTBW", "YBSA", 
                "BOBO", "YRWA", "PIWA", "CEDW", "CHSP", "NOFL", "HAWO", "BRCR", "RBGR", "DEJU", "AMCR", 
                "BAOR", "RBNU", "BHVI", "GCKI", "EATO", "FISP", "HETH", "VEER", "MODO", "BLBW")

yearbo1 <- c('yes', 'no')

for_stage1 <- c('late', 'mature', 'pole')

master_tab <- expand_grid(sps_list1, yearbo1, for_stage1) %>% 
                mutate(res_name = glue("{sps_list1}_b0{yearbo1}_{for_stage1}"))

for (i in 1:nrow(master_tab)){
    sps_loop <- master_tab[i,1] %>% pull()
    yearbo <- master_tab[i,2] %>% pull()
    for_stage <- master_tab[i,3] %>% pull()
    print(master_tab[i,4] %>% pull())
    source("code/fit_model/back2d_covs_scales_3.R")
}