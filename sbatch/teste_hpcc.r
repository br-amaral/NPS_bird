context <- "test_hpcc.R" #rstudioapi::getSourceEditorContext()
cat("\n", "\n", "\n", 
    'Current script:', context, #basename(context[[2]]), 
    "\n", "\n", "\n", "\n")
#if(!require(freshr)){install.packages('freshr')}
freshr::freshr()
library(tidyverse)
library(glue)
colanmes <- colnames
lenght <- length
`%!in%` <- Negate(`%in%`)
#sps_list <- read_rds("data/src/guilds.rds")  %>% 
#                filter(Response_Guild == "InteriorForestObligate") %>% 
#                select(AOU_Code) %>% 
#                distinct() %>% 
#                pull()
sps_list1 <- c("GCFL", "AMGO", "DOWO", "NOCA", "SCTA", "SOSP", "GRCA", "RBWO", "COYE", "WOTH", "RWBL",
                "WBNU", "BTNW", "EAWP", "BCCH", "BLJA", "TUTI", "AMRO", "REVI", "OVEN", "BTBW", "YBSA", 
                "BOBO", "YRWA", "PIWA", "CEDW", "CHSP", "NOFL", "HAWO", "BRCR", "RBGR", "DEJU", "AMCR", 
                "BAOR", "RBNU", "BHVI", "GCKI", "EATO", "FISP", "HETH", "VEER", "MODO", "BLBW")
print("SUCCESS")



