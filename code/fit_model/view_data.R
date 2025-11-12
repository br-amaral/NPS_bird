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

conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::filter)

#! Make functions --------------------------------------
colanmes <- colnames
lenght <- length
`%!in%` <- Negate(`%in%`)

#! Source code -----------------------------------------

#! Import data -----------------------------------------
## file paths
DATA_LOC <- "data/ana_file/"
STEP2_INFO_PATH <- "code/fit_model/mod_key.csv"
## read files
res_key <- read_csv(STEP2_INFO_PATH) %>% 
                filter(step == 3,
                       run == "yes",
                       AOU_Code != "BCCH") %>% 
                mutate(data = glue("{DATA_LOC}{substr(select, 1, 11)}jagsdata{substr(select, 18, 28)}.rds"))  

for(ii in 1:nrow(res_key)){
    sps_jdat <- read_rds(res_key$data[ii])

    sps_jdat2 <- cbind(sps_jdat$y2, sps_jdat$Xp)

    colnames(sps_jdat2)[7] <- "pksize"

    sps_jdat2 <- as_tibble(sps_jdat2)

    sps_jdat2 <- sps_jdat2  %>% 
                    mutate(sps = res_key$AOU_Code[ii])

    if(ii == 1){jags_dat <- sps_jdat2} else {jags_dat <- rbind(jags_dat, sps_jdat2)}
    print(ii)
}

jags_dat2 <- jags_dat %>% 
                select(-site_n, -interval_n) %>% 
                arrange(sps, parkey, year_n) %>%
                distinct() %>%
                filter(bird_detec == 1) 

ggplot(jags_dat2) +
    geom_point(aes(x = sps, y = pksize, color = as.character(parkey)), 
               position = position_jitter(width = 0.2))

ggplot(jags_dat2 %>% select(pksize, parkey) %>% distinct() %>% arrange(parkey)) +
    geom_point(aes(y = pksize, x = as.character(parkey)))
