#? *********************************************************************************
#? ----------------------------  back2d_covs_scales_3  -----------------------------
#? *********************************************************************************
# Code to run model to estimate the effect of different environmental
#   covariates on bird occupancy in several national parks and on three
#   different spatial scales
#
#! Input ----------------------------------------------
#           - data/y_dat8.rds: tibble with bird data (2_create_data_files.R)
#           - data/X.rds: tibble with covariate data (2_create_data_files.R)
#           - data/out/nsite_pk.rds: vector with number of sites in each park
#           - data/src/key_park.rds: vector of all parks being analyzed
#
#! Output ---------------------------------------------
#           - data/model_res/jags_res_{sps}_{park}_run{run_number}.rds: file with result of jags model

# detach packages and clear workspace
#if(!require(freshr)){install.packages("freshr")}
freshr::freshr()

# Load packages --------------------------------------
library(conflicted)
library(tidyverse)
library(glue)

conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::filter)
conflicts_prefer(scales::alpha)
 
#if("sps_list" %in% ls() == FALSE){stop("No species selected #38")}

# Make functions --------------------------------------
colanmes <- colnames
lenght <- length
`%!in%` <- Negate(`%in%`)
sum_na <- function(df){ # sum fuction to ignore NAs, but keep NA if all entries are NA
  if (all(is.na(df))){
    suma <- NA
  }  
  else {    
    suma <- sum(df, na.rm = T)
  }
  return(suma)
}

# Import data -----------------------------------------
## file paths
YDAT_PATH <- "data/y_dat8.rds"
SITE_PK_PATH <- "data/out/nsite_pk.rds"
PARK_PATH <- "data/src/key_park.rds"

sps_list <- read_csv("data/for_int_list.csv")  %>% 
                #filter(Response_Guild == "InteriorForestObligate") %>% 
                select(AOU_Code) %>% 
                distinct() %>% 
                pull()
## read files
y_dat4 <- read_rds(file = YDAT_PATH)

nsite_pk <- read_rds(SITE_PK_PATH)
pk <- read_rds(PARK_PATH) %>%
  dplyr::select(parks) %>%
  pull() %>%
  sort()

rem_pks <- as_tibble(cbind(nsite_pk,pk)) %>% 
              filter(pk %!in% c("ACAD","ELRO","SAIR"))

nsite_pk <- as.numeric(rem_pks$nsite_pk)
pk <- as.vector(rem_pks$pk)

# Filter for species and park ---------------------------------------
## 1 sps several parks
y_dat5 <- y_dat4
# _n means that the 1 is the first occasion for that sps, year, loc, etc, not the first calendar one

y_dat5 <- y_dat5 %>%
  mutate( parkey = as.numeric(parkey),
          sps_it = AOU_Code)

y_dat6 <- y_dat5 %>% 
  dplyr::filter(sps_it %in% sps_list,
                park %in% pk
  )

if(length(sps_list) == 1){
  print(glue("analazing one species: {sps_list}"))
  } else {
  print(glue('analazing the community: {sps_list}'))
}

glu1 <- paste(shQuote(sort(unique(y_dat6$sps_it))), collapse=", ")
spsglue <- glue("the species are {glu1}, and parks are")
parkglue <- paste(shQuote(sort(unique(y_dat6$park))), collapse=", ")
print(paste(spsglue,parkglue))

## check the number of detections for the species
y_dat6 %>% 
    filter(bird_detec == 1) %>% 
    select(AOU_Code, park) %>% table()

## get only sps that are in at least 3 parks
sps_3pk <- 
    y_dat6 %>% 
        filter(bird_detec == 1) %>%           # at least one detection
        select(AOU_Code, park) %>% 
        table() %>% 
        as_tibble() %>% 
        mutate(n = ifelse(n>0, 1, 0)) %>% 
        group_by(AOU_Code) %>% 
        mutate(n_park = sum(n)) %>% 
        filter(n_park > 1) %>%
        #filter(n_park > 3) %>%                # at least in four parks
        select(AOU_Code, n_park) %>% 
        distinct() %>% 
        select(AOU_Code) %>% 
        pull()

sps_3pk

length(sps_3pk)

## now let's check how they are distributed across sites and years
sps_3pk

length(sps_3pk)

## now let's check how they are distributed across sites and years
sps_3pk

length(sps_3pk)

## now let's check how they are distributed across sites and years
y_dat6 %>% 
    filter(AOU_Code %in% sps_3pk,
           bird_detec == 1) %>% 
    select(AOU_Code, Point_Name, park)  %>% 
    group_by(park, Point_Name, AOU_Code) %>% 
    summarise(n = n()) %>% 
    ungroup() %>% 
    filter(str_detect(park, substr(Point_Name, 1, 4))) %>% 
    arrange(AOU_Code, Point_Name) %>% 
    ggplot(aes(x = Point_Name, y = n, color = AOU_Code, group = AOU_Code)) +
        geom_point() +
        geom_line() +
        facet_wrap(~park, scales = "free") +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 5))

## which percentage of the sites have the species in a park?
nsite_pk_names <- as_tibble(cbind(pk, nsite_pk)) %>% 
                    mutate(nsite_pk = as.numeric(nsite_pk)) %>% 
                    rename(park = pk)

site_ydat <- 
    y_dat6 %>% 
        filter(AOU_Code %in% sps_3pk,
            bird_detec == 1) %>% 
        select(AOU_Code, Point_Name, park)  %>% 
        group_by(park, Point_Name, AOU_Code) %>% 
        summarise(n = n()) %>% 
        ungroup() %>% 
        filter(str_detect(park, substr(Point_Name, 1, 4))) %>% 
        arrange(AOU_Code, Point_Name)  %>% 
                mutate(n = ifelse(n>0, 1, 0)) %>% 
        select(AOU_Code, park) %>% 
        table() %>% 
        as_tibble() %>% 
        left_join(., nsite_pk_names, by = "park") %>% 
        mutate(percent_site = if_else(n > 0, (n/nsite_pk), 0)) %>% 
        # remove the zeros cause it is ok some sps are not in some parks
        filter(percent_site > 0)

nrow(site_ydat %>% filter(percent_site > 0.4))/nrow(site_ydat)

view(site_ydat)

sps_40site <- 
    (site_ydat %>% 
        filter(percent_site > 0.4) %>% 
        select(AOU_Code) %>% 
        distinct() %>% 
        pull() %>% 
        sort())

length(sps_40site)
length(sps_3pk)

## now check year and site
yr_sit_dat <- 
    y_dat6 %>% 
        filter(AOU_Code %in% sps_40site,
            bird_detec > 0,
            !is.na(bird_detec)) %>% 
        select(AOU_Code, bird_detec, Year, Point_Name, park) 
        
yr_sit_dat %>%  
        ggplot(aes(y = Point_Name, x = Year, color = park, group = AOU_Code)) +
                geom_point() +
                facet_wrap(~AOU_Code, scales = "free") +
                theme_bw() +
                theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 5),
                    axis.text.y = element_blank())

sps_yr_site_1 <- 
    yr_sit_dat %>% 
        select(AOU_Code, park) %>% 
        table() %>% 
        as_tibble() %>% 
        filter(n > ((sum(nsite_pk))*lenght(unique(y_dat6$Year))) * 0.01) %>% 
        select(AOU_Code) %>% 
        distinct() %>% 
        pull() %>% 
        sort()

## final pool of species:

y_dat6 %>%  
    filter(AOU_Code %in% sps_yr_site_1) %>% 
    ggplot(aes(y = Point_Name, x = Year, color = park, group = AOU_Code)) +
            geom_point() +
            facet_wrap(~AOU_Code, scales = "free") +
            theme_bw() +
            theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 5),
                axis.text.y = element_blank())

## my lucky 13.
length(sps_yr_site_1)

## get some literature information to choose the variables for this species!!!
sps_covs <- tibble(AOU_Code = sps_yr_site_1,
                   BA = NA,       # tree basal area
                   DEN = NA,      # tree density
                   SHR = NA,      # shrub density
                   DIV = NA,      # diversity
                   EAR = NA,      # early succession basal area
                   MID = NA,      # mid succession basal area
                   LAT = NA,      # late succession basal area
                   CAN = NA,      # canopy cover
                   #SNA = NA,      # snag density
                   DEB = NA)      # wood debris density

sps_covs[which(sps_covs$AOU_Code == "BHVI"), 2:ncol(sps_covs)] <- as.list(c(
    # BA   DEN   SHR   DIV   EAR   MID   LAT   CAN   DEB
       1,   2,    1,    0,    0,    1,    2,    1,    0   )) # https://birdsoftheworld.org/bow/species/buhvir/cur/habitat#breedhab

sps_covs[which(sps_covs$AOU_Code == "BLBW"), 2:ncol(sps_covs)] <- as.list(c(
    # BA   DEN   SHR   DIV   EAR   MID   LAT   CAN   DEB
       0,   1,    0,    0,    0,    0,    1,    1,    0   )) # https://birdsoftheworld.org/bow/species/bkbwar/cur/habitat#breedhab

sps_covs[which(sps_covs$AOU_Code == "BRCR"), 2:ncol(sps_covs)] <- as.list(c(
    # BA   DEN   SHR   DIV   EAR   MID   LAT   CAN   DEB
       2,   1,    0,    1,    0,    0,    1,    1,    1   )) # https://birdsoftheworld.org/bow/species/brncre/cur/habitat#breedhab

sps_covs[which(sps_covs$AOU_Code == "BTBW"), 2:ncol(sps_covs)] <- as.list(c(
    # BA   DEN   SHR   DIV   EAR   MID   LAT   CAN   DEB
       0,   0,     1,   1,    1,    0,    2,    1,    0  )) # https://birdsoftheworld.org/bow/species/btbwar/cur/habitat#breedhab

sps_covs[which(sps_covs$AOU_Code == "BTNW"), 2:ncol(sps_covs)] <- as.list(c(
    # BA   DEN   SHR   DIV   EAR   MID   LAT   CAN   DEB
       1,   1,    0,    0,    0,    2,    2,    1,    0  )) # hhttps://birdsoftheworld.org/bow/species/btnwar/cur/habitat

sps_covs[which(sps_covs$AOU_Code == "DOWO"), 2:ncol(sps_covs)] <- as.list(c(
    # BA   DEN   SHR   DIV   EAR   MID   LAT   CAN   DEB
       1,   1,    0,    1,    0,    0,    0,    1,    1  )) # https://birdsoftheworld.org/bow/species/dowwoo/cur/habitat#breedhab

sps_covs[which(sps_covs$AOU_Code == "HAWO"), 2:ncol(sps_covs)] <- as.list(c(
    # BA   DEN   SHR   DIV   EAR   MID   LAT   CAN   DEB
       1,   1,    0,    0,    0,    0,    1,    1,    1  )) # https://birdsoftheworld.org/bow/species/haiwoo/cur/habitat

sps_covs[which(sps_covs$AOU_Code == "HETH"), 2:ncol(sps_covs)] <- as.list(c(
    # BA   DEN   SHR   DIV   EAR   MID   LAT   CAN   DEB
       2,   1,    1,    0,    1,    0,    0,    1,    0  )) # https://birdsoftheworld.org/bow/species/herthr/cur/breeding#nestsite
    
sps_covs[which(sps_covs$AOU_Code == "OVEN"), 2:ncol(sps_covs)] <- as.list(c(
    # BA   DEN   SHR   DIV   EAR   MID   LAT   CAN   DEB
       1,   0,    1,    0,    2,    2,    1,    1,    0  )) # https://birdsoftheworld.org/bow/species/ovenbi1/cur/habitat#breedhab

sps_covs[which(sps_covs$AOU_Code == "PIWA"), 2:ncol(sps_covs)] <- as.list(c(
    # BA   DEN   SHR   DIV   EAR   MID   LAT   CAN   DEB
       0,   1,    0,    0,    1,    0,    1,    0,    1  )) # https://birdsoftheworld.org/bow/species/pilwoo/cur/habitat

sps_covs[which(sps_covs$AOU_Code == "REVI"), 2:ncol(sps_covs)] <- as.list(c(
    # BA   DEN   SHR   DIV   EAR   MID   LAT   CAN   DEB
       1,   0,    1,    1,    0,    0,    0,    1,    0  )) # https://birdsoftheworld.org/bow/species/reevir1/cur/habitat#breedhab

sps_covs[which(sps_covs$AOU_Code == "SCTA"), 2:ncol(sps_covs)] <- as.list(c(
    # BA   DEN   SHR   DIV   EAR   MID   LAT   CAN   DEB
       1,   1,    0,    1,    0,    2,    1,    1,    0  )) # https://birdsoftheworld.org/bow/species/scatan/cur/habitat#breedhab

sps_covs[which(sps_covs$AOU_Code == "VEER"), 2:ncol(sps_covs)] <- as.list(c(
    # BA   DEN   SHR   DIV   EAR   MID   LAT   CAN   DEB
       0,   1,    1,    0,    1,    0,    0,    1,    0  )) # https://birdsoftheworld.org/bow/species/veery/cur/habitat#breedhab

sps_covs[which(sps_covs$AOU_Code == "WBNU"), 2:ncol(sps_covs)] <- as.list(c(
    # BA   DEN   SHR   DIV   EAR   MID   LAT   CAN   DEB
       2,   0,    1,    0,    0,    2,    1,    1,    0  )) # https://birdsoftheworld.org/bow/species/whbnut/cur/habitat

sps_covs[which(sps_covs$AOU_Code == "WOTH"), 2:ncol(sps_covs)] <- as.list(c(
    # BA   DEN   SHR   DIV   EAR   MID   LAT   CAN   DEB
       0,   1,    1,    0,    2,    0,    1,    1,    0   )) # https://birdsoftheworld.org/bow/species/woothr/cur/habitat#breedhab

sps_covs[which(sps_covs$AOU_Code == "YBSA"), 2:ncol(sps_covs)] <- as.list(c(
    # BA   DEN   SHR   DIV   EAR   MID   LAT   CAN   DEB
       0,    1,    0,    0,    1,    0,   0,    1,    1   )) # https://birdsoftheworld.org/bow/species/yebsap/cur/habitat#breedhab

write_rds(sps_covs, file = "data/out/sps_covs.rds")
