# devtools::install_github('hunter-stanke/rFIA', force = TRUE)

library(rFIA)
library(tidyverse)
library(sp)
library(sf)
library(terra)
#library(raster)
library(glue)
library(tigris)
library(corrplot)

## Download the state subset or Connecticut (requires an internet connection)
## Save as an object to automatically load the data into your current R session!
      # vt <- getFIA(states = 'VT', dir = 'data/FIA', load = FALSE)
      # me <- getFIA(states = 'ME', dir = 'data/FIA', load = FALSE)
      # nh <- getFIA(states = 'NH', dir = 'data/FIA', load = FALSE)
      # ny <- getFIA(states = 'NY', dir = 'data/FIA', load = FALSE)
      # ct <- getFIA(states = 'CT', dir = 'data/FIA', load = FALSE)
      # ma <- getFIA(states = 'MA', dir = 'data/FIA', load = FALSE)
      # ri <- getFIA(states = 'RI', dir = 'data/FIA', load = FALSE)
      # nj <- getFIA(states = 'NJ', dir = 'data/FIA', load = FALSE)

## Get multiple states worth of data (not saved since 'dir' is not specified)
## Load FIA Data from a local directory
db <- readFIA('data/FIA/')

parks <- readRDS(file = "data/src/key_park.rds") %>% 
  dplyr::select(parks) %>% 
  distinct() %>% 
  pull()

# county location of each park
park_county <- matrix(c(
  'ACAD', 'Hancock County', 'Maine',
  'ELRO', 'Dutchess County', 'New York',
  'HOFR', 'Dutchess County', 'New York',
  'MABI', 'Windsor County', 'Vermont',
  'MIMA', 'Middlesex County', 'Massachusetts',
  'MORR', 'Morris County', 'New Jersey',
  'SAGA', 'Sullivan County', 'New Hampshire',
  'SAIR', 'Essex County', 'Massachusetts',
  'SARA', 'Saratoga County', 'New York',
  'VAMA', 'Dutchess County', 'New York',
  'WEFA', 'Fairfield County', 'Connecticut'), 
  ncol = 3, byrow = T) %>% 
  as_tibble()
colnames(park_county) <- c("park", "county", "state")

for(ii in 1:nrow(park_county)){
  
  county_sp <- counties(park_county$state[ii], cb = TRUE)
  
  county_sp2 <- county_sp %>% filter(NAMELSAD == park_county$county[ii])
  
  gg <- ggplot()
  gg <- gg + geom_sf(data = county_sp2, color="black",
                     fill="white", linewidth=2) + 
    theme_bw() + 
    theme(panel.border = element_blank(), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          axis.line = element_blank()) 
  gg 
    
  dbclip2 <- clipFIA(db, mask = county_sp2, mostRecent = FALSE)
  
  name1 <- glue("fia_{park_county$park[ii]}")
  
  assign(name1, dbclip2)
  
  # db2_tree <- dbclip2$TREE[c("DAMAGE_AGENT_CD1", "CN", "PLT_CN", "INVYR", "STATECD", "UNITCD", "COUNTYCD",
  #                       "PLOT", "SUBP", "TREE", "DIA", "DIAHTCD", "HT", "HTCD")]
  # 
  # db2_cond <- dbclip2$COND[c("CN", "PLT_CN", "INVYR", "UNITCD", "PLOT", "CONDID","COUNTYCD", "STATECD",
  #                            "COND_STATUS_CD", "OWNCD", "OWNGRPCD", "FORTYPCD",
  #                            "FLDTYPCD", "STDAGE", "STDSZCD", "SIBASE", "STDORGCD",
  #                            "ALSTKCD", "DSTRBCD1", "DSTRBYR1", "DSTRBCD2", "TRTCD1",
  #                            "PRESNFCD", "BALIVE", "FLDAGE", "ALSTK", "GSSTK", "FORTYPCDCALC",
  #                            "HABTYPCD1", "CARBON_LITTER", "GRAZING_SRS", "HARVEST_TYPE1_SRS",
  #                            "LIVE_CANOPY_CVR_PCT", "NBR_LIVE_STEMS", "DSTRBCD1_P2A", "TRTCD1_P2A",
  #                            "LAND_COVER_CLASS_CD")]
  # db2_plot <- dbclip2$PLOT[c("CN", "INVYR","SRV_CN","CTY_CN","UNITCD", "STATECD",
  #                            "COUNTYCD","PLOT","KINDCD","LAT","LON", "UNITCD", "PLOT")]
  # db2_sur <- dbclip2$SURVEY[c("STATENM", "CN", "INVYR", "STATECD")]
  # 
  # dbclip2$P2VEG_SUBP_STRUCTURE
  # 
  # db3 <- dbclip2[[c("PLOT")]]
  # 
  # db3 <- db3 %>% 
  #   dplyr::select(INVYR, STATECD, UNITCD, COUNTYCD, PLOT, LAT, LON)
  # 
  # db2_tree <- left_join(db2_tree, db3)#, by = c("INVYR", "STATECD", "UNITCD", "COUNTYCD", "PLOT"))
  # db2_cond <- left_join(db2_cond, db3)#, by = c("INVYR", "STATECD", "UNITCD", "COUNTYCD", "PLOT"))
  # db2_sur <- left_join(db2_sur, db3)
  # 
  # db2_tree$park <- park_county$park[ii]
  # db2_cond$park <- park_county$park[ii]
  # db2_plot$park <- park_county$park[ii]
  # db2_sur$park <- park_county$park[ii]
  # 
  # if(ii == 1){
  #   fia_tree <- db2_tree
  #   fia_cond <- db2_cond
  #   fia_plot <- db2_plot
  #   fia_sur <- db2_sur
  # }
  # 
  # if(ii > 1){
  #   fia_tree <- rbind(fia_tree, db2_tree)
  #   fia_cond <- rbind(fia_cond, db2_cond)
  #   fia_plot <- rbind(fia_plot, db2_plot)
  #   fia_sur <- rbind(fia_sur, db2_sur)
  # }
  # 
  #print(nrow(fia_tree))
  
  #spdf_tree <- SpatialPointsDataFrame(coords = db2_tree[, c("LON","LAT")], data = db2_tree[, c("LON","LAT")],
  #                                    proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  #spdf_cond <- SpatialPointsDataFrame(coords = db2_cond[, c("LON","LAT")], data = db2_cond[, c("LON","LAT")],
  #                                    proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  #spdf_plot <- SpatialPointsDataFrame(coords = db2_plot[, c("LON","LAT")], data = db2_plot[, c("LON","LAT")],
  #                                    proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  # spdf_sur <- SpatialPointsDataFrame(coords = db2_sur[, c("LON","LAT")], data = db2_sur[, c("LON","LAT")],
  #                                     proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  
  #spdf_tree$TREE <- db2_tree$TREE
  
  #plot(spdf_tree)
 # sf_tree <- st_as_sf(spdf_tree)
  
  # Create a ggplot object
  # gg +
  #   geom_sf(data = sf_tree, col = "#3ba400", size = 5) +
  #   theme_bw() + 
  #   theme(panel.border = element_blank(), 
  #         panel.grid.major = element_blank(),
  #         panel.grid.minor = element_blank(), 
  #         axis.line = element_blank()) +
  #   geom_sf(data = st_as_sf(pb), col = "black")
}

## get some idea of the variation and choose variables

# Get estimates (rFIA functions) -----------------------------------
## Tree abundance  --------------------------------------------------
# Estimate trees per acre and basal area per acre from FIADB

for(ii in 1:nrow(park_county)){
  # tree per acre - density
  tpaRI <- tpa(get(glue("fia_{park_county$park[ii]}")), totals = TRUE) %>% 
    select(YEAR, TPA, BAA, TREE_TOTAL, BA_TOTAL, TPA_SE, BAA_SE, TREE_TOTAL_SE) %>% 
    mutate(park = park_county$park[ii])
  
  if(ii == 1){
    tpa_tab <- tpaRI
  }
  
  if(ii > 1){
    tpa_tab <- rbind(tpa_tab, tpaRI)
  }
}

### group by species -------------------
for(ii in 1:nrow(park_county)){
  
  tpaRI_sp <- tpa(get(glue("fia_{park_county$park[ii]}")), bySpecies = TRUE) %>% 
    select(YEAR, SPCD,COMMON_NAME, SCIENTIFIC_NAME, TPA, BAA) %>% 
    mutate(park = park_county$park[ii])
  
  if(ii == 1){
    tpa_sp_tab <- tpaRI_sp
  }
  
  if(ii > 1){
    tpa_sp_tab <- rbind(tpa_sp_tab, tpaRI_sp)
  }
}

### group by size class --------------------------
for(ii in 1:nrow(park_county)){
  tpaRI_sc <- tpa(get(glue("fia_{park_county$park[ii]}")), bySizeClass = TRUE) %>% 
    select(YEAR, sizeClass, TPA, BAA) %>% 
    mutate(park = park_county$park[ii])
  
  if(ii == 1){
    tpa_sc_tab <- tpaRI_sc
  }
  
  if(ii > 1){
    tpa_sc_tab <- rbind(tpa_sc_tab, tpaRI_sc)
  }
}
### ownership group ----------------
for(ii in 1:nrow(park_county)){
  tpaRI_own <- tpa(get(glue("fia_{park_county$park[ii]}")), grpBy = OWNGRPCD) %>% 
    select(YEAR, OWNGRPCD, TPA, BAA) %>% 
    mutate(park = park_county$park[ii])
  
  if(ii == 1){
    tpa_own_tab <- tpaRI_own
  }
  
  if(ii > 1){
    tpa_own_tab <- rbind(tpa_own_tab, tpaRI_own)
  }
}


### forest type ---------------
for(ii in 1:nrow(park_county)){
  tpaRI_ft <- tpa(get(glue("fia_{park_county$park[ii]}")), grpBy = FORTYPCD) %>% 
    select(YEAR, FORTYPCD, TPA, BAA) %>% 
    mutate(park = park_county$park[ii])
  
  if(ii == 1){
    tp_ft_tab <- tpaRI_ft
  }
  
  if(ii > 1){
    tp_ft_tab <- rbind(tp_ft_tab, tpaRI_ft)
  }
}

## Biomass -------------------------------
for(ii in 1:nrow(park_county)){
  bio <- biomass(get(glue("fia_{park_county$park[ii]}"))) %>% 
    select(YEAR,BIO_ACRE, CARB_ACRE) %>% 
    mutate(park = park_county$park[ii])
  
  if(ii == 1){
    bio_tab <- bio
  }
  
  if(ii > 1){
    bio_tab <- rbind(bio_tab, bio)
  }
}

## Diversity --------------------------------
for(ii in 1:nrow(park_county)){
  div <- diversity(get(glue("fia_{park_county$park[ii]}"))) %>% 
    select(YEAR, H_a, Eh_a, S_a, H_b, Eh_b, S_b, H_g, Eh_g, S_g,
           Eh_a_SE, S_a_SE) %>% 
    mutate(park = park_county$park[ii])
  if(ii == 1){
    div_tab <- div
  }
  
  if(ii > 1){
    div_tab <- rbind(div_tab, div)
  }
}

## Down woody material function ------------------------
for(ii in 1:nrow(park_county)){
  downwood <- dwm(get(glue("fia_{park_county$park[ii]}"))) %>% 
    select(YEAR, FUEL_TYPE, VOL_ACRE, BIO_ACRE, CARB_ACRE) %>% 
    mutate(park = park_county$park[ii])
  
  if(ii == 1){
    dowo_tab <- downwood
  }
  
  if(ii > 1){
    dowo_tab <- rbind(dowo_tab, downwood)
  }
}

## Growth, recruitment, mortality, and harvest rates function -------------
for(ii in 1:nrow(park_county)){
  bide <- growMort(get(glue("fia_{park_county$park[ii]}"))) %>% 
    select(YEAR ,RECR_TPA, MORT_TPA, REMV_TPA, GROW_TPA, CHNG_TPA, 
           RECR_PERC, MORT_PERC, REMV_PERC, GROW_PERC, CHNG_PERC) %>% 
    mutate(park = park_county$park[ii])
  
  if(ii == 1){
    bide_tab <- bide
  }
  
  if(ii > 1){
    bide_tab <- rbind(bide_tab, bide)
  }
}

## Invasive species ----------------------------
for(ii in 1:nrow(park_county)){
  invs <- invasive(get(glue("fia_{park_county$park[ii]}"))) %>% 
    select("YEAR", "SYMBOL", "COVER_PCT") %>% 
    mutate(park = park_county$park[ii])
  if(ii == 1){
    invs_tab <- invs
  }
  
  if(ii > 1){
    invs_tab <- rbind(invs_tab, invs)
  }
}

## Forest structural stage distribution --------------------
for(ii in 1:nrow(park_county)){
  stastr <- standStruct(get(glue("fia_{park_county$park[ii]}"))) %>% 
    select(YEAR, STAGE, COVER_PCT, COVER_PCT_SE) %>% 
    mutate(park = park_county$park[ii])
  if(ii == 1){
    stastr_tab <- stastr
  }
  
  if(ii > 1){
    stastr_tab <- rbind(stastr_tab, stastr)
  }
}

## Tree growth rates -------------------------
for(ii in 1:nrow(park_county)){
  vitrat <- vitalRates(get(glue("fia_{park_county$park[ii]}"))) %>% 
    select(YEAR, DIA_GROW, BA_GROW, NETVOL_GROW, SAWVOL_GROW, BIO_GROW, BA_GROW_AC,
           NETVOL_GROW_AC, SAWVOL_GROW_AC, BIO_GROW_AC) %>% 
    mutate(park = park_county$park[ii])
  
  if(ii == 1){
    vitrat_tab <- vitrat
  }
  
  if(ii > 1){
    vitrat_tab <- rbind(vitrat_tab, vitrat)
  }
}

# Check for correlation within the tables -----------------------------------
## growth, recruitment, mortality, and harvest rates -------------------------
bide_tab2 <- bide_tab
bide_tab2[sapply(bide_tab2, is.infinite)] <- NA
bide_tab2[,-c(1,ncol(bide_tab2))] %>% 
  cor(use="pairwise.complete.obs") %>% 
  corrplot(method = 'square', order = 'AOE', addCoef.col = 'black', 
           cl.pos = 'n', col = COL2('BrBG'), type = 'lower', number.cex=0.5)
bide_tab2[,c(2:6)] %>% 
  cor(use="pairwise.complete.obs") %>% 
  corrplot(method = 'square', order = 'AOE', addCoef.col = 'black', 
           cl.pos = 'n', col = COL2('BrBG'), type = 'lower', number.cex=0.5)

bide_fim <- bide_tab2[,c("YEAR", "CHNG_TPA", "GROW_TPA", "MORT_TPA", "park")]
bide_fim %>% 
  ggplot(aes(x = park, y = CHNG_TPA)) +
  geom_boxplot(fill="#BF87B3") +
  theme_bw()

bide_fim %>% 
  #filter(park != "ACAD") %>% 
  ggplot(aes(x = park, y = GROW_TPA)) +
  geom_boxplot(fill="#BF87B3") +
  theme_bw()

bide_fim %>% 
  #filter(park != "ACAD") %>% 
  ggplot(aes(x = park, y = MORT_TPA)) +
  geom_boxplot(fill="#BF87B3") +
  ylim(0,5) +
  theme_bw()

bide_fim <- bide_tab2[,c("YEAR", "CHNG_TPA", "MORT_TPA", "park")]
## biomass ---------------------------
bio_tab2 <- bio_tab
bio_tab2[sapply(bio_tab2, is.infinite)] <- NA
bio_tab2[,-c(1,ncol(bio_tab2))] %>% 
  cor(use="pairwise.complete.obs") %>% 
  corrplot(method = 'square', order = 'AOE', addCoef.col = 'black', 
           cl.pos = 'n', col = COL2('BrBG'), type = 'lower', number.cex=0.5)
bio_fim <- bio_tab2[,c('YEAR', 'BIO_ACRE',  'park' )]
bio_fim %>% 
  #filter(park != "ACAD") %>% 
  ggplot(aes(x = park, y = BIO_ACRE)) +
  geom_boxplot(fill="#BF87B3") +
  theme_bw()

## biodiversity ---------------------
div_tab2 <- div_tab
div_tab2[sapply(div_tab2, is.infinite)] <- NA
div_tab2[,-c(1,ncol(div_tab2))] %>% 
  cor(use="pairwise.complete.obs") %>% 
  corrplot(method = 'square', order = 'alphabet', addCoef.col = 'black', 
           cl.pos = 'n', col = COL2('BrBG'), type = 'lower', number.cex=0.5)
div_tab2[,c("H_g", "Eh_a", "S_a")] %>% 
  cor(use="pairwise.complete.obs") %>% 
  corrplot(method = 'square', order = 'AOE', addCoef.col = 'black', 
           cl.pos = 'n', col = COL2('BrBG'), type = 'lower', number.cex=0.5)
div_fim <- div_tab2[,c("YEAR","H_g", "Eh_a", "S_a", "Eh_a_SE", "S_a_SE", "park")]
div_fim %>% 
  #filter(park != "ACAD") %>% 
  ggplot(aes(x = park, y = H_g)) +
  geom_boxplot(fill="#BF87B3") +
  theme_bw()
div_fim %>% 
  #filter(park != "ACAD") %>% 
  ggplot(aes(x = park, y = Eh_a)) +
  geom_boxplot(fill="#BF87B3") +
  theme_bw()
div_fim %>% 
  #filter(park != "ACAD") %>% 
  ggplot(aes(x = park, y = S_a)) +
  geom_boxplot(fill="#BF87B3") +
  theme_bw()

write_rds(div_fim, file = "data/FIA/out/div_fim.rds")

## down wood debris ------------------------
dowo_tab2 <- dowo_tab
dowo_tab2[sapply(dowo_tab2, is.infinite)] <- NA
dowo_tab2[,-c(1:2,ncol(dowo_tab2))] %>% 
  cor(use="pairwise.complete.obs") %>% 
  corrplot(method = 'square', order = 'alphabet', addCoef.col = 'black', 
           cl.pos = 'n', col = COL2('BrBG'), type = 'lower', number.cex=0.5)
dowo_tab2[,c("VOL_ACRE","BIO_ACRE")] %>% 
  cor(use="pairwise.complete.obs") %>% 
  corrplot(method = 'square', order = 'AOE', addCoef.col = 'black', 
           cl.pos = 'n', col = COL2('BrBG'), type = 'lower', number.cex=0.5)
dowo_fim <- dowo_tab2[,c("YEAR","FUEL_TYPE","VOL_ACRE","BIO_ACRE","park")]
dowo_fim %>% 
  #filter(park != "ACAD") %>% 
  ggplot(aes(x = park, y = VOL_ACRE)) +
  geom_boxplot(fill="#BF87B3") +
  theme_bw() +
  facet_wrap(~FUEL_TYPE, scales = "free_y")

dowo_fim %>% 
  #filter(park != "ACAD") %>% 
  ggplot(aes(x = park, y = log(BIO_ACRE))) +
  geom_boxplot(fill="#BF87B3") +
  theme_bw() +
  facet_wrap(~FUEL_TYPE, scales = "free_y")

## invasive ----------------------------------------------
invs_tab2 <- invs_tab
invs_tab2[sapply(invs_tab2, is.infinite)] <- NA
invs_fim <- invs_tab2
invs_fim %>% 
  ggplot(aes(x = park, y = COVER_PCT)) +
  geom_boxplot(fill="#BF87B3") +
  theme_bw() 

## stand structure  ----------------------------------------------
stastr_tab2 <- stastr_tab
stastr_tab2[sapply(stastr_tab2, is.infinite)] <- NA
stastr_fim <- stastr_tab2
stastr_fim %>% 
  #filter(park != "ACAD") %>% 
  ggplot(aes(x = park, y = COVER_PCT)) +
  geom_boxplot(fill="#BF87B3") +
  theme_bw()

write_rds(stastr_tab, file = "data/FIA/out/stand_struct_fim.rds")

## tree abundance  ----------------------------------------------
tp_ft_tab2 <- tp_ft_tab
tp_ft_tab2[sapply(tp_ft_tab2, is.infinite)] <- NA
tp_ft_tab2[,-c(1,ncol(tp_ft_tab2))] %>% 
  cor(use="pairwise.complete.obs") %>% 
  corrplot(method = 'square', order = 'alphabet', addCoef.col = 'black', 
           cl.pos = 'n', col = COL2('BrBG'), type = 'lower', number.cex=0.5)

tp_ft_fim <- tp_ft_tab2
tp_ft_fim %>% 
  #filter(park != "ACAD") %>% 
  ggplot(aes(x = park, y = TPA)) + #, color = as.factor(FORTYPCD))) +
  #geom_point() +
  geom_boxplot(fill="#BF87B3") +
  theme_bw() 

tp_ft_fim %>% 
  #filter(park != "ACAD") %>% 
  ggplot(aes(x = park, y = BAA, color = as.factor(FORTYPCD))) +
  geom_point() +
  theme_bw() 

### ownership  ----------------------------------------------
tpa_own_tab2 <- tpa_own_tab
tpa_own_tab2[sapply(tpa_own_tab2, is.infinite)] <- NA
tpa_own_tab2[,-c(1,ncol(tpa_own_tab2))] %>% 
  cor(use="pairwise.complete.obs") %>% 
  corrplot(method = 'square', order = 'alphabet', addCoef.col = 'black', 
           cl.pos = 'n', col = COL2('BrBG'), type = 'lower', number.cex=0.5)

tpa_own_fim <- tpa_own_tab2
tpa_own_fim %>% 
  #filter(park != "ACAD") %>% 
  ggplot(aes(x = park, y = TPA)) +
  geom_boxplot(fill="#BF87B3") +
  theme_bw() +
  facet_wrap(~OWNGRPCD, scales = "free_y")

tpa_own_tab2 %>% 
  group_by(park, OWNGRPCD) %>% 
  summarise(mean_BAA = mean(BAA)) %>% 
  ggplot(aes(y = as.factor(OWNGRPCD), x = park, fill = mean_BAA)) +
  geom_tile(color = "black") +
  #geom_text(aes(label = round(occn,2)), color = "black", size = 2, angle = 90) +
  theme_bw() +
  #theme(legend.position = "none") +
  ylab("Ownership types") +
  xlab("Parks") +
  scale_fill_viridis_c(option = "plasma") 

tpa_own_fim %>% 
  #filter(park != "ACAD") %>% 
  ggplot(aes(x = park, y = BAA)) +
  geom_boxplot(fill="#BF87B3") +
  theme_bw() +
  facet_wrap(~OWNGRPCD, scales = "free_y")

##
tpa_sc_tab2 <- tpa_sc_tab
tpa_sc_tab2[sapply(tpa_sc_tab2, is.infinite)] <- NA
tpa_sc_tab2[,-c(1:2,ncol(tpa_sc_tab2))] %>% 
  cor(use="pairwise.complete.obs") %>% 
  corrplot(method = 'square', order = 'alphabet', addCoef.col = 'black', 
           cl.pos = 'n', col = COL2('BrBG'), type = 'lower', number.cex=0.5)

tpa_sc_fim <- tpa_sc_tab2[c("YEAR","sizeClass","TPA","BAA","park")]

tpa_sc_fim %>% 
  #filter(park != "ACAD") %>% 
  ggplot(aes(x = park, y = TPA, color = as.factor(sizeClass))) +
  geom_point() +
  theme_bw() 

tpa_sc_fim %>% 
  #filter(park != "ACAD") %>% 
  ggplot(aes(x = park, y = BAA, color = as.factor(sizeClass))) +
  geom_point() +
  theme_bw() 

### species  ----------------------------------------------
tpa_sp_tab2 <- tpa_sp_tab
tpa_sp_tab2[sapply(tpa_sp_tab2, is.infinite)] <- NA
tpa_sp_tab2[,-c(1:4,ncol(tpa_sp_tab2))] %>% 
  cor(use="pairwise.complete.obs") %>% 
  corrplot(method = 'square', order = 'alphabet', addCoef.col = 'black', 
           cl.pos = 'n', col = COL2('BrBG'), type = 'lower', number.cex=0.5)

tpa_sp_tab2 %>% 
  #filter(park != "ACAD") %>% 
  ggplot(aes(x = park, y = TPA, color = as.factor(SPCD))) +
  geom_point() +
  theme_bw() +
  theme(legend.position = "none")

tpa_sp_tab2 %>% 
  #filter(park != "ACAD") %>% 
  ggplot(aes(x = park, y = BAA, color = as.factor(SPCD))) +
  geom_point() +
  theme_bw() +
  theme(legend.position = "none")

tpa_sp_fim <- tpa_sp_tab2[,c("YEAR","SPCD","COMMON_NAME","SCIENTIFIC_NAME","BAA","park" )]

### total ----------------------------------------------
tpa_tab2 <- tpa_tab
tpa_tab2[sapply(tpa_tab2, is.infinite)] <- NA
tpa_tab2[,-c(1,ncol(tpa_tab2))] %>% 
  cor(use="pairwise.complete.obs") %>% 
  corrplot(method = 'square', order = 'alphabet', addCoef.col = 'black', 
           cl.pos = 'n', col = COL2('BrBG'), type = 'lower', number.cex=0.5)
tpa_tab2[,c("BAA","TPA")] %>% 
  cor(use="pairwise.complete.obs") %>% 
  corrplot(method = 'square', order = 'AOE', addCoef.col = 'black', 
           cl.pos = 'n', col = COL2('BrBG'), type = 'lower', number.cex=0.5)
tpa_fim <- tpa_tab2[,c("YEAR","TPA","BAA",
                       "TPA_SE","BAA_SE","TREE_TOTAL_SE",
                       "park")]
tpa_fim %>% 
  #filter(park != "ACAD") %>% 
  ggplot(aes(x = park, y = scale(TPA))) +
  geom_boxplot(fill="#AC7352") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 

tpa_fim %>% 
  #filter(park != "ACAD") %>% 
  ggplot(aes(x = park, y = scale(BAA))) +
  geom_boxplot(fill="#AC7352") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 

tpa_fim %>% 
  filter(park %!in% c("ELRO", "HOFR", "ROVA", "VAMA")) %>% 
  ggplot(aes(x = park, y = BAA, fill = park,)) +
    geom_boxplot() +
    geom_jitter(position=position_jitter(0.2), alpha = 0.5) +
    coord_flip() +
    theme_bw() +
    theme(legend.position="none",
          axis.title.y = element_blank(),
          plot.title = element_text(hjust = 0.5)) +
    labs(title = "County Scale",
        y =" \n  Basal area of live trees \n(>=10cm DBH in m2/ha)") +
    scale_fill_manual(values = met.brewer("Morgenstern")) +
    stat_summary(colour = "red", size = 0.75)


write_rds(tpa_fim, file = "data/FIA/out/tpa_fim.rds")
##
vitrat_tab2 <- vitrat_tab
vitrat_tab2[sapply(vitrat_tab2, is.infinite)] <- NA
vitrat_tab2[,-c(1,ncol(vitrat_tab2))] %>% 
  cor(use="pairwise.complete.obs") %>% 
  corrplot(method = 'square', order = 'alphabet', addCoef.col = 'black', 
           cl.pos = 'n', col = COL2('BrBG'), type = 'lower', number.cex=0.5)

  vitrat_tab2 %>% 
  #filter(park != "ACAD") %>% 
  ggplot(aes(x = park, y = DIA_GROW)) +
  geom_boxplot(fill="#BF87B3") +
  theme_bw() 

vitrat_tab2 %>% 
  #filter(park != "ACAD") %>% 
  ggplot(aes(x = park, y = SAWVOL_GROW)) +
  geom_boxplot(fill="#BF87B3") +
  theme_bw() 

vitrat_tab2[,c("DIA_GROW",
               #"BA_GROW", 
               #"NETVOL_GROW", 
               "SAWVOL_GROW"
               #"BIO_GROW"
               )] %>% 
  cor(use="pairwise.complete.obs") %>% 
  corrplot(method = 'square', order = 'AOE', addCoef.col = 'black', 
           cl.pos = 'n', col = COL2('BrBG'), type = 'lower', number.cex=0.5)

vitrat_fim <- vitrat_tab2[,c("YEAR","DIA_GROW","SAWVOL_GROW","park")]

bide_fim
bio_fim
div_fim
dowo_fim
invs_fim     # not much variation
stastr_tab2  # not much variation
tp_ft_fim    # potentially but have to explore the forest type thing
tpa_own_fim  # similar but better: there"re less categories at ownership type
tpa_sc_fim   # similar but lot's of size classes
tpa_sp_fim   # lots of species too
tpa_fim
vitrat_fim

big_vars <- full_join(bide_fim, bio_fim, by = c("YEAR", "park"))
big_vars <- full_join(big_vars, div_fim, by = c("YEAR", "park"))
big_vars <- full_join(big_vars, tpa_fim, by = c("YEAR", "park"))
big_vars <- full_join(big_vars, vitrat_fim, by = c("YEAR", "park"))
                      
big_vars <- big_vars %>% relocate("park")

big_vars2 <- big_vars
big_vars2[sapply(big_vars2, is.infinite)] <- NA
big_vars2[,-c(1:2)] %>% 
  cor(use="pairwise.complete.obs") %>% 
  corrplot(method = 'square', order = 'alphabet', addCoef.col = 'black', 
           cl.pos = 'n', col = COL2('BrBG'), type = 'lower', number.cex=0.5)
big_vars2 %>% colnames()
big_vars2[,-c(1:2,4,9, 12)] %>% 
  cor(use="pairwise.complete.obs") %>% 
  corrplot(method = 'square', order = 'alphabet', addCoef.col = 'black', 
           cl.pos = 'n', col = COL2('BrBG'), type = 'lower', number.cex=0.5)

big_vars_fim <- big_vars2[,-c(4,9,12)]

left_join(big_vars_fim , local_cors_p, by = "park") %>% 
  select(-park) %>% 
  cor(use="pairwise.complete.obs") %>% 
  corrplot(method = 'square', addCoef.col = 'black', 
           cl.pos = 'n', col = COL2('BrBG'), type = 'lower', number.cex=0.5)













years <- c(2001,2004, 2006, 2008, 2011, 2013, 2016, 2019)

# Choose buffer extends and if the buffer is around the park area or the sites
# ext_b <- "park"    ;     
ext_b <- "site"

if (ext_b == "site") {
  buffers <- c(50, 150, 300)  ## distance is in meters!!!
}

if (ext_b == "park") {
  buffers <- c(100, 500, 1000, 2000)  ## distance is in meters!!!
}

buffers_n <- gsub("\\+", "", as.character(buffers))

nsite_pk <- read_csv(file = "data/nsite_pk.csv") %>% 
  pull() %>% 
  as.vector()


if(ext_b == "park") {
  for(i in 11:length(parks)){ # ACAD, MABI, MIMA, MORR, SAGA, SARA, SAIR, VAMA, WEFA
    print(parks[i])
    #for(b in 1:length(buffers)){
      name4 <- glue("{parks[i]}_buf{buffers_n[b]}")
      path_export3 <- glue("data/park_raster/{parks[i]}")
      buf <- read_rds(file = glue("{path_export3}/{name4}.rds"))
      dbclip1 <- clipFIA(db, mask = st_sf(buf))
      name_exp <- glue("{parks[i]}_buf{buffers_n[b]}_fiadb")
      write_rds(dbclip1, file = "data/FIA/processed/{name_exp}.rds")
      assign(name_exp, dbclip1)
    #}
  }
}

if(ext_b == "site") {
  for(i in 1:length(parks)){
    print(parks[i])
    for(b in 1:length(buffers)){
      for(s in 1:sites_n){
        s2 <- as.character(s)
        if(nchar(s2) < 2){s2 <- glue("0{s2}")}
        name4 <- glue("{parks[i]}_buf{buffers_n[b]}_site{s2}")
        path_export3 <- glue("data/park_raster/{parks[i]}")
        dbclip1 <- clipFIA(db, mask = st_sf(buf))
        write_rds(dbclip1, file = "data/FIA/processed/{parks[i]}_buf{buffers_n[b]}_site{s2}_fiadb.rds}")
      }
    }  
  }
}

dbclip1 <- clipFIA(db, mask = st_sf(bufs1100))
dbclip2 <- clipFIA(db, mask = buf)


# Check spatial coverage of plots held in the database
plotFIA(db)

# start with the tree table
summary(db)
db2 <- dbclip1$TREE
db3 <- dbclip1[[c("PLOT")]]

db3 <- db3 %>% 
  dplyr::select(INVYR, STATECD, UNITCD, COUNTYCD, PLOT, LAT, LON)

db2 <- left_join(db2, db3, by = c("INVYR", "STATECD", "UNITCD", "COUNTYCD", "PLOT"))

spdf <- SpatialPointsDataFrame(coords = db2[, c("LON","LAT")], data = db2[, c("LON","LAT")],
                               proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
spdf$TREE <- db2$TREE

plot(spdf)

ras <- rast(df2[,c('LON','LAT','TREE')], type = "xyz")

star <- st_as_stars(spdf)
plot(star)

df2 <- db2[1:5000, ]
b <- rasterFromXYZ(df2)
plot(b)

points <- SpatialPoints(df2[,c('LON','LAT')], df2[,c('TREE')])
pixels <- SpatialPixelsDataFrame(points, tolerance = 0.916421, points@data)
raster <- raster(pixels[,'TREE'])


library("raster")
r <- raster::raster(spdf)

r2 <- terra::rasterize(db2)

library(sp)
library(sf)
#> Linking to GEOS 3.6.1, GDAL 2.2.3, PROJ 4.9.3
library(dplyr, warn.conflicts = F)
library(ggmap, quietly = T)
meuse <- st_as_sf(spdf,coords = 1:2)

meuse_map <- get_stamenmap(
  bbox = unname(st_bbox(meuse)),
  zoom = 5, maptype = 'toner-lite', source = 'stamen'
) %>% ggmap()

meuse_map + 
  geom_sf(
    data = meuse, 
    aes(color = TREE), 
    #color = 'red', 
    alpha = 0.5,
    show.legend = 'point', inherit.aes = F
  )

ggplot() + 
  geom_sf(data = meuse, aes(colour = TREE)) 

spdf %>% 
  ggplot(aes(LON, LAT,  colour = -TREE)) +
  geom_point() 

db$TREE$TREE
# More explicity select only the more recent data
riMR <- clipFIA(db, mostRecent = TRUE)

plotFIA(riMR)

## Select a County 
kc <- countiesRI[2,] ## SF Multipolygon object

## Subset the data
riKC <- clipFIA(fiaRI, mask = kc, mostRecent = FALSE)

## Most recent subset, within Kent County
riKC <- clipFIA(fiaRI, mask = kc)

## TPA & BAA for the most recent inventory year
tpaRI_MR <- tpa(riMR)

## All Inventory Years Available (i.e., returns a time series)
tpaRI <- tpa(fiaRI)







