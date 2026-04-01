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
library(patchwork)

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

files <- list.files(
  path    = "data/ana_file/",
  pattern = "_step1_jagsdata_2",
  full.names = FALSE   # returns full file paths, not just names
)

most_recent_per_sps <- tibble(path = files) %>%
  mutate(
    file = basename(path),
    sps  = str_extract(file, "^[A-Z]+")   # grabs leading species code
  ) %>%
  group_by(sps) %>%
  slice_max(order_by = file, n = 1) %>%   # alphabetical max = most recent date
  ungroup()

# Pull as named vector if needed
file_list <- set_names(most_recent_per_sps$path, most_recent_per_sps$sps)

for(ii in 1:length(file_list)){
    sps_jdat <- read_rds(glue("data/ana_file/{file_list[ii]}"))

    sps_jdat2 <- cbind(sps_jdat$y, sps_jdat$Xp)

    colnames(sps_jdat2)[7] <- "pksize"

    sps_jdat2 <- as_tibble(sps_jdat2)

    sps_jdat2 <- sps_jdat2  %>% 
                    mutate(sps = names(file_list[ii]))

    if(ii == 1){jags_dat <- sps_jdat2} else {jags_dat <- rbind(jags_dat, sps_jdat2)}
    print(ii)
}

for(ii in 1:nrow(res_key)){
    sps_jdat <- read_rds(res_key$data[ii])

    sps_jdat2 <- cbind(sps_jdat$y, sps_jdat$Xp)

    colnames(sps_jdat2)[7] <- "pksize"

    sps_jdat2 <- as_tibble(sps_jdat2)

    sps_jdat2 <- sps_jdat2  %>% 
                    mutate(sps = res_key$AOU_Code[ii])

    if(ii == 1){jags_dat <- sps_jdat2} else {jags_dat <- rbind(jags_dat, sps_jdat2)}
    print(ii)
}

jags_dat %>% filter(sps == "BAWW") %>% select(bird_detec) %>% is.na() %>% table()

jags_dat2 <- jags_dat %>% 
                #select(-site_n, -interval_n) %>% 
                arrange(sps, parkey, year_n) %>%
                #distinct() %>%
                filter(bird_detec == 1) 

jags_dat2 %>% filter(sps == "BAWW") %>% select(bird_detec) %>% table()

ggplot(jags_dat2) +
    geom_point(aes(x = sps, y = pksize, color = as.character(parkey)), 
               position = position_jitter(width = 0.2))

ggplot(jags_dat2 %>% select(pksize, parkey) %>% distinct() %>% arrange(parkey)) +
    geom_point(aes(y = pksize, x = as.character(parkey)))

write_rds(jags_dat2, file = "data/out/all_sps_data.rds")

sps_order <- read_csv(file = "data/src/sps_order.csv") %>% 
                pull(Aou_code)

key_site <- read_rds(file = "data/key_park.rds") %>% 
                filter(parks %!in% c("ELRO", "ACAD", "SAIR")) %>% 
                mutate(parkey = seq(1, nrow(.),1)) %>% 
                relocate(parkey)

jags_dat3 <- jags_dat2 %>% 
                left_join(., key_site, by = "parkey")

table(jags_dat3$parks); table(jags_dat3$parkey)

jags_dat4 <- jags_dat3 %>%
                filter(sps != "BCCH") %>% 
                group_by(parks, park_long) %>%
                mutate(max_year = max(year_n)) %>%
                ungroup() %>%
                select(-c(year_n, Year, parkey, pksize, network, id )) %>%
                group_by(parks, sps) %>%
                summarise(bird_detec = sum(bird_detec, na.rm = TRUE), .groups = "drop") %>%
                group_by(parks) %>%
                mutate(Detections = sum(bird_detec),
                       `Species Richness` = sum(bird_detec > 0)) %>%
                ungroup() %>%
                pivot_wider(names_from = sps, values_from = bird_detec)%>%
                relocate(all_of(sps_order), .after= `Species Richness`)

jags_dat4$BAWW %>% sum(na.rm = T)

jags_dat4


# Evenly-spaced breaks on the log1p bar, labeled with real values
bar_breaks_real <- c(1, 5, 20, 50, 150, 350, 700, 1200)

phylo_order2 <- read_rds(file = "data/src/sps_phylo_order.rds")  %>% 
                rename(sps = Aou_code) %>% 
                mutate(sps_name = factor(sps_name, levels = sps_name))  # Use current order as levels

park_size_ord <- read_rds("data/park_size.rds") %>% 
                    arrange(area) %>% 
                    mutate(park_ord = seq(1, nrow(.),1))

park_levels <- park_size_ord %>%
  arrange(park_ord) %>%
  pull(park)   # park_ord 1 = bottom, 9 = top after fct_rev()

shared_scale <- function() {
  scale_fill_distiller(
    palette   = "Oranges",
    direction = 1,
    na.value  = "grey92",
    limits    = c(0, log1p(1200)),               # linear scale on log1p values
    breaks    = log1p(bar_breaks_real),          # where ticks sit on bar
    labels    = bar_breaks_real,                 # but label with real numbers
    name      = "Detections",
    guide = guide_colorbar(
        barheight    = unit(8, "cm"),            # taller bar
        barwidth     = unit(1.2, "cm"),          # wider bar — increase this
        ticks        = TRUE,
        frame.colour = "grey40",
        ticks.colour = "grey40",
        label.theme  = element_text(size = 12)   # tick label size
    )
  )
}

# --- p_sum ---
p_sum <- jags_dat4 %>%
  select(parks, Richness = `Species Richness`, Detections) %>%
  pivot_longer(-parks) %>%
  mutate(
    name  = factor(name, levels = c("Richness", "Detections")),
    parks = factor(parks, levels = park_levels)          # <-- ordered
  ) %>%
  ggplot(aes(x = name, y = fct_rev(parks), fill = log1p(value))) +
  geom_tile(color = "white", linewidth = 1.2) +
  geom_rect(aes(xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf),
            fill=NA, color="black", linewidth=0.7, inherit.aes=FALSE) +
  geom_text(aes(label = value), size = 4.3) +
  shared_scale() +
  theme_minimal() +
  theme(
    panel.grid    = element_blank(),
    axis.title.y  = element_text(size = 15),
    axis.text.y   = element_text(hjust = 1, size = 12),
    axis.text.x   = element_text(angle = 35, vjust = 1, hjust = 1, size = 13, face = "bold"),
    legend.position = "none"
  ) +
  labs(x = NULL, y = "Park \n")

# --- p_sps ---
p_sps <- jags_dat4 %>%
  select(parks, all_of(sps_order)) %>%
  pivot_longer(-parks, names_to = "sps", values_to = "det") %>%
  left_join(phylo_order2, by = "sps") %>%
  mutate(
    sps_name = factor(sps_name, levels = levels(phylo_order2$sps_name)),
    parks    = factor(parks, levels = park_levels)       # <-- same ordered factor
  ) %>%
  ggplot(aes(x = sps_name, y = fct_rev(parks), fill = log1p(det))) +
  geom_tile(color = "white") +
  geom_text(aes(label = det), size = 4.3, na.rm = TRUE) +
  shared_scale() +
  geom_rect(aes(xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf),
            fill=NA, color="black", linewidth=0.7, inherit.aes=FALSE) +
  theme_minimal() +
  theme(
    panel.grid      = element_blank(),
    axis.text.x     = element_text(angle = 35, vjust = 1, hjust = 1, size = 12),
    legend.position = "right",
    axis.ticks.y    = element_blank(),
    axis.text.y     = element_blank()
  ) +
  labs(x = NULL, y = NULL)

p_sum <- p_sum + theme(plot.margin = margin(5, 2, 5, 5))
p_sps <- p_sps + theme(plot.margin = margin(5, 5, 5, 2))

(p_combined <- (p_sum | p_sps) +
  plot_layout(widths = c(2.5, 17)) &
  theme(legend.title = element_blank()))

ggsave("figures/heatmap_detecs.pdf", plot = p_combined,
       width = 13, height = 6.8, units = "in", dpi = 1000)

parkey <- key_site %>% 
            select(parks, park_long) %>% 
            mutate(short_name = str_extract(park_long, "^.*(?=\\s+National)") %>% str_trim())

# Create short names with line breaks where needed
park_labels <- park_names %>%
  mutate(park_short = str_extract(park_long, "^.*(?=\\s+National)") %>% str_trim()) %>%
  select(parks, park_short)

# Custom labels with manual line breaks (named vector: name = parks code)
park_label_vec <- c(
  HOFR = "Home Of Franklin\nD Roosevelt",
  MABI = "Marsh-Billings-\nRockefeller",
  MIMA = "Minute Man",
  MORR = "Morristown",
  SAGA = "Saint-Gaudens",
  SARA = "Saratoga",
  VAMA = "Vanderbilt\nMansion",
  WEFA = "Weir Farm"
)

p_sum2 <- p_sum + scale_y_discrete(labels = park_label_vec) 

(p_combined2 <- (p_sum2 | p_sps) +
  plot_layout(widths = c(2.5, 17)) &
  theme(legend.title = element_blank()))

ggsave("figures/heatmap_detecs.pdf", plot = p_combined2,
       width = 14, height = 6.8, units = "in", dpi = 1000)


#
## file paths
XDAT_PATH <- "data/X.rds"

## read files
X10 <- read_rds(file = XDAT_PATH)

X_covs <- X10 %>% 
            filter(interval_n == 1) %>% 
            dplyr::select(Point_Name,
                          BA_m2ha_Conifer_coun,
                          BA_m2ha_Conifer_park,
                          BA_m2ha_Conifer_site,
                          BA_m2ha_coun,
                          BA_m2ha_park,
                          BA_m2ha_site,
                          BA_m2ha_large_coun,
                          BA_m2ha_large_park,
                          BA_m2ha_large_site,
                          shrub_cov_coun,
                          shrub_avg_cov_park,
                          shrub_avg_cov_site,
                          treeden_ha_coun,
                          treeden_ha_park,
                          treeden_ha_site
                          ) %>% 
            mutate(#AOU_code = sps_loop2,
                   park = substr(Point_Name,1,4)) %>% 
            distinct()

#######################
# ── Updated labels with units & new order ─────────────────────────────────────
nice_labs_covs <- c(
  BA_m2ha         = "Basal area\n(m²/ha)",
  BA_m2ha_Conifer = "Conifer Basal Area\n(m²/ha)",
  BA_m2ha_large   = "Late succes.\nBasal Area\n(m²/ha)",
  shrub_cov       = "Shrub cover\n(% cover)",
  treeden_ha      = "Tree density\n(stems/ha)"
)

# ── Reorder cov_lookup & factor levels ────────────────────────────────────────
cov_lookup <- tribble(
  ~cov_label,       ~site_col,              ~park_col,              ~coun_col,
  "BA_m2ha",        "BA_m2ha_site",         "BA_m2ha_park",         "BA_m2ha_coun",
  "BA_m2ha_Conifer","BA_m2ha_Conifer_site", "BA_m2ha_Conifer_park", "BA_m2ha_Conifer_coun",
  "BA_m2ha_large",  "BA_m2ha_large_site",   "BA_m2ha_large_park",   "BA_m2ha_large_coun",
  "shrub_cov",      "shrub_avg_cov_site",   "shrub_avg_cov_park",   "shrub_cov_coun",
  "treeden_ha",     "treeden_ha_site",      "treeden_ha_park",      "treeden_ha_coun"
)

# ── Rebuild all_dat ────────────────────────────────────────────────────────────
site_dat <- map_dfr(seq_len(nrow(cov_lookup)), \(i)
  X_covs %>%
    select(park, value = all_of(cov_lookup$site_col[i])) %>%
    mutate(cov_label = cov_lookup$cov_label[i], scale = "Stand")
)
park_dat <- map_dfr(seq_len(nrow(cov_lookup)), \(i)
  X_covs %>%
    select(park, value = all_of(cov_lookup$park_col[i])) %>%
    distinct() %>%
    mutate(cov_label = cov_lookup$cov_label[i], scale = "Park")
)
coun_dat <- map_dfr(seq_len(nrow(cov_lookup)), \(i)
  X_covs %>%
    select(park, value = all_of(cov_lookup$coun_col[i])) %>%
    distinct() %>%
    mutate(cov_label = cov_lookup$cov_label[i], scale = "Region")
)

all_dat <- bind_rows(site_dat, park_dat, coun_dat) %>%
  mutate(
    park      = factor(park, levels = park_levels),
    scale     = factor(scale, levels = c("Stand", "Park", "Region")),
    cov_label = factor(cov_label, levels = cov_lookup$cov_label)   # order from lookup ✓
  )

# ── Plot ───────────────────────────────────────────────────────────────────────
p_covs <- ggplot(all_dat, aes(x = scale, y = value)) +
  geom_boxplot(
    data  = all_dat %>% filter(scale == "Stand"),
    width = 0.5, fill = "#e5f5e0", color = "#74c476",
    outlier.shape = NA
  ) +
  geom_jitter(
    data  = all_dat %>% filter(scale == "Stand"),
    width = 0.15, height = 0,
    alpha = 0.5, size = 1.2, color = "#74c476"
  ) +
  geom_point(
    data  = all_dat %>% filter(scale == "Park"),
    size  = 3.5, color = "#238b45", shape = 19
  ) +
  geom_point(
    data  = all_dat %>% filter(scale == "Region"),
    size  = 3.5, color = "#00441b", shape = 18
  ) +
  # NO scale_x_discrete(position = "top") → default bottom → LEFT after flip ✓
  facet_grid(
    park ~ cov_label,
    scales   = "free_x",
    switch   = "y",                               # park strips → LEFT ✓
    labeller = labeller(
      cov_label = as_labeller(nice_labs_covs),
      park      = as_labeller(park_label_vec)
    )
  ) +
  coord_flip() +
  theme_minimal() +
  theme(
    panel.grid.minor     = element_blank(),
    panel.grid.major.x   = element_line(color = "grey92"),
    panel.border         = element_rect(color = "grey70", fill = NA, linewidth = 0.5),
    strip.text.x         = element_text(face = "bold", size = 15, lineheight = 0.85), # covariate title
    strip.text.y.left    = element_text(
                             angle = 90, face = "bold", size = 12.5, # park name
                             lineheight = 0.85
                           ),
    strip.placement      = "outside",             # strips outside axis labels ✓
    axis.text.y          = element_text(size = 12, hjust = 1),  # Stand/Park/Region
    axis.text.x          = element_text(size = 13),
    axis.title           = element_blank(),
    axis.ticks.x = element_line(color = "grey40"),   # x only
    axis.ticks.y = element_line(color = "grey40"),   # y only
    axis.ticks.length.x = unit(0.2, "cm"),
    axis.ticks.length.y = unit(0.15, "cm")
  )

ggsave("figures/covariates_multiscale.pdf", plot = p_covs,
       width = 12, height = 14, units = "in", dpi = 1000)
