#? *********************************************************************************
#? -------------------------------   plot_xcovs.R   --------------------------------
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
library(conflicted)
library(glue)
library(purrr)
library(tidyverse)

conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::filter)
# conflicts_prefer(scales::alpha)

#! Make functions --------------------------------------
colanmes <- colnames
lenght <- length
`%!in%` <- Negate(`%in%`)

#! Source code -----------------------------------------

#! Import data -----------------------------------------
## file paths
COV_SCA_PATH <- "data/out/raw_x_covs.rds"

## read files
X <- read_rds(COV_SCA_PATH)

# Based on your previous code, you likely have these patterns:
# Option 1: Using matches() with regex (recommended)
covariate_groups <- list(
  tree_density = select(X, starts_with("treeden")),
  basal_area = select(X, matches("^BA_m2ha_(park|coun|site)")),
  conifer_BA = select(X, starts_with("BA_m2ha_perc_con")),
  late_successional = select(X, starts_with("BA_m2ha_perc_la")),
  shrub_BA = select(X, starts_with("shrub_"))
)

# Check what's in each group
map(covariate_groups, names)

# Function to reshape and plot covariate groups
reshape_and_plot <- function(data, group_name) {
  
  # Get columns for this group
  group_data <- data %>%
    dplyr::select(park, Point_Name, all_of(names(covariate_groups[[group_name]]))) %>%
    
    # Reshape to long format
    pivot_longer(
      cols = -c(park, Point_Name),
      names_to = "variable",
      values_to = "value"
    ) %>%
    
    # Extract scale from variable name
    mutate(
      scale = case_when(
        str_detect(variable, "_site") ~ "Site",
        str_detect(variable, "_park") ~ "Park", 
        str_detect(variable, "_coun") ~ "County"
      ),
      scale = factor(scale, levels = c("Site", "Park", "County"))
    ) %>%
    
    # Remove rows with missing scale (shouldn't happen but safety check)
    filter(!is.na(scale))
  
  return(group_data)
}

# Apply to each group
tree_density_long <- reshape_and_plot(X, "tree_density")
basal_area_long <- reshape_and_plot(X, "basal_area")
conifer_BA_long <- reshape_and_plot(X, "conifer_BA")
late_successional_long <- reshape_and_plot(X, "late_successional")
shrub_BA_long <- reshape_and_plot(X, "shrub_BA")

# Function for individual point plots
create_individual_plot <- function(data, title, y_label) {
  
  plot_data <- data  %>% 
                filter(!is.na(value))
  
  ggplot(plot_data, aes(x = Point_Name, y = value, group = park)) +
    geom_point(alpha = 0.6, size = 1.5) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 3),
          plot.title = element_text(hjust = 0.5)) +
    facet_wrap(park~scale, scales = "free_x", ncol = 3) +
    labs(title = title, y = y_label)
} 

# Create individual plots
create_individual_plot(tree_density_long, 
                        "Tree Density", 
                        "Density (stems/ha)")

ggsave("figures/test_scalesX.png", plot = last_plot(), device = "png", width = 6, height = 18)

# List of reshaped data and plot titles
covariate_plots <- list(
  tree_density = list(data = tree_density_long, title = "Tree Density", ylab = "Density (stems/ha)"),
  basal_area = list(data = basal_area_long, title = "Basal Area", ylab = "Basal Area (m2/ha)"),
  conifer_BA = list(data = conifer_BA_long, title = "Conifer Basal Area %", ylab = "Conifer BA (%)"),
  late_successional = list(data = late_successional_long, title = "Late Successional BA %", ylab = "Late Successional BA (%)"),
  shrub_BA = list(data = shrub_BA_long, title = "Shrub Basal Area", ylab = "Shrub BA")
)

# Loop to create and save each plot
for (cov_name in names(covariate_plots)) {
  plot_obj <- create_individual_plot(
    covariate_plots[[cov_name]]$data,
    covariate_plots[[cov_name]]$title,
    covariate_plots[[cov_name]]$ylab
  )
  cat(cov_name)
  ggsave(
    filename = glue("figures/plot_{cov_name}_scalesX.png"),
    plot = plot_obj,
    device = "png",
    width = 6,
    height = 18
  )
}

## add correlation between scales
library(GGally)

tree_density_wide <- tree_density_long %>%
  select(Point_Name, scale, value) %>%
  distinct() %>% 
  pivot_wider(names_from = scale, values_from = value)  %>% 
  drop_na()

ggpairs(
  tree_density_wide %>% select(Site, Park, County),
  title = "Correlation of Tree Density Across Scales (All Parks)",
  upper = list(continuous = wrap("cor", digits = 2, stars = FALSE))
) + theme_bw() + theme(plot.title = element_text(hjust = 0.5))

basal_area_wide <- basal_area_long %>%
  select(Point_Name, scale, value) %>%
  distinct() %>% 
  pivot_wider(names_from = scale, values_from = value)  %>% 
  drop_na()

ggpairs(
  basal_area_wide %>% select(Site, Park, County),
  title = "Correlation of Tree Basal Area Across Scales (All Parks)",
  upper = list(continuous = wrap("cor", digits = 2, stars = FALSE))
) + theme_bw() + theme(plot.title = element_text(hjust = 0.5))

late_successional_wide <- late_successional_long %>%
  select(Point_Name, scale, value) %>%
  distinct() %>% 
  pivot_wider(names_from = scale, values_from = value)  %>% 
  drop_na()

ggpairs(
  late_successional_wide %>% select(Site, Park, County),
  title = "Correlation of Late Success. Tree Basal Area Accross Scales (All Parks)",
  upper = list(continuous = wrap("cor", digits = 2, stars = FALSE))
) + theme_bw() + theme(plot.title = element_text(hjust = 0.5))

conifer_BA_wide <- conifer_BA_long %>%
  select(Point_Name, scale, value) %>%
  distinct() %>% 
  pivot_wider(names_from = scale, values_from = value)  %>% 
  drop_na()

ggpairs(
  conifer_BA_wide %>% select(Site, Park, County),
  title = "Correlation of Conifer Tree Basal Area Accross Scales (All Parks)",
  upper = list(continuous = wrap("cor", digits = 2, stars = FALSE))
) + theme_bw() + theme(plot.title = element_text(hjust = 0.5))

shrub_BA_wide <- shrub_BA_long %>%
  select(Point_Name, scale, value) %>%
  distinct() %>% 
  pivot_wider(names_from = scale, values_from = value)  %>% 
  drop_na()

ggpairs(
  shrub_BA_wide %>% select(Site, Park, County),
  title = "Correlation of Shrub Accross Scales (All Parks)",
  upper = list(continuous = wrap("cor", digits = 2, stars = FALSE))
) + theme_bw() + theme(plot.title = element_text(hjust = 0.5))
