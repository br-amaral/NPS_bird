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

# Print script file name
context <- rstudioapi::getSourceEditorContext()
cat("\n", "\n", "\n", 'Current script: ', basename(context[[2]]), "\n", "\n", "\n", "\n")

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
                filter( !is.na(value))
  
  ggplot(plot_data, aes(x = Point_Name, y = value, group = park)) +
    geom_point(alpha = 0.6, size = 1.5) +
    theme_minimal() +
    facet_wrap(park~scale, scales = "free")
}

# Create individual plots
p1_individual <- create_individual_plot(tree_density_long, 
                                       "Tree Density", 
                                       "Density (stems/ha)")

ggsave("figures/plot_density_scalesX.svg", plot = last_plot(), device = "png", width = 6, height = 9)
