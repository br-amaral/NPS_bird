library(tidyverse)
# Define your list of packages
my_packages <- read_csv("data/used_r_packages.csv")  %>% pull(package)
# Get a named list containing detailed metadata for each package
package_info <- lapply(my_packages, packageDescription)
all_installed <- installed.packages()
info_table <- all_installed[all_installed[, "Package"] %in% my_packages,  "Version"]

info_table
as.data.frame(info_table)
