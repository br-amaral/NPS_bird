library(tidyverse)
# Define your list of packages
my_packages <- read_csv("data/used_r_packages.csv")  %>% pull(package)
# Get a named list containing detailed metadata for each package
package_info <- lapply(my_packages, packageDescription)
all_installed <- installed.packages()
info_table <- all_installed[all_installed[, "Package"] %in% my_packages,  c("Package","Version")]

info_table
info_table <- as.data.frame(info_table)

`%!in%` <- Negate(`%in%`)

my_local_packages <- my_packages[which(my_packages %!in% info_table$Package)]
write_rds(my_local_packages, file  = "data/my_local_packages.rds")

my_local_packages <- read_rds( file  = "data/my_local_packages.rds")

package_info <- lapply(my_local_packages, packageDescription)
all_installed <- installed.packages()
info_table <- all_installed[all_installed[, "Package"] %in% my_local_packages,  c("Package","Version")]

info_table <- as.data.frame(info_table)
info_table

missing_for_office <- my_local_packages[which(my_local_packages %!in% info_table$Package)]
missing_for_office

write_rds(missing_for_office, file  = "data/missing_for_office.rds")
