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
#  renv::restore()

# Installed new packages?
#  renv::snapshot()

# detach packages and clear workspace
if(!require(freshr)){install.packages('freshr')}
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
res_mod_file <- "BTNW_step2_output_2025_08_03run1"

## read files
res_mod <- read_rds(glue("data/model_res/{res_mod_file}.rds"))

MCMCplot(res_mod,
         main = res_mod_file,
         params = "beta",
         ref_ovl = TRUE)

names(res_mod)

res_mod$samples  %>% str()
