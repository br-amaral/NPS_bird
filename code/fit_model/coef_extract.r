#? *********************************************************************************
#? -------------------------------  coef_extract.r  --------------------------------
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
COEF_TABLE_PATH <- "data/mod_key.csv"

## read files
coef_path_file <- read_csv(COEF_TABLE_PATH) %>%
        filter(run == "yes") %>% 
        filter(step == 3) %>% 
        mutate(AOU_Code = substr(result, 1, 4))

for(ii in 1:nrow(coef_path_file)) {

    loop_sps <- substr(coef_path_file$result[ii], 1, 4)

    loop_run <- substr(coef_path_file$result[ii], nchar(coef_path_file$result[ii]) - 7, nchar(coef_path_file$result[ii]) - 4)

    quants <- ifelse((as.numeric(substr(loop_run, 4, 4)) %% 2 == 0) == TRUE, "25_75", "3_7")

     selec_files <- 
      list.files(path = file.path(getwd(),"data/model_res/"),
                                          pattern = "SCA_SEL_PARS",
                                          full.names = FALSE)  %>% 
                as_tibble() %>% 
                mutate(sps = substr(value, 1, 4)) %>% 
                filter(sps == loop_sps) %>% 
                filter(str_detect(value, quants)) %>%  # Filter for rows containing the quants text
                pull(value)

      if(lenght(selec_files) == 1) {coef_path_file$select[ii] <- selec_files}
      if(lenght(selec_files) == 2) {
        
        if(selec_files[1] %in% coef_path_file$select) {coef_path_file$select[ii] <- selec_files[2]}
        if(selec_files[1] %!in% coef_path_file$select) {coef_path_file$select[ii] <- selec_files[1]}
      
      }
}


samples_jags <- read_rds(glue("data/model_res/{coef_path_file$result[ii]}"))
beta_sca_names <- read_rds(glue("data/model_res/{coef_path_file$select[ii]}")) %>% 
      filter(overlap0 == "no") %>%
      add_row() %>%  # Add empty row for 3 alphas + 1 be4ta park size
      add_row() %>%  
      add_row() %>%  
      add_row()     

# Get summary with median and credible intervals
coef_summary <- MCMCsummary(samples_jags,
                           params = c("beta", "alpha"), #, "beta0", "alpha0"),  # specify parameters
                           probs = c(0.025, 0.5, 0.975),  # 2.5%, median, 97.5%
                           round = 3) %>% 
                    mutate(coef = rownames(.)) %>% 
                    as_tibble() %>% 
                    relocate(coef) %>%
                    mutate(coef = gsub("\\[|\\]", "", coef))

if((nrow(beta_sca_names)) != nrow(coef_summary)) {
      stop(glue("error in {coef_path_file$result[ii]}"))
      
      } else { coef_summary2 <- cbind(coef_summary, beta_sca_names) %>% 
                                    mutate(sps = substr(coef_path_file$result[ii], 1, 4))
      }

# Extract specific columns
median_estimates <- coef_summary$`50%`
lower_ci <- coef_summary$`2.5%`
upper_ci <- coef_summary$`97.5%`
