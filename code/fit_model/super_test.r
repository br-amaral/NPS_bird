# super test anbd coefficients!

# script to get the results from step 1, step 2 all sca old, and step 2 new res and compare them
# het all coeffient and values to plot so I dont have to look at the .rds model results to make any figures

freshr::freshr()
#
# Load packages -------------------------------------------------------------------
library(conflicted)
library(tidyverse)
library(glue)
library(MCMCvis)
library(rjags)
library(BayesPostEst)
#
conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::filter)
# conflicts_prefer(scales::alpha)
#
# Make functions ------------------------------------------------------------------
colanmes <- colnames
lenght <- length
`%!in%` <- Negate(`%in%`)

if(substr(getwd(), 1, 3) == "/Us") {direc <- "local"} else {direc <- "hpc"}

if(direc == "local"){
    master_tab <- read_csv("/Users/bamaral/Library/CloudStorage/OneDrive-MichiganStateUniversity/GitHubOne/NPS_bird_copy/code/fit_model/mod_key.csv") %>%
            #filter(run == "yes") %>% 
            filter(step %in% c(2,3,4)) %>% 
            distinct() %>% 
            arrange(AOU_Code, step)

    } else {master_tab <- read_csv("code/fit_model/mod_key.csv") %>%
            #filter(run == "yes") %>% 
            filter(step %in% c(2,3,4)) %>% 
            distinct()%>% 
            arrange(AOU_Code, step)
    }

master_tab <- master_tab %>% filter(AOU_Code != "BCCH")

coef_tab <- function(row_index){
    sps_loop <- master_tab[row_index,]

    (spslp <- sps_loop$AOU_Code)
    (steplp <- sps_loop$step)
    (resultlp <- sps_loop$result)
    (scalelp <- sps_loop$select)

    res_mod <- read_rds(glue("data/model_res/{resultlp}.rds"))

    scales_names <- grep("^scales_", colnames(res_mod[[1]]), value = TRUE) 
    (all_params <- c("mu.alpha0", "mu.beta0", "beta", #"beta_int", 
                "alpha", scales_names))

    coef_tablp <- MCMCsummary(res_mod,
                              params = all_params,
                              probs = c(0.1, 0.5, 0.9),  # 80% credible intervals (10%, 50%, 90%)
                              round = 2)

    # get coefs names
     coef_tablp1 <- coef_tablp %>%
        rownames_to_column("coef") %>%
        # Add species and step information
        mutate(
            sps = spslp,
            step = steplp,
            result_file = resultlp,
            select_file = scalelp,
            # Check if 80% CI overlaps zero
            overlap_zero = case_when(
              # Both bounds positive - doesn't contain zero (significant positive)
              `10%` > 0 & `90%` > 0 ~ "no",
              # Both bounds negative - doesn't contain zero (significant negative)  
              `10%` < 0 & `90%` < 0 ~ "no",
              # Lower bound ≤ 0 AND upper bound ≥ 0 - contains zero (not significant)
              `10%` <= 0 & `90%` >= 0 ~ "yes",
              `90%` <= 0 & `10%` >= 0 ~ "yes",
              # Edge case: shouldn't happen but safety net
              TRUE ~ "unknown"
            ),
            # Additional helper columns for interpretation
            effect_direction = case_when(
                `10%` > 0 & `90%` > 0 ~ "positive",
                `10%` < 0 & `90%` < 0 ~ "negative",
                `10%` <= 0 & `90%` >= 0 ~ "non_significant",
                TRUE ~ "unclear"
            ),
            is_significant_80 = overlap_zero == "no"
        )

        # Get selected scales from the scale selection model
        if(str_detect(resultlp, "step2") == TRUE) {  # Only for step 2 models
        
            sca_mod <- read_rds(glue("data/model_res/{scalelp}.rds"))

            sca_mod1 <- sca_mod %>% 
                          mutate(coef = as.character(glue("{substr(betas, 1, 4)}[{substr(betas, 5, 5)}]"))) %>% 
                          select(coef, sca_sel, sca1, sca2, sca3)
            
            # Add selected scale info to coefficient table
            coef_tablp2 <- coef_tablp1 %>%
                              relocate(sps, step,
                                       coef, mean, sd, `10%`, `50%`, `90%`, Rhat, n.eff,
                                       overlap_zero, effect_direction, is_significant_80)  %>% 
                              as_tibble() %>% 
                              left_join(., sca_mod1, by = "coef")
        } else {coef_tablp2 <- coef_tablp1}
    
    return(coef_tablp2)
}

# Initialize empty tibble outside the function
coef_fim <- tibble()

for(ii in 1:nrow(master_tab)){
# Check if all three strings are equal (different steps and models for the same species)
  if(substr(master_tab$result[ii], 1, 4) == master_tab$AOU_Code[ii] && 
      master_tab$AOU_Code[ii] == substr(master_tab$select[ii], 1, 4)) {
      # All three are equal
    } else {
      stop(glue("\n\n\n error on {master_tab$result[ii]} on row {ii}\n\n\n"))
    }
  print(ii)
  sps_result <- coef_tab(ii)
  coef_fim <- bind_rows(coef_fim, sps_result)
}

write_rds(coef_fim, file = "data/out/super_test_table.rds")

# coef_fim <- read_rds(file = "data/out/super_test_table.rds")
