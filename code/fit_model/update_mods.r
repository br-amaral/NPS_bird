library(tidyverse)
library(glue)
library(jagsUI) 
library(MCMCvis)

COEF_TABLE_PATH <- "code/fit_model/mod_key.csv"
if(substr(getwd(), 1, 3) == "/Us") {direc <- "local"} else {direc <- "hpc"}
# if(direc == "local"){COEF_TABLE_PATH <- glue("/Users/bamaral/Library/CloudStorage/OneDrive-MichiganStateUniversity/GitHubOne/NPS_bird_copy/{COEF_TABLE_PATH}")}
# if(direc == "local"){COEF_TABLE_PATH <- glue("/Users/bamaral/Documents/GitHub/NPS_bird_copy/{COEF_TABLE_PATH}")}

## read files
coef_update <- read_csv(COEF_TABLE_PATH) %>%
        filter(run == "yes") %>% 
        filter(step == 3) %>% 
        mutate(AOU_Code = substr(result, 1, 4)) %>% 
        filter(AOU_Code %in% c("BLBW", "SCTA", "YBSA", "WBNU", "VEER", "REVI",
                                "OVEN", "HAWO", "DOWO", "BTNW", "BRCR", "BAWW"))

for(ii in 1:nrow(coef_update)){
    # Get samples from your jagsUI object

    test_file <- read_rds(glue("data/model_res/{coef_update$result[ii]}.rds"))
    samples <- test_file$samples

    # Check Rhat using MCMCvis
    rhat_summary <- MCMCsummary(test_file, round = 3)
    max_rhat <- max(rhat_summary$Rhat, na.rm = TRUE)
    print(paste("Max Rhat:", max_rhat))

    # Check which parameters have high Rhat
    high_rhat <- rhat_summary[rhat_summary$Rhat > 1.1, ]
    how_many <- 0
    
    test_file2 <- test_file

    if(nrow(high_rhat) > 0) {
      if(how_many > 0){test_file2 <- model_updated}
      model_updated <- update(test_file2, n.iter = 80000)
      how_many <- how_many + 1

      rhat_summary_up <- MCMCsummary(model_updated, round = 3)
      max_rhat_up <- max(rhat_summary_up$Rhat, na.rm = TRUE)
      high_rhat <- rhat_summary_up[rhat_summary_up$Rhat > 1.1, ]
    }

    write_rds(model_updated, file = glue("data/model_res/{coef_update$result[ii]}_updated{how_many}.rds"))
}