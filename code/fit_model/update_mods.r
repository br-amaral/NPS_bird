library(tidyverse)
library(glue)
library(jagsUI) 
library(MCMCvis)

COEF_TABLE_PATH <- "code/fit_model/mod_key.csv"
if(substr(getwd(), 1, 3) == "/Us") {direc <- "local"} else {direc <- "hpc"}

## read files
coef_update <- read_csv(COEF_TABLE_PATH) %>%
        filter(run == "yes") %>% 
        filter(step == 3) %>% 
         mutate(AOU_Code = substr(result, 1, 4)) #%>% 
        # filter(AOU_Code %in% c("BLBW", "SCTA", "YBSA", "WBNU", "VEER", "REVI",
        #                         "OVEN", "HAWO", "DOWO", "BTNW", "BRCR", "BAWW"))

cat("Total rows in mod_key.csv:", nrow(coef_update), "\n")

for(ii in 1:nrow(coef_update)){
    
    cat(paste("Processing", ii, "of", nrow(coef_update), ":", coef_update$result[ii], "\n"))
    
    # Force garbage collection before each iteration
    gc()
    
    tryCatch({
        # Load model
        test_file <- read_rds(glue("data/model_res/{coef_update$result[ii]}.rds"))
        
        # Check Rhat using MCMCvis
        rhat_summary <- MCMCsummary(test_file, round = 3)
        max_rhat <- max(rhat_summary$Rhat, na.rm = TRUE)
        cat(paste("Max Rhat:", max_rhat, "\n"))
        
        # Check which parameters have high Rhat
        high_rhat <- rhat_summary[rhat_summary$Rhat > 1.1, ]
        
        if(nrow(high_rhat) > 0) {
            cat(paste("Found", nrow(high_rhat), "parameters with Rhat > 1.1\n"))
            cat("Updating model with 50,000 additional iterations...\n")
            
            # Update with fewer iterations to avoid memory issues
            model_updated <- update(test_file, n.iter = 70000)  # Reduced from 80000
            
            # Check convergence again
            rhat_summary_up <- MCMCsummary(model_updated, round = 3)
            max_rhat_up <- max(rhat_summary_up$Rhat, na.rm = TRUE)
            cat(paste("Updated Max Rhat:", max_rhat_up, "\n"))
            
            # Save updated model
            write_rds(model_updated, file = glue("data/model_res/{coef_update$result[ii]}_updated.rds"))
            cat("Updated model saved\n")
            
            # Clean up memory
            rm(model_updated, rhat_summary_up)
            
        } else {
            cat("All parameters converged (Rhat <= 1.1)\n")
            cat("No update needed\n")
        }
        
        # Clean up after each iteration
        rm(test_file, rhat_summary, high_rhat)
        gc()
        
    }, error = function(e) {
        cat(paste("Error processing", coef_update$result[ii], ":", e$message, "\n"))
    })
    
    cat("---\n")
}

cat("All models processed!\n")