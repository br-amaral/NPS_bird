
freshr::freshr()

library(tidyverse)
library(jagsUI)
library(bayesplot)
library(glue)

step_number_define <- 2

if(substr(getwd(), 1, 3) == "/Us") {direc <- "local"} else {direc <- "hpc"}

if(direc == "local"){
    master_tab <- read_csv("/Users/bamaral/Library/CloudStorage/OneDrive-MichiganStateUniversity/GitHubOne/NPS_bird_copy/code/fit_model/mod_key.csv") %>%
            filter(run == "yes") %>% 
            filter(step %in% c(step_number_define)) %>% 
            distinct()

    dir_mod <- "/Users/bamaral/Library/CloudStorage/OneDrive-MichiganStateUniversity/GitHubOne/NPS_bird_copy/models/"
            
    } else {
        # This covers both HPC and mounted HPC directory
        master_tab <- read_csv("code/fit_model/mod_key.csv") %>%
            filter(run == "yes") %>% 
            filter(step %in% c(step_number_define)) %>% 
            distinct()
            
        dir_mod <- "models/"  # Relative path from current working directory
            
}

# ---- MCMC settings ----
ni <- 10000
nb <- 4000
nc <- 5
nt <- 2

paste('\n ************************************* \n \n \n   Running Models:', '\n',
      '  What? =', "Prior Predictive Check step 1", '\n',
      '  Number of sps =', nrow(master_tab), '\n',
      '  Total iterations =', nb + ni, '\n',
      '  Started running on =', Sys.time(),  '\n \n \n',
      '**************************************') %>% cat()

# ---- Parameters to monitor ----
parameters <- c("prop_occ", "total_detections",
                "psi", "p", "Z", "y_sim",
                "beta", "alpha",
                "mu.beta0","tau.beta0","mu.alpha0","tau.alpha0")

for (key_ite in 1:nrow(master_tab)){
    # key_ite <- 4
    tib_loop <- master_tab[key_ite, ]

    sps_loop <- tib_loop$AOU_Code
    date_step1 <- substr(tib_loop$result, 19, 28)

    SPS_DATA_PATH <- list.files('data/ana_file/', 
                                pattern = glue('{sps_loop}_step1_jagsdata'), 
                                ignore.case = TRUE) %>% 
                                tail(1)

    mod_name1 <- list.files(glue("{dir_mod}"), pattern = glue("^{sps_loop}"))

    mod_name <- glue("{dir_mod}{mod_name1}")

    ## Turn that model in the model with prior!!!

    # Read the file
    lines <- readLines(mod_name)

    # Find the line containing certain characters - DEBUG VERSION
    # First, let's see what lines contain "y[a,1]"
    debug_lines <- grep("y\\[a,1\\]", lines, value = TRUE)
    cat("Lines containing y[a,1]:\n")
    print(debug_lines)
    
    # find pattern
    target_pattern <- "y\\[a,1\\].*dbern"
    target_line <- grep(target_pattern, lines)
    cat("Simplified pattern found at:", target_line, "\n")
    
    # Insert new row after the found line
    new_row <- "    y_sim[a] ~ dbern(p[y[a,2], y[a,3], y[a,4], y[a,5]] * Z[y[a,2], y[a,3], y[a,4]])} # prior pred"

    if(length(target_line) > 0) {
      # Insert after the first match
      insert_pos <- target_line[1]
      new_lines <- c(lines[1:insert_pos], 
                     new_row, 
                     lines[(insert_pos + 1):length(lines)])
    }

    # Find start and end of the chunk to remove
    start_pattern <- "# Posterior predictive check - simulate new detection data"
    end_pattern <- "bpvalue <- step\\(fit\\.sim - fit\\.obs\\)"

    start_line <- grep(start_pattern, new_lines)
    end_line <- grep(end_pattern, new_lines)

    if(length(start_line) > 0 && length(end_line) > 0) {
      # Remove lines from start to end (inclusive)
      new_lines2 <- new_lines[-(start_line:end_line)]}

    # again!
        # Find start and end of the chunk to remove
    start_pattern <- "# Posterior predictive check - simulate new occupancy"
    end_pattern <- "Z\\.new\\[y2\\[b,2\\],y2\\[b,3\\],y2\\[b,4\\]\\] ~ dbern\\(psi\\[y2\\[b,2\\],y2\\[b,3\\],y2\\[b,4\\]\\]\\)"

    start_line <- grep(start_pattern, new_lines2)
    end_line <- grep(end_pattern, new_lines2)

    if(length(start_line) > 0 && length(end_line) > 0) {
      # Remove lines from start to end (inclusive)
      new_lines3 <- new_lines2[-(start_line:end_line)]}  

    # Write back to file
    writeLines(new_lines2, glue("{dir_mod}prior_{mod_name1}"))
    
    # ---- Prepare data for prior predictive run ----
    # Use the real covariates if you want to check priors conditional on covariate distribution.
    # Alternatively, simulate covariates with same scaling used in model fitting.
    jags_data <- read_rds(glue("data/ana_file/{SPS_DATA_PATH}"))
    
    y <- jags_data$y
    jags_data$y[,1] <- rep(NA_integer_, jags_data$nrowy)
    jags_data$y2[,1] <- rep(NA_integer_, jags_data$nrowy2)

    # Run model
    out_prior <- jags(data = jags_data,
                      inits = NULL,
                      parameters.to.save = parameters,
                      model.file =  glue("{dir_mod}prior_{mod_name1}"),
                      n.chains = nc,
                      n.iter = ni,
                      n.burnin = nb,
                      n.thin = nt,
                      parallel = TRUE)

    #summary(out_prior)
    
    write_rds(out_prior, file = glue("data/ana_file/prior_predictive_check_{sps_loop}_{date_step1}_step2.rds"))

    # Extract y_sim samples correctly from jagsUI object
    y_sim_samples <- out_prior$sims.list$y_sim  # Matrix: iterations x y_sim parameters
    # For bayesplot, you may need to transpose or subset
    # Check dimensions first
    cat("Dimensions of y_sim samples:", dim(y_sim_samples), "\n")
    cat("Dimensions of observed y:", dim(y), "\n")

    # Use the correct y_sim samples for posterior predictive check
    # Replace NAs with 0s in the observed data
    y_observed <- y[,1]
    y_observed[is.na(y_observed)] <- 0

    max_density <- max(max(density(y_observed)$y), 
                       max(density(as.vector(y_sim_samples))$y))  # Calculate max density from both observed and simulated data

    ppc_dens_overlay(y = as.vector(y_observed), 
                     yrep = y_sim_samples, # Use subset of posterior draws
                     size = 0.9) +
                     theme_bw() + 
                     ylim(0, max_density * 1.05) +                     
                     scale_x_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1), 
                                        labels = c(0, 0.25, 0.5, 0.75, 1),
                                        limits = c(-0.25, 1.25)) +
                     scale_color_manual(values = c("darkblue", "#915F6D"),
                                        name = "Data Type",
                                        labels = c("Observed", "Simulated")) +
                     labs(title = glue("Prior Predictive Check: Detection Data for {sps_loop}"),
                          subtitle = "Comparing observed vs simulated detection patterns",
                          x = "\n Detection/Non-detection", 
                          y = "Density\n") +
                     theme(
                       # Remove minor grid lines, keep only major
                       panel.grid.minor = element_blank(),
                       panel.grid.major = element_line(linewidth = 0.5, color = "gray85"),
                       # Center title and subtitle, increase font sizes
                       plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
                       plot.subtitle = element_text(size = 14, hjust = 0.5),
                       # Increase axis text and labels
                       axis.text = element_text(size = 12),
                       axis.title = element_text(size = 14, face = "bold"),
                       # Increase legend text and center legend title
                       legend.text = element_text(size = 12),
                       legend.title = element_text(size = 13, face = "bold", hjust = 0.5)
                     ) +
                     geom_vline(xintercept = 0, color = "black", linetype = "dashed", linewidth = 0.8) +
                     geom_vline(xintercept = 1, color = "black", linetype = "dashed", linewidth = 0.8)

    # Assign plot to variable for saving
    ppc_plot <- last_plot()
    
    # Save as SVG file
    svg_filename <- glue("figures/prior_predictive_check_{sps_loop}_{date_step1}_step2.svg")
    ggsave(filename = svg_filename, 
           plot = ppc_plot, 
           device = "svg",
           width = 10, 
           height = 7, 
           units = "in")
    
    cat(glue("Saved PriPC plot for {sps_loop} as: {svg_filename}\n"))

 }
