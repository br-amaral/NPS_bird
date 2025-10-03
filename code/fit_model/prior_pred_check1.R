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

    mod_name <- "/Users/bamaral/Library/CloudStorage/OneDrive-MichiganStateUniversity/GitHubOne/NPS_bird_copy/models/model_prior_pred.txt"
            
    } else {master_tab <- read_csv("code/fit_model/mod_key.csv") %>%
            filter(run == "yes") %>% 
            filter(step %in% c(step_number_define)) %>% 
            distinct()
            
    mod_name <- "/models/model_prior_pred.txt"
            
}

# ---- MCMC settings ----
ni <- 10000
nb <- 4000
nc <- 5
nt <- 2

paste('\n ************************************* \n \n \n   Running Models:', '\n',
      '  Test?', test, '\n',
      '  What? =', "Prior Predictive Check", '\n',
      '  Number of sps =', nrow(master_tab), '\n',
      '  Total iterations =', nburnin + niterations, '\n',
      '  Started running on =', Sys.time(),  '\n \n \n',
      '**************************************') %>% cat()

# ---- Parameters to monitor ----
parameters <- c("prop_occ", "total_detections",
                "psi", "p", "Z", "y_sim",
                "beta", "alpha",
                "scales_beta1","scales_beta2","scales_beta3","scales_beta4","scales_beta5",
                "mu.beta0","tau.beta0","mu.alpha0","tau.alpha0")

for (key_ite in 1:nrow(master_tab)){
    # key_ite <- 1
    tib_loop <- master_tab[key_ite, ]

    sps_loop <- tib_loop$AOU_Code
    date_step1 <- substr(tib_loop$result, 19, 28)

    SPS_DATA_PATH <- glue('data/ana_file/{sps_loop}_step1_jagsdata_{date_step1}.rds') 

    # ---- Prepare data for prior predictive run ----
    # Use the real covariates if you want to check priors conditional on covariate distribution.
    # Alternatively, simulate covariates with same scaling used in model fitting.
    jags_data <- read_rds(SPS_DATA_PATH)
    
    y <- jags_data$y
    jags_data$y[,1] <- rep(NA_integer_, jags_data$nrowy)
    jags_data$y2[,1] <- rep(NA_integer_, jags_data$nrowy2)

    # Run model
    out_prior <- jags(data = jags_data,
                      inits = NULL,
                      parameters.to.save = parameters,
                      model.file = mod_name,
                      n.chains = nc,
                      n.iter = ni,
                      n.burnin = nb,
                      n.thin = nt,
                      parallel = TRUE)

    #summary(out_prior)
    
    write_rds(out_prior, file = glue("data/ana_file/prior_predictive_check_{sps_loop}_{date_step1}_step1.rds"))

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
                       max(density(as.vector(y_sim_samples))$y)) 

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
                     geom_vline(xintercept = 0, color = "black", linetype = "dashed", size = 0.8) +
                     geom_vline(xintercept = 1, color = "black", linetype = "dashed", size = 0.8)

    # Assign plot to variable for saving
    ppc_plot <- last_plot()
    
    # Save as SVG file
    svg_filename <- glue("figures/prior_predictive_check_{sps_loop}_{date_step1}_step1.svg")
    ggsave(filename = svg_filename, 
           plot = ppc_plot, 
           device = "svg",
           width = 10, 
           height = 7, 
           units = "in")
    
    cat(glue("Saved PriPC plot for {sps_loop} as: {svg_filename}\n"))

 }