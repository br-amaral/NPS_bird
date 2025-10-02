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
library(jagsUI)  # For pp.check() function
library(patchwork)  # For combining plots

conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::filter)

step_number_define <- 2

if(substr(getwd(), 1, 3) == "/Us") {direc <- "local"} else {direc <- "hpc"}

if(direc == "local"){
    master_tab <- read_csv("/Users/bamaral/Library/CloudStorage/OneDrive-MichiganStateUniversity/GitHubOne/NPS_bird_copy/code/fit_model/mod_key.csv") %>%
            filter(run == "yes") %>% 
            filter(step %in% c(step_number_define)) %>% 
            distinct()
            
    } else {master_tab <- read_csv("code/fit_model/mod_key.csv") %>%
            filter(run == "yes") %>% 
            filter(step %in% c(step_number_define)) %>% 
            distinct()            
}

paste('\n ************************************* \n \n \n   Running Models:', '\n',
      '  What? =', "Posterior Predictive Check", '\n',
      '  Number of sps =', nrow(master_tab), '\n',
      '  Started running on =', Sys.time(),  '\n \n \n',
      '**************************************') %>% cat()

for (key_ite in 1:nrow(master_tab)){ 
  
   # key_ite <- 1
  tib_loop <- master_tab[key_ite, ]

  sps_loop <- tib_loop$AOU_Code
  date_step1 <- substr(tib_loop$result, 19, 28)

  file_name <- "BTNW_step2_output_2025_10_02run1"

  ## read files
  samples_jags <- read_rds(glue("data/model_res/{file_name}.rds"))

  # Extract results
  posterior_samples <- as.matrix(samples_jags$samples)

  # Check Bayesian p-value
  bpvalue <- posterior_samples[, "bpvalue"]
  mean(bpvalue)  # Should be around 0.5 for good fit

  # Compare observed vs predicted detection rates
  mean_y_obs <- posterior_samples[, "mean.y"]
  mean_y_new <- posterior_samples[, "mean.y.new"]

  # Freeman-Tukey statistic
  fit_obs <- posterior_samples[, "fit.obs"]
  fit_sim <- posterior_samples[, "fit.sim"]

  # sanity check ;)
  pp.check(samples_jags, observed = 'fit.obs', simulated = 'fit.sim')

  # Create beautiful ggplot for Freeman-Tukey statistic
  plot1 <- ggplot(data = data.frame(observed = fit_obs, simulated = fit_sim), 
        aes(x = observed, y = simulated)) +
    geom_point(alpha = 0.5, size = 2, color = "#915F6D") +
    geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed", size = 1) +
    labs(title = "Freeman-Tukey Statistic",
        subtitle = "Observed vs Simulated Fit Statistics",
        x = "\n Observed data",
        y = "Simulated data\n") +
    theme_bw() +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5),
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 12, face = "bold"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(size = 0.5, color = "gray85")
    ) +
    #coord_equal() +  # 1:1 aspect ratio
    annotate("text", x = Inf, y = -Inf, 
            label = glue("Bayesian P-value: {round(mean(bpvalue),2)} \n"), 
            hjust = 1.1, vjust = -0.45, color = "black", size = 5)

  # Create beautiful ggplot for detection rates
  plot2 <- ggplot(data = data.frame(observed = mean_y_obs, predicted = mean_y_new), 
        aes(x = observed, y = predicted)) +
    geom_point(alpha = 0.5, size = 2, color = "#915F6D") +
    geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed", size = 1) +
    labs(title = "Detection Rates",
        subtitle = "Observed vs Predicted Detection Rates",
        x = "\n Observed Detection Rate",
        y = "Predicted Detection Rate\n") +
    theme_bw() +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5),
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 12, face = "bold"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(size = 0.5, color = "gray85")
    ) # +
  #   coord_equal()  # 1:1 aspect ratio 

  # Combine plots side by side and save as SVG
  combined_plot <- plot1 + plot2 + 
    plot_layout(ncol = 2, nrow = 1) +  # Side by side layout
    plot_annotation(
      title = "Posterior Predictive Checks",
      theme = theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5))
    )

  # Display the combined plot
  print(combined_plot)

  # Save as SVG with 1:1 aspect ratio for each plot
  svg_filename <- glue("posterior_predictive_checks_{file_name}_{Sys.Date()}.svg")
  ggsave(filename = svg_filename, 
        plot = combined_plot, 
        device = "svg",
        width = 16,   # Wide enough for two plots side by side
        height = 8,   # Height maintains roughly 1:1 ratio for each plot
        units = "in")

  cat(glue("Saved combined posterior predictive check plots as: {svg_filename}\n"))
}