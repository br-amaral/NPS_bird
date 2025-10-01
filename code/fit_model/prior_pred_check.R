#? *********************************************************************************
#? -------------------------------   Prior Predictive Check   --------------------
#? *********************************************************************************
#
#! Code to run prior predictive checks for occupancy model
#
#! Purpose: Check if priors generate reasonable simulated data BEFORE seeing observed data

#! Load packages ---------------------------------------
library(tidyverse)
library(conflicted)
library(glue)
library(MCMCvis)
library(jagsUI)

conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::filter)

#! Load your data (same as main analysis) -------------
# Load your data prep script or data objects
# source("your_data_prep_script.R")
# This should create: y, y2, X1, X2, X3, X4, X5, Xp, Xa, Xb, etc.

#! Set up JAGS data for prior predictive check --------
# Same data as main model but NO observed y data
jags_data_prior <- list(
  # Dimensions
  nrowy = jags_data$nrowy,
  nrowy2 = jags_data$nrowy2,
  n_pkM = jags_data$n_pkM,
  n_yrM = jags_data$n_yrM,
  n_bs = jags_data$n_bs,
  n_as = jags_data$n_as,
  
  # Index matrices (same as main model)
  y = jags_data$y,        # Keep structure but values won't be used for inference
  y2 = jags_data$y2,
  
  # Covariates (same as main model)
  X1 = jags_data$X1, 
  X2 = jags_data$X2, 
  X3 = jags_data$X3, 
  X4 = jags_data$X4, 
  X5 = jags_data$X5,
  Xp = jags_data$Xp, 
  Xa = jags_data$Xa, 
  Xb = jags_data$Xb
)

#! Parameters to monitor -------------------------------
params_prior <- c(
  "beta", "alpha", "beta0", "alpha0",
  "mu.beta0", "mu.alpha0", "tau.beta0", "tau.alpha0",
  "scales_beta1", "scales_beta2", "scales_beta3", "scales_beta4", "scales_beta5",
  "mean.y.prior",
  "sum.y.prior", "n.y.prior", "n.obs",
  "y.prior", "Z.prior"
)

#! Run prior predictive check -------------------------
# Shorter run since we're just checking priors
prior_samples <- jags(
  data = jags_data_prior,
  parameters.to.save = params_prior,
  model.file = "/Users/bamaral/Library/CloudStorage/OneDrive-MichiganStateUniversity/GitHubOne/NPS_bird_copy/models/mod_prior_predictive_clean.txt",
  n.chains = 2,
  n.iter = 200,   
  n.burnin = 50,
  n.thin = 2
)
# Prior predictive checks don't have observed data, so no deviance

#! Analyze prior predictive results -------------------

# Extract samples properly for mcmc.list
prior_chains <- as.matrix(prior_samples$samples)

# Check what parameters we actually have
cat("\\n=== AVAILABLE PARAMETERS ===\\n")
param_names <- colnames(prior_chains)
print("Parameter types available:")
print(table(sub("\\[.*", "", param_names)))

# Look for y.prior parameters specifically
y_prior_params <- grep("y\\.prior", param_names, value = TRUE)
cat("\\nNumber of y.prior parameters:", length(y_prior_params))
if(length(y_prior_params) > 0) {
  cat("\\nFirst few y.prior parameters:", head(y_prior_params, 10))
}

# Check for summary statistics
summary_params <- c("mean.y.prior", "sum.y.prior", "n.y.prior", "n.obs")
available_summaries <- summary_params[summary_params %in% param_names]
cat("\\nAvailable summary parameters:", paste(available_summaries, collapse = ", "))

# Plot y.prior if available
if(length(y_prior_params) > 0) {
  # Extract all y.prior values
  y_prior_samples <- prior_chains[, y_prior_params]
  y_prior_samples <- y_prior_samples[ ,4:ncol(y_prior_samples)]

  # Plot distribution of simulated detections
  par(mfrow = c(1, 2))
  hist(as.vector(y_prior_samples), breaks = 50, 
       main = glue("Prior Predictive Distribution for {sps}\n(All y.prior values)"), 
       xlab = "Detection (0/1)")
  
  # Plot detection rate across iterations
  if("mean.y.prior" %in% param_names) {
    hist(prior_chains[, "mean.y.prior"], breaks = 30,
         main = glue("Prior Predicted Detection Rate for {sps}"), 
         xlab = "Mean Detection Rate")
  }
  par(mfrow = c(1, 1))
} else {
  cat("\\ny.prior parameters not found - check if they were excluded due to size")
}

# 1. Check parameter ranges from priors
cat("\\n=== PRIOR PARAMETER SUMMARIES ===\\n")
print("Beta coefficients (occupancy effects):")
beta_cols <- grep("^beta\\[", colnames(prior_chains))
print(summary(prior_chains[, beta_cols[1:6]]))  
# Create histograms for each beta coefficient
beta_matrix <- prior_chains[, beta_cols[1:6]]
par(mfrow = c(2, 3))  # 2 rows, 3 columns for 6 histograms
for(i in 1:ncol(beta_matrix)) {
  hist(beta_matrix[, i], 
       main = paste("Beta", i, "Coefficient for", sps), 
       xlab = paste("Beta[", i, "] Value"),
       col = "lightblue",
       breaks = 30)
  abline(v = 0, col = "red", lty = 2)  # Add reference line at 0
}
par(mfrow = c(1, 1))  # Reset plot layout

print("\\nAlpha coefficients (detection effects):") 
alpha_cols <- grep("^alpha\\[", colnames(prior_chains))
print(summary(prior_chains[, alpha_cols[1:3]]))  # First 3 alphas

# Create histograms for each alpha coefficient
alpha_matrix <- prior_chains[, alpha_cols[1:3]]
par(mfrow = c(1, 3))  # 1 row, 3 columns for 3 histograms
for(i in 1:ncol(alpha_matrix)) {
  hist(alpha_matrix[, i], 
       main = paste("Alpha", i, "Coefficient for", sps), 
       xlab = paste("Alpha[", i, "] Value"),
       col = "lightgreen",
       breaks = 30)
  abline(v = 0, col = "red", lty = 2)  # Add reference line at 0
}
par(mfrow = c(1, 1))  # Reset plot layout

# 2. Check simulated occupancy and detection rates
print("\\nMean detection probability from priors:")
print("(Detection probability calculation simplified - check individual p.prior values if needed)")

print("\\nMean detection rate from priors:")
print(summary(prior_chains[, "mean.y.prior"]))

# 3. Check for extreme values - simplified version
cat("\\n=== BASIC SUMMARIES ===\\n")
print("Mean occupancy probability:")
print("(Calculate manually from Z.prior samples in R - JAGS array limitations)")

print("\\nTotal detections simulated:")
if("sum.y.prior" %in% colnames(prior_chains)) {
  print(summary(prior_chains[, "sum.y.prior"]))
}

# 4. Compare simulated vs observed detection rates
if(exists("y")) {  # If you have observed data loaded
  observed_detection_rate <- mean(y[,1], na.rm = TRUE)
  simulated_detection_rates <- prior_chains[, "mean.y.prior"]
  
  cat("\\n=== PRIOR VS OBSERVED COMPARISON ===\\n")
  print(paste("Observed detection rate:", round(observed_detection_rate, 3)))
  print(paste("Prior predicted detection rate (mean):", round(mean(simulated_detection_rates), 3)))
  print(paste("Prior predicted detection rate (95% CI):", 
              round(quantile(simulated_detection_rates, 0.025), 3), "-",
              round(quantile(simulated_detection_rates, 0.975), 3)))
  
  # Plot comparison
  hist(simulated_detection_rates, main = "Prior Predicted vs Observed Detection Rate",
       xlab = "Detection Rate", breaks = 30, col = "lightblue")
  abline(v = observed_detection_rate, col = "red", lwd = 2, 
         main = paste("Observed =", round(observed_detection_rate, 3)))
  legend("topright", "Observed", col = "red", lwd = 2)
}

#! Diagnostic plots -----------------------------------
# Plot beta coefficients from priors
par(mfrow = c(2, 2))

# Beta coefficients
beta_samples <- prior_chains[, grep("^beta\\[", colnames(prior_chains))[1:6]]  # 6 betas now
boxplot(beta_samples, main = "Beta Coefficients from Priors",
        xlab = "Beta Parameter", ylab = "Value")
abline(h = 0, col = "red", lty = 2)

# Alpha coefficients  
alpha_samples <- prior_chains[, grep("^alpha\\[", colnames(prior_chains))[1:3]]
boxplot(alpha_samples, main = "Alpha Coefficients from Priors",
        xlab = "Alpha Parameter", ylab = "Value")
abline(h = 0, col = "red", lty = 2)


par(mfrow = c(1, 1))

#! Interpretation guidelines --------------------------
cat("\\n=== PRIOR PREDICTIVE CHECK INTERPRETATION ===\\n")
cat("1. Detection rates should be reasonable (e.g., 0.1-0.8)\\n")
cat("2. Occupancy probabilities should cover reasonable range (e.g., 0-1, not all extreme)\\n") 
cat("3. Coefficients shouldn't be too extreme (check if |beta| > 5 frequently)\\n")
cat("4. Simulated detection rate should overlap with observed (if available)\\n")
cat("5. If results seem unreasonable, consider:\\n")
cat("   - Tightening priors (smaller variance)\\n")
cat("   - Centering/scaling covariates\\n")
cat("   - Using more informative priors\\n")

# Save results
saveRDS(prior_samples, "data/model_res/prior_predictive_check.rds")
cat("\\nPrior predictive check results saved to: data/model_res/prior_predictive_check.rds\\n")