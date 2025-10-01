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
  nrowy = nrowy,
  nrowy2 = nrowy2,
  n_pkM = n_pkM,
  n_yrM = n_yrM,
  n_bs = n_bs,
  n_as = n_as,
  
  # Index matrices (same as main model)
  y = y,        # Keep structure but values won't be used for inference
  y2 = y2,
  
  # Covariates (same as main model)
  X1 = X1, X2 = X2, X3 = X3, X4 = X4, X5 = X5,
  Xp = Xp, Xa = Xa, Xb = Xb
)

#! Parameters to monitor -------------------------------
params_prior <- c(
  "beta", "alpha", "beta0", "alpha0",
  "mu.beta0", "mu.alpha0", "tau.beta0", "tau.alpha0",
  "scales_beta1", "scales_beta2", "scales_beta3", "scales_beta4", "scales_beta5", "scales_beta6",
  "mean.psi.prior", "mean.p.prior", "mean.y.prior",
  "max.psi.prior", "min.psi.prior", "max.p.prior", "min.p.prior",
  "y.prior", "Z.prior", "psi.prior", "p.prior"
)

#! Run prior predictive check -------------------------
# Shorter run since we're just checking priors
prior_samples <- jags(
  data = jags_data_prior,
  parameters.to.save = params_prior,
  model.file = "models/mod_prior_predictive.txt",
  n.chains = 3,
  n.iter = 2000,   # Shorter than main analysis
  n.burnin = 500,
  n.thin = 1
)

#! Analyze prior predictive results -------------------

# Extract samples
prior_chains <- as.matrix(prior_samples$samples)

# 1. Check parameter ranges from priors
cat("\\n=== PRIOR PARAMETER SUMMARIES ===\\n")
print("Beta coefficients (occupancy effects):")
beta_cols <- grep("^beta\\[", colnames(prior_chains))
print(summary(prior_chains[, beta_cols[1:7]]))  # First 7 betas

print("\\nAlpha coefficients (detection effects):") 
alpha_cols <- grep("^alpha\\[", colnames(prior_chains))
print(summary(prior_chains[, alpha_cols[1:3]]))  # First 3 alphas

# 2. Check simulated occupancy and detection rates
cat("\\n=== SIMULATED DATA SUMMARIES ===\\n")
print("Mean occupancy probability from priors:")
print(summary(prior_chains[, "mean.psi.prior"]))

print("\\nMean detection probability from priors:")
print(summary(prior_chains[, "mean.p.prior"]))

print("\\nMean detection rate from priors:")
print(summary(prior_chains[, "mean.y.prior"]))

# 3. Check for extreme values
cat("\\n=== CHECKING FOR EXTREME VALUES ===\\n")
print("Range of occupancy probabilities:")
print(paste("Min:", round(mean(prior_chains[, "min.psi.prior"]), 3)))
print(paste("Max:", round(mean(prior_chains[, "max.psi.prior"]), 3)))

print("\\nRange of detection probabilities:")
print(paste("Min:", round(mean(prior_chains[, "min.p.prior"]), 3)))
print(paste("Max:", round(mean(prior_chains[, "max.p.prior"]), 3)))

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
beta_samples <- prior_chains[, grep("^beta\\[", colnames(prior_chains))[1:7]]
boxplot(beta_samples, main = "Beta Coefficients from Priors",
        xlab = "Beta Parameter", ylab = "Value")
abline(h = 0, col = "red", lty = 2)

# Alpha coefficients  
alpha_samples <- prior_chains[, grep("^alpha\\[", colnames(prior_chains))[1:3]]
boxplot(alpha_samples, main = "Alpha Coefficients from Priors",
        xlab = "Alpha Parameter", ylab = "Value")
abline(h = 0, col = "red", lty = 2)

# Occupancy probabilities
hist(prior_chains[, "mean.psi.prior"], main = "Prior Predicted Mean Occupancy",
     xlab = "Mean Occupancy Probability", breaks = 30)

# Detection probabilities
hist(prior_chains[, "mean.p.prior"], main = "Prior Predicted Mean Detection",
     xlab = "Mean Detection Probability", breaks = 30)

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