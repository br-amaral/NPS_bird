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

conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::filter)

#! Make functions --------------------------------------
colanmes <- colnames
lenght <- length
`%!in%` <- Negate(`%in%`)

#! Source code -----------------------------------------

#! Import data -----------------------------------------
## file paths

## read files

# Extract results
posterior_samples <- as.matrix(samples_jags$samples)

# Check Bayesian p-value
bpvalue <- posterior_samples[, "bpvalue"]
mean(bpvalue)  # Should be around 0.5 for good fit

# Compare observed vs predicted detection rates
mean_y_obs <- posterior_samples[, "mean.y"]
mean_y_new <- posterior_samples[, "mean.y.new"]

plot(mean_y_obs, mean_y_new, 
     xlab = "Observed detection rate", 
     ylab = "Predicted detection rate")
abline(0, 1, col = "red")

# Freeman-Tukey statistic
fit_obs <- posterior_samples[, "fit.obs"]
fit_sim <- posterior_samples[, "fit.sim"]

plot(fit_obs, fit_sim, 
     xlab = "Observed fit statistic", 
     ylab = "Simulated fit statistic")
abline(0, 1, col = "red")

pp.check(samples_jags, observed = 'fit.obs', simulated = 'fit.sim')
