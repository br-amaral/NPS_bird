library(jagsUI)

# ---- Prepare data for prior predictive run ----
# Use the real covariates if you want to check priors conditional on covariate distribution.
# Alternatively, simulate covariates with same scaling used in model fitting.

jags_data <- list(
  n_pkM = npk,
  n_yrM = lenght(years),
  n_bs  = n_bs,
  n_as  = n_as,
  nrowy2 = nrow(y2),
  tot_sites = nrow(y2),
  nrowy  = nrow(y),
  X1 = X1,  # dimension: [nrowy2, n_scales]
  X2 = X2,
  X3 = X3,
  X4 = X4,
  X5 = X5,
  Xp = Xp,  # park size vector aligned with b
  Xa = Xa,  # covariates for detection (matrix with rows = nrowy)
  Xb = Xb,
  y2 = y2,  # indexing matrix as in model
  y = y,  # observed y not used; set NA
  n_sites_per_park = nsite_pk,  # used in summary calculation
  max_sites = max(nsite_pk),
  nocc = 5
)

jags_data$y[,1] <- rep(NA_integer_, jags_data$nrowy)
jags_data$y2[,1] <- rep(NA_integer_, jags_data$nrowy2)

# ---- Parameters to monitor ----
parameters <- c("prop_occ", "total_detections",
                "psi", "p", "Z", "y_sim",
                "beta", "alpha",
                "scales_beta1","scales_beta2","scales_beta3","scales_beta4","scales_beta5",
                "mu.beta0","tau.beta0","mu.alpha0","tau.alpha0")

# ---- MCMC settings ----
ni <- 3000
nb <- 1000
nc <- 3
nt <- 1

mod_name <- "/Users/bamaral/Library/CloudStorage/OneDrive-MichiganStateUniversity/GitHubOne/NPS_bird_copy/models/model_prior_pred.txt"
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

print(out_prior)

# Summarize the prior draws for y_sim
summary(as.vector(out_prior$sims.list$y_sim))

# prop_occ and total_detections are monitored; extract MCMC samples
library(coda)
samps <- (out_prior$samples)  # jagsUI stores samples

# Extract prop_occ
prop_occ_samps <- unlist(lapply(out_prior$samples, function(chain) chain[,"prop_occ"]))
hist(prop_occ_samps, breaks = 40, main = "Prior predictive: proportion occupied", xlab="prop occupied")

# total detections
tot_det_samps <- unlist(lapply(out_prior$samples, function(chain) chain[,"total_detections"]))
hist(tot_det_samps, breaks = 40, main = "Prior predictive: total detections", xlab="total detections")

# Check distributions of scales selection
table_scales1 <- table(unlist(lapply(out_prior$samples, function(chain) chain[,"scales_beta1"])))
table_scales1 / sum(table_scales1)  # should be ~ uniform (1/3 each) unless posterior changes

# Look at a few simulated y patterns (first 100 y_sim draws from first chain)
ysim_mat <- do.call(rbind, lapply(out_prior$samples, function(chain) chain[, grepl("^y_sim\\[", colnames(chain))]))
# Each row is a draw; pick a few rows to visualize counts by site or interval, etc.


# Plot the simulated data against the covariate
# A good way to visualize the prior is to plot a random subset of prior draws
# Get 50 draws from the posterior for y_sim
draws <- out_prior$sims.list$y_sim

# Plot the covariate against the simulated outcomes
hist( out_prior$sims.list$y_sim[1, ], main= "Simulated Outcome (y_sim)")

# Add a draw from the observed data if you have it for reference
# (For the actual fit, but not for the prior check itself)
# lines(x, your_observed_y, col = "red", lwd = 2)

# Plot the prior distribution for a coefficient
hist(out_prior$sims.list$beta[1], main = "Prior for beta1", xlab = "beta1")
