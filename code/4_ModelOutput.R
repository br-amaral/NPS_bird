# *****************************************
# -----------   4_ModelOutput   -----------
# *****************************************
# Script to get mcmc sample results and look at the results
#
# input:    - :
#           - :
# output:   - :
#           - :

# start R cleaning workspace and detaching packages  -----------
detach()
rm(list = ls(all.names = TRUE))

# load packages -----------------
library(tidyverse)
library(MCMCvis)
library(glue)

library(conflicted)
conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::filter)

# Load jags model file ------------------

samples_jags <- read_rds("data/model_res/samples_jags3_multisps_covsACAD_25000its_run4.rds")
samples_jags <- read_rds("data/model_res/samples_jags3_BLISS_multispsACAD_25000its_run5.rds")

MCMCsummary(samples_jags,
            params = c("mu.alpha0", "mu.alpha1", "mu.alpha2", "mu.alpha3",
                       "mu.beta0", "mu.beta1"),
            round = 2)

MCMCtrace(samples_jags,
          params = c("mu.alpha0", "mu.alpha1", "mu.alpha2", "mu.alpha3",
                     "mu.beta0", "mu.beta1"),
          ind = TRUE,
          pdf = FALSE)

par(mfrow = c(1,1))
MCMCplot(samples_jags,
         params = c("mu.alpha0", "mu.alpha1", "mu.alpha2", "mu.alpha3",
                    "mu.beta0", "mu.beta1"),
         ref_ovl = TRUE)

# scale selection plots and objects:

sca_beta1 <- MCMCchains(samples_jags, params = 'scales_beta1')
ncovs <- 1
selected_scales = rep(NA, 1)
for (i in 1:ncovs) {
  tb_mcmc_scales_i = table(sca_beta1)
  
  selected_scales[i] = as.integer(names(which.max(tb_mcmc_scales_i)))
}

selected_scales

sca_beta1p <- as_tibble(sca_beta1) %>% 
  mutate(new = 1)
sca_beta1p <- pivot_longer(sca_beta1p, -new, names_to = "site", values_to = "selected_scale") %>% 
  select(site, selected_scale) %>% 
  arrange(site)

# colors are sites
ggplot(aes(x = selected_scale, y = (..count..)/sum(..count..), fill = site), data = sca_beta1p) + 
  geom_histogram(position = "stack", binwidth = 0.5) + 
  theme_bw() +
  theme(legend.position = "none") +
  ylab("Frequency") + xlab("Selected scale") 

ggplot(aes(x = selected_scale, fill = site), data = sca_beta1p) + 
  theme_bw() +
  theme(legend.position = "none") +
  ylab("Frequency") + xlab("Selected scale") +
  geom_density(alpha = 0.08, color = "gray36") +
  scale_x_continuous(limits = c(0, 6))

