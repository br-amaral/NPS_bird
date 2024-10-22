# *********************************************************************************
# -------------------------------   Plot model output   ---------------------------
# *********************************************************************************
# Code to make plots
#
hg <- httpgd::hgd()
# detach packages and clear workspace
if(!require(freshr)){install.packages('freshr')}
freshr::freshr()
#
# Load packages ---------------------------------------
library(conflicted)
library(tidyverse)
library(glue)
library(MCMCvis)
library(rjags)
#
conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::filter)
# conflicts_prefer(scales::alpha)
#
# Make functions --------------------------------------
colanmes <- colnames
lenght <- length
`%!in%` <- Negate(`%in%`)

#! Import data -----------------------------------------
## file paths and read files
# when loading the model results, get the most updated file?
file_name <- "2024_10_22_BHVI_parks_10000its_2min_spscov_run1"

samples_jags <- read_rds(glue("data/model_res/{file_name}.rds"))

# get parameter names
scales_names <- grep("^scales_", colnames(samples_jags[[1]]), value = TRUE)
all_params <- c("mu.alpha0", "mu.beta0", "beta", "alpha", scales_names)

#! Par estimates ----------------------------------
#par(mfrow = c(1,1))
MCMCplot(samples_jags,
         params = all_params,
         #ci = c(50, 89),
         main = file_name,
         ref_ovl = TRUE)

#! Traceplots ----------------------
MCMCtrace(samples_jags,
          params = all_params,
          #main = file_name,
          ind = TRUE,
          pdf = FALSE,
          #filename = glue("figures/preliminary/jags_res_GCFL_b0yes_parks_20000its_LESSHRrun1"),
          exact = TRUE,
          Rhat = TRUE,
          n.eff = TRUE) 

#! Summary --------------------------------------------
MCMCsummary(samples_jags,
            round = 2)

#! get beta parameters ----------------------------
(pars_select <- cbind(
  c(
    #'beta[2]',
    'beta[2]',
    'beta[2]'
  ),
  c(1,2)) %>% 
  as_tibble() %>% 
  rename(beta = V1,
         scale = V2))
#write_rds(pars_select, file = glue("data/model_res/{file_name}_PARS.rds"))




#TODO:
summary(samples_jags)

#NOTE:
MCMCsummary(samples_jags,
            params = all_params,
            round = 2)

MCMCtrace(samples_jags,
            params = c("alpha0", "beta0"),
            ind = TRUE,
            pdf = FALSE,
            #filename = glue("figures/preliminary/trace2_jags_res_COYE_b0yes_parks_10000its_LESSrun1"),
            exact = TRUE,
            Rhat = TRUE,
            n.eff = TRUE)

MCMCplot(samples_jags,
         params = c("alpha0","beta0"),
         ref_ovl = TRUE)

MCMCplot(samples_jags,
         params = c("mu.beta0","beta",
                    "mu.alpha0","alpha"),
         ref_ovl = TRUE)

# scale selection plots and objects:

sca_beta1 <- MCMCchains(samples_jags, params = 'scales_beta3')
selected_scales = rep(NA, 1)
#for (i in 1:3) {
  tb_mcmc_scales_i = table(sca_beta1)
  
  selected_scales = as.integer(names(which.max(tb_mcmc_scales_i)))
#}

sca_beta1
selected_scales

sca_beta1p <- as_tibble(sca_beta1) %>%  
  mutate(new = 1)
sca_beta1p <- pivot_longer(sca_beta1p, -new, names_to = "site", values_to = "selected_scale") %>% 
  select(site, selected_scale) %>% 
  arrange(site)

ggplot(aes(x = selected_scale, 
           y = (..count..)/sum(..count..), 
           fill = (..count..)/sum(..count..)), 
           data = sca_beta1p) + 
  geom_histogram(position = "stack", binwidth = 0.5) + 
  theme_bw() +
  ylab("Frequency") + 
  xlab("Selected scale") +
  scale_y_continuous(limits = c(0, 1)) +
  scale_fill_gradient(
    name = "Frequency",
    low = "#ecffdd", high = "#0a2701",  # Customize gradient colors
    limits = c(0, 1),  # Explicitly set the limits for the fill scale
    guide = guide_colorbar(ticks = FALSE))+  # Remove ticks from legend bar+
  theme(
    legend.title = element_text(size = 14),  # Increase legend title size
    legend.text = element_text(size = 12),   # Increase legend text size
    legend.key.size = unit(3, "cm")        # Increase legend key size
  )


