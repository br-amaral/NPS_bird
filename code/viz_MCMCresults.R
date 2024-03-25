library(MCMCvis)

rm(list = ls())

name <- "M06_BTNWMORR2"

samples_jags <- readRDS(glue("data/model_res/{name}.rds"))

MCMCtrace(samples_jags,
          #params = 'mu.beta0',
          ind = TRUE,
          pdf = FALSE)

MCMCsummary(samples_jags,
            # params = 'alpha',
            round = 2)

MCMCplot(samples_jags,
         # params = 'beta',
         ref_ovl = TRUE)

# scale
ncovs <- 1
sca_beta1 <- MCMCchains(samples_jags, params = 'scales_beta1')
selected_scales = rep(NA, 1)
for (i in 1:ncovs) {
  tb_mcmc_scales_i = table(sca_beta1)
  
  selected_scales[i] = as.integer(names(which.max(tb_mcmc_scales_i)))
}

sca_beta1
selected_scales

sca_beta1p <- as_tibble(sca_beta1) %>%
  mutate(new = 1)
sca_beta1p <- pivot_longer(sca_beta1p, -new, names_to = "site", values_to = "selected_scale") %>%
  dplyr::select(site, selected_scale) %>%
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
