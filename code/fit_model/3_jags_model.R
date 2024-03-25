library(tidyverse)
library(jagsUI)

y_dat6 <- read_rds(file = "data/y_dat6.rds")
X10 <- read_rds(file = "data/X10.rds")

nrow(X10) == nrow(y_dat6)

# filtering for a single sps
fil_dat <- y_dat6 %>% 
  filter(bird_detec > 0) %>% 
  select(park, sps_it) %>% 
  table() %>% 
  t()

raw_dat <- y1 %>% 
  filter(Bird_Count > 0) %>% 
  select(Admin_Unit_Code, AOU_Code) %>% 
  table() %>% 
  t()

dim(fil_dat) == dim(raw_dat)
sum(fil_dat) == sum(raw_dat)

X10$sps <- y_dat6$spskey

fil_dat

y_dat6 <- y_dat6 %>% 
  filter(parkey == 1,
         spskey == 87)

X10 <- X10 %>% 
  filter(park == "ACAD",
         sps == 87)

nrow(X10) == nrow(y_dat6)

sps_pk_nth <- read_rds(file = "data/sps_pk_nth.rds")

# make sure all my covariates are the same dimentions as
(n_spsM <- y_dat6$sps_it %>% unique() %>% length())
(n_pkM <- y_dat6$park %>% unique() %>% length())
(nyears <- y_dat6$year_s %>% unique() %>% length())

y <- y_dat6 %>% 
  select(bird_detec, spskey, parkey, site_n, year_s, interval_n, #year_n,
         yr_st, spskey_p, Year) %>% 
  arrange(spskey_p, parkey, site_n, yr_st, interval_n)
  
X <- X10 %>% 
  select(siteBA_s, siteDEN_s, 
         parkBA_s, parkDEN_s, 
         counBA_s, counDEN_s, 
         area_s, 
         date_jul,time_jul)

table(is.na(X$siteBA_s))
table(is.na(X$siteDEN_s))
table(is.na(X$parkBA_s))
table(is.na(X$parkDEN_s))
table(is.na(X$counBA_s))
table(is.na(X$counDEN_s))
table(is.na(X$date_jul))
table(is.na(X$time_jul))

# initial values
Zst <- y %>% 
  group_by(spskey, spskey_p, site_n, Year) %>% 
  mutate(z = ifelse(sum(bird_detec, na.rm = T) == 0, 0, 1)) %>% 
  ungroup() %>% 
  select(z)

# # starting values
# for(i in 1:n_spsM){	     # species i
#   for(r in 1:n_pkM){	  # park r
#     beta0[i,r]
#     alpha0[i,r]
#   }
#   
#   for(j in 1:n_bs){     # number of betas
#     beta[j,i]
#   }
#   
#   for(j in 1:n_as){     # number of betas
#     alpha[j,i]
#   }
# }
colnames(y) <- c("bird_detec", "spskey", "parkey", "sitekey", "yearkey", "intervalkey",
                 "year_site", "spskey_park",
                 "Year")
y <- data.matrix(y)

# number of alphas and betas
n_bs <- 4
n_as <- 3

# model
str(jags.data <- list(y = y,                    # bird detection array
                      n_bs = n_bs,
                      n_as = n_as,
                      X = X,
                      nrowy = nrow(y),
                      nyrM = nyears
))

inits <- function()list(Z = Zst#, beta0 = rnorm(10,0.6), beta1 = rnorm(10,0.6)
)

niterations <- 20
burnin <- 5
nchains <- 1
print(niterations)

cat("\n\n\n running jags \n\n\n\n")
params <- c("beta0", "beta", "alpha0", "alpha",
            "mu.beta0", "mu.beta", "mu.alpha0", "mu.alpha")
## initialize JAGS
jags_model <- jagsUI::jags(jags.data,
                           "models/mod_1_vector1park1sps.txt",
                           inits = inits,
                           parameters.to.save = params,
                           n.chains = nchains,
                           n.adapt = max(50, ceiling(.1 * niterations)),
                           n.iter = niterations
)

y %>% as_tibble() %>% 
      filter(
             #bird_detec == 1,
             #spskey == 1,
             #parkey == 1,
             sitekey == 1,
             yearkey == 1,
             #interval_n == 1,
             #year_n == 1,
             #spskey_p == 17
             )

## remove the covariate effects
# model
str(jags.data <- list(y = y,                    # bird detection array
                      nrowy = nrow(y),
                      nyrM = nyears
))

inits <- function()list(Z = Zst#, beta0 = rnorm(10,0.6), beta1 = rnorm(10,0.6)
)

niterations <- 20
burnin <- 5
nchains <- 1
print(niterations)

cat("\n\n\n running jags \n\n\n\n")
params <- c("beta0","alpha0")
## initialize JAGS
jags_model <- jagsUI::jags(jags.data,
                           "models/mod_1_vector1park1sps_simpler.txt",
                           inits = inits,
                           parameters.to.save = params,
                           n.chains = nchains,
                           n.adapt = max(50, ceiling(.1 * niterations)),
                           n.iter = niterations
)

# remove year from beta zero
# model
str(jags.data <- list(y = y,                    # bird detection array
                      nrowy = nrow(y)))

inits <- function()list(Z = Zst#, beta0 = rnorm(10,0.6), beta1 = rnorm(10,0.6)
)

niterations <- 20
burnin <- 5
nchains <- 1
print(niterations)

cat("\n\n\n running jags \n\n\n\n")
params <- c("beta0","alpha0")
## initialize JAGS
jags_model <- jagsUI::jags(jags.data,
                           "models/mod_1_vector1park1sps_simpler_1bo.txt",
                           inits = inits,
                           parameters.to.save = params,
                           n.chains = nchains,
                           n.adapt = max(50, ceiling(.1 * niterations)),
                           n.iter = niterations
)




# p = spskey_p, parkey, site_n, year_n, interval_n
y[,2] <- y[,8] <- 1

str(jags.data <- list(y = y,                    # bird detection array
                      nrowy = nrow(y)))

jags_model <- jagsUI::jags(jags.data,
                           "models/mod_2_vector.txt",
                           inits = inits,
                           parameters.to.save = params,
                           n.chains = nchains,
                           n.adapt = max(50, ceiling(.1 * niterations)),
                           n.iter = niterations)

## minus y[a,7] year N

str(jags.data <- list(y = y,                    # bird detection array
                      n_pkM = n_pkM,   # number of parks
                      nrowy = nrow(y)
))

jags_model <- jagsUI::jags(jags.data,
                           "models/mod_2_vector.txt",
                           inits = inits,
                           parameters.to.save = params,
                           n.chains = nchains,
                           n.adapt = max(50, ceiling(.1 * niterations)),
                           n.iter = niterations)

## minus y[a,8] species
