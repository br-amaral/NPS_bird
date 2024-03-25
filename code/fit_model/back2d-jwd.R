library(tidyverse)
library(rjags)
library(MCMCvis)
 
# y_dat4 <- read_rds(file = "data/y_dat4.rds")
# X10 <- read_rds(file = "data/X10.rds")
# nsite_pk <- read_rds("data/out/nsite_pk.rds")  
# pk <- read_rds("data/src/key_park.rds") %>% 
#   select(parks) %>% 
#   pull() %>% 
#   sort()

load("data/datJDnov2023.RData")

## 1 sps several parks
y_dat6 <- y_dat4
str(y_dat6)
# JWD: notice here that the parkey column is a character vector. This is 
#      what is causing problems later on down the road. When you use the 
#      arrange function below, it orders the parkey variable in order, but 
#      since it is a character vector the order is 1, 10, 2, 3, ..., instead of 
#      1, 2, 3, 4, ..... Then, when you convert y using the data.matrix() 
#      function, this function first converts the character vector to a factor and then a numeric
#      variable, which resulted in an index in the final "y" being created that 
#      does not match the original index (i.e., "10" was being converted to 2), 
#      which is what was used to create Zst. 
# JWD: so, you should first just convert this to numeric, and then it works.
y_dat6 <- y_dat6 %>%
  mutate(parkey = as.numeric(parkey))

nrow(X10) == nrow(y_dat6)

y <- y_dat6 %>% 
  select(bird_detec, parkey, site_n, year_s, interval_n, #year_n,
         yr_st, Year) %>% 
  arrange(parkey, site_n, yr_st, interval_n)

## trick for coding = only interval one
y2 <- y %>% 
  filter(interval_n == 1)

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
  select(bird_detec, parkey, site_n, year_s, interval_n) %>% 
  group_by(parkey, site_n, year_s) %>% 
  mutate(z = ifelse(sum(bird_detec, na.rm = T) == 0, 0, 1)) %>% 
  ungroup() %>% 
  filter(interval_n == 1) 

site_vec <- seq(1,max(nsite_pk),1)
(npk <- length(unique(y_dat4$parkey)))
(pk <- sort(unique(y_dat4$parkey)))
years <- y_dat4 %>% 
  select(Year) %>% 
  distinct() %>% 
  arrange() %>% 
  pull()
ninterval <- 10

Zst2 <- 
  array(NA, 
        dim = c(npk,
                max(nsite_pk),
                length(years)
        ),
        dimnames = list(pk,
                        site_vec,
                        years
        ))

for(a in 1:nrow(Zst)){
  #   a <- which(Zst$parkey == 10 & Zst$site_n == 1 & Zst$year_s == 1, arr.ind = T)
  zl <- Zst[a,]
  
  r <- as.numeric(zl$parkey)
  j <- zl$site_n 
  t <- zl$year_s

  Zst2[r,j,t] <- as.numeric(zl$z)
  
}


colnames(y) <- c("bird_detec", "parkey", "sitekey", "yearkey", "intervalkey","year_site","Year")

y <- data.matrix(y)
y2 <- data.matrix(y2)

nrow(y_dat6)
nrow(y)
nrow(y2)*ninterval
nrow(X)

# number of alphas and betas
n_bs <- 1
n_as <- 1

# model
str(jags.data <- list(y = y,
                      y2 = y2,
                      n_bs = n_bs,
                      n_as = n_as,
                      nrowy = nrow(y),
                      nrowy2 = nrow(y2)
))

inits <- function()list(Z = Zst2#, beta0 = rnorm(10,0.6), beta1 = rnorm(10,0.6)
)

niterations <- 5000
burnin <- 1000
nchains <- 4
print(niterations)

cat("\n\n\n running jags \n\n\n\n")
params <- c("beta0","beta", "alpha0", "alpha",
            "mu.beta0", "tau.beta0", "mu.alpha0", "tau.alpha0") # Z, psi

## initialize JAGS
jags_model <- rjags::jags.model(
  file = "models/mod_1_vector1spsparks.txt",
  data = jags.data,
  inits = inits,
  n.chains = nchains,
  n.adapt = max(100, ceiling(.1 * niterations)),
  quiet = FALSE
)

cat("\n\n\n first done \n\n\n\n")

# burn-in
if (burnin > 0) {
  message(paste("burn-in:", burnin, "iterations"))
  rjags::jags.samples(
    jags_model,
    variable.names = params,
    n.iter = niterations,
    thin = 5
  )
}

# write_rds(jags_model,
#           file = glue("data/model_res/M12.rds"))

cat("\n\n\n second done \n\n\n\n")

# posterior simulation
samples_jags <- coda.samples(
  jags_model,
  variable.names = params,
  n.iter = niterations,
  thin = 5
)

cat("\n\n\n third done \n\n\n\n")

# code to check the data and initial values
r <- 10   # what is the deal with 7 versus 10? (they both have the same values and 10 does not work)
j <- 1
t <- 1
Zst %>% filter(parkey == r, site_n == j, year_s == t)
y_dat6 %>% filter(parkey == r, site_n == j, year_s == t)
Zst2[r,j,t]
# row 73 has the values in the loop

#################################################################################################################
