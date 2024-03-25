## Load packages -------------------------------------------------------------------------------- 
library(tidyverse)
library(lubridate)
library(glue)
library(jagsUI)

# setwd("~/Library/Mobile Documents/com~apple~CloudDocs/NPS_birds")

colnmaes <- colnames

# select one park 
park <- "MABI"

# select one species
sps <- "OVEN"

dat <- read_rds(file = "data/NETNtib.rds")

if(exists("park")) dat <- dat %>% 
  filter(park_code == park)

field_dat <- dat$field_data[1][[1]]

visits <- dat$visits[1][[1]] %>%   ## start time
  rename(observer_name = Observer)    

points <- dat$points[1][[1]] %>%   ## PT_DESC
  dplyr::select(-Transect_Name) 

visits2 <- left_join(visits, points, by = c("Admin_Unit_Code", "Point_Name", "Survey_Type"))

field_dat2 <- left_join(field_dat, visits2, by = c("Admin_Unit_Code", "Transect_Name", "Point_Name", "Survey_Type", "Visit", "EventDate", "Year" ))

## variables 
# filters
# park == MABI
# species == OVEN
# bcr == !$
# 
# # levels:
# ## year
# 
# ## site
# forest (PT_DESC) - N
# 
# ## occasion
# EventDate - detec
# Initial_Three_Min_Cnt - detec
# Observer - detec
# Skill_Level - detec
# StartTime - detec
# 
# ## interval
# Distance - detec
# ID_Method_Code - detec - table(field_dat$ID_Method_Code) - too few visuals

## pg 332 removal sampling -----------------------------------------------

# Remove NAs!
field_dat2 <- field_dat2 %>% 
  mutate(Interval = ifelse(Interval == "NR", NA, Interval)) %>% 
  filter(!is.na(Year),
         !is.na(Point_Name),
         !is.na(EventDate),
         !is.na(Interval),
         !is.na(Distance)) 

# years
years <- field_dat2$Year %>% unique() %>% sort()
years_s <- years - (years[1]-1)
(nyr_s <- max(years_s))

# sites
(nsite <- length(visits$Point_Name %>% unique()))
sites <- visits$Point_Name %>% unique() %>% sort()

# intervals
ninterval <- field_dat2$Interval %>% as.numeric() %>% unique() %>% length()

intervals <- glue("{seq(1, ninterval, 1)}_int") %>% 
  cbind(seq(1, ninterval, 1),
        seq((ninterval - 1), 0, by = -1)) %>% 
  as_tibble()
colnames(intervals) <- c("intervals_n", "int_s", "Interval")

# distances
ndist <- field_dat2$Distance_id %>% unique() %>% length()

dist <- field_dat2$Distance %>% factor(levels = c( "< 50 Meters", "> 50 Meters")) %>% as.numeric() %>% -1

# get bird observation
y1 <- field_dat2 %>% 
  cbind(.,dist) %>% 
  left_join(., occs[,1:2], by = "EventDate") %>% 
  left_join(., intervals[,2:3], by = "Interval") %>% 
  filter(!Interval == "NR") %>% 
  select(-Interval) %>% 
  mutate(int_s = as.numeric(int_s))

# start with one species
if(exists("sps")) y1 <- y1 %>% 
  filter(AOU_Code == sps)

# remove vizualizations (only 12 total in MABI)
y1 <- y1 %>% 
  filter(ID_Method != "Visualization")

y <- array(0, 
           dim = c(nyr_s, nsite, ninterval, ndist),
           dimnames = list(years_s, sites, intervals$int_s, c(1,2)))

test <- as_tibble(matrix(NA, ncol = 5, nrow = 1))
colnames(test) <- c("t", "j", "k", "d")

for(t in 1:nyr_s){
  for(j in 1:nsite) {
    for(k in 1:ninterval){
      for(d in 1:ndist){
        y[t,j,k,d] <- y1 %>% 
          filter(Year == years[t],
                 Point_Name == sites[j],
                 int_s == k,
                 dist == d - 1
                 ) %>% 
          select(Bird_Count) %>% 
          sum()
        if(y[t,j,k,d] > 0) {test <- rbind(test, cbind(t, j, k, d))}
      }
    }
  }
  print(t)
}

# saveRDS(y, file = "ymaster.rds")
sum(y) == sum(y1$Bird_Count)  ## :(

# get covariates

# X2 = EventDate, X3 = ID_Method_Code, X4 = Initial_Three_Min_Cnt,
#   X5 = Observer, X6 = Skill_Level, X7 = Year, X8 = Point_Name
X1 <- array(0, 
            dim = c(nyr_s, nsite, ninterval, ndist),
            dimnames = list(years_s, sites, intervals$int_s, c(1,2)))

X8 <- X7 <- X6 <- X5 <- X4 <- X3 <- X2 <- X1

y1 %>% filter(EventDate == 164, Point_Name == "MABI1001", Year == 0, dist == 1)

for(t in 1:nyr_s){
  for(j in 1:nsite) {
    for(k in 1:ninterval){
      for(d in 1:ndist){
        if (nrow(y1 %>% filter(Year == years[t], Point_Name == sites[j], int_s == k, dist == d - 1)) > 0) {
          lopd <- y1 %>% filter(Year == years[t], Point_Name == sites[j], int_s == k, dist == d - 1) %>% distinct()
          X8[t,j,k,d] <- lopd$Point_Name
          X7[t,j,k,d] <- lopd$Year
          X6[t,j,k,d] <- lopd$Skill_Level
          X5[t,j,k,d] <- lopd$Observer
          X4[t,j,k,d] <- lopd$Initial_Three_Min_Cnt
          X3[t,j,k,d] <- lopd$ID_Method_Code
          X2[t,j,k,d] <- lopd$EventDate
          X1[t,j,k,d] <- lopd$PT_DESC
        }
      }
    }
  }
  print(t)
}













y3 <- aperm(y, c(4,3,1,2))

# remove occasions
y4 <- apply(y3, MARGIN=c(1, 2, 3), sum)

sum(y3) == sum(y4)

# forest type
X <- as_tibble(matrix(0, ncol = 1, nrow = n_site))
colnames(X)[1] <- "Point_Name"
X$Point_Name <- site_n
X <- left_join(X, y2[,c("Point_Name","PT_DESC")], by = "Point_Name") %>% 
  rename(Forest = PT_DESC) %>% 
  mutate(Forest = ifelse(Forest=="N. Hardwoods",0,1)) %>% 
  distinct() %>% 
  dplyr::select(Forest)

## alpha
det_vars <- y2 %>% 
  dplyr::select(EventDate, ID_Method_Code, Initial_Three_Min_Cnt,
                Observer, Skill_Level, Year, Point_Name, Distance, int_s) %>% 
  distinct() %>% 
  mutate(EventDate = format(EventDate, "%j"),
         Year = Year - min_yr,
         ID_Method_Code = ifelse(ID_Method_Code == 'A', 0, 1),
         Distance = ifelse(Distance == "< 50 Meters", 0, 1))

det_vars2 <- y2  %>% 
  dplyr::select(Year, Point_Name, int_s) %>% 
  distinct()

det_vars3 <- left_join(det_vars2, det_vars, by = c("Year", "Point_Name"))

X2 <- X3 <- X4 <- X6 <- X7 <- as_tibble(matrix(as.numeric(0), nrow = n_site))
X5 <- X8 <- as_tibble(matrix(as.character(NA),  nrow = n_site))
                                        
for(t in 1:yr_mx_s){
  for(j in 1:n_site){
    X2[t,j] <- det_vars2 %>% filter(Point_Name == site_n[j], Year == yrs[t]) %>% select(EventDate) %>% filter(row_number()==1) %>% as.numeric()
    X3[t,j] <- det_vars2 %>% filter(Point_Name == site_n[j], Year == yrs[t]) %>% select(ID_Method_Code) %>% filter(row_number()==1) %>% as.numeric()
    X4[t,j] <- det_vars2 %>% filter(Point_Name == site_n[j], Year == yrs[t]) %>% select(Initial_Three_Min_Cnt) %>% filter(row_number()==1) %>% as.numeric()
    X5[t,j] <- det_vars2 %>% filter(Point_Name == site_n[j], Year == yrs[t]) %>% select(Observer) %>% filter(row_number()==1) %>% as.character()
    X6[t,j] <- det_vars2 %>% filter(Point_Name == site_n[j], Year == yrs[t]) %>% select(Skill_Level) %>% filter(row_number()==1) %>% as.numeric()
    X7[t,j] <- det_vars2 %>% filter(Point_Name == site_n[j], Year == yrs[t]) %>% select(Year) %>% filter(row_number()==1) %>% as.numeric()
    X8[t,j] <- ifelse((det_vars2 %>% filter(Point_Name == site_n[j], Year == yrs[t]) %>% select(Point_Name) %>% filter(row_number()==1) %>% nrow() == 0),
                      NA, det_vars2 %>% filter(Point_Name == site_n[j], Year == yrs[t]) %>% select(Point_Name) %>% filter(row_number()==1) %>% pull())
  }
}

# model - abundance of one species varying according to site using removal sampling (no year, no occasion, no covariates)
cat("
model {
  # Prior distributions
  p0 ~ dunif(0,1)
  alpha0 <- logit(p0)

  beta0 ~ dnorm(0, 0.01)
  beta1 ~ dnorm(0, 0.01)

  for(i in 1:n_site){            ## sites
    # logit-linear model for detection: understory cover effect
    logit(p[i]) <- alpha0 #+ alpha1 * X[i,1]
    
    # log-linear model for abundance: UFC D TRBA D UFC:TRBA
    log(lambda[i]) <- beta0 + beta1*X[i,1] #+ beta2*X[i,2] + beta3*X[i,2]*X[i,1]

    for(j in 1:n_interval){      ## intervals
    # Poisson parameter [ multinomial cellprobs x expected abundance
      pi[i,j] <- (p[i] * (1 - p[i])^(j-1)) * lambda[i]
      y[i,j] ~ dpois(pi[i,j])
    }

    # Generate predictions of N[i]
    N[i] ~ dpois(lambda[i])
    } 
  }",
fill=TRUE,
file="modelP2.txt")

# Bundle up the data and inits
data <- list(y = y3, n_interval = n_interval, n_site = n_site, X = X) #, X=as.matrix(siteCovs(ovenFrame)))

str(data) # Good practice to always inspect your BUGS data

inits <- function(){
  list (p0 = runif(1), beta0=runif(1), beta1=runif(1))
}
# Define parameters to save and MCMC settings
params <- c("p0", "alpha0", "beta0", "beta1","N")
nc <- 3 ; ni <- 6000 ; nb <- 1000 ; nt <- 1

out <- jags(data, inits, params, "modelP2.txt", n.thin=nt,
            n.chains = nc, n.burnin = nb, n.iter = ni)
print(out, 3)
summary(out)

