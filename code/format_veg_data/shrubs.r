library(tidyverse)

shrubc <- read_rds("data/out/shrub_county.rds")
shrubs <- read_rds("data/out/shrub_site.rds")

# FIA ----------------------------------------------------------------------------
table(shrubc %>% select(YEAR, PLT_CN))  %>% dim()
table(table(shrubc %>% select(YEAR, PLT_CN)) == 0)
table(shrubc %>% select(YEAR, PLT_CN))  %>% sum()
# 13 years and 74 sites in total of 962 possible occasions
# only 360 samples in 74 combinations of site year (shrub, vine, 0-2 ft, 2.1- 6ft)

par(mfrow = c(2,1))
hist(shrubc$PROP_COVER)

# NPS ----------------------------------------------------------------------------
table(shrubs %>% select(SampleYear, Plot_Name))  %>% dim()
table(table(shrubs %>% select(SampleYear, Plot_Name)) == 0)
table(shrubs %>% select(SampleYear, Plot_Name))  %>% sum()
# 17 years and 176 sites in total of 2992 possible occasions
# only 1650 samples in 791 combinations of site year ("Ground", "Mid-understory")

hist(shrubs$shrub_avg_cov)

