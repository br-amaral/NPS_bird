## teste
library(tidyverse)
test <- cbind(rep(1:3,3), rep(5:7,3))

test2 <- as_tibble(test)

write_rds(test2, file = "data/test2.rds")