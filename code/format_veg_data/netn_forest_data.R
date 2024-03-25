library(corrplot)
library(tidyverse)

percentForest <- read_rds(file = "~/Documents/GitHub/NPS_birds/paper_scripts/Doser2021/percentforest.rds")
regen <- read_rds(file = "~/Documents/GitHub/NPS_birds/paper_scripts/Doser2021/regen.rds")
basalArea <- read_rds(file = "~/Documents/GitHub/NPS_birds/paper_scripts/Doser2021/basalArea.rds")

percentForest <- percentForest[-c(5,7),] %>% as_tibble()
basalArea <- basalArea[-c(5,7),] %>% as_tibble()
regen <- regen[-c(5,7),] %>% as_tibble()

basare %>% 
  ggplot(aes(x = park, y = basare)) +
  geom_point()


parks_less <- c("ACAD","MABI","MIMA","MORR","SAGA","WEFA")

percentForest$park <- parks_less
basalArea$park <- parks_less
regen$park <- parks_less

perfor <- pivot_longer(percentForest,
                       cols = starts_with("V"),
                       names_to = "site",
                       values_to = "perfor",
                       values_drop_na = TRUE) %>% 
  distinct()
basare <- pivot_longer(basalArea,
                       cols = starts_with("V"),
                       names_to = "site",
                       values_to = "basare",
                       values_drop_na = TRUE)
regene <- pivot_longer(regen,
                       cols = starts_with("V"),
                       names_to = "site",
                       values_to = "regen", 
                       values_drop_na = TRUE)

local_cors <-full_join(perfor, basare, by = c("park","site"))
local_cors <-full_join(local_cors, regene, by = c("park","site"))

local_cors[,3:5] %>% 
  cor(use="pairwise.complete.obs") %>% 
  corrplot(method = 'square', addCoef.col = 'black', 
           cl.pos = 'n', col = COL2('BrBG'), type = 'lower', number.cex=0.5)

local_cors_p <- local_cors %>% 
  group_by(park) %>% 
  summarise(perfor_m = mean(perfor, na.rm = T),
            basare_m = mean(basare, na.rm = T),
            regen_m = mean(regen, na.rm = T))


text_cors <- left_join(local_cors, local_cors_p, by = "park")
text_cors[,c(3,6)]%>% 
  cor(use="pairwise.complete.obs") %>% 
  corrplot(method = 'square', addCoef.col = 'black', 
           cl.pos = 'n', col = COL2('BrBG'), type = 'lower', number.cex=0.5)

text_cors[,c(4,7)]%>% 
  cor(use="pairwise.complete.obs") %>% 
  corrplot(method = 'square', addCoef.col = 'black', 
           cl.pos = 'n', col = COL2('BrBG'), type = 'lower', number.cex=0.5)

text_cors[,c(5,8)]%>% 
  cor(use="pairwise.complete.obs") %>% 
  corrplot(method = 'square', addCoef.col = 'black', 
           cl.pos = 'n', col = COL2('BrBG'), type = 'lower', number.cex=0.5)

local_cors %>% 
  pivot_longer(cols = c(perfor, basare,  regen), names_to = 'metric', values_to = 'value') %>% 
  ggplot() +
  geom_boxplot(aes(x = park, y = value), fill="#AC7352") +
  theme_bw() +
  facet_wrap(~metric, scales = "free_y") +
  theme(axis.text.x = element_text(angle = 90), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 

local_cors_p %>% 
  pivot_longer(cols = c(perfor_m, basare_m, regen_m), names_to = 'metric', values_to = 'value') %>% 
  ggplot() +
  geom_point(aes(x = park, y = value), fill= "#AC7352",#"#BF87B3",
             size = 4, colour="black", pch=21) +
  theme_bw() +
  facet_wrap(~metric, scales = "free_y") +
  theme(axis.text.x = element_text(angle = 90), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 

comp <- left_join(local_cors, local_cors_p, by = "park")

comp <- comp %>% 
  dplyr::select(park, site, perfor, perfor_m, basare, basare_m)

tpa_fim2 <- tpa_fim %>% 
  dplyr::select(park, TPA, BAA) %>% 
  group_by(park) %>% 
  summarize(TPA_m = mean(TPA),
            BAA_m = mean(BAA))

comp2 <- left_join( tpa_fim2, comp,by = "park")

comp2[,c(2:3,5:8)] %>% 
  cor(use="pairwise.complete.obs") %>% 
  corrplot(method = 'square', addCoef.col = 'black', 
           cl.pos = 'n', col = COL2('BrBG'), type = 'lower', number.cex=0.5)

