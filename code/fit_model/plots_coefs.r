library(tidyverse)
library(glue)
library(ggplot2)
library(viridis)

hg <- httpgd::hgd()

dat <- read_csv("data/coefs_res_w0.csv")  %>% 
            arrange(sca, cov, sps)  %>% 
            mutate(sps_p = glue("{row_number()}_{sps}")) %>% 
            as_tibble() %>% 
            mutate(sps = toupper(sps),
                   sps_p = factor(sps_p, levels = sps_p)) %>% 
            rename(Covariate = cov)

cov_name <- cbind(unique(dat$Covariate),
                  c("Tree Basal Area",
                    "Tree Density",
                    "Tree Diversity",
                    "Late Successional Tree Density",
                    "Early Successional Tree Density",
                    "Shrub Density")) %>% 
            as_tibble() %>% 
            rename(Covariate = V1,
                   cov_name = V2)  %>% 
            mutate(cov_name = factor(cov_name, 
                              levels = c("Tree Basal Area",
                                         "Early Successional Tree Density",
                                         "Late Successional Tree Density",
                                         "Tree Density",
                                         "Tree Diversity",
                                         "Shrub Density")))

sca_name <- cbind(unique(dat$sca),
                  c("Site Scale",
                    "Park Scale",
                    "Landscape Scale")) %>% 
            as_tibble() %>% 
            rename(sca = V1,
                   sca_name = V2)  %>% 
            mutate(sca_name = factor(sca_name, 
                              levels = c("Site Scale",
                                         "Park Scale",
                                         "Landscape Scale")),
                   sca = as.numeric(sca))

dat <- left_join(dat, cov_name, by = "Covariate") %>% 
       select(-Covariate)  %>% 
       rename(Covariate = cov_name) %>% 
       left_join(.,sca_name, by = "sca") %>% 
       select(-sca) %>% 
       rename(sca = sca_name) 

ggplot(dat) +
        geom_linerange(aes(x = sps_p, ymin = low, ymax = up, color = sps)) +
        geom_point(aes(x = sps_p, y = mean, color = sps, shape = Covariate)) +
        theme_bw() +
        theme(panel.grid = element_blank(),
              axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
        geom_hline(yintercept = 0,  color = "black") +
        scale_color_viridis_d(name = "Species", option = "C") +
        labs(x = NULL,  # Removes the x-axis title
             y = "Covariate effect size")  # Adds a title to the y-axis

ggsave("my_plot.svg", plot = last_plot(), device = "svg", width = 8, height = 5)

ggplot(dat) +
        geom_linerange(aes(x = sps_p, ymin = low, ymax = up, color = sps)) +
        geom_point(aes(x = sps_p, y = mean, color = sps, shape = Covariate)) +
        theme_bw() +
        theme(panel.grid = element_blank(),
              axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
        geom_hline(yintercept = 0,  color = "black") +
        scale_color_viridis_d(name = "Species", option = "C") +
        labs(x = NULL,  # Removes the x-axis title
             y = "Covariate effect size") + # Adds a title to the y-axis
        facet_wrap(~sca, nrow = 3)

ggplot(dat) +
        geom_linerange(aes(x = sps, ymin = low, ymax = up, color = Covariate)) +
        geom_point(aes(x = sps, y = mean, color = Covariate)) +
        theme_bw() +
        theme(panel.grid = element_blank(),
              axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
        geom_hline(yintercept = 0,  color = "black") +
        scale_color_viridis_d(name = "Covariate", option = "C") +
        labs(x = NULL,  # Removes the x-axis title
             y = "Covariate effect size") + # Adds a title to the y-axis
        facet_wrap(~sca, nrow = 3)

# add all variables that were not selected. without CI for step 1, with CI for step 2

ggplot(dat) +
        geom_linerange(aes(x = Covariate, ymin = low, ymax = up, color = sps)) +
        geom_point(aes(x = Covariate, y = mean, color = sps)) +
        theme_bw() +
        theme(panel.grid = element_blank(),
              axis.text.x = element_text(size = 10), 
              axis.text.y = element_text(size = 12),
              axis.title.x = element_text(size = 14), 
              axis.title.y = element_text(size = 14),
              strip.text = element_text(size = 16, face = "bold")) +
        geom_hline(yintercept = 0,  color = "black") +
        scale_color_viridis_d(name = "Species", option = "C") +
        scale_x_discrete(labels = c("Tree Basal \nArea",
                                    "Early Success. \nTree Density",
                                    "Late Success. \nTree Density",
                                    "Tree Density",
                                    "Tree Diversity",
                                    "Shrub Density")) + # Wraps text by replacing spaces with line breaks
        labs(x = NULL,  # Removes the x-axis title
             y = "Covariate effect size \n") + # Adds a title to the y-axis
        facet_wrap(~sca, nrow = 3)

ggsave("my_plot2.svg", plot = last_plot(), device = "svg", width = 9, height = 8)

#### with zeros ----------------------------------------------------------
dat <- read_csv("data/coefs_res_w0.csv")  %>% 
            arrange(sca, cov, sps)  %>% 
            mutate(sps_p = glue("{row_number()}_{sps}")) %>% 
            as_tibble() %>% 
            mutate(sps = toupper(sps),
                   sps_p = factor(sps_p, levels = sps_p)) %>% 
            rename(Covariate = cov)

cov_name <- cbind(unique(dat$Covariate),
                  c("Tree Basal Area",
                    "Tree Density",
                    "Tree Diversity",
                    "Late Successional Tree Density",
                    "Early Successional Tree Density",
                    "Shrub Density")) %>% 
            as_tibble() %>% 
            rename(Covariate = V1,
                   cov_name = V2)  %>% 
            mutate(cov_name = factor(cov_name, 
                              levels = c("Tree Basal Area",
                                         "Early Successional Tree Density",
                                         "Late Successional Tree Density",
                                         "Tree Density",
                                         "Tree Diversity",
                                         "Shrub Density")))

sca_name <- cbind(sort(unique(dat$sca), decreasing = F),
                  c("Site Scale","Park Scale","Landscape Scale")) %>% 
            as_tibble() %>% 
            rename(sca = V1,
                   sca_name = V2)  %>% 
            mutate(sca_name = factor(sca_name, 
                              levels = c("Site Scale","Park Scale","Landscape Scale")),
                   sca = as.numeric(sca))

dat <- left_join(dat, cov_name, by = "Covariate") %>% 
       select(-Covariate)  %>% 
       rename(Covariate = cov_name) %>% 
       left_join(.,sca_name, by = "sca") %>% 
       select(-sca) %>% 
       rename(sca = sca_name) 

dat <- dat %>% 
      mutate(sca_col = "darkolivegreen2")
dat$sca_col <- ifelse(dat$sca == "Park Scale", "darkolivegreen3", dat$sca_col)
dat$sca_col <- ifelse(dat$sca == "Landscape Scale", "darkolivegreen4", dat$sca_col)

dat$sps <- ifelse(dat$step != 3, "grey", dat$sps)

data_zero <- dat %>%
                filter(sps == "grey")
dat_sps <-  dat %>%
                filter(sps != "grey")

sca_col <- c("#B0EDB9", "#64CC81", "#088A0F")
sca <- c("Site Scale","Park Scale","Landscape Scale")
dat_col <- as_tibble(cbind(sca_col, sca))  %>% 
                  mutate(sca = factor(sca, 
                        levels = c("Site Scale","Park Scale","Landscape Scale")))
library(ggh4x)


ggplot() +
  geom_rect(data = dat_col, aes(fill = sca_col),
            xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.3) +
            scale_fill_identity() +
        geom_linerange(data = data_zero, aes(x = cov_sps, ymin = low, ymax = up), color = "#A8B3AA", linewidth = 1) +
        geom_point(data = data_zero, aes(x = cov_sps, y = mean), color = "#A8B3AA", size = 2.5) +
        geom_linerange(data = dat_sps, aes(x = cov_sps, ymin = low, ymax = up), color = "black", linewidth = 1) +
        geom_point(data = dat_sps, aes(x = cov_sps, y = mean), color = "black", size = 2.5) +
        theme_bw() +
        theme(panel.grid = element_blank(),
              axis.text.x = element_text(size = 10, angle = 90), 
              axis.text.y = element_text(size = 10),
              axis.title.x = element_text(size = 14), 
              axis.title.y = element_text(size = 14),
              axis.line = element_line(color = "black", linewidth = 0.7), # Adjusts axis line thickness
              strip.text = element_text(size = 12, face = "bold"),
              panel.spacing = unit(0,"line"),
              panel.border = element_rect(color = "black", linewidth = 0.6), #  borders thickness
              strip.background=element_rect(color="black", fill="white", linewidth = 0.8)) +
        geom_hline(yintercept = 0,  color = "gray", linetype = "dashed") +
        scale_color_viridis_d(name = "Species", option = "C") +
 #       scale_x_discrete(labels = c("Tree Basal \nArea",
 #                                   "Early Success. \nTree Density",
 #                                   "Late Success. \nTree Density",
 #                                   "Tree Density",
 #                                   "Tree Diversity",
 #                                   "Shrub Density")) + # Wraps text by replacing spaces with line breaks
        scale_y_continuous(breaks = seq(-5, 5, by = 2.5), limits = c(-5.2, 5.2)) + # Sets y-axis breaks from -5 to 5 with step of 1
        labs(x = NULL,  # Removes the x-axis title
             y = "Covariate effect size \n") + # Adds a title to the y-axis
        scale_x_discrete(labels = toupper(substr(dat$sps_p, nchar(as.character(dat$sps_p))-3, nchar(as.character(dat$sps_p))))) +   # add species names a xaxis labels
        #facet_wrap(~sca, nrow = 3)
        facet_nested(sca ~ Covariate, scales = "free_x", space = "free_x",
                     labeller = labeller(Covariate = c("Tree Basal Area" = "Tree Basal \nArea", 
                                                       "Early Successional Tree Density" = "Early Success. \nTree Density",
                                                       "Late Successional Tree Density" = "Late Success. \nTree Density"
                     )))



ggsave("manus_figs/fig3new.svg", plot = last_plot(), device = "svg", width = 9, height = 8)


