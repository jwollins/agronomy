### HAU Conservation Ag Experiment
## Agronomy - Applications
## Joe Collins 
## 2024-12-08

setwd(dir = "~/OneDrive - Harper Adams University/Data/agronomy/")

## 01 Packages ####

suppressPackageStartupMessages({
  if (!require(dplyr)) install.packages("dplyr")
  if (!require(ggplot2)) install.packages("ggplot2")
  if (!require(ggpubr)) install.packages("ggpubr")
  if (!require(gridExtra)) install.packages("gridExtra")
  if (!require(readxl)) install.packages("readxl")
  if (!require(readr)) install.packages("readr")
  if (!require(plotrix)) install.packages("plotrix")
  if (!require(lmerTest)) install.packages("lmerTest")
  if (!require(openxlsx)) install.packages("openxlsx")
  
  library(dplyr) # for "glimpse" and data manipulation
  library(ggplot2) # general plotting
  library(ggpubr) # custom plotting
  library(gridExtra) # grid plotting
  library(readxl) # read .xlsx files
  library(readr) # read .txt files
  library(plotrix) # standard error
  library(lmerTest) # linear mixed effect models
  library(openxlsx) # read xl files 
})


## 02 - Data ####

ap_dat <- read.csv(file = "data/spray_plans/normalized_application_data.csv")

ap_dat$treatment <- as.factor(ap_dat$treatment)
ap_dat$crop <- as.factor(ap_dat$crop)
ap_dat$year <- as.factor(ap_dat$year)



title <- expression(Active~Ingredient~rate~(g~ha^{-1}))

ggplot(data = ap_dat, 
       aes(x = year, 
           y = normalized_rate_g_ha, group = treatment, 
           fill = treatment)) + 
  geom_bar(stat = "identity", 
           position = "dodge") + 
  labs(
    x = title,
    y = title,
    subtitle = title, 
    caption = "") +
  theme_bw() +
  scale_fill_manual(values=c("turquoise3","tomato2"), 
                    name = "Treatment") +
  theme(strip.text.x = element_text(size = 12, 
                                    color = "black", 
                                    face = "bold.italic"), 
        legend.position = "bottom", 
        axis.title.x = element_blank()) +
  # geom_errorbar(aes(ymin = mean - se, 
  #                   ymax = mean + se),
  #               width=.2,                    # Width of the error bars
  #               position=position_dodge(.9)) +
  facet_wrap(~ category, 
             ncol = 3, 
             scales = 'free_y') 

ggsave(filename = "plots/ap_plot_all.png", width = 10, height = 7)


### 02.1 summary stats ####

# Calculates mean, sd, se and IC - block
ap_sum <- ap_dat %>%
  group_by(treatment, year, category) %>%
  summarise( 
    n = n(),
    mean = mean(normalized_rate_g_ha),
    sd = sd(normalized_rate_g_ha),
    sum = sum(normalized_rate_g_ha)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))


## 03 - Plots ####

### FUNGICIDE PLOT ####

ap_sum_filtered <- ap_sum %>%
  filter(category == "Fungicide")

y_title <- expression(Active~Ingredient~rate~(g~ha^{-1}))

f1 <- ggplot(data = ap_sum_filtered, 
             aes(x = treatment, 
                 y = sum, 
                 fill = treatment)) + 
  geom_bar(stat = "identity", 
           color = "black", 
           position = "dodge") + 
  labs(
    x = "Treatment",
    y = y_title,
    subtitle = "Fungicide", 
    caption = "") +
  theme_bw() +
  scale_fill_manual(values=c("turquoise3","tomato2"), 
                    name = "Treatment") +
  theme(strip.text.x = element_text(size = 12, 
                                    color = "black", 
                                    face = "bold.italic"), 
        legend.position = "bottom", 
        axis.text.x = element_blank(), 
        axis.title.x = element_blank()) +
  # geom_errorbar(aes(ymin = mean - se, 
  #                   ymax = mean + se),
  #               width=.2,                    # Width of the error bars
  #               position=position_dodge(.9)) +
  facet_wrap(~ year, 
             ncol = 3, 
             scales = 'free_x') 

f1



### HERBICIDE PLOT ####

ap_sum_filtered <- ap_sum %>%
  filter(category == "Herbicide")

y_title <- expression(Active~Ingredient~rate~(g~ha^{-1}))

h1 <- ggplot(data = ap_sum_filtered, 
             aes(x = treatment, 
                 y = sum, 
                 fill = treatment)) + 
  geom_bar(stat = "identity", 
           color = "black", 
           position = "dodge") + 
  labs(
    x = "Treatment",
    y = element_blank(),
    subtitle = "Herbicide", 
    caption = "") +
  theme_bw() +
  scale_fill_manual(values=c("turquoise3","tomato2"), 
                    name = "Treatment") +
  theme(strip.text.x = element_text(size = 12, 
                                    color = "black", 
                                    face = "bold.italic"), 
        legend.position = "bottom", 
        axis.text.x = element_blank(), 
        axis.title.x = element_blank()) +
  # geom_errorbar(aes(ymin = mean - se, 
  #                   ymax = mean + se),
  #               width=.2,                    # Width of the error bars
  #               position=position_dodge(.9)) +
  facet_wrap(~ year, 
             ncol = 3, 
             scales = 'free_x') 

h1



### INSECTICIDE PLOT ####

ap_sum_filtered <- ap_sum %>%
  filter(category == "Insecticide")


y_title <- expression(Active~Ingredient~rate~(g~ha^{-1}))

i1 <- ggplot(data = ap_sum_filtered, 
             aes(x = treatment, 
                 y = sum, 
                 fill = treatment)) + 
  geom_bar(stat = "identity", 
           color = "black", 
           position = "dodge") + 
  labs(
    x = "Treatment",
    y = element_blank(),
    subtitle = "Insecticide", 
    caption = "") +
  theme_bw() +
  scale_fill_manual(values=c("tomato2"), 
                    name = "Treatment") +
  theme(strip.text.x = element_text(size = 12, 
                                    color = "black", 
                                    face = "bold.italic"), 
        legend.position = "bottom", 
        axis.text.x = element_blank(), 
        axis.title.x = element_blank()) +
  # geom_errorbar(aes(ymin = mean - se, 
  #                   ymax = mean + se),
  #               width=.2,                    # Width of the error bars
  #               position=position_dodge(.9)) +
  facet_wrap(~ year, 
             ncol = 3, 
             scales = 'free_x') 

i1




### PGR PLOT ####

ap_sum_filtered <- ap_sum %>%
  filter(category == "PGR")

y_title <- expression(Active~Ingredient~rate~(g~ha^{-1}))

pgr1 <- ggplot(data = ap_sum_filtered, 
             aes(x = treatment, 
                 y = sum, 
                 fill = treatment)) + 
  geom_bar(stat = "identity", 
           color = "black", 
           position = "dodge") + 
  labs(
    x = "Treatment",
    y = element_blank(),
    subtitle = "PGR", 
    caption = "") +
  theme_bw() +
  scale_fill_manual(values=c("turquoise3","tomato2"), 
                    name = "Treatment") +
  theme(strip.text.x = element_text(size = 12, 
                                    color = "black", 
                                    face = "bold.italic"), 
        legend.position = "bottom", 
        axis.text.x = element_blank(), 
        axis.title.x = element_blank()) +
  # geom_errorbar(aes(ymin = mean - se, 
  #                   ymax = mean + se),
  #               width=.2,                    # Width of the error bars
  #               position=position_dodge(.9)) +
  facet_wrap(~ year, 
             ncol = 3, 
             scales = 'free_x') 

pgr1



### MOLLUSCICIDE PLOT ####

ap_sum_filtered <- ap_sum %>%
  filter(category == "Molluscicide")




y_title <- expression(Active~Ingredient~rate~(g~ha^{-1}))

m1 <- ggplot(data = ap_sum_filtered, 
             aes(x = treatment, 
                 y = sum, 
                 fill = treatment)) + 
  geom_bar(stat = "identity", 
           color = "black", 
           position = "dodge") + 
  labs(
    x = "Treatment",
    y = element_blank(),
    subtitle = "Molluscicide", 
    caption = "") +
  theme_bw() +
  scale_fill_manual(values=c("turquoise3","tomato2"), 
                    name = "Treatment") +
  theme(strip.text.x = element_text(size = 12, 
                                    color = "black", 
                                    face = "bold.italic"), 
        legend.position = "bottom", 
        axis.text.x = element_blank(), 
        axis.title.x = element_blank()) +
  # geom_errorbar(aes(ymin = mean - se, 
  #                   ymax = mean + se),
  #               width=.2,                    # Width of the error bars
  #               position=position_dodge(.9)) +
  facet_wrap(~ year, 
             ncol = 3, 
             scales = 'free_x') 

m1



ggarrange(f1, h1, i1, pgr1, m1, 
          ncol = 5, nrow = 1, 
          common.legend = TRUE, 
          legend = "bottom", 
          widths = c(1, 1, 1, 0.5, 0.5), labels = c("A", "B", "C", "D", "E"))

ggsave(filename = "plots/AI_plot.png", width = 15, height = 6)

















