## HEADER ####
## who: J Collins
## what: crop growth
## when: 2024-10-15

getwd()

setwd("~/OneDrive - Harper Adams University/Data/agronomy/")

## 00 packages ####

library(ggplot2)
library(ggpubr)
library(dplyr) # summary table 
library(ggpmisc)
library(ggsignif) # significance on barplots
library(readxl)

## 01 DATA ####

dat <- read_excel(path = "data/crop_growth/crop_data.xlsx")

dat <- filter(dat, treatment == "Conventional" | treatment == "Conservation")

# Organise factors
dat$plot <- as.factor(dat$plot)
dat$block <- as.factor(dat$block)
dat$Treatment <- as.factor(dat$treatment)

dat$target_plants_m2 <- 0
dat$target_plants_m2 <- ifelse(test = dat$crop == "Spring Beans", yes = 50, no = dat$target_plants_m2)
dat$target_plants_m2 <- ifelse(test = dat$crop == "Winter Wheat", yes = 260, no = dat$target_plants_m2)
dat$target_plants_m2 <- ifelse(test = dat$crop == "Oilseed Rape", yes = 20, no = dat$target_plants_m2)
dat$target_plants_m2 <- ifelse(test = dat$crop == "Spring Barley", yes = 300, no = dat$target_plants_m2)

# drilling rates 
dat$drilling_rate_kg_ha <- 0
dat$drilling_rate_kg_ha <- ifelse(test = dat$crop == "Spring Beans" & dat$treatment == "Conventional",
                                  yes = 300, no = dat$drilling_rate_kg_ha)
dat$drilling_rate_kg_ha <- ifelse(test = dat$crop == "Spring Beans" & dat$treatment == "Conservation",
                                  yes = 330, no = dat$drilling_rate_kg_ha)

dat$drilling_rate_kg_ha <- ifelse(test = dat$crop == "Winter Wheat" & dat$treatment == "Conventional",
                                  yes = 200, no = dat$drilling_rate_kg_ha)
dat$drilling_rate_kg_ha <- ifelse(test = dat$crop == "Winter Wheat" & dat$treatment == "Conservation",
                                  yes = 200, no = dat$drilling_rate_kg_ha)

dat$drilling_rate_kg_ha <- ifelse(test = dat$crop == "Oilseed Rape" & dat$treatment == "Conventional",
                                  yes = 2.5, no = dat$drilling_rate_kg_ha)
dat$drilling_rate_kg_ha <- ifelse(test = dat$crop == "Spring Barley" & dat$treatment == "Conservation",
                                  yes = 200, no = dat$drilling_rate_kg_ha)

# tgw
dat$tgw <- 0
dat$tgw <- ifelse(test = dat$crop == "Spring Beans" & dat$treatment == "Conventional",
                                  yes = 550, no = dat$tgw)
dat$tgw <- ifelse(test = dat$crop == "Spring Beans" & dat$treatment == "Conservation",
                                  yes = 550, no = dat$tgw)

dat$tgw <- ifelse(test = dat$crop == "Winter Wheat" & dat$treatment == "Conventional",
                                  yes = 55, no = dat$tgw)
dat$tgw <- ifelse(test = dat$crop == "Winter Wheat" & dat$treatment == "Conservation",
                                  yes = 55, no = dat$tgw)

dat$tgw <- ifelse(test = dat$crop == "Oilseed Rape" & dat$treatment == "Conventional",
                                  yes = 5, no = dat$tgw)
dat$tgw <- ifelse(test = dat$crop == "Spring Barley" & dat$treatment == "Conservation",
                                  yes = 50, no = dat$tgw)



dat$seeds_m2 <- (dat$drilling_rate_kg_ha * 100) / dat$tgw

dat$pc_reccomended_plants <- (dat$plants_m2 / dat$target_plants_m2) * 100

dat$loss_pc <- (1 - (dat$plants_m2 / dat$seeds_m2)) * 100

dat$harvest_index <- (dat$grain_m2 / (dat$biomass_dm_m2 + dat$grain_m2)) * 100


dat <- dat %>%
  mutate(across(6:ncol(dat), as.numeric))

# View the result
glimpse(dat)



## 02 SUMMARY STATS ####

# Calculates mean, sd, se and IC - block
plants_m2_sum <- dat %>%
  group_by(treatment, year, crop) %>%
  summarise( 
    n=n(),
    mean=mean(plants_m2),
    sd=sd(plants_m2)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))

# Calculates mean, sd, se and IC - block
plants_pc_m2_sum <- dat %>%
  group_by(treatment, year, crop) %>%
  summarise( 
    n=n(),
    mean=mean(pc_reccomended_plants),
    sd=sd(pc_reccomended_plants)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))

loss_pc_sum <- dat %>%
  group_by(treatment, year, crop) %>%
  summarise( 
    n=n(),
    mean=mean(loss_pc),
    sd=sd(loss_pc)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))

shoots_m2_sum <- dat %>%
  group_by(treatment, year) %>%
  summarise( 
    n=n(),
    mean=mean(shoots_m2),
    sd=sd(shoots_m2)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))

ears_m2_sum <- dat %>%
  group_by(treatment, year) %>%
  summarise( 
    n=n(),
    mean=mean(pods_ears_m2),
    sd=sd(pods_ears_m2)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))

biomass_dm_m2_sum <- dat %>%
  group_by(treatment, year) %>%
  summarise( 
    n=n(),
    mean=mean(biomass_dm_m2),
    sd=sd(biomass_dm_m2)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))


height_sum <- dat %>%
  group_by(treatment, year) %>%
  summarise( 
    n=n(),
    mean=mean(crop_height_cm),
    sd=sd(crop_height_cm)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))

harvest_index_sum <- dat %>%
  group_by(treatment, year) %>%
  summarise( 
    n=n(),
    mean=mean(harvest_index),
    sd=sd(harvest_index)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))




## 03 PLOTS ####

### PLANTS M2 PLOT ####

title_exp <- expression(Plants~(M^{2}))  # this is the legend title with correct notation

y_title <- expression(Plants~(M^{2}))

c1 <- ggplot(data = plants_m2_sum, 
             aes(x = treatment, 
                 y = mean, 
                 fill = treatment)) + 
  geom_bar(stat = "identity", 
           color = "black", 
           position = "dodge") + 
  labs(
    x = "Treatment",
    y = y_title,
    subtitle = title_exp, 
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
  geom_errorbar(aes(ymin = mean - se, 
                    ymax = mean + se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) +
  facet_wrap(~ year + crop, 
             ncol = 4, 
             scales = 'free_x') 

c1


### percentage establishment plot ####

title_exp <- expression(Plants~(M^{2}))  # this is the legend title with correct notation

y_title <- expression(Plants~(M^{2}))

c2 <- ggplot(data = plants_pc_m2_sum, 
             aes(x = treatment, 
                 y = mean, 
                 fill = treatment)) + 
  geom_bar(stat = "identity", 
           color = "black", 
           position = "dodge") + 
  labs(
    x = "Treatment",
    y = "Target plant population (%)",
    subtitle = "Percentage achieved of reccomended plant population (%)", 
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
  geom_errorbar(aes(ymin = mean - se, 
                    ymax = mean + se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) +
  facet_wrap(~ year, 
             ncol = 4, 
             scales = 'free_x') 

c2


# geom_signif(
#   data = subset(my_sum, year == 2022), # Subset data for Crop1
#   comparisons = list(c("Conventional", "Conservation")),
#   map_signif_level = TRUE,
#   textsize = 4,
#   tip_length = 0.01, 
#   annotations = "NS.", 
#   fontface = 'italic', 
#   y_position = c(20) # Adjust y-position if necessary
# ) +
# geom_signif(
#   data = subset(my_sum, year == "2023"), # Subset data for Crop2
#   comparisons = list(c("Conventional", "Conservation")),
#   map_signif_level = TRUE,
#   textsize = 4,
#   tip_length = 0.01,
#   annotations = "NS.",
#   fontface = 'italic', 
#   y_position = c(20) # Adjust y-position if necessary
# ) 


### percentage losses ####

title_exp <- expression(Losses~("%")~(M^{2}))  # this is the legend title with correct notation

y_title <- expression(Losses~("%")~(M^{2}))

c3 <- ggplot(data = loss_pc_sum, 
             aes(x = treatment, 
                 y = mean, 
                 fill = treatment)) + 
  geom_bar(stat = "identity", 
           color = "black", 
           position = "dodge") + 
  labs(
    x = "Treatment",
    y = "Losses (%)",
    subtitle = "Percentage of losses from seed planted (%)", 
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
  geom_errorbar(aes(ymin = mean - se, 
                    ymax = mean + se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) +
  facet_wrap(~ year, 
             ncol = 4, 
             scales = 'free_x') 

c3


ggarrange(c1, c2, c3, ncol = 3, common.legend = TRUE, legend = "bottom")

ggsave(filename = "plots/fig_plant_est_plot.png", width = 14, height = 6)






### biomass dm m2 ####

title_exp <- expression(Biomass~dry~matter~(g~M^{2}))  # this is the legend title with correct notation

y_title <- expression(Biomass~dry~matter~(g~M^{2}))

c4 <- ggplot(data = biomass_dm_m2_sum, 
             aes(x = treatment, 
                 y = mean, 
                 fill = treatment)) + 
  geom_bar(stat = "identity", 
           color = "black", 
           position = "dodge") + 
  labs(
    x = "Treatment",
    y = y_title,
    subtitle = title_exp, 
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
  geom_errorbar(aes(ymin = mean - se, 
                    ymax = mean + se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) +
  facet_wrap(~ year, 
             ncol = 4, 
             scales = 'free_x') 

c4



### crop height ####

title_exp <- expression(Crop~heightr~(cm))  # this is the legend title with correct notation

y_title <- expression(Crop~heightr~(cm))

c5 <- ggplot(data = height_sum, 
             aes(x = treatment, 
                 y = mean, 
                 fill = treatment)) + 
  geom_bar(stat = "identity", 
           color = "black", 
           position = "dodge") + 
  labs(
    x = "Treatment",
    y = y_title,
    subtitle = title_exp, 
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
  geom_errorbar(aes(ymin = mean - se, 
                    ymax = mean + se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) +
  facet_wrap(~ year, 
             ncol = 4, 
             scales = 'free_x') 

c5




### ears / pods m2 ####

title_exp <- expression(Ears/pods~(M^{-1}))  # this is the legend title with correct notation

y_title <- expression(Crop~heightr~(cm))

c6 <- ggplot(data = ears_m2_sum, 
             aes(x = treatment, 
                 y = mean, 
                 fill = treatment)) + 
  geom_bar(stat = "identity", 
           color = "black", 
           position = "dodge") + 
  labs(
    x = "Treatment",
    y = y_title,
    subtitle = title_exp, 
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
  geom_errorbar(aes(ymin = mean - se, 
                    ymax = mean + se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) +
  facet_wrap(~ year, 
             ncol = 4, 
             scales = 'free_x') 

c6


### shoots m2 ####

title_exp <- expression(Shoots~(M^{-1}))  # this is the legend title with correct notation

y_title <- expression(Shoots~(M^{-1}))

c7 <- ggplot(data = shoots_m2_sum, 
             aes(x = treatment, 
                 y = mean, 
                 fill = treatment)) + 
  geom_bar(stat = "identity", 
           color = "black", 
           position = "dodge") + 
  labs(
    x = "Treatment",
    y = y_title,
    subtitle = title_exp, 
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
  geom_errorbar(aes(ymin = mean - se, 
                    ymax = mean + se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) +
  facet_wrap(~ year, 
             ncol = 4, 
             scales = 'free_x') 

c7



### harvest index ####

title_exp <- expression(havrest~index)  # this is the legend title with correct notation

y_title <- expression(havrest~index)

c8 <- ggplot(data = harvest_index_sum, 
             aes(x = treatment, 
                 y = mean, 
                 fill = treatment)) + 
  geom_bar(stat = "identity", 
           color = "black", 
           position = "dodge") + 
  labs(
    x = "Treatment",
    y = y_title,
    subtitle = title_exp, 
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
  geom_errorbar(aes(ymin = mean - se, 
                    ymax = mean + se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) +
  facet_wrap(~ year, 
             ncol = 4, 
             scales = 'free_x') 

c8



ggarrange(c4, c5, c6, c7, c8, ncol = 3, nrow = 2, common.legend = TRUE, legend = "bottom")

ggsave(filename = "plots/fig_crop_growth_plot.png", width = 14, height = 6)

