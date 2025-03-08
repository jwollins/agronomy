## HEADER ####
## who: J Collins
## what: crop growth
## when: 2024-10-15

setwd(rstudioapi::getActiveProject())

getwd()




#_____________________________________####
# Packages ####

library(ggplot2)
library(ggpubr)
library(dplyr) # summary table 
library(ggpmisc)
library(ggsignif) # significance on barplots
library(readxl)
library(lme4)
library(emmeans)
library(kableExtra)
library(janitor)



#_____________________________________####
# DATA ####

dat <- read.csv(file = "sym_link_agronomy_data/data/crop_growth/processed_crop_data.csv")

# Organise factors
dat$plot <- as.factor(dat$plot)
dat$block <- as.factor(dat$block)
dat$treatment <- as.factor(dat$treatment)
dat$year <- as.factor(dat$year)





#_______________________________________________####
# stats & distributions ####


source(file = "~/Documents/GitHub/phd_tools/fun_distribution_plots.R")
source(file = "~/Documents/GitHub/phd_tools/fun_glm_diagnostic_plots.R")




#__________####
# ~ Plant pop percentage ####

names(dat)

# plot the distribution
distribution_plots(data = dat,
                   variable = dat$pc_reccomended_plants,
                   colour = dat$pc_reccomended_plants)

# save it
ggsave(filename = "sym_link_agronomy_data/plots/distributions/dist_pc_reccomended_plants.png", 
       width = 10, height = 2.25)


# model it
lm_model <- lmer(formula = pc_reccomended_plants ~ treatment + (1 | year) + (1 | crop), 
                 data = dat)

# View summary
summary(lm_model)

# Run pairwise comparisons for the 'Treatment' factor
pairwise_comparisons <- emmeans(lm_model, pairwise ~ treatment)

# View the results of pairwise comparisons
summary(pairwise_comparisons)


diagnostic_plots_glm(model = lm_model)

ggsave(filename = "sym_link_agronomy_data/plots/model_diagnostics/model_pc_reccomended_plants.png", 
       width = 10, height = 3.5)





# ~ Seed loss percentage ####

names(dat)

# plot the distribution
distribution_plots(data = dat,
                   variable = dat$loss_pc,
                   colour = dat$loss_pc)

# save it
ggsave(filename = "sym_link_agronomy_data/plots/distributions/dist_loss_pc.png", 
       width = 10, height = 2.25)


# model it
lm_model <- lmer(formula = loss_pc ~ treatment + (1 | year) + (1 | crop), 
                 data = dat)

# View summary
summary(lm_model)

# Run pairwise comparisons for the 'Treatment' factor
pairwise_comparisons <- emmeans(lm_model, pairwise ~ treatment)

# View the results of pairwise comparisons
summary(pairwise_comparisons)


diagnostic_plots_glm(model = lm_model)

ggsave(filename = "sym_link_agronomy_data/plots/model_diagnostics/model_loss_pc.png", 
       width = 10, height = 3.5)






























