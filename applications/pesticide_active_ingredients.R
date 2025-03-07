### HAU Conservation Ag Experiment
## Agronomy - Applications
## Joe Collins 
## 2024-12-08


setwd(rstudioapi::getActiveProject())

getwd()


#___________________________________________####
# Packages ####

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
  if (!require(emmeans)) install.packages("emmeans")
  
  library(dplyr) # for "glimpse" and data manipulation
  library(ggplot2) # general plotting
  library(ggpubr) # custom plotting
  library(gridExtra) # grid plotting
  library(readxl) # read .xlsx files
  library(readr) # read .txt files
  library(plotrix) # standard error
  library(lmerTest) # linear mixed effect models
  library(openxlsx) # read xl files 
  library(emmeans)
})



#___________________________________________####
# Functions ####


source(file = "~/Documents/GitHub/phd_tools/fun_distribution_plots.R")

source(file = "~/Documents/GitHub/phd_tools/fun_glm_diagnostic_plots.R")




#___________________________________________####
# Load Data ####



# ~ read data ####
dat <- read.csv(file = "sym_link_agronomy_data/data/applications_data/normalised_spray_fert_data.csv")


# ~ set factors ####

dat$treatment <- as.factor(dat$treatment)
dat$year <- as.factor(dat$year)
dat$category <- as.factor(dat$category)




# ~ filter the data ####

spray_dat <- filter(.data = dat, activity == "Application" & category != "Fertiliser")

fert_dat <- filter(.data = dat, activity == "Application" & category == "Fertiliser")


# Filter out rows where 'category' is 'Fertiliser'
fert_dat <- fert_dat %>%
  filter(chem_element != "NA")

# set the factor levels
fert_dat$chem_element <- factor(fert_dat$chem_element, levels = c("N","P", "K", "S", "Ca", "Mg", "B", "Cu", "Mn", "Mo", "Zn"))


#________________________________________________####
# summarise the data ####

# ~ sprays ####



## ~~ by year ####

# Group by 'treatment' and 'crop' and calculate the sum of 'normalized_rate_kg_ha'
summary_spray_dat_yt <- spray_dat %>%
  group_by(treatment, year, category) %>%
  summarise(Total_Active_Ingredient_kg_ha = sum(normalized_rate_kg_ha, na.rm = TRUE), .groups = 'drop')

# Create a dataframe with all possible combinations
spray_grid <- expand.grid(
  treatment = unique(summary_spray_dat_yt$treatment),
  year = unique(summary_spray_dat_yt$year),
  category = unique(summary_spray_dat_yt$category)
)


# Merge with existing data, filling missing values with 0
summary_spray_dat_yt <- spray_grid %>%
  left_join(summary_spray_dat_yt, by = c("treatment", "year", "category")) %>%
  mutate(Total_Active_Ingredient_kg_ha = ifelse(is.na(Total_Active_Ingredient_kg_ha), 0, Total_Active_Ingredient_kg_ha))






## ~~ by treatment ####

summary_spray_dat_t <- spray_dat %>%
  group_by(treatment, category) %>%
  summarise(Total_Active_Ingredient_kg_ha = sum(normalized_rate_kg_ha, na.rm = TRUE), .groups = 'drop')


glimpse(summary_spray_dat_t)







#__________####
# ~ fert ####


## ~~ by year ####

summary_fert_dat_yt <- fert_dat %>%
  group_by(treatment, year, chem_element) %>%
  summarise(Total_Chem_Element_kg_ha = sum(normalized_rate_kg_ha, na.rm = TRUE), .groups = 'drop')


# Create a dataframe with all possible combinations
fert_grid <- expand.grid(
  treatment = unique(summary_fert_dat_yt$treatment),
  year = unique(summary_fert_dat_yt$year),
  chem_element = unique(summary_fert_dat_yt$chem_element)
)


# Merge with existing data, filling missing values with 0
summary_fert_dat_yt <- fert_grid %>%
  left_join(summary_fert_dat_yt, by = c("treatment", "year", "chem_element")) %>%
  mutate(Total_Chem_Element_kg_ha = ifelse(is.na(Total_Chem_Element_kg_ha), 0, Total_Chem_Element_kg_ha))








## ~~ by treatment ####


summary_fert_dat_t <- spray_dat %>%
  group_by(treatment, chem_element) %>%
  summarise(Total_Chem_Element_kg_ha = sum(normalized_rate_kg_ha, na.rm = TRUE), .groups = 'drop')




# ~ expand the df to all combo's

# Create a dataframe with all possible combinations
spray_full_df <- expand.grid(
  treatment = unique(spray_dat$treatment),
  year = unique(spray_dat$year),
  category = unique(spray_dat$category)
)


# Create a dataframe with all possible combinations
fert_full_df <- expand.grid(
  treatment = unique(fert_dat$treatment),
  year = unique(fert_dat$year),
  chem_element = unique(fert_dat$chem_element)
)






#___________________________________________####
# Statistics #####



#__________####
# ~ Pesticides ####

## ~~ Herbicides ####

names(summary_spray_dat_t)
unique(summary_spray_dat_t$category)
dat <- filter(.data = summary_spray_dat_yt, category == "Herbicide")

# plot the distribution
distribution_plots(data = dat,
                   variable = dat$Total_Active_Ingredient_kg_ha,
                   colour = dat$Total_Active_Ingredient_kg_ha)

# save it
ggsave(filename = "sym_link_agronomy_data/plots/distributions/dist_herbicides.png", 
       width = 10, height = 2.25)


# model it
glm_model <- glmer(formula = Total_Active_Ingredient_kg_ha ~ treatment + (1 | year), 
                   data = dat, 
                   family = Gamma(link = "log"))

# View summary
summary(glm_model)

# Run pairwise comparisons for the 'Treatment' factor
pairwise_comparisons <- emmeans(glm_model, pairwise ~ treatment)

# View the results of pairwise comparisons
summary(pairwise_comparisons)


diagnostic_plots_glm(model = glm_model)

ggsave(filename = "sym_link_agronomy_data/plots/model_diagnostics/model_diag_herbicides.png", 
       width = 10, height = 3.5)






## ~~ Fungicides ####

names(summary_spray_dat_t)
unique(summary_spray_dat_t$category)
dat <- filter(.data = summary_spray_dat_yt, category == "Fungicide")

# plot the distribution
distribution_plots(data = dat,
                   variable = dat$Total_Active_Ingredient_kg_ha,
                   colour = dat$Total_Active_Ingredient_kg_ha)

# save it
ggsave(filename = "sym_link_agronomy_data/plots/distributions/dist_fungicide.png", 
       width = 10, height = 2.25)


# model it
glm_model <- glmer(formula = Total_Active_Ingredient_kg_ha + 1 ~ treatment + (1 | year), 
                   data = dat, 
                   family = Gamma(link = "log"))

# View summary
summary(glm_model)

# Run pairwise comparisons for the 'Treatment' factor
pairwise_comparisons <- emmeans(glm_model, pairwise ~ treatment)

# View the results of pairwise comparisons
summary(pairwise_comparisons)


diagnostic_plots_glm(model = glm_model)

ggsave(filename = "sym_link_agronomy_data/plots/model_diagnostics/model_diag_fungicide.png", 
       width = 10, height = 3.5)





## ~~ Insecticides ####

names(summary_spray_dat_t)
unique(summary_spray_dat_t$category)
dat <- filter(.data = summary_spray_dat_yt, category == "Insecticide")

# plot the distribution
distribution_plots(data = dat,
                   variable = dat$Total_Active_Ingredient_kg_ha,
                   colour = dat$Total_Active_Ingredient_kg_ha)

# save it
ggsave(filename = "sym_link_agronomy_data/plots/distributions/dist_insecticide.png", 
       width = 10, height = 2.25)


# model it
glm_model <- glmer(formula = Total_Active_Ingredient_kg_ha + 1 ~ treatment + (1 | year), 
                   data = dat, 
                   family = Gamma(link = "log"))

# View summary
summary(glm_model)

# Run pairwise comparisons for the 'Treatment' factor
pairwise_comparisons <- emmeans(glm_model, pairwise ~ treatment)

# View the results of pairwise comparisons
summary(pairwise_comparisons)


diagnostic_plots_glm(model = glm_model)

ggsave(filename = "sym_link_agronomy_data/plots/model_diagnostics/model_diag_insecticide.png", 
       width = 10, height = 3.5)






## ~~ Dessicant ####

names(summary_spray_dat_t)
unique(summary_spray_dat_t$category)
dat <- filter(.data = summary_spray_dat_yt, category == "Desiccant")

# plot the distribution
distribution_plots(data = dat,
                   variable = dat$Total_Active_Ingredient_kg_ha,
                   colour = dat$Total_Active_Ingredient_kg_ha)

# save it
ggsave(filename = "sym_link_agronomy_data/plots/distributions/dist_Desiccant.png", 
       width = 10, height = 2.25)


# model it
glm_model <- glmer(formula = Total_Active_Ingredient_kg_ha + 1 ~ treatment + (1 | year), 
                   data = dat, 
                   family = Gamma(link = "log"))

# View summary
summary(glm_model)

# Run pairwise comparisons for the 'Treatment' factor
pairwise_comparisons <- emmeans(glm_model, pairwise ~ treatment)

# View the results of pairwise comparisons
summary(pairwise_comparisons)


diagnostic_plots_glm(model = glm_model)

ggsave(filename = "sym_link_agronomy_data/plots/model_diagnostics/model_diag_Desiccant.png", 
       width = 10, height = 3.5)




## ~~ Molluscicide ####

names(summary_spray_dat_t)
unique(summary_spray_dat_t$category)
dat <- filter(.data = summary_spray_dat_yt, category == "Molluscicide")

# plot the distribution
distribution_plots(data = dat,
                   variable = dat$Total_Active_Ingredient_kg_ha,
                   colour = dat$Total_Active_Ingredient_kg_ha)

# save it
ggsave(filename = "sym_link_agronomy_data/plots/distributions/dist_Molluscicide.png", 
       width = 10, height = 2.25)


# model it
glm_model <- glmer(formula = Total_Active_Ingredient_kg_ha + 1 ~ treatment + (1 | year), 
                   data = dat, 
                   family = Gamma(link = "log"))

# View summary
summary(glm_model)

# Run pairwise comparisons for the 'Treatment' factor
pairwise_comparisons <- emmeans(glm_model, pairwise ~ treatment)

# View the results of pairwise comparisons
summary(pairwise_comparisons)


diagnostic_plots_glm(model = glm_model)

ggsave(filename = "sym_link_agronomy_data/plots/model_diagnostics/model_diag_Molluscicide.png", 
       width = 10, height = 3.5)




## ~~ PGR ####

names(summary_spray_dat_t)
unique(summary_spray_dat_t$category)
dat <- filter(.data = summary_spray_dat_yt, category == "PGR")

# plot the distribution
distribution_plots(data = dat,
                   variable = dat$Total_Active_Ingredient_kg_ha,
                   colour = dat$Total_Active_Ingredient_kg_ha)

# save it
ggsave(filename = "sym_link_agronomy_data/plots/distributions/dist_PGR.png", 
       width = 10, height = 2.25)


# model it
glm_model <- glmer(formula = Total_Active_Ingredient_kg_ha + 1 ~ treatment + (1 | year), 
                   data = dat, 
                   family = Gamma(link = "log"))

# View summary
summary(glm_model)

# Run pairwise comparisons for the 'Treatment' factor
pairwise_comparisons <- emmeans(glm_model, pairwise ~ treatment)

# View the results of pairwise comparisons
summary(pairwise_comparisons)


diagnostic_plots_glm(model = glm_model)

ggsave(filename = "sym_link_agronomy_data/plots/model_diagnostics/model_diag_PGR.png", 
       width = 10, height = 3.5)







# ~ Total kg pesticides ####


# Group by 'treatment' and 'crop' and calculate the sum of 'normalized_rate_kg_ha'
dat <- spray_dat %>%
  group_by(treatment, year) %>%
  summarise(Total_Active_Ingredient_kg_ha = sum(normalized_rate_kg_ha, na.rm = TRUE), .groups = 'drop')


# Create a dataframe with all possible combinations
spray_grid <- expand.grid(
  treatment = unique(summary_spray_dat_yt$treatment),
  year = unique(summary_spray_dat_yt$year)
)

# Merge with existing data, filling missing values with 0
dat <- spray_grid %>%
  left_join(dat, by = c("treatment", "year")) %>%
  mutate(Total_Active_Ingredient_kg_ha = ifelse(is.na(Total_Active_Ingredient_kg_ha), 0, Total_Active_Ingredient_kg_ha))


# plot the distribution
distribution_plots(data = dat,
                   variable = dat$Total_Active_Ingredient_kg_ha,
                   colour = dat$Total_Active_Ingredient_kg_ha)

# save it
ggsave(filename = "sym_link_agronomy_data/plots/distributions/dist_all_pesticides.png", 
       width = 10, height = 2.25)


# model it
glm_model <- glmer(formula = Total_Active_Ingredient_kg_ha + 1 ~ treatment + (1 | year), 
                   data = dat, 
                   family = Gamma(link = "inverse"))

# View summary
summary(glm_model)

# Run pairwise comparisons for the 'Treatment' factor
pairwise_comparisons <- emmeans(glm_model, pairwise ~ treatment)

# View the results of pairwise comparisons
summary(pairwise_comparisons)


diagnostic_plots_glm(model = glm_model)

ggsave(filename = "sym_link_agronomy_data/plots/model_diagnostics/model_diag_all_pesticides.png", 
       width = 10, height = 3.5)







#__________####
# ~ Fertiliser ####



## ~~ Total kg pesticides ####


# Group by 'treatment' and 'crop' and calculate the sum of 'normalized_rate_kg_ha'
dat <- fert_dat %>%
  group_by(treatment, year) %>%
  summarise(Total_Active_Ingredient_kg_ha = sum(normalized_rate_kg_ha, na.rm = TRUE), .groups = 'drop')


# Create a dataframe with all possible combinations
fert_grid <- expand.grid(
  treatment = unique(summary_fert_dat_yt$treatment),
  year = unique(summary_fert_dat_yt$year)
)

# Merge with existing data, filling missing values with 0
dat <- fert_grid %>%
  left_join(dat, by = c("treatment", "year")) %>%
  mutate(Total_Active_Ingredient_kg_ha = ifelse(is.na(Total_Active_Ingredient_kg_ha), 0, Total_Active_Ingredient_kg_ha))


# plot the distribution
distribution_plots(data = dat,
                   variable = dat$Total_Active_Ingredient_kg_ha,
                   colour = dat$Total_Active_Ingredient_kg_ha)

# save it
ggsave(filename = "sym_link_agronomy_data/plots/distributions/dist_all_fert.png", 
       width = 10, height = 2.25)


# model it
glm_model <- glmer(formula = Total_Active_Ingredient_kg_ha + 1 ~ treatment + (1 | year), 
                   data = dat, 
                   family = Gamma(link = "log"))

# View summary
summary(glm_model)

# Run pairwise comparisons for the 'Treatment' factor
pairwise_comparisons <- emmeans(glm_model, pairwise ~ treatment)

# View the results of pairwise comparisons
summary(pairwise_comparisons)


diagnostic_plots_glm(model = glm_model)

ggsave(filename = "sym_link_agronomy_data/plots/model_diagnostics/model_diag_all_fert.png", 
       width = 10, height = 3.5)





## ~~ N ####

names(summary_fert_dat_yt)
unique(summary_fert_dat_yt$chem_element)
dat <- filter(.data = summary_fert_dat_yt, chem_element == "N")

# plot the distribution
distribution_plots(data = dat,
                   variable = dat$Total_Chem_Element_kg_ha,
                   colour = dat$Total_Chem_Element_kg_ha)

# save it
ggsave(filename = "sym_link_agronomy_data/plots/distributions/dist_N.png", 
       width = 10, height = 2.25)


# model it
glm_model <- glmer(formula = Total_Chem_Element_kg_ha + 1 ~ treatment + (1 | year), 
                   data = dat, 
                   family = Gamma(link = "log"))

# View summary
summary(glm_model)

# Run pairwise comparisons for the 'Treatment' factor
pairwise_comparisons <- emmeans(glm_model, pairwise ~ treatment)

# View the results of pairwise comparisons
summary(pairwise_comparisons)


diagnostic_plots_glm(model = glm_model)

ggsave(filename = "sym_link_agronomy_data/plots/model_diagnostics/model_diag_N.png", 
       width = 10, height = 3.5)





## ~~ P ####

names(summary_fert_dat_yt)
unique(summary_fert_dat_yt$chem_element)
dat <- filter(.data = summary_fert_dat_yt, chem_element == "P")

# plot the distribution
distribution_plots(data = dat,
                   variable = dat$Total_Chem_Element_kg_ha,
                   colour = dat$Total_Chem_Element_kg_ha)

# save it
ggsave(filename = "sym_link_agronomy_data/plots/distributions/dist_P.png", 
       width = 10, height = 2.25)


# model it
glm_model <- glmer(formula = Total_Chem_Element_kg_ha + 1 ~ treatment + (1 | year), 
                   data = dat, 
                   family = Gamma(link = "log"))

# View summary
summary(glm_model)

# Run pairwise comparisons for the 'Treatment' factor
pairwise_comparisons <- emmeans(glm_model, pairwise ~ treatment)

# View the results of pairwise comparisons
summary(pairwise_comparisons)


diagnostic_plots_glm(model = glm_model)

ggsave(filename = "sym_link_agronomy_data/plots/model_diagnostics/model_diag_P.png", 
       width = 10, height = 3.5)




## ~~ K ####

names(summary_fert_dat_yt)
unique(summary_fert_dat_yt$chem_element)
dat <- filter(.data = summary_fert_dat_yt, chem_element == "K")

# plot the distribution
distribution_plots(data = dat,
                   variable = dat$Total_Chem_Element_kg_ha,
                   colour = dat$Total_Chem_Element_kg_ha)

# save it
ggsave(filename = "sym_link_agronomy_data/plots/distributions/dist_K.png", 
       width = 10, height = 2.25)


# model it
glm_model <- glmer(formula = Total_Chem_Element_kg_ha + 1 ~ treatment + (1 | year), 
                   data = dat, 
                   family = Gamma(link = "log"))

# View summary
summary(glm_model)

# Run pairwise comparisons for the 'Treatment' factor
pairwise_comparisons <- emmeans(glm_model, pairwise ~ treatment)

# View the results of pairwise comparisons
summary(pairwise_comparisons)


diagnostic_plots_glm(model = glm_model)

ggsave(filename = "sym_link_agronomy_data/plots/model_diagnostics/model_diag_K.png", 
       width = 10, height = 3.5)





## ~~ S ####

names(summary_fert_dat_yt)
unique(summary_fert_dat_yt$chem_element)
dat <- filter(.data = summary_fert_dat_yt, chem_element == "S")

# plot the distribution
distribution_plots(data = dat,
                   variable = dat$Total_Chem_Element_kg_ha,
                   colour = dat$Total_Chem_Element_kg_ha)

# save it
ggsave(filename = "sym_link_agronomy_data/plots/distributions/dist_S.png", 
       width = 10, height = 2.25)


# model it
glm_model <- glmer(formula = Total_Chem_Element_kg_ha + 1 ~ treatment + (1 | year), 
                   data = dat, 
                   family = Gamma(link = "log"))

# View summary
summary(glm_model)

# Run pairwise comparisons for the 'Treatment' factor
pairwise_comparisons <- emmeans(glm_model, pairwise ~ treatment)

# View the results of pairwise comparisons
summary(pairwise_comparisons)


diagnostic_plots_glm(model = glm_model)

ggsave(filename = "sym_link_agronomy_data/plots/model_diagnostics/model_diag_S.png", 
       width = 10, height = 3.5)





## ~~ B ####

names(summary_fert_dat_yt)
unique(summary_fert_dat_yt$chem_element)
dat <- filter(.data = summary_fert_dat_yt, chem_element == "B")

# plot the distribution
distribution_plots(data = dat,
                   variable = dat$Total_Chem_Element_kg_ha,
                   colour = dat$Total_Chem_Element_kg_ha)

# save it
ggsave(filename = "sym_link_agronomy_data/plots/distributions/dist_B.png", 
       width = 10, height = 2.25)


# model it
glm_model <- glmer(formula = Total_Chem_Element_kg_ha + 1 ~ treatment + (1 | year), 
                   data = dat, 
                   family = Gamma(link = "log"))

# View summary
summary(glm_model)

# Run pairwise comparisons for the 'Treatment' factor
pairwise_comparisons <- emmeans(glm_model, pairwise ~ treatment)

# View the results of pairwise comparisons
summary(pairwise_comparisons)


diagnostic_plots_glm(model = glm_model)

ggsave(filename = "sym_link_agronomy_data/plots/model_diagnostics/model_diag_B.png", 
       width = 10, height = 3.5)

