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





# ~ summarise the data ####

## ~~ sprays ####

# Group by 'treatment' and 'crop' and calculate the sum of 'normalized_rate_kg_ha'
summary_spray_dat_yt <- spray_dat %>%
  group_by(treatment, year, category) %>%
  summarise(Total_Active_Ingredient_kg_ha = sum(normalized_rate_kg_ha, na.rm = TRUE), .groups = 'drop')

summary_spray_dat_t <- spray_dat %>%
  group_by(treatment, category) %>%
  summarise(Total_Active_Ingredient_kg_ha = sum(normalized_rate_kg_ha, na.rm = TRUE), .groups = 'drop')



## ~~ fert ####

summary_fert_dat_yt <- fert_dat %>%
  group_by(treatment, year, category) %>%
  summarise(chem_element = sum(chem_element, na.rm = TRUE), .groups = 'drop')

summary_fert_dat_t <- spray_dat %>%
  group_by(treatment, category) %>%
  summarise(chem_element = sum(chem_element, na.rm = TRUE), .groups = 'drop')







# ~ ppp data ####

ap_cat_sum <- read.csv(file = "sym_link_agronomy_data/data/applications_data/AI_category_summary.csv")

ap_cat_sum$treatment <- as.factor(ap_cat_sum$treatment)
ap_cat_sum$year <- as.factor(ap_cat_sum$year)
ap_cat_sum$category <- as.factor(ap_cat_sum$category)


# Create a dataframe with all possible combinations
full_df <- expand.grid(
  treatment = unique(ap_cat_sum$treatment),
  year = unique(ap_cat_sum$year),
  category = unique(ap_cat_sum$category)
)

# Merge with existing data, filling missing values with 0
ap_cat_sum_fixed_yt <- full_df %>%
  left_join(ap_cat_sum, by = c("treatment", "year", "category")) %>%
  mutate(Total_Active_Ingredient_kg_ha = ifelse(is.na(Total_Active_Ingredient_kg_ha), 0, Total_Active_Ingredient_kg_ha))


ap_cat_sum_fixed_yt






# ~ fertiliser data ####

fert_el_sum <- read.csv(file = "data/processed_data/fert_elem_sum.csv")
fert_el_sum$treatment <- as.factor(fert_el_sum$treatment)
fert_el_sum$year <- as.factor(fert_el_sum$year)
fert_el_sum$chem_element <- as.factor(fert_el_sum$chem_element)

# Filter out rows where 'category' is 'Fertiliser'
fert_el_sum <- fert_el_sum %>%
  filter(chem_element != "NA")

# set the factor levels
fert_el_sum$chem_element <- factor(fert_el_sum$chem_element, levels = c("N","P", "K", "S", "Ca", "Mg", "B", "Cu", "Mn", "Mo", "Zn"))
levels(fert_el_sum$chem_element)




# Create a dataframe with all possible combinations
full_df <- expand.grid(
  treatment = unique(fert_el_sum$treatment),
  year = unique(fert_el_sum$year),
  chem_element = unique(fert_el_sum$chem_element)
)



# Merge with existing data, filling missing values with 0
fert_el_sum_fixed_yt <- full_df %>%
  left_join(fert_el_sum, by = c("treatment", "year", "chem_element")) %>%
  mutate(Total_Active_Ingredient_kg_ha = ifelse(is.na(Total_Active_Ingredient_kg_ha), 0, Total_Active_Ingredient_kg_ha))








#___________________________________________####
# Statistics #####


# N ####

names(dat)

# Calculates mean, sd, se and IC - block
ecotox_indic_x_rate <- 
  dat %>%
  group_by(treatment, year) %>%
  summarise( 
    n = n(),
    sum = sum(ecotox_indic_x_rate, na.rm = TRUE),
    sd = sd(ecotox_indic_x_rate, na.rm = TRUE)
  ) %>%
  mutate( se = sd/sqrt(n))  %>%
  mutate( ic = se * qt((1-0.05)/2 + .5, n-1)) %>% 
  arrange(year)

ecotox_indic_x_rate


distribution_plots(data = dat, 
                   variable = dat$ecotox_indic_x_rate, 
                   colour = dat$ecotox_indic_x_rate)












