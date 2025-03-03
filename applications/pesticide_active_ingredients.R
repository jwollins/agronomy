### HAU Conservation Ag Experiment
## Agronomy - Applications
## Joe Collins 
## 2024-12-08




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
# Load Data ####




setwd(dir = "~/OneDrive - Harper Adams University/Data/LCA/")

# ~ ppp data ####
ap_cat_sum <- read.csv(file = "data/processed_data/AI_category_summary.csv")
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
ap_cat_sum_fixed <- full_df %>%
  left_join(ap_cat_sum, by = c("treatment", "year", "category")) %>%
  mutate(Total_Active_Ingredient_kg_ha = ifelse(is.na(Total_Active_Ingredient_kg_ha), 0, Total_Active_Ingredient_kg_ha))







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
fert_el_sum_fixed <- full_df %>%
  left_join(fert_el_sum, by = c("treatment", "year", "chem_element")) %>%
  mutate(Total_Active_Ingredient_kg_ha = ifelse(is.na(Total_Active_Ingredient_kg_ha), 0, Total_Active_Ingredient_kg_ha))








#___________________________________________####
# PLOTS #####



setwd(dir = "~/OneDrive - Harper Adams University/Data/agronomy/plots/")



glimpse(ap_cat_sum)


# ~ Crop PPP category plot ####

ggplot(data = ap_cat_sum_fixed, 
       aes(x = year, 
           y = Total_Active_Ingredient_kg_ha, 
           group = treatment, 
           fill = treatment)) + 
  geom_bar(stat = "identity", 
           color = "black", 
           position = position_dodge(width = 0.9), 
           width = 0.9) +  
  labs(
    x = "Crop Type",
    y = expression(Total~Active~Ingredient~(kg^{-1}))  ) +
  theme_bw() +
  scale_fill_manual(values = c("turquoise3", "tomato2"), 
                    name = "Treatment") +
  theme(
    strip.text.x = element_text(size = 12, 
                                color = "black", 
                                face = "bold.italic"), 
    legend.position = "bottom", 
    axis.title.x = element_blank()
  ) +
  facet_wrap(~ category, 
             ncol = 3, 
             scales = 'free_y') 


ggsave(filename = "application_plots/ap_plot_all.png", width = 10, height = 5)








# ~ Fert element plot ####

ggplot(data = fert_el_sum_fixed, 
       aes(x = year, 
           y = Total_Active_Ingredient_kg_ha, 
           group = treatment, 
           fill = treatment)) + 
  geom_bar(stat = "identity", 
           position = "dodge", 
           colour = "black") + 
  labs(
    x = "Crop Type",
    y = expression(Chemical~Element~(kg^{-1}))  ) +
  theme_bw() +
  scale_fill_manual(values = c("turquoise3", "tomato2"), 
                    name = "Treatment") +
  theme(
    strip.text.x = element_text(size = 12, 
                                color = "black", 
                                face = "bold.italic"), 
    legend.position = "bottom", 
    axis.title.x = element_blank()
  ) +
  facet_wrap(~ chem_element, 
             ncol = 3, 
             scales = 'free_y')

ggsave(filename = "application_plots/fert_chem_elem_plot.png", width = 10, height = 7)



ggplot(data = fert_el_sum_fixed, 
       aes(x = year, 
           y = Total_Active_Ingredient_kg_ha, 
           group = treatment, 
           fill = treatment)) + 
  geom_bar(stat = "identity", 
           position = "dodge", 
           colour = "black") + 
  labs(
    x = "Crop Type",
    y = expression(Chemical~Element~(kg^{-1}))  ) +
  theme_bw() +
  scale_fill_manual(values = c("turquoise3", "tomato2"), 
                    name = "Treatment") +
  theme(
    strip.text.x = element_text(size = 12, 
                                color = "black", 
                                face = "bold.italic"), 
    legend.position = "bottom", 
    axis.title.x = element_blank()
  ) +
  facet_wrap(~ chem_element, 
             ncol = 4, 
             scales = 'free_y')



ggsave(filename = "application_plots/fert_chem_elem_plot2.png", width = 12, height = 7)





















#___________________________________________####
# OLD Plots ####
# 
# 
# # ~ AI plot ####
# 
# title <- expression(Active~Ingredient~rate~(g~ha^{-1}))
# 
# ggplot(data = ap_cat_sum, 
#        aes(x = year, 
#            y = normalized_rate_g_ha, group = treatment, 
#            fill = treatment)) + 
#   geom_bar(stat = "identity", 
#            position = "dodge") + 
#   labs(
#     x = title,
#     y = title,
#     subtitle = title, 
#     caption = "") +
#   theme_bw() +
#   scale_fill_manual(values=c("turquoise3","tomato2"), 
#                     name = "Treatment") +
#   theme(strip.text.x = element_text(size = 12, 
#                                     color = "black", 
#                                     face = "bold.italic"), 
#         legend.position = "bottom", 
#         axis.title.x = element_blank()) +
#   # geom_errorbar(aes(ymin = mean - se, 
#   #                   ymax = mean + se),
#   #               width=.2,                    # Width of the error bars
#   #               position=position_dodge(.9)) +
#   facet_wrap(~ category, 
#              ncol = 3, 
#              scales = 'free_y') 
# 
# ggsave(filename = "plots/ap_plot_all.png", width = 10, height = 7)
# 
# 
# ### 02.1 summary stats ####
# 
# # Calculates mean, sd, se and IC - block
# ap_sum <- ap_dat %>%
#   group_by(treatment, year, category) %>%
#   summarise( 
#     n = n(),
#     mean = mean(normalized_rate_g_ha),
#     sd = sd(normalized_rate_g_ha),
#     sum = sum(normalized_rate_g_ha)
#   ) %>%
#   mutate( se=sd/sqrt(n))  %>%
#   mutate( ic=se * qt((1-0.05)/2 + .5, n-1))
# 
# 
# ## 03 - Plots ####
# 
# ### FUNGICIDE PLOT ####
# 
# ap_sum_filtered <- ap_sum %>%
#   filter(category == "Fungicide")
# 
# y_title <- expression(Active~Ingredient~rate~(g~ha^{-1}))
# 
# f1 <- ggplot(data = ap_sum_filtered, 
#              aes(x = treatment, 
#                  y = sum, 
#                  fill = treatment)) + 
#   geom_bar(stat = "identity", 
#            color = "black", 
#            position = "dodge") + 
#   labs(
#     x = "Treatment",
#     y = y_title,
#     subtitle = "Fungicide", 
#     caption = "") +
#   theme_bw() +
#   scale_fill_manual(values=c("turquoise3","tomato2"), 
#                     name = "Treatment") +
#   theme(strip.text.x = element_text(size = 12, 
#                                     color = "black", 
#                                     face = "bold.italic"), 
#         legend.position = "bottom", 
#         axis.text.x = element_blank(), 
#         axis.title.x = element_blank()) +
#   # geom_errorbar(aes(ymin = mean - se, 
#   #                   ymax = mean + se),
#   #               width=.2,                    # Width of the error bars
#   #               position=position_dodge(.9)) +
#   facet_wrap(~ year, 
#              ncol = 3, 
#              scales = 'free_x') 
# 
# f1
# 
# 
# 
# ### HERBICIDE PLOT ####
# 
# ap_sum_filtered <- ap_sum %>%
#   filter(category == "Herbicide")
# 
# y_title <- expression(Active~Ingredient~rate~(g~ha^{-1}))
# 
# h1 <- ggplot(data = ap_sum_filtered, 
#              aes(x = treatment, 
#                  y = sum, 
#                  fill = treatment)) + 
#   geom_bar(stat = "identity", 
#            color = "black", 
#            position = "dodge") + 
#   labs(
#     x = "Treatment",
#     y = element_blank(),
#     subtitle = "Herbicide", 
#     caption = "") +
#   theme_bw() +
#   scale_fill_manual(values=c("turquoise3","tomato2"), 
#                     name = "Treatment") +
#   theme(strip.text.x = element_text(size = 12, 
#                                     color = "black", 
#                                     face = "bold.italic"), 
#         legend.position = "bottom", 
#         axis.text.x = element_blank(), 
#         axis.title.x = element_blank()) +
#   # geom_errorbar(aes(ymin = mean - se, 
#   #                   ymax = mean + se),
#   #               width=.2,                    # Width of the error bars
#   #               position=position_dodge(.9)) +
#   facet_wrap(~ year, 
#              ncol = 3, 
#              scales = 'free_x') 
# 
# h1
# 
# 
# 
# ### INSECTICIDE PLOT ####
# 
# ap_sum_filtered <- ap_sum %>%
#   filter(category == "Insecticide")
# 
# 
# y_title <- expression(Active~Ingredient~rate~(g~ha^{-1}))
# 
# i1 <- ggplot(data = ap_sum_filtered, 
#              aes(x = treatment, 
#                  y = sum, 
#                  fill = treatment)) + 
#   geom_bar(stat = "identity", 
#            color = "black", 
#            position = "dodge") + 
#   labs(
#     x = "Treatment",
#     y = element_blank(),
#     subtitle = "Insecticide", 
#     caption = "") +
#   theme_bw() +
#   scale_fill_manual(values=c("tomato2"), 
#                     name = "Treatment") +
#   theme(strip.text.x = element_text(size = 12, 
#                                     color = "black", 
#                                     face = "bold.italic"), 
#         legend.position = "bottom", 
#         axis.text.x = element_blank(), 
#         axis.title.x = element_blank()) +
#   # geom_errorbar(aes(ymin = mean - se, 
#   #                   ymax = mean + se),
#   #               width=.2,                    # Width of the error bars
#   #               position=position_dodge(.9)) +
#   facet_wrap(~ year, 
#              ncol = 3, 
#              scales = 'free_x') 
# 
# i1
# 
# 
# 
# 
# ### PGR PLOT ####
# 
# ap_sum_filtered <- ap_sum %>%
#   filter(category == "PGR")
# 
# y_title <- expression(Active~Ingredient~rate~(g~ha^{-1}))
# 
# pgr1 <- ggplot(data = ap_sum_filtered, 
#              aes(x = treatment, 
#                  y = sum, 
#                  fill = treatment)) + 
#   geom_bar(stat = "identity", 
#            color = "black", 
#            position = "dodge") + 
#   labs(
#     x = "Treatment",
#     y = element_blank(),
#     subtitle = "PGR", 
#     caption = "") +
#   theme_bw() +
#   scale_fill_manual(values=c("turquoise3","tomato2"), 
#                     name = "Treatment") +
#   theme(strip.text.x = element_text(size = 12, 
#                                     color = "black", 
#                                     face = "bold.italic"), 
#         legend.position = "bottom", 
#         axis.text.x = element_blank(), 
#         axis.title.x = element_blank()) +
#   # geom_errorbar(aes(ymin = mean - se, 
#   #                   ymax = mean + se),
#   #               width=.2,                    # Width of the error bars
#   #               position=position_dodge(.9)) +
#   facet_wrap(~ year, 
#              ncol = 3, 
#              scales = 'free_x') 
# 
# pgr1
# 
# 
# 
# ### MOLLUSCICIDE PLOT ####
# 
# ap_sum_filtered <- ap_sum %>%
#   filter(category == "Molluscicide")
# 
# 
# 
# 
# y_title <- expression(Active~Ingredient~rate~(g~ha^{-1}))
# 
# m1 <- ggplot(data = ap_sum_filtered, 
#              aes(x = treatment, 
#                  y = sum, 
#                  fill = treatment)) + 
#   geom_bar(stat = "identity", 
#            color = "black", 
#            position = "dodge") + 
#   labs(
#     x = "Treatment",
#     y = element_blank(),
#     subtitle = "Molluscicide", 
#     caption = "") +
#   theme_bw() +
#   scale_fill_manual(values=c("turquoise3","tomato2"), 
#                     name = "Treatment") +
#   theme(strip.text.x = element_text(size = 12, 
#                                     color = "black", 
#                                     face = "bold.italic"), 
#         legend.position = "bottom", 
#         axis.text.x = element_blank(), 
#         axis.title.x = element_blank()) +
#   # geom_errorbar(aes(ymin = mean - se, 
#   #                   ymax = mean + se),
#   #               width=.2,                    # Width of the error bars
#   #               position=position_dodge(.9)) +
#   facet_wrap(~ year, 
#              ncol = 3, 
#              scales = 'free_x') 
# 
# m1
# 
# 
# ### JOINT PLOT ####
# 
# ggarrange(f1, h1, i1, pgr1, m1, 
#           ncol = 5, nrow = 1, 
#           common.legend = TRUE, 
#           legend = "bottom", 
#           widths = c(1, 1, 1, 0.5, 0.5), labels = c("A", "B", "C", "D", "E"))
# 
# ggsave(filename = "plots/AI_plot.png", width = 15, height = 6)
# 
















