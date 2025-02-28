## HEADER ####
## who: J Collins
## what: crop growth
## when: 2024-10-15

getwd()

# setwd("~/OneDrive - Harper Adams University/Data/agronomy/")




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







#_____________________________________####
# DATA ####


# ~ load raw data ####

dat <- read_excel(path = "~/OneDrive - Harper Adams University/Data/agronomy/data/crop_growth/crop_data.xlsx")


# ~ filter treatment ####

dat <- filter(dat, treatment == "Conventional" | treatment == "Conservation")



# ~ factors ####

# Organise factors
dat$plot <- as.factor(dat$plot)
dat$block <- as.factor(dat$block)
dat$treatment <- as.factor(dat$treatment)
dat$year <- as.factor(dat$year)





# ~ target plants per m2 ####

dat$target_plants_m2 <- 0
dat$target_plants_m2 <- ifelse(test = dat$crop == "Spring Beans", yes = 50, no = dat$target_plants_m2)
dat$target_plants_m2 <- ifelse(test = dat$crop == "Winter Wheat", yes = 260, no = dat$target_plants_m2)
dat$target_plants_m2 <- ifelse(test = dat$crop == "Oilseed Rape", yes = 20, no = dat$target_plants_m2)
dat$target_plants_m2 <- ifelse(test = dat$crop == "Spring Barley", yes = 300, no = dat$target_plants_m2)





# ~ drilling rates ####


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
dat$drilling_rate_kg_ha <- ifelse(test = dat$crop == "Oilseed Rape" & dat$treatment == "Conservation",
                                  yes = 3, no = dat$drilling_rate_kg_ha)
dat$drilling_rate_kg_ha <- ifelse(test = dat$crop == "Spring Barley" & dat$treatment == "Conservation",
                                  yes = 200, no = dat$drilling_rate_kg_ha)




# ~ tgw ####


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
dat$tgw <- ifelse(test = dat$crop == "Oilseed Rape" & dat$treatment == "Conservation",
                  yes = 5, no = dat$tgw)
dat$tgw <- ifelse(test = dat$crop == "Spring Barley" & dat$treatment == "Conservation",
                                  yes = 50, no = dat$tgw)



# ~ seeds per m2 ####

dat$seeds_m2 <- (dat$drilling_rate_kg_ha * 100) / dat$tgw





# ~ recommended plant per m2 ####

dat$pc_reccomended_plants <- (dat$plants_m2 / dat$target_plants_m2) * 100





# ~ loss pc ####

dat$loss_pc <- (1 - (dat$plants_m2 / dat$seeds_m2)) * 100

# Replace negative values with 0
dat$loss_pc <- pmax(dat$loss_pc, 0)

# Replace all zeros in the dataset with NA
dat <- dat %>% mutate(across(everything(), ~ replace(., . == 0, NA)))


dat <- dat %>%
  mutate(across(6:ncol(dat), as.numeric))




# ~ harvest index ####

dat$harvest_index <- (dat$grain_m2 / (dat$biomass_dm_m2 + dat$grain_m2)) * 100

# View the result
glimpse(dat)











#_____________________________________####
# SUMMARY STATS ####


# ~ plants per m2 ####

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



# Change the order of 'treatment' using factor()
plants_m2_sum <- plants_m2_sum %>%
  mutate(treatment = factor(treatment, levels = c("Conservation", "Conventional")),
         year = factor(year, levels = c("2022", "2023", "2024")),
         crop = factor(crop, levels = c("Spring Beans", "Winter Wheat", "Oilseed Rape", "Spring Barley")))




# ~ plants pc per m2 ####

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

# Change the order of 'treatment' using factor()
plants_pc_m2_sum <- plants_pc_m2_sum %>%
  mutate(treatment = factor(treatment, levels = c("Conservation", "Conventional")),
         year = factor(year, levels = c("2022", "2023", "2024")),
         crop = factor(crop, levels = c("Spring Beans", "Winter Wheat", "Oilseed Rape", "Spring Barley")))




# ~ loss pc ####

glimpse(dat)

loss_pc_sum <- dat %>%
  group_by(treatment, year, crop) %>%
  summarise(
    n = sum(!is.na(loss_pc)),  # Count of non-NA values
    mean = mean(loss_pc, na.rm = TRUE),
    sd = sd(loss_pc, na.rm = TRUE)
  ) %>%
  mutate(se = sd / sqrt(n)) %>%
  mutate(ic = se * qt((1 - 0.05) / 2 + .5, n - 1))


# Change the order of 'treatment' using factor()
loss_pc_sum <- loss_pc_sum %>%
  mutate(treatment = factor(treatment, levels = c("Conservation", "Conventional")),
         year = factor(year, levels = c("2022", "2023", "2024")),
         crop = factor(crop, levels = c("Spring Beans", "Winter Wheat", "Oilseed Rape", "Spring Barley")))


glimpse(loss_pc_sum)

loss_pc_sum$mean <- ifelse(loss_pc_sum$treatment == "Conservation" & 
                             loss_pc_sum$crop == "Oilseed Rape", yes = 100, no = loss_pc_sum$mean)


# new_row <- as.data.frame(x = c("Conventional", "2024", "Spring Barley", "0", "0", "0", "0"))
# 
# test <- rbind(loss_pc_sum, new_row)


# ~ shoots per m2 ####

shoots_m2_sum <- dat %>%
  group_by(treatment, year) %>%
  summarise( 
    n=n(),
    mean=mean(shoots_m2),
    sd=sd(shoots_m2)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))



# ~ ears per m2 ####

ears_m2_sum <- dat %>%
  group_by(treatment, year) %>%
  summarise( 
    n=n(),
    mean=mean(pods_ears_m2),
    sd=sd(pods_ears_m2)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))




# ~ biomass dm per m2 ####

biomass_dm_m2_sum <- dat %>%
  group_by(treatment, year) %>%
  summarise( 
    n=n(),
    mean=mean(biomass_dm_m2),
    sd=sd(biomass_dm_m2)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))



# ~ height per m2 ####

height_sum <- dat %>%
  group_by(treatment, year) %>%
  summarise( 
    n=n(),
    mean=mean(crop_height_cm),
    sd=sd(crop_height_cm)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))



# ~ harvest index ####

harvest_index_sum <- dat %>%
  group_by(treatment, year) %>%
  summarise( 
    n=n(),
    mean=mean(harvest_index),
    sd=sd(harvest_index)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))










#_____________________________________####
# PLOTS ####



# ~ PLANTS M2 PLOT ####

title_exp <- expression(Plants~(M^{2}))  # this is the legend title with correct notation

y_title <- expression(Plants~(M^{2}))

c1 <- ggplot(data = plants_m2_sum, 
             aes(x = treatment, 
                 y = mean, 
                 fill = treatment)) + 
  geom_bar(stat = "identity", 
           color = "black", 
           position = position_dodge(width = 0.9), 
           width = 0.9) +  
  labs(
    x = "Treatment",
    y = y_title,
    subtitle = title_exp, 
    caption = "") +
  theme_bw() +
  scale_fill_manual(values=c("turquoise3","tomato2"), 
                    name = "Treatment") +
  theme(strip.text.x = element_text(size = 8, 
                                    color = "black", 
                                    face = "bold.italic"), 
        legend.position = "bottom", 
        axis.text.x = element_blank(), 
        axis.title.x = element_blank()) +
  geom_errorbar(aes(ymin = mean - se, 
                    ymax = mean + se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) +
  facet_grid(. ~ crop)  # Ensures equal width for all facet columns

c1


# ~ percentage establishment plot ####

title_exp <- expression(Plants~(M^{2}))  # this is the legend title with correct notation

y_title <- expression(Plants~(M^{2}))

c2 <- ggplot(data = plants_pc_m2_sum, 
             aes(x = treatment, 
                 y = mean, 
                 fill = treatment)) + 
  geom_bar(stat = "identity", 
           color = "black", 
           position = position_dodge(width = 0.9), 
           width = 0.9) +  
  labs(
    x = "Treatment",
    y = "Target plant population (%)",
    subtitle = "Reccomended plant population (%)", 
    caption = "") +
  theme_bw() +
  scale_fill_manual(values=c("turquoise3","tomato2"), 
                    name = "Treatment") +
  theme(strip.text.x = element_text(size = 8, 
                                    color = "black", 
                                    face = "bold.italic"), 
        legend.position = "bottom", 
        axis.text.x = element_blank(), 
        axis.title.x = element_blank()) +
  geom_errorbar(aes(ymin = mean - se, 
                    ymax = mean + se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) +
  facet_grid(. ~ crop)  # Ensures equal width for all facet columns

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





# ~ percentage losses ####

title_exp <- expression(Losses~("%")~(M^{2}))  # this is the legend title with correct notation

y_title <- expression(Losses~("%")~(M^{2}))


c3 <- ggplot(data = loss_pc_sum, 
             aes(x = treatment, 
                 y = mean, 
                 fill = treatment)) + 
  geom_bar(stat = "identity", 
           color = "black", 
           position = position_dodge(width = 0.9), 
           width = 0.9) +  
  labs(
    x = "Treatment",
    y = "Losses (%)",
    subtitle = "Losses from seed planted (%)", 
    caption = "") +
  theme_bw() +
  scale_fill_manual(values = c("turquoise3", "tomato2"), 
                    name = "Treatment") +
  theme(strip.text.x = element_text(size = 8, 
                                    color = "black", 
                                    face = "bold.italic"), 
        legend.position = "bottom", 
        axis.text.x = element_blank(), 
        axis.title.x = element_blank()) +
  geom_errorbar(aes(ymin = mean - se, 
                    ymax = mean + se),
                width = 0.2,                  
                position = position_dodge(0.9)) + 
  facet_grid(. ~ crop)  # Ensures equal width for all facet columns


c3


ggarrange(c1, c2, c3, ncol = 3, common.legend = TRUE, legend = "bottom")

ggsave(filename = "~/OneDrive - Harper Adams University/Data/agronomy/plots/crop_establishment/fig_plant_est_plot.png", 
       width = 11.2, height = 4)








# ~ biomass dm m2 ####

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







# ~ crop height ####

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








# ~ ears / pods m2 ####

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







# ~ shoots m2 ####

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






# ~ harvest index ####

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










#_____________________________________####
# STATS ####



# ~ functions ####


source(file = "~/Documents/GitHub/phd_tools/fun_shapiro_wilks.R")
source(file = "~/Documents/GitHub/phd_tools/fun_distribution_tests.R")
source(file = "~/Documents/GitHub/phd_tools/fun_lmm_diagnostic_plots.R")
source(file = "~/Documents/GitHub/phd_tools/fun_glm_diagnostic_plots.R")







# ~ visualisation ####




# ~ histograms ####

selected_columns <- dat[, c(6:8, 9:12, 17:18)]

# Replace all zeros with NA in the entire dataframe
# dat[dat == 0] <- NA

# Function to plot histogram and QQ plot for a single variable
plot_histogram <- function(var) {
  p1 <- ggplot(dat, aes_string(x = var)) +
    geom_histogram(bins = 30, fill = "blue", color = "black", alpha = 0.7) +
    theme_minimal() +
    labs(title = var, x = var, y = "Frequency")
  
  # p2 <- ggqqplot(dat_y1[[var]], title = paste("QQ Plot of", var)) +
  #   theme_minimal()
  
  return(list(p1))
}

# Apply function to all selected variables and store the plots
plots <- lapply(names(selected_columns), plot_histogram)

# Flatten the list of plots into a single list
combined_plots <- do.call(c, plots)

# Arrange all the plots in a grid layout
ggarrange(plotlist = combined_plots, ncol = 3, nrow = 3)

ggsave(filename = "plots/histograms.png")







# ~ qqplots ####

# Function to plot histogram and QQ plot for a single variable
plot_qq <- function(var) {
  p2 <- ggqqplot(dat[[var]], title = var) +
    theme_minimal()
  
  return(list(p2))
}

# Apply function to all selected variables and store the plots
plots <- lapply(names(selected_columns), plot_qq)

# Flatten the list of plots into a single list
combined_plots <- do.call(c, plots)

# Arrange all the plots in a grid layout
ggarrange(plotlist = combined_plots, ncol = 3, nrow = 3)

ggsave(filename = "plots/qq_plots.png")










#_____________________________________####
# TRANSFORMATIONS ####




# ~ Percentage data ####

# Ensure the percentages are converted to proportions (0 to 1)
dat$pc_reccomended_plants <- dat$pc_reccomended_plants / 100
dat$loss_pc <- dat$loss_pc / 100

# Apply Arcsine Square Root Transformation
dat$pc_reccomended_plants_arcsine <- asin(sqrt(dat$pc_reccomended_plants))
dat$loss_pc_arcsine <- asin(sqrt(dat$loss_pc))





# ~ left skewed data ####

# Check for values <= 0 (as log transformation can't handle these)
dat$plants_m2 <- ifelse(dat$plants_m2 <= 0, NA, dat$plants_m2)
dat$shoots_m2 <- ifelse(dat$shoots_m2 <= 0, NA, dat$shoots_m2)
dat$pods_ears_m2 <- ifelse(dat$pods_ears_m2 <= 0, NA, dat$pods_ears_m2)

# Apply log transformation (log + 1 for values close to 0)
dat$log_plants_m2 <- log(dat$plants_m2)
dat$log_shoots_m2 <- log(dat$shoots_m2)
dat$log_pods_ears_m2 <- log(dat$pods_ears_m2)


selected_columns <- dat[, c(6:8, 9:12, 17:24)]
# Apply function to all selected variables and store the plots
plots <- lapply(names(selected_columns), plot_histogram)

# Flatten the list of plots into a single list
combined_plots <- do.call(c, plots)

# Arrange all the plots in a grid layout
ggarrange(plotlist = combined_plots, ncol = 3, nrow = 5)








# ~ test distribution ####

# ~~ SOURCE DISTRIB TESTS ####

source(file = "~/Documents/GitHub/phd_tools/fun_distribution_tests.R")

glimpse(dat)
colnames(dat)


# ~~ POISSON ####

# Specify the columns to check (replace with your actual columns)
columns_to_check <- c(6:24)  # Example column indices (plants_m2, shoots_m2, pods_ears_m2, etc.)
# Run the Poisson test on selected columns
poisson_results_multiple <- check_poisson_multiple(dat, columns_to_check)
# Print the results
print(poisson_results_multiple)




# ~~ GUASSIAN DIST ####

# # Example usage:
columns_to_check <- c(6:24)  # Specify your columns of interest
distribution_results <- check_guassian(dat, columns_to_check)
print(distribution_results)




# ~~ EXPONENTIAL DIST ####

columns_to_check <- c(6:24)  # Specify the columns you want to check
exponential_results <- check_exponential_distribution(dat, columns_to_check)
print(exponential_results)




# ~~ GAMMA DIST ####

columns_to_check <- c(6:24)  # Specify the columns you want to check
gamma_results <- check_gamma_distribution(dat, columns_to_check)
print(gamma_results)




# ~~ homoscedasticity ####

# Define columns to check (for example, columns 6:19)
columns_to_check <- colnames(dat)[6:19]

# Apply the function with 'treatment' as the group column
bartlett_results <- check_homogeneity_variance_bartlett(dat, columns_to_check, "treatment")

# Print the results
print(bartlett_results)





# ~ JOIN DF'S ####

dist_stats_df <- cbind(poisson_results_multiple, 
                       distribution_results[,2:ncol(distribution_results)],
                       exponential_results[,2:ncol(exponential_results)],
                       gamma_results[,2:ncol(gamma_results)],
                       bartlett_results[,2:ncol(bartlett_results)])

glimpse(dist_stats_df)

write.csv(x = dist_stats_df, file = "stats/distrib_stats_crop_est.csv")







# ~ overdispersion test ####

source(file = "~/Documents/GitHub/phd_tools/fun_overdispersion_test.R")

colnames(dat)

# Specify column numbers to test for overdispersion
response_column_indices <- c(6:8, 9:12, 17:18)  # Replace with the indices of the columns to test
response_columns <- colnames(dat)[response_column_indices]  # Get the column names

# Specify explanatory variables
explanatory_columns <- c("year", "crop", "treatment")

# Initialize a results list
results_list <- lapply(response_columns, function(response_column) {
  test_overdispersion(dat, response_column, explanatory_columns)
})

# Convert results into a dataframe
overdis_df <- do.call(rbind, lapply(results_list, as.data.frame))

# Print results
print(overdis_df)

write.csv(x = overdis_df, file = "stats/overdispersion_stats.csv")

# Create a LaTeX table
overdis_table <- overdis_df %>%
  kbl(format = "latex", booktabs = TRUE, caption = "My Table", label = "MyLabel", digits = 2) %>%
  kable_styling(
    latex_options = c("hold_position", "scale_down"), # Avoid 'tabu'
    full_width = FALSE,                 # Set to FALSE for `tabular`
    font_size = 15                     # Adjust font size for readability
  ) %>%
  row_spec(0, bold = TRUE)

print(overdis_table)

# Capture the LaTeX table as a character string
latex_code <- as.character(overdis_table)

# Save the LaTeX table to a .txt file
write(latex_code, file = "stats/overdispersion_stats.txt")









#_____________________________________####
# GLM ####

# ~ plants_m2 ####

# Fit a GLMM with Gamma distribution (for positively skewed data)
glmm_model <- glmer(plants_m2 ~ treatment + (1 | block) + (1 | crop) + (1 | year), 
                    family = Gamma(link = "log"), 
                    data = dat)

# View summary
summary(glmm_model)

# Run pairwise comparisons for the 'Treatment' factor
pairwise_comparisons <- emmeans(glmm_model, pairwise ~ treatment)

# View the results of pairwise comparisons
summary(pairwise_comparisons)

diagnostic_plots_glm(model = glmm_model)



# ~ shoots_m2 ####

# Fit a GLMM with Gamma distribution (for positively skewed data)
glmm_model <- glmer(shoots_m2 ~ treatment + (1 | block) + (1 | crop) + (1 | year), 
                    family = Gamma(link = "log"), 
                    data = dat)

# View summary
summary(glmm_model)

# Run pairwise comparisons for the 'Treatment' factor
pairwise_comparisons <- emmeans(glmm_model, pairwise ~ treatment)

# View the results of pairwise comparisons
summary(pairwise_comparisons)

diagnostic_plots_glm(model = glmm_model)





# ~ pods_ears_m2 ####

# Fit a GLMM with Gamma distribution (for positively skewed data)
glmm_model <- glmer(pods_ears_m2 ~ treatment + (1 | block) + (1 | crop) + (1 | year), 
                    family = Gamma(link = "log"), 
                    data = dat)
# View summary
summary(glmm_model)
# Run pairwise comparisons for the 'Treatment' factor
pairwise_comparisons <- emmeans(glmm_model, pairwise ~ treatment)
# View the results of pairwise comparisons
summary(pairwise_comparisons)

diagnostic_plots_glm(model = glmm_model)





# ~ biomass_dm_m2 ####

# Fit a linear mixed-effects model (LMM)
lmm_model <- lmer(biomass_dm_m2 ~ treatment + (1 | block) + (1 | crop) + (1 | year), 
                  data = dat)
# View the summary of the model
summary(lmm_model)
# Perform pairwise comparisons for the Treatment factor
pairwise_comparisons <- emmeans(lmm_model, pairwise ~ treatment)
# View the results of the pairwise comparisons
summary(pairwise_comparisons)






# ~ loss percentage ####

# Fit a GLMM with Gamma distribution (for positively skewed data)
glmm_model <- glmer(pc_reccomended_plants ~ treatment + (1 | block) + (1 | crop) + (1 | year), 
                    family = Gamma(link = "log"), 
                    data = dat)
# View summary
summary(glmm_model)
# Run pairwise comparisons for the 'Treatment' factor
pairwise_comparisons <- emmeans(glmm_model, pairwise ~ treatment)
# View the results of pairwise comparisons
summary(pairwise_comparisons)

diagnostic_plots_glm(model = glmm_model)






# ~ loss percentage ####

# Fit a GLMM with Gamma distribution (for positively skewed data)
glmm_model <- glmer(loss_pc ~ treatment + (1 | block) + (1 | crop) + (1 | year), 
                    family = Gamma(link = "log"), 
                    data = dat)
# View summary
summary(glmm_model)
# Run pairwise comparisons for the 'Treatment' factor
pairwise_comparisons <- emmeans(glmm_model, pairwise ~ treatment)
# View the results of pairwise comparisons
summary(pairwise_comparisons)

diagnostic_plots_glm(model = glmm_model)








