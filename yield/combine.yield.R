## HEADER ####
## who: J Collins
## what: combine yield
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
library(lme4)
library(emmeans)

## 01 DATA ####

 dat <- read_excel(path = "data/yield/all_years_combine_yield.xlsx")
 
 dat <- filter(dat, treatment == "Conventional" | treatment == "Conservation")
 
 # Organise factors
 dat$plot <- as.factor(dat$plot)
 dat$block <- as.factor(dat$block)
 dat$Treatment <- as.factor(dat$treatment)
 
 
 # Define average UK yield per hectare for each crop
 uk_yield_ha <- c(
   "Spring Beans" = 3.5,   # Example average in tonnes per hectare
   "Winter Wheat" = 9.0,   # Example average
   "Spring Barley" = 6.3,
   "Oilseed Rape" = 3.4
 )
 
 # Add the average UK yield per hectare column
 dat <- dat %>%
   mutate(
     uk_yield_t_ha = uk_yield_ha[crop],  # Map the average yield based on the crop
     yield_percent_uk = (corrected_plot_t_ha / uk_yield_t_ha) * 100  # Calculate percentage
   )
 
 # View the result
 glimpse(dat)
 

 
## 02 Summary Table ####
 
 # Calculates mean, sd, se and IC - block
 yield_sum <- dat %>%
   group_by(treatment, year, crop) %>%
   summarise( 
     n=n(),
     mean=mean(corrected_plot_t_ha),
     sd=sd(corrected_plot_t_ha)
   ) %>%
   mutate( se=sd/sqrt(n))  %>%
   mutate( ic=se * qt((1-0.05)/2 + .5, n-1))
 
 
 # Calculates mean, sd, se and IC - block
 yield_pc_sum <- dat %>%
   group_by(treatment, year) %>%
   summarise( 
     n=n(),
     mean=mean(yield_percent_uk),
     sd=sd(yield_percent_uk)
   ) %>%
   mutate( se=sd/sqrt(n))  %>%
   mutate( ic=se * qt((1-0.05)/2 + .5, n-1))
 
 
 ## 03 Visual tests ####
 
 ### Desity plot ####
 
 # Density plot and Q-Q plot can be used to check normality visually.
 # 
 # Density plot: the density plot provides a visual judgment about whether the distribution is bell shaped.
 
 ggdensity(dat$corrected_plot_t_ha, 
           main = "Combine Yield",
           xlab = "Total Yield")
 
 ### Q plot ####
 
 # Q-Q plot: Q-Q plot (or quantile-quantile plot) draws the correlation between a 
 # given sample and the normal distribution. A 45-degree reference line is also plotted.
 
 ggqqplot(dat$corrected_plot_t_ha, 
          main = "Total Score")
 
 
 
 ### Normality test ####
 
 # Visual inspection, described in the previous section, is usually unreliable. 
 # It’s possible to use a significance test comparing the sample distribution to a 
 # normal one in order to ascertain whether data show or not a serious deviation from normality.
 # 
 # There are several methods for normality test such as Kolmogorov-Smirnov (K-S)
 # normality test and Shapiro-Wilk’s test.
 
 # Shapiro-Wilk’s method is widely recommended for normality test and it provides 
 # better power than K-S. It is based on the correlation between the data and the corresponding normal scores.
 
 # From the output, the p-value > 0.05 implying that the distribution of the data 
 # are not significantly different from normal distribution. In other words, we can assume the normality.
 
 shapiro.test(dat$corrected_plot_t_ha)
 
 
 ### Histogram ####
 
 gghistogram(dat$corrected_plot_t_ha)
 
 
 ### Box plots ####
 # ++++++++++++++++++++
 # Plot weight by group and color by group
 
 ggplot(dat, aes(x = Treatment, 
                 y = corrected_plot_t_ha, colour = Treatment)) + 
   geom_boxplot() 
 
 
 
 ### STATS ####

 # filter to Treatment 
 ca.dat <-  filter(dat, Treatment == "Conservation")
 con.dat <- filter(dat, Treatment == "Conventional")
 
 ca.min <- min(ca.dat$corrected_plot_t_ha)
 ca.max <- max(ca.dat$corrected_plot_t_ha)
 ca.mean <- mean(ca.dat$corrected_plot_t_ha)
 
 con.min <- min(con.dat$corrected_plot_t_ha)
 con.max <- max(con.dat$corrected_plot_t_ha)
 con.mean <- mean(con.dat$corrected_plot_t_ha)
 
 
 
 ## check distribution ####
 
 source(file = "~/Documents/GitHub/phd_tools/fun_distribution_tests.R")
 source(file = "~/Documents/GitHub/phd_tools/fun_lmm_diagnostic_plots.R")
 
 colnames(dat)
 cols_to_analyse <- c(11,18,19,22)
 check_gamma_distribution(data = dat, columns_to_check = cols_to_analyse)
 check_guassian(data = dat, columns_to_check = cols_to_analyse)
 
 
 #### GLM ####
 
 # run glm with 
 glm <- glm(corrected_plot_t_ha ~ Treatment * crop, 
            family = gaussian, 
            data = dat)

 
 summary(glm) 
 
 # Perform pairwise comparisons
 pairwise_results <- emmeans(glm, pairwise ~ Treatment | crop)
 
 # View the pairwise comparison results
 summary(pairwise_results)
 

 diagnostic_plots_glm(glm)
 

 # Fit a linear mixed-effects model (LMM)
 
 lmm_model <- lmer(corrected_plot_t_ha ~ Treatment + (1 | year), 
                   data = dat)
 
 # View the summary of the model
 summary(lmm_model)
 
 
 # Perform pairwise comparisons for the Treatment factor
 pairwise_comparisons <- emmeans(lmm_model, pairwise ~ Treatment)
 
 # View the results of the pairwise comparisons
 summary(pairwise_comparisons)
 
 # To extract only the p-values of the pairwise comparisons:
 pairwise_comparisons$contrasts
 
 
 
 ### pc yield UK ####
 
 ### year as a random effect 
 
 lmm_model <- lmer(yield_percent_uk ~ Treatment + (1 | year), 
                   data = dat)
 
 # View the summary of the model
 summary(lmm_model)
 
 
 # Perform pairwise comparisons for the Treatment factor
 pairwise_comparisons <- emmeans(lmm_model, pairwise ~ Treatment)
 
 # View the results of the pairwise comparisons
 summary(pairwise_comparisons)
 
 # To extract only the p-values of the pairwise comparisons:
 pairwise_comparisons$contrasts
 
 
 
 
 ### year in the model 
 
 lmm_model <- lmer(yield_percent_uk ~ Treatment + year + (1 | block), 
                   data = dat)
 
 # View the summary of the model
 summary(lmm_model)
 
 
 # Perform pairwise comparisons for the Treatment factor
 pairwise_comparisons <- emmeans(lmm_model, pairwise ~ Treatment)
 
 # View the results of the pairwise comparisons
 summary(pairwise_comparisons)
 
 # To extract only the p-values of the pairwise comparisons:
 pairwise_comparisons$contrasts
 
 
 diagnostic_plots_lmm(model = lmm_model)
 

 

 
 
 
 
 ## 04 BARPLOT ####
 
 
 ### YIELD PLOT ####
 
 title_exp <- expression(Combine~Yield~(t~ha^{-1}))  # this is the legend title with correct notation
 
 y_title <- expression(Combine~Yield~(t~ha^{-1}))
 
 yield_sum$crop <- factor(x = yield_sum$crop ,levels = c("Spring Beans", "Winter Wheat", "Spring Barley", "Oilseed Rape"))
 
y1 <- ggplot(data = yield_sum, 
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
   theme(strip.text.x = element_text(size = 10, 
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

y1
 
 
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
 
 
 # ggsave(filename = "p_plot.png", 
 #        path = "plots/", width = 8, height = 5)
 
 

 
 
### PERCENTAGE YIELD ####
 
 y2 <- ggplot(data = yield_pc_sum, 
              aes(x = treatment, 
                  y = mean, 
                  fill = treatment)) + 
   geom_bar(stat = "identity", 
            color = "black", 
            position = "dodge") + 
   labs(
     x = "Treatment",
     y = "Percentage of UK average yield (%)",
     subtitle = "Percentage of UK average yield (%)", 
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
              scales = 'free_x', ) 

 
y2 




ggarrange(y1, y2, ncol = 2, common.legend = TRUE, legend = "bottom")

ggsave(filename = "plots/fig_yield_plot.png", width = 10, height = 4)


