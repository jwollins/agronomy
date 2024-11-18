## HEADER ####
## who: J Collins
## what: Wheat yield 2023
## when: 2023-09-05

## 00 packages ####

library(ggplot2)
library(ggpubr)
library(dplyr) # summary table 
library(ggpmisc)
library(ggsignif) # significance on barplots

## 01 DATA ####
 dat <- read.csv(file = "yield/combine.yield/data/2023.yield.wheat.csv")
 
 # Organise factors
 dat$plot <- as.factor(dat$plot)
 dat$block <- as.factor(dat$block)
 dat$Treatment <- as.factor(dat$Treatment)

 
## 02 Summary Table ####
 
 # Calculates mean, sd, se and IC - block
 my_sum <- dat %>%
   group_by(Treatment) %>%
   summarise( 
     n=n(),
     mean=mean(corrected_plot_t.ha),
     sd=sd(corrected_plot_t.ha)
   ) %>%
   mutate( se=sd/sqrt(n))  %>%
   mutate( ic=se * qt((1-0.05)/2 + .5, n-1))
 
 
 ## 03 Visual tests ####
 
 ### Desity plot ####
 
 # Density plot and Q-Q plot can be used to check normality visually.
 # 
 # Density plot: the density plot provides a visual judgment about whether the distribution is bell shaped.
 
 ggdensity(dat$corrected_plot_t.ha, 
           main = "Combine Yield",
           xlab = "Total Yield")
 
 ### Q plot ####
 
 # Q-Q plot: Q-Q plot (or quantile-quantile plot) draws the correlation between a 
 # given sample and the normal distribution. A 45-degree reference line is also plotted.
 
 ggqqplot(dat$corrected_plot_t.ha, 
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
 
 shapiro.test(dat$corrected_plot_t.ha)
 
 
 ### Histogram ####
 
 gghistogram(dat$corrected_plot_t.ha)
 
 
 ### Box plots ####
 # ++++++++++++++++++++
 # Plot weight by group and color by group
 
 ggplot(dat, aes(x = Treatment, 
                 y = corrected_plot_t.ha)) + 
   geom_boxplot() 
 
 
 
 ### STATS ####

 # filter to Treatment 
 ca.dat <-  filter(dat, Treatment == "Conservation")
 con.dat <- filter(dat, Treatment == "Conventional")
 
 ca.min <- min(ca.dat$corrected_plot_t.ha)
 ca.max <- max(ca.dat$corrected_plot_t.ha)
 ca.mean <- mean(ca.dat$corrected_plot_t.ha)
 
 con.min <- min(con.dat$corrected_plot_t.ha)
 con.max <- max(con.dat$corrected_plot_t.ha)
 con.mean <- mean(con.dat$corrected_plot_t.ha)
 
 
 #### GLM ####
 
 # run glm with 
 glm <- glm(corrected_plot_t.ha ~ Treatment,
             data = dat)
 
 summary(glm) 
 anova(glm) 
 
 aov1 <- aov(corrected_plot_t.ha ~ Treatment,
             data = dat)
 
 
 ### 04 BARPLOT ####
 
 #title_exp <- expression(Plant~Organic~Matter~"%")  # this is the legend title with correct notation
 title_exp_long <- expression(Wheat~Combine~Yield~(t~ha^{-1}))  # this is the legend title with correct notation
 
 # Error bars represent standard error of the mean
 f <- ggplot(my_sum, 
             aes(x=Treatment, 
                 y=mean)) + 
   geom_bar(position=position_dodge(), 
            stat="identity", 
            fill = "white", 
            color = "black") + 
   labs(title= title_exp_long,
        x="Treatment", 
        y = title_exp_long, 
        subtitle = "August 2023") +
   ylim(0,13) +
   geom_errorbar(aes(ymin=mean-se, 
                     ymax=mean+se),
                 width=.2,                    # Width of the error bars
                 position=position_dodge(.9)) + 
   theme_linedraw() + 
   theme(axis.text = element_text(size = 8),  # Axis text size
         axis.title = element_text(size = 10),
         plot.title = element_text(size = 11),
         plot.subtitle = element_text(size = 8))               
 
 f


 #open png for file save and define size and resolution
 png(paste("yield/combine.yield/plots/", "wheat.combine.yield.barplot.2023", ".png", sep=""),
     width=600, height=600, res=150)


f <- f + geom_signif(comparisons = list(c("Conventional", "Conservation")),
                         map_signif_level = TRUE,
                         annotations = c("*"),
                     y_position = c(12),
                     textsize = 5)  # Change significance levels manual

f

dev.off()
