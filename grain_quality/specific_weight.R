## Wheat HH data manipulation 


## This script takes a scrappy output from the DICKEY John and merges replciates, adds plots and treatments.


## 00 PACKAGES ####

# Load necessary library
library(dplyr)
library(readxl)


## 01 DATA ####

setwd(dir = "~/OneDrive - Harper Adams University/Data/2023 wheat/yield/")

dat <- read_excel(path = "hh_yield.xlsx")

glimpse(dat)


## 02 MERGE REPLICATES ####


### specific weight ####

# Summarize the dataframe
dat <- dat %>%
  group_by(sample) %>%                          # Group by the "sample" column
  summarise(
    mass_g = sum(mass_g, na.rm = TRUE),         # Sum "mass_g"
    moisture = mean(moisture, na.rm = TRUE),    # Calculate the mean of "moisture"
    specific_weight = mean(specific_weight, na.rm = TRUE), # Calculate the mean of "specific_weight"
    temp = mean(temp, na.rm = TRUE)             # Calculate the mean of "temp"
  )

# View the summarized dataframe
glimpse(dat)




## 03 ADD PLOTS AND TREATMENT DATA ####

dat$sample <- as.numeric(dat$sample)

dat$sample[49] <- 22
# 
# dat$plot <- NA
# 
# dat$plot <- if_else(condition = dat$sample > 0 & dat$sample < 26, 
#                     true = 1,
#                     false = dat$plot)
# 
# dat$plot <- if_else(condition = dat$sample >= 26 & dat$sample <= 50, 
#                     true = 2,
#                     false = dat$plot)
# 
# dat$plot <- if_else(condition = dat$sample >= 51 & dat$sample <= 75, 
#                     true = 3,
#                     false = dat$plot)
# 
# dat$plot <- if_else(condition = dat$sample >= 76 & dat$sample <= 100, 
#                     true = 4,
#                     false = dat$plot)
# 
# dat$plot <- if_else(condition = dat$sample >= 101 & dat$sample <= 125, 
#                     true = 5,
#                     false = dat$plot)
# 
# dat$plot <- if_else(condition = dat$sample >= 126 & dat$sample <= 150, 
#                     true = 6,
#                     false = dat$plot)
# 
# dat$plot <- if_else(condition = dat$sample >= 151 & dat$sample <= 175, 
#                     true = 7,
#                     false = dat$plot)
# 
# dat$plot <- if_else(condition = dat$sample >= 176 & dat$sample <= 200, 
#                     true = 8,
#                     false = dat$plot)
# 
# dat$plot <- if_else(condition = dat$sample >= 201 & dat$sample <= 225, 
#                     true = 9,
#                     false = dat$plot)
# 
# dat$plot <- if_else(condition = dat$sample >= 226 & dat$sample <= 250, 
#                     true = 10,
#                     false = dat$plot)
# 
# 
# dat$treatment <- if_else(condition = dat$plot == 1 |
#                            dat$plot == 3 |
#                            dat$plot == 5 |
#                            dat$plot == 8 |
#                            dat$plot == 10, 
#                          true = "Conventional", 
#                          false = "Conservation")


# Order rows by the 'ID' column numerically
dat <- dat[order(dat$sample), ]

# Print the ordered dataframe
print(dat)




### add the cordinates ####

temp_dat <- read.csv(file = "~/OneDrive - Harper Adams University/Data/basic_data_template.csv")

# Remove the last row
temp_dat <- temp_dat[-nrow(temp_dat), ]

dat_test <- cbind(dat, temp_dat)


# WRITE THE CSV FILE
write.csv(x = dat_test, file = "~/OneDrive - Harper Adams University/Data/agronomy/data/yield/wheat_hh_yield.csv")






## 04 SUMMARY STATS ####

### specific weight ####

# Calculates mean, sd, se and IC - block
dat_sum <- dat %>%
  group_by(treatment) %>%
  summarise( 
    n=n(),
    mean = mean(specific_weight),
    sd = sd(specific_weight)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))



### mositure ####

# Calculates mean, sd, se and IC - block
moisture_sum <- dat %>%
  group_by(treatment) %>%
  summarise( 
    n=n(),
    mean = mean(moisture),
    sd = sd(moisture)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))


### yield ####

# Calculates mean, sd, se and IC - block
yield_sum <- dat %>%
  group_by(treatment) %>%
  summarise( 
    n=n(),
    mean = mean(mass_g),
    sd = sd(mass_g)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))


## 05 PLOTS ####


### 05.1 SPECIFIC WEIGHT ####

# this is the legend title with correct notation
title_exp <- expression(Grain~specific~weight~(kg~hl^{-1}))
y_title <- expression(Specific~weight~(kg~hl^{-1}))

sw <-    ggplot(data = dat_sum, 
               aes(x = treatment, 
                   y = mean, 
                   fill = treatment)) + 
  geom_bar(stat = "identity", 
           color = "black", 
           position = "dodge") + 
  labs(
    x = element_blank(),
    y = y_title,
    subtitle = title_exp, 
    caption = "") +
  theme_bw() +
  scale_fill_manual(values=c("tomato2", "turquoise3"), 
                    name = "Treatment") +
  theme(strip.text.x = element_text(size = 12, 
                                    color = "black", 
                                    face = "bold.italic"), 
        legend.position = "bottom", 
        axis.text.x = element_blank()) +
  geom_errorbar(aes(ymin = mean - se, 
                    ymax = mean + se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) 

sw



### 05.2 MOISTURE ####

# this is the legend title with correct notation
title_exp <- expression(Grain~mositure~("%"))
y_title <- expression(Grain~mositure~("%"))

m <-    ggplot(data = moisture_sum, 
                aes(x = treatment, 
                    y = mean, 
                    fill = treatment)) + 
  geom_bar(stat = "identity", 
           color = "black", 
           position = "dodge") + 
  labs(
    x = element_blank(),
    y = y_title,
    subtitle = title_exp, 
    caption = "") +
  theme_bw() +
  scale_fill_manual(values=c("tomato2", "turquoise3"), 
                    name = "Treatment") +
  theme(strip.text.x = element_text(size = 12, 
                                    color = "black", 
                                    face = "bold.italic"), 
        legend.position = "bottom", 
        axis.text.x = element_blank()) +
  geom_errorbar(aes(ymin = mean - se, 
                    ymax = mean + se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) 


m




### 05.3 HH Yield ####

# this is the legend title with correct notation
title_exp <- expression(Grain~mass~(g~m^{-1}))
y_title <- expression(Grain~mass~(g~m^{-1}))

y <-    ggplot(data = yield_sum, 
                aes(x = treatment, 
                    y = mean, 
                    fill = treatment)) + 
  geom_bar(stat = "identity", 
           color = "black", 
           position = "dodge") + 
  labs(
    x = element_blank(),
    y = y_title,
    subtitle = title_exp, 
    caption = "") +
  theme_bw() +
  scale_fill_manual(values=c("tomato2", "turquoise3"), 
                    name = "Treatment") +
  theme(strip.text.x = element_text(size = 12, 
                                    color = "black", 
                                    face = "bold.italic"), 
        legend.position = "bottom", 
        axis.text.x = element_blank()) +
  geom_errorbar(aes(ymin = mean - se, 
                    ymax = mean + se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) 


y


ggarrange(sw,m,y,
          ncol = 3, 
          common.legend = TRUE, legend = "bottom")

dir.create(path = "plots/")

ggsave(filename = "plots/wheat_hh_plots.png", width = 10, height = 6)





