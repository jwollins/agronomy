## script to plot crop rotations as a gantt chart
## jw collins 
## 

##  00 SETUP ###

setwd(dir = "~/OneDrive - Harper Adams University/Data/agronomy/")



## 01 PACKAGES ####

# {pacman} package to laod all packages if required. 
install.packages("pacman")

# call required packages and install if required. 
pacman::p_load(tidyverse, plan, scales)



## 02 DATA ####

df <- data.frame(treatment = c("Conservation", "Conventional",    # Y1 treatments
                               "Conservation", "Conventional",    # Y2 treatments
                               "Conservation", "Conventional",    # Y3 treatments
                               "Conservation"),  # redrilled crop
                 
                     start = c("2022-03-28", "2022-03-28",    # Y1 drilling dates
                               "2022-10-12", "2022-10-15",    # Y2 drilling dates 
                               "2023-09-07", "2023-09-10",    # Y3 drilling dates
                               "2024-03-01"),   
                 
                       end = c("2022-10-03", "2022-10-03",    # Y1 harvest dates
                               "2023-08-20", "2023-08-20",    # Y2 harvest dates
                               "2024-02-01", "2024-08-09",    # Y3 harvest dates 
                               "2024-09-14"),   
                 
                     crop = c("Spring Beans", "Spring Beans",    # Y1 crops
                              "Winter Wheat", "Winter Wheat",    # Y2 crops 
                              "Oilseed Rape", "Oilseed Rape",    # Y3 crops
                              "Spring Barley"))    

head(df)

df <- df %>% 
  mutate(start = as.Date(start), end = as.Date(end))

df_tidy <- df %>% 
  gather(key=date_type, value=date, -treatment, -crop)



# Convert Date column to Date type if not already
df_tidy$date <- as.Date(df_tidy$date)

class(df_tidy$date)


# Plot using position_dodge to separate treatments
ggplot(df_tidy, aes(x = fct_rev(fct_inorder(crop)), 
                    y = date, 
                    color = treatment)) +
  geom_line(aes(group = interaction(crop, treatment)), 
            size = 10, 
            position = position_dodge(width = 0.8)) +
  coord_flip() +
  labs(x = "Crop", 
       y = "Date") + 
  theme_minimal() +
  theme(legend.position = "bottom", 
        axis.text.x = element_text(angle = 45, 
                                   vjust = 0.5, 
                                   hjust = 0.5),
        axis.text.y = element_text(angle = 45, 
                                   vjust = 1.5, 
                                   hjust = 0.8)) +
  scale_y_date(
    breaks = date_breaks("8 weeks"),  # Adjust the frequency of x-axis ticks
    labels = date_format("%m/%Y")  # Format the date labels
  ) + 
  guides(color = guide_legend(title = "Treatment")) 

ggsave(filename = "plots/crop_roation_gantt.png", width = 10, height = 5)













