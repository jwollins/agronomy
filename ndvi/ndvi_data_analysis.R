## NDVI ANALYSIS 
## J COLLINS 
## 2024-11-19


getwd()

setwd("~/OneDrive - Harper Adams University/Data/")




## 00 PACKAGES ####

# Load required libraries
library(raster)
library(sf)
library(lubridate)
library(ggplot2)
library(gganimate)
library(transformr)
library(scales)
library(dplyr)




## 01 DATA ####


# Load the shapefile and remove Z/M dimensions
all_plots <- st_read("Shapefiles/Plots/Full plots/all.plots.shp")
all_plots <- st_zm(all_plots)  # Removes Z/M dimensions

# List all raster files in the directory
raster_files <- list.files("ndvi/data/QGIS.rasters", pattern = "\\.tif$", full.names = TRUE)




## 02 DATA FORLOOP ####

# Define the reference raster (dat1)
ref_raster <- brick("ndvi/data/QGIS.rasters/2022_03_08_QGIS.tif")

# Get the list of files
new_files <- list.files("ndvi/data/2023_06_to_2024_10_psscene_analytic_sr_udm2/PSScene/", 
                        pattern = "\\.tif$", full.names = TRUE)

# Create an empty list to store processed rasters
processed_rasters <- list()

# Loop through each file in the list
for (raster_file in new_files) {
  
  # Load the raster
  dat2 <- brick(raster_file)
  
  # Reproject dat2 to match the CRS of dat1
  dat2_reprojected <- projectRaster(dat2, crs = crs(ref_raster))
  
  # Resample dat2 to match the resolution of dat1
  dat2_resampled <- resample(dat2_reprojected, ref_raster)
  
  # Crop dat2 to match the extent of dat1
  dat2_cropped <- crop(dat2_resampled, extent(ref_raster))
  
  # If needed, select specific layers from dat2 to match the number of layers in dat1
  dat2_final <- dat2_cropped[[1:4]]  # Adjust the layer index if necessary
  
  # Add the processed raster to the list
  processed_rasters[[raster_file]] <- dat2_final
}

# Now processed_rasters contains all the reprojected, resampled, and cropped rasters


names(raster_files)



# test bind 

raster_files <- c(raster_files, processed_rasters)


######## TEST RUN #############

# Initialize an empty list to store results
all_results <- list()

# Loop through each raster file
for (raster_file in raster_files) {
  # Extract the date from the filename
  date_str <- gsub("^(\\d{8})_.*", "\\1", basename(raster_file))
  raster_date <- as.Date(date_str, format = "%Y%m%d")
  
  # Load the raster and calculate NDVI
  dat <- brick(raster_file)
  
  # Check and align CRS
  if (!st_crs(all_plots) == crs(dat)) {
    all_plots <- st_transform(all_plots, crs = crs(dat))  # Reproject shapefile if needed
  }
  
  # Check if extents overlap
  raster_extent <- extent(dat)
  plots_extent <- extent(st_bbox(all_plots))
  
  # If the extents do not overlap, skip this raster
  if (is.null(intersect(raster_extent, plots_extent))) {
    warning(paste("Raster and shapefile extents do not overlap for", raster_file))
    next  # Skip this raster if extents do not overlap
  }
  
  # Calculate NDVI
  dat <- ndviFUN(dat)
  
  # Crop and mask raster
  cropped_raster <- crop(dat, plots_extent)
  trimmed_raster <- mask(cropped_raster, all_plots)
  
  # Calculate mean NDVI for each polygon
  ndvi_values <- extract(trimmed_raster, all_plots, fun = mean, na.rm = TRUE, df = TRUE)
  
  # Create a dataframe with the results
  ndvi_df <- data.frame(
    Plot_ID = all_plots$Name,  # Replace 'Name' with the appropriate column name
    Mean_NDVI = ndvi_values[, 2],  # Adjust index if necessary
    Date = raster_date
  )
  
  # Append the results to the list
  all_results[[raster_file]] <- ndvi_df
}


# Combine all individual dataframes into one single dataframe
final_results <- do.call(rbind, all_results)

final_results <- final_results[final_results$Mean_NDVI >= 0 & !is.na(final_results$Mean_NDVI), ]

final_results$treatment <- ifelse(test = final_results$Plot_ID == "Plot 1" |
                                    final_results$Plot_ID == "Plot 3" |
                                    final_results$Plot_ID == "Plot 5" |
                                    final_results$Plot_ID == "Plot 8" |
                                    final_results$Plot_ID == "Plot 10", 
                                  yes = "Conventional", no = "Conservation")

# View the final combined dataframe
head(final_results)


write.csv(x = final_results, file = "ndvi/data/prcoessed.data/new_ndvi_data.csv")





## 03 SUMMARY STATS ####


ndvi_sum <- final_results %>%
  group_by(treatment, Date) %>%
  summarise( 
    n = n(),
    mean = mean(Mean_NDVI, na.rm = TRUE),  # Exclude NAs when calculating mean
    sd = sd(Mean_NDVI, na.rm = TRUE)       # Exclude NAs when calculating sd
  ) %>%
  mutate(se = sd / sqrt(n)) %>%
  mutate(ic = se * qt((1 - 0.05) / 2 + 0.5, n - 1))  # Confidence interval

glimpse(ndvi_sum)







## 04 PLOTS ####

ggplot(ndvi_sum, aes(y = mean, x = Date, color = treatment)) +
  geom_point() +  # Add points for each observation
  geom_line(aes(group = treatment), alpha = 0.7) +  # Add lines to connect points by treatment
  geom_ribbon(aes(ymin = mean - se, ymax = mean + se, fill = treatment), alpha = 0.2) +  # Add the error ribbon
  theme_minimal() +  # Clean theme
  labs(
    x = "Date",
    y = "Mean NDVI",
    color = "Treatment",
    fill = "Treatment"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), # Rotate x-axis labels for clarity
        legend.position = "bottom") +
  scale_x_date(
    breaks = date_breaks("month"),  # Adjust the frequency of x-axis ticks
    labels = date_format("%m/%Y")  # Format the date labels
  )






















## 02 FUNCTIONS ####

# Define NDVI function
ndviFUN <- function(raster_stack) {
  # Assuming the raster has at least two bands: Red (band 1) and NIR (band 2)
  nir <- raster_stack[[2]]
  red <- raster_stack[[1]]
  ndvi <- (nir - red) / (nir + red)
  return(ndvi)
}



## 03 NDVI ####

# Initialize an empty list to store results
all_results <- list()

# Loop through each raster file
for (raster_file in raster_files) {
  # Extract the date from the filename
  date_str <- gsub(".*_(\\d{4}_\\d{2}_\\d{2})_.*", "\\1", basename(raster_file))
  raster_date <- as.Date(date_str, format = "%Y_%m_%d")
  
  # Load the raster and calculate NDVI
  dat <- brick(raster_file)
  
  # Check and align CRS
  if (!st_crs(all_plots) == crs(dat)) {
    all_plots <- st_transform(all_plots, crs = crs(dat))  # Reproject shapefile if needed
  }
  
  # Check if extents overlap
  raster_extent <- extent(dat)
  plots_extent <- extent(st_bbox(all_plots))
  
  # If the extents do not overlap, skip this raster
  if (is.null(intersect(raster_extent, plots_extent))) {
    warning(paste("Raster and shapefile extents do not overlap for", raster_file))
    next  # Skip this raster if extents do not overlap
  }
  
  # Calculate NDVI
  dat <- ndviFUN(dat)
  
  # Crop and mask raster
  cropped_raster <- crop(dat, plots_extent)
  trimmed_raster <- mask(cropped_raster, all_plots)
  
  # Calculate mean NDVI for each polygon
  ndvi_values <- extract(trimmed_raster, all_plots, fun = mean, na.rm = TRUE, df = TRUE)
  
  # Create a dataframe with the results
  ndvi_df <- data.frame(
    Plot_ID = all_plots$Name,  # Replace 'Name' with the appropriate column name
    Mean_NDVI = ndvi_values[, 2],  # Adjust index if necessary
    Date = raster_date
  )
  
  # Append the results to the list
  all_results[[raster_file]] <- ndvi_df
}

# Combine all results into one dataframe
final_ndvi_df <- do.call(rbind, all_results)

final_ndvi_df$treatment <- ifelse(test = final_ndvi_df$Plot_ID == "Plot 1" |
                                    final_ndvi_df$Plot_ID == "Plot 3" |
                                    final_ndvi_df$Plot_ID == "Plot 5" |
                                    final_ndvi_df$Plot_ID == "Plot 8" |
                                    final_ndvi_df$Plot_ID == "Plot 10", 
                                  yes = "Conventional", no = "Conservation")

class(final_ndvi_df)
glimpse(final_ndvi_df)


# Save the combined dataframe to CSV
write.csv(final_ndvi_df, "ndvi/data/prcoessed.data/NDVI_by_plot_all_dates.csv", row.names = FALSE)






###########################################################################


dat1 <- read.csv(file = "~/OneDrive - Harper Adams University/Data/ndvi/data/prcoessed.data/NDVI_by_plot_all_dates.csv")
dat2 <- read.csv(file = "~/OneDrive - Harper Adams University/Data/ndvi/data/prcoessed.data/new_ndvi_data.csv")

dat2 <- dat2[,2:ncol(dat2)]

final_ndvi_df <- rbind(dat1, dat2)

write_csv(x = final_ndvi_df, file = "ndvi/data/prcoessed.data/final_ndvi_df.csv")


## 03 SUMMARY STATS ####


ndvi_sum <- final_ndvi_df %>%
  group_by(treatment, Date) %>%
  summarise( 
    n = n(),
    mean = mean(Mean_NDVI, na.rm = TRUE),  # Exclude NAs when calculating mean
    sd = sd(Mean_NDVI, na.rm = TRUE)       # Exclude NAs when calculating sd
  ) %>%
  mutate(se = sd / sqrt(n)) %>%
  mutate(ic = se * qt((1 - 0.05) / 2 + 0.5, n - 1))  # Confidence interval

glimpse(ndvi_sum)







## 04 PLOTS ####

# Convert Date column to Date type if not already
ndvi_sum$Date <- as.Date(ndvi_sum$Date)

ndvi_sum$treatment <- factor(ndvi_sum$treatment, levels = c("Conventional", "Conservation"))



# ~ basic plot ####

ggplot(data = ndvi_sum, 
       aes(y = mean, x = Date, color = treatment)) +
  geom_point(size = 1) +  # Add points for each observation
  geom_line(
    aes(group = treatment), 
    alpha = 0.7) +  # Add lines to connect points by treatment
  geom_ribbon(
    aes(ymin = mean - se, ymax = mean + se, fill = treatment), 
    alpha = 0.3, 
    color = NA) +  # Add the error ribbon
  theme_bw() +  # Clean theme
  labs(
    x = "Date",
    y = "Mean NDVI",
    color = "Treatment",
    fill = "Treatment"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), # Rotate x-axis labels for clarity
        legend.position = "bottom") +
  scale_x_date(
    breaks = date_breaks("8 weeks"),  # Adjust the frequency of x-axis ticks
    labels = date_format("%m/%Y")  # Format the date labels
  )

ggsave(filename = "agronomy/plots/fig_ndvi_plot.png", width = 8, height = 4)







# ~ moving average plot ####

library(zoo)

ndvi_sum <- ndvi_sum %>%
  arrange(treatment, Date) %>%  # Ensure data is sorted correctly
  group_by(treatment) %>%
  mutate(mean_smooth = rollmean(mean, k = 5, fill = NA, align = "center"))  # Moving average

ggplot(ndvi_sum, aes(x = Date)) +
  geom_line(aes(y = mean, color = "Raw NDVI"), size = 1, alpha = 0.5) +  # Raw data (faded)
  geom_line(aes(y = mean_smooth, color = "Smoothed NDVI"), size = 1.2) +  # Smoothed data
  facet_wrap(~ treatment) +
  labs(y = "NDVI", x = "Date", title = "NDVI Trends with Moving Average") +
  scale_color_manual(values = c("Raw NDVI" = "grey50", "Smoothed NDVI" = "blue")) +
  theme_minimal()

# ndvi_sum <- ndvi_sum %>%
#   group_by(treatment) %>%
#   mutate(mean_smooth = rollapply(mean, width = 5, FUN = median, fill = NA, align = "center"))

library(zoo)
ndvi_sum <- ndvi_sum %>%
  arrange(treatment, Date) %>%  # Ensure data is sorted correctly
  group_by(treatment) %>%
  mutate(
    mean_smooth = rollmean(mean, k = 5, fill = NA, align = "center"),
    se_smooth = rollmean(se, k = 5, fill = NA, align = "center")  # Smooth the error too
  )






ggplot(data = ndvi_sum, 
       aes(y = mean_smooth, x = Date, color = treatment)) +
  geom_point(aes(y = mean), size = 1, alpha = 0.5) +  # Plot original points (faded)
  geom_line(aes(group = treatment), alpha = 0.8, size = 1.2) +  # Smoothed line
  geom_ribbon(
    aes(ymin = mean_smooth - se_smooth, ymax = mean_smooth + se_smooth, fill = treatment), 
    alpha = 0.3, 
    color = NA) +  # Smoothed error ribbon
  theme_bw() +  # Clean theme
  labs(
    x = "Date",
    y = "Mean NDVI (Smoothed)",
    color = NULL,
    fill = NULL
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), # Rotate x-axis labels for clarity
        legend.position = "bottom") +
  scale_x_date(
    breaks = date_breaks("8 weeks"),  # Adjust the frequency of x-axis ticks
    labels = date_format("%m/%Y")  # Format the date labels
  )


ggsave(filename = "~/OneDrive - Harper Adams University/Data/agronomy/plots/fig_ndvi_moving_mean_plot.png", width = 8, height = 4)



ggplot(data = ndvi_sum, 
       aes(y = mean_smooth, x = Date, color = treatment)) +
  geom_point(aes(y = mean), size = 1, alpha = 0.5) +  # Plot original points (faded)
  geom_line(aes(group = treatment), alpha = 0.8, size = 1.2) +  # Smoothed line
  geom_ribbon(
    aes(ymin = mean_smooth - se_smooth, ymax = mean_smooth + se_smooth, fill = treatment), 
    alpha = 0.3, 
    color = NA) +  # Smoothed error ribbon
  theme_bw() +  # Clean theme
  labs(
    x = "Date",
    y = "Mean NDVI (Smoothed)",
    color = NULL,
    fill = NULL
  ) +
  geom_vline(xintercept = as.numeric(as.Date("2022-10-03")),  # Example date for the vertical line
             linetype = "dashed",
             color = "black") +
  geom_vline(xintercept = as.numeric(as.Date("2023-08-20")),  # Example date for the vertical line
             linetype = "dashed",
             color = "black") +
  geom_vline(xintercept = as.numeric(as.Date("2024-03-15")),  # Example date for the vertical line
             linetype = "dashed",
             color = "turquoise3") +
  annotate(geom = "text", 
           x = as.Date("2022-04-05"),
           y = 0.4, 
           label = "Spring Beans", 
           fontface = "bold") +
  annotate(geom = "text", 
           x = as.Date("2023-02-05"),
           y = 0.4, 
           label = "Winter Wheat", 
           fontface = "bold") +
  annotate(geom = "text", 
           x = as.Date("2023-11-25"),
           y = 0.4, 
           label = "Oilseed Rape", 
           fontface = "bold") + 
  annotate(geom = "text", 
           x = as.Date("2024-07-01"),
           y = 0.4, 
           label = "Spring Barley", 
           fontface = "bold",
           color = "turquoise3") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1), # Rotate x-axis labels for clarity
        legend.position = "bottom") +
  scale_x_date(
    breaks = date_breaks("8 weeks"),  # Adjust the frequency of x-axis ticks
    labels = date_format("%m/%Y"),  # Format the date labels
    limits = c(as.Date("2022-02-01"), as.Date("2024-10-01"))
  ) 


ggsave(filename = "~/OneDrive - Harper Adams University/Data/agronomy/plots/fig_ndvi_moving_mean_plot_with_lines.png", width = 8, height = 4)






shapiro.test(final_ndvi_df$Mean_NDVI)


glm_model <- glm(formula = Mean_NDVI ~ treatment, data = final_ndvi_df)

summary(glm_model)






## STATS ####

### visualisation ####

dat <- read.csv(file = "ndvi/data/prcoessed.data/NDVI_by_plot_all_dates.csv")

dat1 <- read.csv(file = "ndvi/data/prcoessed.data/NDVI_by_plot_all_dates.csv")
dat2 <- read.csv(file = "ndvi/data/prcoessed.data/new_ndvi_data.csv")

dat2 <- dat2[,2:ncol(dat2)]

dat <- rbind(dat1, dat2)

glimpse(dat)



# Plot histogram
a <- ggplot(dat, aes(x = Mean_NDVI, fill = treatment)) +
  geom_histogram(binwidth = 0.02, color = "black", alpha = 0.7, position = "dodge") +
  labs(
    x = "Mean NDVI",
    y = "Count",
    fill = "Treatment"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    legend.position = "bottom"
  )


# Create QQ plot
b <- ggplot(dat, aes(sample = Mean_NDVI, color = treatment)) +
  stat_qq() +
  stat_qq_line() +
  labs(
    x = "Theoretical Quantiles",
    y = "Sample Quantiles",
    color = "Treatment"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    legend.position = "bottom"
  )

ggarrange(a,b, 
          ncol = 2, 
          common.legend = TRUE, 
          legend = "bottom", labels = c("A", "B"))

ggsave(filename = "agronomy/plots/ndvi_data_vis.png", width = 14, height = 6)


### normality ####

shapiro.test(x = dat$Mean_NDVI)

bartlett.test(Mean_NDVI ~ treatment, data = dat)


### glm model ####
dat$Mean_NDVI[dat$Mean_NDVI <= 0] <- 1e-6

glm_model <- glm(Mean_NDVI ~ treatment, 
                 data = dat, 
                 family = Gamma(link = "log"))

summary(glm_model)




## by crop 

# Convert the Date column to Date format
dat$Date <- as.Date(dat$Date, format = "%Y-%m-%d")

# Create new column 'crop' based on the 'Date' column
dat <- dat %>%
  mutate(crop = case_when(
    Date >= as.Date("2022-03-01") & Date <= as.Date("2022-10-01") ~ "Spring Beans",
    Date >= as.Date("2022-10-15") & Date <= as.Date("2023-08-01") ~ "Winter Wheat",
    Date >= as.Date("2023-09-01") & Date <= as.Date("2024-08-01") ~ "Oilseed Rape",
    TRUE ~ "Other"
  ))

# Check the changes
glimpse(dat)

osr <- filter(.data = dat, crop == "Other")



glm_model <- glm(Mean_NDVI ~ treatment * crop, 
                 data = dat, 
                 family = Gamma(link = "log"))

summary(glm_model)

writeLines(capture.output(glm_model), "agronomy/stats/ndvi_glm_model_summary.txt")

# Perform pairwise comparisons using emmeans
emmeans_res <- emmeans(glm_model, pairwise ~ treatment | crop)

summary(emmeans_res)

writeLines(capture.output(emmeans_res), "agronomy/stats/emmeans_res_summary.txt")








