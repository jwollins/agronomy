## NDVI ANALYSIS 
## J COLLINS 
## 2024-11-19


## 00 PACKAGES ####

# Load required libraries
library(raster)
library(sf)
library(lubridate)
library(ggplot2)
library(gganimate)
library(transformr)
library(scales)


## 01 DATA ####


# Load the shapefile and remove Z/M dimensions
all_plots <- st_read("Shapefiles/Plots/Full plots/all.plots.shp")
all_plots <- st_zm(all_plots)  # Removes Z/M dimensions

# List all raster files in the directory
raster_files <- list.files("ndvi/data/QGIS.rasters", pattern = "\\.tif$", full.names = TRUE)


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

# Print the final dataframe
glimpse(final_ndvi_df)




###########################################################################


## 03 SUMMARY STATS ####

dat <- final_ndvi_df

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
library(scales)


ggplot(ndvi_sum, aes(y = mean, x = Date, color = treatment)) +
  geom_point() +  # Add points for each observation
  geom_line(aes(group = treatment), alpha = 0.7) +  # Add lines to connect points by treatment
  geom_ribbon(aes(ymin = mean - se, ymax = mean + se, fill = treatment), alpha = 0.2) +  # Add the error ribbon
  theme_minimal() +  # Clean theme
  labs(
    title = "NDVI vs Date by Treatment with Error Ribbon",
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



## animation ####


final_ndvi_df$Date <- as.Date(final_ndvi_df$Date)
head(final_ndvi_df$Date)



p <- ggplot(final_ndvi_df, aes(x = Plot_ID, y = Mean_NDVI, color = treatment)) +
  geom_point(size = 3) + 
  theme_minimal() +
  labs(title = 'NDVI Over Time', x = 'Plot ID', y = 'Mean NDVI') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Base plot without animation
ggplot(final_ndvi_df, aes(x = Plot_ID, y = Mean_NDVI, color = treatment)) +
  geom_point(size = 3) + 
  theme_minimal() +
  labs(title = 'NDVI Over Time', x = 'Plot ID', y = 'Mean NDVI') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Try adding transition_time
p + transition_time(Date)


animated_plot <- p + 
  transition_states(Date, transition_length = 2, state_length = 1) +
  labs(subtitle = 'Date: {closest_state}')

# Animate with `transition_states`
animate(animated_plot, nframes = length(unique(final_ndvi_df$Date)), 
        duration = 10, width = 800, height = 600)








