## Title: NDVI analysis: planet data
## Author: J Collins 
## Date: 2023-06-29
## About: Load, analyse and plot planet raster data to NDVI images.  
## last edit: 2023-06-29

## Contents ####
## Packages 
## Functions
## Load data 


## Packages ####

library(sp)
library(rgdal)
library(raster)
library(ggplot2)
library(viridis)
library(rasterVis)
library(terra)
library(sf)

## Functions ####

source("scripts/functions/ndviFUN.R")

## Load data ####

### shapefiles ####
CON_shp <- st_read("Shapefiles/experiment.plots/Buffered plots/_ON.plots.buffered.shp")
CA_shp <- st_read("Shapefiles/experiment.plots/Buffered plots/CA.plots.buffered.shp")

plot(CA_shp)

## Rasters ####

# create template extent 

extent.temp <- brick("data/QGIS.rasters/2022_03_23_QGIS.tif")
extent(extent.temp)

### rasters - CONVENTIONAL ####

my_shp <- CON_shp

# dat <- brick("data/QGIS.rasters/2022_03_05_QGIS.tif")
# ndvi1 <- ndviFUN(dat)
# extent(ndvi1) # wrong extent
# plot(ndvi1) # not working 

dat <- brick("data/QGIS.rasters/2022_03_08_QGIS.tif")
dat <- ndviFUN(dat)
extent(dat)
plot(dat)
con_stack <- dat
date.list <- as.Date("2022/03/08")

dat <- brick("data/QGIS.rasters/2022_03_19_QGIS.tif")
dat <- ndviFUN(dat)
extent(dat)
plot(dat)
con_stack <- stack(con_stack, dat)
date.list <- c(date.list, as.Date("2022/03/19"))

dat <- brick("data/QGIS.rasters/2022_03_23_QGIS.tif")
dat <- ndviFUN(dat)
extent(dat)
plot(dat)
con_stack <- stack(con_stack, dat)
date.list <- c(date.list, as.Date("2022/03/23"))


# dat <- brick("data/QGIS.rasters/2022_03_28_QGIS.tif")
# dat <- ndviFUN(dat)
# extent(dat)
# plot(dat) # awful
# con_stack <- stack(con_stack, dat)

# dat <- brick("data/QGIS.rasters/2022_04_04_QGIS.tif")
# dat <- ndviFUN(dat)
# extent(dat) # wrong extent
# extent(dat) <- extent(extent.temp) # convert extent
# extent(dat) # correct extent
# plot(dat)
# con_stack <- stack(con_stack, dat)
# date.list <- c(date.list, "2022_04_04")


dat <- brick("data/QGIS.rasters/2022_04_10_QGIS.tif")
dat <- ndviFUN(dat)
extent(dat)
plot(dat)
con_stack <- stack(con_stack, dat)
date.list <- c(date.list, as.Date("2022/04/10"))

dat <- brick("data/QGIS.rasters/2022_04_20_QGIS.tif")
dat <- ndviFUN(dat)
extent(dat)
plot(dat)
con_stack <- stack(con_stack, dat)
date.list <- c(date.list, as.Date("2022/02/20"))

dat <- brick("data/QGIS.rasters/2022_06_15_QGIS.tif")
dat <- ndviFUN(dat)
extent(dat)
plot(dat)
con_stack <- stack(con_stack, dat)
date.list <- c(date.list, as.Date("2022/06/15"))


dat <- brick("data/QGIS.rasters/2022_06_21_QGIS.tif")
dat <- ndviFUN(dat)
extent(dat)
plot(dat)
con_stack <- stack(con_stack, dat)
date.list <- c(date.list, as.Date("2022/06/21"))

dat <- brick("data/QGIS.rasters/2022_07_09_QGIS.tif")
dat <- ndviFUN(dat)
extent(dat)
plot(dat)
con_stack <- stack(con_stack, dat)
date.list <- c(date.list, as.Date("2022/07/09"))

dat <- brick("data/QGIS.rasters/2022_07_16_QGIS.tif")
dat <- ndviFUN(dat)
extent(dat)
plot(dat)
con_stack <- stack(con_stack, dat)
date.list <- c(date.list, as.Date("2022/07/16"))

dat <- brick("data/QGIS.rasters/2022_07_19_QGIS.tif")
dat <- ndviFUN(dat)
extent(dat)
plot(dat)
con_stack <- stack(con_stack, dat)
date.list <- c(date.list, as.Date("2022/07/19"))

dat <- brick("data/QGIS.rasters/2022_08_09_QGIS.tif")
dat <- ndviFUN(dat)
extent(dat)
plot(dat)
con_stack <- stack(con_stack, dat)
date.list <- c(date.list, as.Date("2022/08/09"))

dat <- brick("data/QGIS.rasters/2022_08_12_QGIS.tif")
dat <- ndviFUN(dat)
extent(dat)
plot(dat)
con_stack <- stack(con_stack, dat)
date.list <- c(date.list, as.Date("2022/08/12"))

dat <- brick("data/QGIS.rasters/2022_08_14_QGIS.tif")
dat <- ndviFUN(dat)
extent(dat)
plot(dat)
con_stack <- stack(con_stack, dat)
date.list <- c(date.list, as.Date("2022/08/14"))

dat <- brick("data/QGIS.rasters/2022_08_28_QGIS.tif")
dat <- ndviFUN(dat)
extent(dat)
plot(dat) # not great
con_stack <- stack(con_stack, dat)
date.list <- c(date.list, as.Date("2022/08/28"))

dat <- brick("data/QGIS.rasters/2022_09_17_QGIS.tif")
dat <- ndviFUN(dat)
extent(dat)
plot(dat)
con_stack <- stack(con_stack, dat)
date.list <- c(date.list, as.Date("2022/09/17"))

dat <- brick("data/QGIS.rasters/2022_10_08_QGIS.tif")
dat <- ndviFUN(dat)
extent(dat)
plot(dat) # not great
con_stack <- stack(con_stack, dat)
date.list <- c(date.list, as.Date("2022/10/08"))

dat <- brick("data/QGIS.rasters/2022_10_11_QGIS.tif")
dat <- ndviFUN(dat)
extent(dat)
plot(dat) 
con_stack <- stack(con_stack, dat)
date.list <- c(date.list, as.Date("2022/10/11"))

dat <- brick("data/QGIS.rasters/2022_10_13_QGIS.tif")
dat <- ndviFUN(dat)
extent(dat)
plot(dat) 
con_stack <- stack(con_stack, dat)
date.list <- c(date.list, as.Date("2022/10/13"))

dat <- brick("data/QGIS.rasters/2022_10_17_QGIS.tif")
dat <- ndviFUN(dat)
extent(dat)
plot(dat) 
con_stack <- stack(con_stack, dat)
date.list <- c(date.list, as.Date("2022/10/17"))

dat <- brick("data/QGIS.rasters/2022_10_18_QGIS.tif")
dat <- ndviFUN(dat)
extent(dat)
plot(dat) 
con_stack <- stack(con_stack, dat)
date.list <- c(date.list, as.Date("2022/10/18"))

dat <- brick("data/QGIS.rasters/2022_10_25_QGIS.tif")
dat <- ndviFUN(dat)
extent(dat)
plot(dat) 
con_stack <- stack(con_stack, dat)
date.list <- c(date.list, as.Date("2022/10/25"))

dat <- brick("data/QGIS.rasters/2022_10_28_QGIS.tif")
dat <- ndviFUN(dat)
extent(dat)
plot(dat) 
con_stack <- stack(con_stack, dat)
date.list <- c(date.list, as.Date("2022/10/28"))

dat <- brick("data/QGIS.rasters/2022_11_03_QGIS.tif")
dat <- ndviFUN(dat)
extent(dat)
plot(dat) 
con_stack <- stack(con_stack, dat)
date.list <- c(date.list, as.Date("2022/11/03"))

dat <- brick("data/QGIS.rasters/2022_11_19_QGIS.tif")
dat <- ndviFUN(dat)
extent(dat)
plot(dat) 
con_stack <- stack(con_stack, dat)
date.list <- c(date.list, as.Date("2022/11/19"))

dat <- brick("data/QGIS.rasters/2022_11_25_QGIS.tif")
dat <- ndviFUN(dat)
extent(dat)
plot(dat) 
con_stack <- stack(con_stack, dat)
date.list <- c(date.list, as.Date("2022/11/25"))

dat <- brick("data/QGIS.rasters/2022_12_03_QGIS.tif")
dat <- ndviFUN(dat)
extent(dat)
plot(dat) 
con_stack <- stack(con_stack, dat)
date.list <- c(date.list, as.Date("2022/12/03"))

dat <- brick("data/QGIS.rasters/2022_12_07_QGIS.tif")
dat <- ndviFUN(dat)
extent(dat)
plot(dat) 
con_stack <- stack(con_stack, dat)
date.list <- c(date.list, as.Date("2022/12/07"))

dat <- brick("data/QGIS.rasters/2022_12_15_QGIS.tif")
dat <- ndviFUN(dat)
extent(dat)
plot(dat) 
con_stack <- stack(con_stack, dat)
date.list <- c(date.list, as.Date("2022/12/15"))

dat <- brick("data/QGIS.rasters/2022_12_20_QGIS.tif")
dat <- ndviFUN(dat)
extent(dat)
plot(dat) 
con_stack <- stack(con_stack, dat)
date.list <- c(date.list, as.Date("2022/12/20"))

dat <- brick("data/QGIS.rasters/2022_12_26_QGIS.tif")
dat <- ndviFUN(dat)
extent(dat)
plot(dat) 
con_stack <- stack(con_stack, dat)
date.list <- c(date.list, as.Date("2022/12/26"))

dat <- brick("data/QGIS.rasters/2023_01_16_QGIS.tif")
dat <- ndviFUN(dat)
extent(dat)
plot(dat) 
con_stack <- stack(con_stack, dat)
date.list <- c(date.list, as.Date("2023/01/16"))

dat <- brick("data/QGIS.rasters/2023_01_26_QGIS.tif")
dat <- ndviFUN(dat)
extent(dat)
plot(dat) 
con_stack <- stack(con_stack, dat)
date.list <- c(date.list, as.Date("2023/01/26"))

dat <- brick("data/QGIS.rasters/2023_01_30_QGIS.tif")
dat <- ndviFUN(dat)
extent(dat)
plot(dat) 
con_stack <- stack(con_stack, dat)
date.list <- c(date.list, as.Date("2023/01/30"))

dat <- brick("data/QGIS.rasters/2023_02_06_QGIS.tif")
dat <- ndviFUN(dat)
extent(dat)
plot(dat) 
con_stack <- stack(con_stack, dat)
date.list <- c(date.list, as.Date("2023/02/06"))

dat <- brick("data/QGIS.rasters/2023_02_09_QGIS.tif")
dat <- ndviFUN(dat)
extent(dat)
plot(dat) 
con_stack <- stack(con_stack, dat)
date.list <- c(date.list, as.Date("2023/02/09"))

dat <- brick("data/QGIS.rasters/2023_02_23_QGIS.tif")
dat <- ndviFUN(dat)
extent(dat)
plot(dat) 
con_stack <- stack(con_stack, dat)
date.list <- c(date.list, as.Date("2023/02/23"))

dat <- brick("data/QGIS.rasters/2023_03_23_QGIS.tif")
dat <- ndviFUN(dat)
extent(dat)
plot(dat) 
con_stack <- stack(con_stack, dat)
date.list <- c(date.list, as.Date("2023/03/23"))

dat <- brick("data/QGIS.rasters/2023_03_25_QGIS.tif")
dat <- ndviFUN(dat)
extent(dat)
plot(dat) 
con_stack <- stack(con_stack, dat)
date.list <- c(date.list, as.Date("2023/03/25"))

dat <- brick("data/QGIS.rasters/2023_04_04_QGIS.tif")
dat <- ndviFUN(dat)
extent(dat)
plot(dat) 
con_stack <- stack(con_stack, dat)
date.list <- c(date.list, as.Date("2023/04/04"))

# dat <- brick("data/QGIS.rasters/2023_04_08_QGIS.tif")
# dat <- ndviFUN(dat)
# extent(dat)
# plot(dat) # shit
# con_stack <- stack(con_stack, dat)
# date.list <- c(date.list, as.Date("2023/04/08"))

dat <- brick("data/QGIS.rasters/2023_04_15_QGIS.tif")
dat <- ndviFUN(dat)
extent(dat)
plot(dat) 
con_stack <- stack(con_stack, dat)
date.list <- c(date.list, as.Date("2023/04/15"))

dat <- brick("data/QGIS.rasters/2023_04_18_QGIS.tif")
dat <- ndviFUN(dat)
extent(dat)
plot(dat) 
con_stack <- stack(con_stack, dat)
date.list <- c(date.list, as.Date("2023/04/18"))

dat <- brick("data/QGIS.rasters/2023_04_20_QGIS.tif")
dat <- ndviFUN(dat)
extent(dat)
plot(dat) 
con_stack <- stack(con_stack, dat)
date.list <- c(date.list, as.Date("2023/04/20"))

dat <- brick("data/QGIS.rasters/2023_04_26_QGIS.tif")
dat <- ndviFUN(dat)
extent(dat)
plot(dat) 
con_stack <- stack(con_stack, dat)
date.list <- c(date.list, as.Date("2023/04/26"))

dat <- brick("data/QGIS.rasters/2023_05_13_QGIS.tif")
dat <- ndviFUN(dat)
extent(dat)
plot(dat) 
con_stack <- stack(con_stack, dat)
date.list <- c(date.list, as.Date("2023/05/13"))

dat <- brick("data/QGIS.rasters/2023_05_15_QGIS.tif")
dat <- ndviFUN(dat)
extent(dat)
plot(dat) 
con_stack <- stack(con_stack, dat)
date.list <- c(date.list, as.Date("2023/05/15"))

dat <- brick("data/QGIS.rasters/2023_05_17_QGIS.tif")
dat <- ndviFUN(dat)
extent(dat)
plot(dat) 
con_stack <- stack(con_stack, dat)
date.list <- c(date.list, as.Date("2023/05/17"))

dat <- brick("data/QGIS.rasters/2023_05_20_QGIS.tif")
dat <- ndviFUN(dat)
extent(dat)
plot(dat) 
con_stack <- stack(con_stack, dat)
date.list <- c(date.list, as.Date("2023/05/20"))

dat <- brick("data/QGIS.rasters/2023_05_24_QGIS.tif")
dat <- ndviFUN(dat)
extent(dat)
plot(dat) 
con_stack <- stack(con_stack, dat)
date.list <- c(date.list, as.Date("2023/05/24"))

dat <- brick("data/QGIS.rasters/2023_05_27_QGIS.tif")
dat <- ndviFUN(dat)
extent(dat)
plot(dat) 
con_stack <- stack(con_stack, dat)
date.list <- c(date.list, as.Date("2023/05/27"))

dat <- brick("data/QGIS.rasters/2023_06_02_QGIS.tif")
dat <- ndviFUN(dat)
extent(dat)
plot(dat) 
con_stack <- stack(con_stack, dat)
date.list <- c(date.list, as.Date("2023/06/02"))

dat <- brick("data/QGIS.rasters/2023_06_08_QGIS.tif")
dat <- ndviFUN(dat)
extent(dat)
plot(dat) 
con_stack <- stack(con_stack, dat)
date.list <- c(date.list, as.Date("2023/06/08"))

dat <- brick("data/QGIS.rasters/2023_06_10_QGIS.tif")
dat <- ndviFUN(dat)
extent(dat)
plot(dat) 
con_stack <- stack(con_stack, dat)
date.list <- c(date.list, as.Date("2023/06/10"))

dat <- brick("data/QGIS.rasters/2023_06_12_QGIS.tif")
dat <- ndviFUN(dat)
extent(dat)
plot(dat) 
con_stack <- stack(con_stack, dat)
date.list <- c(date.list, as.Date("2023/06/12"))

dat <- brick("data/QGIS.rasters/2023_06_14_QGIS.tif")
dat <- ndviFUN(dat)
extent(dat)
plot(dat) 
con_stack <- stack(con_stack, dat)
date.list <- c(date.list, as.Date("2023/06/14"))

# dat <- brick("data/QGIS.rasters/2023_06_16_QGIS.tif")
# dat <- ndviFUN(dat)
# extent(dat)
# plot(dat) # shit
# con_stack <- stack(con_stack, dat)
# date.list <- c(date.list, as.Date("2023/06/16"))

dat <- brick("data/QGIS.rasters/2023_06_21_QGIS.tif")
dat <- ndviFUN(dat)
extent(dat)
plot(dat) 
con_stack <- stack(con_stack, dat)
date.list <- c(date.list, as.Date("2023/06/21"))

dat <- brick("data/QGIS.rasters/2023_06_26_QGIS.tif")
dat <- ndviFUN(dat)
extent(dat)
plot(dat) 
con_stack <- stack(con_stack, dat)
date.list <- c(date.list, as.Date("2023/06/26"))

# calculate mean NDVI for each raster
avg_con_stack <- cellStats(con_stack,mean)

# convert output array to data.frame
avg_con_stack <- as.data.frame(avg_con_stack)

avg_con_stack$Treatment <- "Conventional"

avg_con_stack$Date <- date.list

colnames(avg_con_stack) <- c("NDVI", "Treatment", "Date")


# plot NDVI
ggplot(avg_con_stack, aes(Date, NDVI, color = Treatment), na.rm=TRUE) +
  geom_point(size = 2) + 
  geom_line(size = 1) +
  ggtitle("NDVI - Harper Adams Conservation Agriculture Experiment") +
  xlab("Date") + ylab("Mean NDVI") +
  theme(text = element_text(size=10))




### rasters - CONSERVATION ####

my_shp <- CA_shp

# dat <- brick("data/QGIS.rasters/2022_03_05_QGIS.tif")
# ndvi1 <- ndviFUN(dat)
# extent(ndvi1) # wrong extent
# plot(ndvi1) # not working 

dat <- brick("data/QGIS.rasters/2022_03_08_QGIS.tif")
dat <- ndviFUN(dat)
extent(dat)
plot(dat)
ca_stack <- dat
date.list <- as.Date("2022/03/08")

dat <- brick("data/QGIS.rasters/2022_03_19_QGIS.tif")
dat <- ndviFUN(dat)
extent(dat)
plot(dat)
ca_stack <- stack(ca_stack, dat)
date.list <- c(date.list, as.Date("2022/03/19"))

dat <- brick("data/QGIS.rasters/2022_03_23_QGIS.tif")
dat <- ndviFUN(dat)
extent(dat)
plot(dat)
ca_stack <- stack(ca_stack, dat)
date.list <- c(date.list, as.Date("2022/03/23"))


# dat <- brick("data/QGIS.rasters/2022_03_28_QGIS.tif")
# dat <- ndviFUN(dat)
# extent(dat)
# plot(dat) # awful
# ca_stack <- stack(ca_stack, dat)

# dat <- brick("data/QGIS.rasters/2022_04_04_QGIS.tif")
# dat <- ndviFUN(dat)
# extent(dat) # wrong extent
# extent(dat) <- extent(extent.temp) # cavert extent
# extent(dat) # correct extent
# plot(dat)
# ca_stack <- stack(ca_stack, dat)
# date.list <- c(date.list, "2022_04_04")


dat <- brick("data/QGIS.rasters/2022_04_10_QGIS.tif")
dat <- ndviFUN(dat)
extent(dat)
plot(dat)
ca_stack <- stack(ca_stack, dat)
date.list <- c(date.list, as.Date("2022/04/10"))

dat <- brick("data/QGIS.rasters/2022_04_20_QGIS.tif")
dat <- ndviFUN(dat)
extent(dat)
plot(dat)
ca_stack <- stack(ca_stack, dat)
date.list <- c(date.list, as.Date("2022/02/20"))

dat <- brick("data/QGIS.rasters/2022_06_15_QGIS.tif")
dat <- ndviFUN(dat)
extent(dat)
plot(dat)
ca_stack <- stack(ca_stack, dat)
date.list <- c(date.list, as.Date("2022/06/15"))


dat <- brick("data/QGIS.rasters/2022_06_21_QGIS.tif")
dat <- ndviFUN(dat)
extent(dat)
plot(dat)
ca_stack <- stack(ca_stack, dat)
date.list <- c(date.list, as.Date("2022/06/21"))

dat <- brick("data/QGIS.rasters/2022_07_09_QGIS.tif")
dat <- ndviFUN(dat)
extent(dat)
plot(dat)
ca_stack <- stack(ca_stack, dat)
date.list <- c(date.list, as.Date("2022/07/09"))

dat <- brick("data/QGIS.rasters/2022_07_16_QGIS.tif")
dat <- ndviFUN(dat)
extent(dat)
plot(dat)
ca_stack <- stack(ca_stack, dat)
date.list <- c(date.list, as.Date("2022/07/16"))

dat <- brick("data/QGIS.rasters/2022_07_19_QGIS.tif")
dat <- ndviFUN(dat)
extent(dat)
plot(dat)
ca_stack <- stack(ca_stack, dat)
date.list <- c(date.list, as.Date("2022/07/19"))

dat <- brick("data/QGIS.rasters/2022_08_09_QGIS.tif")
dat <- ndviFUN(dat)
extent(dat)
plot(dat)
ca_stack <- stack(ca_stack, dat)
date.list <- c(date.list, as.Date("2022/08/09"))

dat <- brick("data/QGIS.rasters/2022_08_12_QGIS.tif")
dat <- ndviFUN(dat)
extent(dat)
plot(dat)
ca_stack <- stack(ca_stack, dat)
date.list <- c(date.list, as.Date("2022/08/12"))

dat <- brick("data/QGIS.rasters/2022_08_14_QGIS.tif")
dat <- ndviFUN(dat)
extent(dat)
plot(dat)
ca_stack <- stack(ca_stack, dat)
date.list <- c(date.list, as.Date("2022/08/14"))

dat <- brick("data/QGIS.rasters/2022_08_28_QGIS.tif")
dat <- ndviFUN(dat)
extent(dat)
plot(dat) # not great
ca_stack <- stack(ca_stack, dat)
date.list <- c(date.list, as.Date("2022/08/28"))

dat <- brick("data/QGIS.rasters/2022_09_17_QGIS.tif")
dat <- ndviFUN(dat)
extent(dat)
plot(dat)
ca_stack <- stack(ca_stack, dat)
date.list <- c(date.list, as.Date("2022/09/17"))

dat <- brick("data/QGIS.rasters/2022_10_08_QGIS.tif")
dat <- ndviFUN(dat)
extent(dat)
plot(dat) # not great
ca_stack <- stack(ca_stack, dat)
date.list <- c(date.list, as.Date("2022/10/08"))

dat <- brick("data/QGIS.rasters/2022_10_11_QGIS.tif")
dat <- ndviFUN(dat)
extent(dat)
plot(dat) 
ca_stack <- stack(ca_stack, dat)
date.list <- c(date.list, as.Date("2022/10/11"))

dat <- brick("data/QGIS.rasters/2022_10_13_QGIS.tif")
dat <- ndviFUN(dat)
extent(dat)
plot(dat) 
ca_stack <- stack(ca_stack, dat)
date.list <- c(date.list, as.Date("2022/10/13"))

dat <- brick("data/QGIS.rasters/2022_10_17_QGIS.tif")
dat <- ndviFUN(dat)
extent(dat)
plot(dat) 
ca_stack <- stack(ca_stack, dat)
date.list <- c(date.list, as.Date("2022/10/17"))

dat <- brick("data/QGIS.rasters/2022_10_18_QGIS.tif")
dat <- ndviFUN(dat)
extent(dat)
plot(dat) 
ca_stack <- stack(ca_stack, dat)
date.list <- c(date.list, as.Date("2022/10/18"))

dat <- brick("data/QGIS.rasters/2022_10_25_QGIS.tif")
dat <- ndviFUN(dat)
extent(dat)
plot(dat) 
ca_stack <- stack(ca_stack, dat)
date.list <- c(date.list, as.Date("2022/10/25"))

dat <- brick("data/QGIS.rasters/2022_10_28_QGIS.tif")
dat <- ndviFUN(dat)
extent(dat)
plot(dat) 
ca_stack <- stack(ca_stack, dat)
date.list <- c(date.list, as.Date("2022/10/28"))

dat <- brick("data/QGIS.rasters/2022_11_03_QGIS.tif")
dat <- ndviFUN(dat)
extent(dat)
plot(dat) 
ca_stack <- stack(ca_stack, dat)
date.list <- c(date.list, as.Date("2022/11/03"))

dat <- brick("data/QGIS.rasters/2022_11_19_QGIS.tif")
dat <- ndviFUN(dat)
extent(dat)
plot(dat) 
ca_stack <- stack(ca_stack, dat)
date.list <- c(date.list, as.Date("2022/11/19"))

dat <- brick("data/QGIS.rasters/2022_11_25_QGIS.tif")
dat <- ndviFUN(dat)
extent(dat)
plot(dat) 
ca_stack <- stack(ca_stack, dat)
date.list <- c(date.list, as.Date("2022/11/25"))

dat <- brick("data/QGIS.rasters/2022_12_03_QGIS.tif")
dat <- ndviFUN(dat)
extent(dat)
plot(dat) 
ca_stack <- stack(ca_stack, dat)
date.list <- c(date.list, as.Date("2022/12/03"))

dat <- brick("data/QGIS.rasters/2022_12_07_QGIS.tif")
dat <- ndviFUN(dat)
extent(dat)
plot(dat) 
ca_stack <- stack(ca_stack, dat)
date.list <- c(date.list, as.Date("2022/12/07"))

dat <- brick("data/QGIS.rasters/2022_12_15_QGIS.tif")
dat <- ndviFUN(dat)
extent(dat)
plot(dat) 
ca_stack <- stack(ca_stack, dat)
date.list <- c(date.list, as.Date("2022/12/15"))

dat <- brick("data/QGIS.rasters/2022_12_20_QGIS.tif")
dat <- ndviFUN(dat)
extent(dat)
plot(dat) 
ca_stack <- stack(ca_stack, dat)
date.list <- c(date.list, as.Date("2022/12/20"))

dat <- brick("data/QGIS.rasters/2022_12_26_QGIS.tif")
dat <- ndviFUN(dat)
extent(dat)
plot(dat) 
ca_stack <- stack(ca_stack, dat)
date.list <- c(date.list, as.Date("2022/12/26"))

dat <- brick("data/QGIS.rasters/2023_01_16_QGIS.tif")
dat <- ndviFUN(dat)
extent(dat)
plot(dat) 
ca_stack <- stack(ca_stack, dat)
date.list <- c(date.list, as.Date("2023/01/16"))

dat <- brick("data/QGIS.rasters/2023_01_26_QGIS.tif")
dat <- ndviFUN(dat)
extent(dat)
plot(dat) 
ca_stack <- stack(ca_stack, dat)
date.list <- c(date.list, as.Date("2023/01/26"))

dat <- brick("data/QGIS.rasters/2023_01_30_QGIS.tif")
dat <- ndviFUN(dat)
extent(dat)
plot(dat) 
ca_stack <- stack(ca_stack, dat)
date.list <- c(date.list, as.Date("2023/01/30"))

dat <- brick("data/QGIS.rasters/2023_02_06_QGIS.tif")
dat <- ndviFUN(dat)
extent(dat)
plot(dat) 
ca_stack <- stack(ca_stack, dat)
date.list <- c(date.list, as.Date("2023/02/06"))

dat <- brick("data/QGIS.rasters/2023_02_09_QGIS.tif")
dat <- ndviFUN(dat)
extent(dat)
plot(dat) 
ca_stack <- stack(ca_stack, dat)
date.list <- c(date.list, as.Date("2023/02/09"))

dat <- brick("data/QGIS.rasters/2023_02_23_QGIS.tif")
dat <- ndviFUN(dat)
extent(dat)
plot(dat) 
ca_stack <- stack(ca_stack, dat)
date.list <- c(date.list, as.Date("2023/02/23"))

dat <- brick("data/QGIS.rasters/2023_03_23_QGIS.tif")
dat <- ndviFUN(dat)
extent(dat)
plot(dat) 
ca_stack <- stack(ca_stack, dat)
date.list <- c(date.list, as.Date("2023/03/23"))

dat <- brick("data/QGIS.rasters/2023_03_25_QGIS.tif")
dat <- ndviFUN(dat)
extent(dat)
plot(dat) 
ca_stack <- stack(ca_stack, dat)
date.list <- c(date.list, as.Date("2023/03/25"))

dat <- brick("data/QGIS.rasters/2023_04_04_QGIS.tif")
dat <- ndviFUN(dat)
extent(dat)
plot(dat) 
ca_stack <- stack(ca_stack, dat)
date.list <- c(date.list, as.Date("2023/04/04"))

# dat <- brick("data/QGIS.rasters/2023_04_08_QGIS.tif")
# dat <- ndviFUN(dat)
# extent(dat)
# plot(dat) # shit
# ca_stack <- stack(ca_stack, dat)
# date.list <- c(date.list, as.Date("2023/04/08"))

dat <- brick("data/QGIS.rasters/2023_04_15_QGIS.tif")
dat <- ndviFUN(dat)
extent(dat)
plot(dat) 
ca_stack <- stack(ca_stack, dat)
date.list <- c(date.list, as.Date("2023/04/15"))

dat <- brick("data/QGIS.rasters/2023_04_18_QGIS.tif")
dat <- ndviFUN(dat)
extent(dat)
plot(dat) 
ca_stack <- stack(ca_stack, dat)
date.list <- c(date.list, as.Date("2023/04/18"))

dat <- brick("data/QGIS.rasters/2023_04_20_QGIS.tif")
dat <- ndviFUN(dat)
extent(dat)
plot(dat) 
ca_stack <- stack(ca_stack, dat)
date.list <- c(date.list, as.Date("2023/04/20"))

dat <- brick("data/QGIS.rasters/2023_04_26_QGIS.tif")
dat <- ndviFUN(dat)
extent(dat)
plot(dat) 
ca_stack <- stack(ca_stack, dat)
date.list <- c(date.list, as.Date("2023/04/26"))

dat <- brick("data/QGIS.rasters/2023_05_13_QGIS.tif")
dat <- ndviFUN(dat)
extent(dat)
plot(dat) 
ca_stack <- stack(ca_stack, dat)
date.list <- c(date.list, as.Date("2023/05/13"))

dat <- brick("data/QGIS.rasters/2023_05_15_QGIS.tif")
dat <- ndviFUN(dat)
extent(dat)
plot(dat) 
ca_stack <- stack(ca_stack, dat)
date.list <- c(date.list, as.Date("2023/05/15"))

dat <- brick("data/QGIS.rasters/2023_05_17_QGIS.tif")
dat <- ndviFUN(dat)
extent(dat)
plot(dat) 
ca_stack <- stack(ca_stack, dat)
date.list <- c(date.list, as.Date("2023/05/17"))

dat <- brick("data/QGIS.rasters/2023_05_20_QGIS.tif")
dat <- ndviFUN(dat)
extent(dat)
plot(dat) 
ca_stack <- stack(ca_stack, dat)
date.list <- c(date.list, as.Date("2023/05/20"))

dat <- brick("data/QGIS.rasters/2023_05_24_QGIS.tif")
dat <- ndviFUN(dat)
extent(dat)
plot(dat) 
ca_stack <- stack(ca_stack, dat)
date.list <- c(date.list, as.Date("2023/05/24"))

dat <- brick("data/QGIS.rasters/2023_05_27_QGIS.tif")
dat <- ndviFUN(dat)
extent(dat)
plot(dat) 
ca_stack <- stack(ca_stack, dat)
date.list <- c(date.list, as.Date("2023/05/27"))

dat <- brick("data/QGIS.rasters/2023_06_02_QGIS.tif")
dat <- ndviFUN(dat)
extent(dat)
plot(dat) 
ca_stack <- stack(ca_stack, dat)
date.list <- c(date.list, as.Date("2023/06/02"))

dat <- brick("data/QGIS.rasters/2023_06_08_QGIS.tif")
dat <- ndviFUN(dat)
extent(dat)
plot(dat) 
ca_stack <- stack(ca_stack, dat)
date.list <- c(date.list, as.Date("2023/06/08"))

dat <- brick("data/QGIS.rasters/2023_06_10_QGIS.tif")
dat <- ndviFUN(dat)
extent(dat)
plot(dat) 
ca_stack <- stack(ca_stack, dat)
date.list <- c(date.list, as.Date("2023/06/10"))

dat <- brick("data/QGIS.rasters/2023_06_12_QGIS.tif")
dat <- ndviFUN(dat)
extent(dat)
plot(dat) 
ca_stack <- stack(ca_stack, dat)
date.list <- c(date.list, as.Date("2023/06/12"))

dat <- brick("data/QGIS.rasters/2023_06_14_QGIS.tif")
dat <- ndviFUN(dat)
extent(dat)
plot(dat) 
ca_stack <- stack(ca_stack, dat)
date.list <- c(date.list, as.Date("2023/06/14"))

# dat <- brick("data/QGIS.rasters/2023_06_16_QGIS.tif")
# dat <- ndviFUN(dat)
# extent(dat)
# plot(dat) # shit
# ca_stack <- stack(ca_stack, dat)
# date.list <- c(date.list, as.Date("2023/06/16"))

dat <- brick("data/QGIS.rasters/2023_06_21_QGIS.tif")
dat <- ndviFUN(dat)
extent(dat)
plot(dat) 
ca_stack <- stack(ca_stack, dat)
date.list <- c(date.list, as.Date("2023/06/21"))

dat <- brick("data/QGIS.rasters/2023_06_26_QGIS.tif")
dat <- ndviFUN(dat)
extent(dat)
plot(dat) 
ca_stack <- stack(ca_stack, dat)
date.list <- c(date.list, as.Date("2023/06/26"))

# calculate mean NDVI for each raster
avg_ca_stack <- cellStats(ca_stack,mean)

# cavert output array to data.frame
avg_ca_stack <- as.data.frame(avg_ca_stack)

avg_ca_stack$Treatment <- "Conservation"

avg_ca_stack$Date <- date.list

colnames(avg_ca_stack) <- c("NDVI", "Treatment", "Date")


# plot NDVI
ggplot(avg_ca_stack, aes(Date, NDVI, color = Treatment), na.rm=TRUE) +
  geom_point(size=3) + 
  geom_line(size = 1) +
  ggtitle("NDVI - Harper Adams Conservation Agriculture Experiment") +
  xlab("Date") + ylab("Mean NDVI") +
  theme(text = element_text(size=10))



## bind df's together ####


stack.all <- rbind(avg_con_stack, avg_ca_stack)

stack.all$Treatment <- as.factor(stack.all$Treatment)



## plot ####


#open png for file save and define size and resolution
png(paste("plots/", "line.plot.2", ".png", ".png", sep=""),
    width=1200, height=1000, res=150)

ggplot(data = stack.all, 
       aes(x = Date, 
           y = NDVI, 
           group = Treatment, 
           color = Treatment)) +
  geom_line(position=position_jitter(w=1, h=0)) + 
  xlab("Date") +
  ylab("NDVI") + 
  scale_x_date(date_labels = "%m-%Y", 
               date_breaks = "1 month", 
               date_minor_breaks = "1 month") +
  theme() +
  geom_point(size = 0.5, 
             color = "black",
             position=position_jitter(w=1, h=0)) + 
  ggtitle(label = "Normalized Difference Vegetation Index (NDVI) Time Series Plot", 
          subtitle = "March 2021 - June 2023") + 
  geom_vline(xintercept = as.numeric(as.Date(c("2022-03-15", "2022-10-15"))), 
             linetype="dotted", 
            color = "red", 
            size=1) +
  annotate(geom = "text", x = as.Date(c("2022-03-15", "2022-10-15")),
           y = 0.895, 
           label = "Cultivation",
           fontface = "bold",
           color = "red") + 
  theme(legend.position="bottom",
        axis.text.x=element_text(angle=60, hjust=1), 
        panel.background = element_rect(fill = "white")) + 
  scale_color_manual(values=c("turquoise3", "tomato2"))

           
dev.off() # turn dev off

## write to CSV ####

# create new df to prevent changes to file
NDVI_toWrite <- stack.all

# drop the row.names column 
row.names(NDVI_toWrite) <- NULL

# create a .csv of mean NDVI values being sure to give descriptive name
# write.csv(DateFrameName, file="NewFileName")
write.csv(NDVI_toWrite, file = "data/prcoessed.data/ndvi.data.csv")


