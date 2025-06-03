###### 0. Load packages ####
library(terra)
library(tidyverse)
library(raster)

setwd("D:/VegetationImpact")

################## 01 Merge land cover raster data for Europe and North America ##########################################

# Load raster data for two different geographic regions
r_EA <- rast("./EA_Results/EA_LandCover_30km.tif")
r_NA <- rast("./NA_Results/NA_LandCover_30km.tif")

# Ensure that the resolution and coordinate reference system (CRS) of the two rasters are consistent
# You can use the resample() function to match resolution
# You can use the project() function to align coordinate systems

# Merge the raster data
r_landcover <- merge(r_EA, r_NA)

# Plot each raster to check visually
plot(r_EA)
plot(r_NA)
plot(r_landcover)

# Save the merged raster to disk
writeRaster(r_landcover, filename = "./EA+NA_Results/EA+NA_LandCover_30km.tif",
            overwrite = TRUE)