###### 0. Load Packages ####
library(terra)
library(tidyverse)
library(raster)

setwd("D:/VegetationImpact")

##################  01 Get the average LST image for each year  EA #####################################
# Get the paths of all folders
folder_path <- "./01 Download/02 LandSurfaceTemperature_download/EA_LST/" 
all_folders <- list.dirs(folder_path, full.names = TRUE, recursive = FALSE)
for (folder in all_folders) {
  file_list <- list.files(folder, pattern = "\\.tif$", full.names = TRUE)
  LST_year <- rast(file_list)     # Read images into an sds-multi-layer dataset object
  mean_LST_year <- mean(LST_year, na.rm = TRUE) # Calculate the average
  year <- basename(folder)        # Get the year
  output_folder <- paste0("./EA_Results/", "0.actLSTmean_yr")
  dir.create(output_folder, showWarnings = FALSE)
  output_file <- file.path(output_folder, paste0(year, "_actLSTmean.tif"))
  writeRaster(mean_LST_year, filename = output_file, overwrite = TRUE)
}
