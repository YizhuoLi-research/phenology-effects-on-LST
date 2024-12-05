###### 0. Load Packages ####

# Purpose: Load necessary libraries for raster processing, data manipulation, and file handling.
# This code merges the PHE (Phenological Events) DOY (Day of Year) raster data for both Eurasia and 
# North America. It processes the data for each year and each phenological event,   then merges the
# corresponding raster files from both regions into a single output file, saving it in the appropriate directory.

library(terra)
library(tidyverse)
library(raster)

# Set the working directory
setwd("D:/VegetationImpact")

##################   01 Merge PHE_DOY Raster Data for Europe and North America (6 events)  #################################

# Define the types of PHE files to process
phe_events <- c("phe1_DOY_", "phe2_DOY_", "phe3_DOY_", "phe4_DOY_", "phe5_DOY_", "phe6_DOY_")

# Loop through each PHE file type
for (j in 1:length(phe_events)) {
  
  # Dynamically create the file list by matching each phe_x type in both NA and EA directories
  file_list1 <- list.files("./NA_Results/0.phe_DOY/0common_pixel/", 
                           pattern = paste0(phe_events[j], ".*\\.tif$"), 
                           full.names = TRUE)
  file_list11 <- list.files("./EA_Results/0.phe_DOY/0common_pixel/", 
                            pattern = paste0(phe_events[j], ".*\\.tif$"), 
                            full.names = TRUE)
  
  # Convert the file lists into raster objects
  rasters1 <- lapply(file_list1, rast)
  rasters11 <- lapply(file_list11, rast)
  
  # Dynamically create output folder names, remove the final underscore from phe_events[j] to form the folder name
  output_folder <- paste0("./EA+NA_Results/merge_Phe_DOY_years/merged_", sub("_$", "", phe_events[j]))
  
  # Check and create the output folder if it does not exist
  if (!dir.exists(output_folder)) {
    dir.create(output_folder, showWarnings = FALSE, recursive = TRUE)
  }
  
  # Loop through each raster file and merge them
  for (i in seq_along(rasters1)) {
    # Merge the rasters
    merged_raster <- merge(rasters1[[i]], rasters11[[i]])
    
    # Extract the year from the file name (assuming the last 4 characters represent the year)
    year <- substr(basename(file_list1[i]), nchar(basename(file_list1[i])) - 7, nchar(basename(file_list1[i])) - 4)
    
    # Define the output file path
    filename <- file.path(output_folder, paste0("EA+NA_merged_", phe_events[j], year, ".tif"))
    
    # Write the merged raster to the output file
    writeRaster(merged_raster, filename = filename, overwrite = TRUE)
  }
}
