###### 0. Load packages ####
library(terra)
library(tidyverse)
library(raster)

setwd("D:/VegetationImpact")

##################   01 Merge sum_diff raster data for Europe and America (6)  #################################

# Define the sum_diff file types to be processed
sum_diff_types <- c("sum_diff_12", "sum_diff_23", "sum_diff_34", "sum_diff_45", "sum_diff_56", "sum_diff_16")

# Loop through each sum_diff file type
for (j in 1:length(sum_diff_types)) {
  
  # Create corresponding file lists by dynamically matching each sum_diff_x pattern
  file_list1 <- list.files("./NA_Results/0.sum_diff/0common_pixel/", 
                           pattern = paste0(sum_diff_types[j], ".*\\.tif$"), 
                           full.names = TRUE)
  file_list11 <- list.files("./EA_Results/0.sum_diff/0common_pixel/", 
                            pattern = paste0(sum_diff_types[j], ".*\\.tif$"), 
                            full.names = TRUE)
  
  # Convert the file lists into raster objects
  rasters1 <- lapply(file_list1, rast)
  rasters11 <- lapply(file_list11, rast)
  
  # Define the output folder path, dynamically creating the folder name
  output_folder <- paste0("./EA+NA_Results/merge_Sum_diff_years/merged_", sum_diff_types[j])
  
  # Create the output folder if it doesn't exist
  if (!dir.exists(output_folder)) {
    dir.create(output_folder, showWarnings = FALSE, recursive = TRUE)
  }
  
  # Loop through each raster file and merge them
  for (i in seq_along(rasters1)) {
    # Merge the rasters
    merged_raster <- merge(rasters1[[i]], rasters11[[i]])
    
    # Extract the year from the file name (assumes the last 4 characters represent the year)
    year <- substr(basename(file_list1[i]), nchar(basename(file_list1[i])) - 7, nchar(basename(file_list1[i])) - 4)
    
    # Define the output file path
    filename <- file.path(output_folder, paste0("EA+NA_merged_", sum_diff_types[j], "_", year, ".tif"))
    
    # Write the merged raster to the file
    writeRaster(merged_raster, filename = filename, overwrite = TRUE)
  }
}
