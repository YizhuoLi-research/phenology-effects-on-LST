library(terra)
library(raster)
library(ggplot2)
library(dplyr)

setwd("D:/VegetationImpact")

##################   04 Merge diff_result raster data for Europe and America (6)  #################################
# Loop to process average_diff_1 to average_diff_6
for (j in 1:6) {
  
  # Create the corresponding file list, using dynamic pattern matching for each average_diff_x
  file_list1 <- list.files("./NA_Results/0.diff_result/0common_pixel/", 
                           pattern = paste0("average_diff_", j, ".*\\.tif$"), 
                           full.names = TRUE)
  file_list11 <- list.files("./EA_Results/0.diff_result/0common_pixel/", 
                            pattern = paste0("average_diff_", j, ".*\\.tif$"), 
                            full.names = TRUE)
  
  # Convert the file list to raster objects
  rasters1 <- lapply(file_list1, rast)
  rasters11 <- lapply(file_list11, rast)
  
  # Output folder path, dynamically create folder name
  output_folder <- paste0("./EA+NA_Results/merge_average_diff_years/merged_average_diff_", j)
  
  # Create the output folder if it doesn't exist
  if (!dir.exists(output_folder)) {
    dir.create(output_folder, showWarnings = FALSE, recursive = TRUE)
  }
  
  # Loop through each raster file and merge them
  for (i in seq_along(rasters1)) {
    # Merge the rasters
    merged_raster <- merge(rasters1[[i]], rasters11[[i]])
    
    # Extract the year (assuming the last 4 characters of the filename represent the year)
    year <- substr(basename(file_list1[i]), nchar(basename(file_list1[i])) - 7, nchar(basename(file_list1[i])) - 4)
    
    # Define the output file path
    filename <- file.path(output_folder, paste0("EA+NA_merged_average_diff_", j, "_", year, ".tif"))
    
    # Write the merged raster to the file
    writeRaster(merged_raster, filename = filename, overwrite = TRUE)
  }
}
