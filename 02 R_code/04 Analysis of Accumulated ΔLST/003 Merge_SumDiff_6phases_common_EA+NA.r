###### 0. Load packages and data ####
library(terra)
library(tidyverse)
library(raster)
library(ggplot2)
library(patchwork)
library(scales)

setwd("D:/VegetationImpact")

################     0. Obtain the average image of 9 years difference (6 PHE images)   ####################
#####################  Calculate the average for the NA region

# Get the list of .tif files in the NA region
file_list <- list.files("./NA_Results/0.sum_diff/0common_pixel", pattern = "\\.tif$", 
                        full.names = TRUE)

# Extract the first 11 characters from the file names for grouping (e.g., SOS -- 9 years)
file_groups <- substr(basename(file_list), 1, 11)  

# Get unique group names
file_groups_name <- unique(file_groups)

# Create a new folder to save the averaged images
new_folder <- "./NA_Results/0.sum_diff/NA_SUM_9yearAverge"
dir.create(new_folder, showWarnings = FALSE)

# Calculate the average for each group and save the results
for (group in file_groups_name) {
  # Filter the files belonging to the current group
  current_files <- file_list[file_groups == group]
  
  # Read the images of the current group and calculate the average
  current_images <- lapply(current_files, raster::raster)
  average_image <- raster::mean(stack(current_images), na.rm = TRUE)
  
  # Generate the new file name and save the averaged image
  new_file_name <- file.path(new_folder, paste0(group, ".tif"))
  raster::writeRaster(average_image, filename = new_file_name, overwrite = TRUE)
}


#####################  Calculate the average for the EA region

# Get the list of .tif files in the EA region
file_list <- list.files("./EA_Results/0.sum_diff/0common_pixel", pattern = "\\.tif$", 
                        full.names = TRUE)

# Extract the first 11 characters from the file names for grouping (e.g., SOS -- 9 years)
file_groups <- substr(basename(file_list), 1, 11)  

# Get unique group names
file_groups_name <- unique(file_groups)

# Create a new folder to save the averaged images
new_folder <- "./EA_Results/0.sum_diff/EA_SUM_9yearAverge"
dir.create(new_folder, showWarnings = FALSE)

# Calculate the average for each group and save the results
for (group in file_groups_name) {
  # Filter the files belonging to the current group
  current_files <- file_list[file_groups == group]
  
  # Read the images of the current group and calculate the average
  current_images <- lapply(current_files, raster::raster)
  average_image <- raster::mean(stack(current_images), na.rm = TRUE)
  
  # Generate the new file name and save the averaged image
  new_file_name <- file.path(new_folder, paste0(group, ".tif"))
  raster::writeRaster(average_image, filename = new_file_name, overwrite = TRUE)
}



################    ## 0. Merge the average difference images of 9 years (6 PHE images)   ####################

# Create the output directory for merged files
dir.create(output_path <- "./EA+NA_Results/merged_sum_diff_average", showWarnings = FALSE, recursive = TRUE)

# Get the list of averaged files for both NA and EA regions
NA_SUM_files <- list.files("./NA_Results/0.sum_diff/NA_SUM_9yearAverge", pattern = "\\.tif$", full.names = TRUE)
EA_SUM_files <- list.files("./EA_Results/0.sum_diff/EA_SUM_9yearAverge", pattern = "\\.tif$", full.names = TRUE)

# Check if the number of files is the same in both regions
if (length(NA_SUM_files) != length(EA_SUM_files)) {
  stop("The number of files does not match and cannot be paired.")
}


# Merge corresponding files one by one
for (i in seq_along(NA_SUM_files)) {
  # Load the NA and EA rasters
  NA_raster <- rast(NA_SUM_files[i])
  EA_raster <- rast(EA_SUM_files[i])
  
  # Use merge to combine the two rasters
  merged_raster <- merge(NA_raster, EA_raster, 
                         filename = file.path(output_path, paste0("merged_", basename(NA_SUM_files[i]))))
  
  # Print status message
  print(paste("Merged file:", basename(NA_SUM_files[i])))
}
