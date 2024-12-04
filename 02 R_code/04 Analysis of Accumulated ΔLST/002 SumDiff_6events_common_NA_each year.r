###### 0. Load libraries ####
library(terra)
library(tidyverse)
library(raster)

setwd("D:/VegetationImpact")

# Extract pixels that contain data for all six phenophases each year
################## 01 Extract common pixels raster images (9 years) #################################

# Define the range of years
years <- 2013:2021

# Define data directory paths
input_dir  <- "./NA_Results/0.sum_diff/"
output_dir <- "./NA_Results/0.sum_diff/0common_pixel/"
dir.create(output_dir, showWarnings = FALSE)

# Loop to process data for each year
for (year in years) {
  ####### 1 Read TIF files for each year
  sum_diff_12 <- rast(paste0(input_dir, "sum_diff_12_", year, ".tif"))
  sum_diff_23 <- rast(paste0(input_dir, "sum_diff_23_", year, ".tif"))
  sum_diff_34 <- rast(paste0(input_dir, "sum_diff_34_", year, ".tif"))
  sum_diff_45 <- rast(paste0(input_dir, "sum_diff_45_", year, ".tif"))
  sum_diff_56 <- rast(paste0(input_dir, "sum_diff_56_", year, ".tif"))
  sum_diff_16 <- rast(paste0(input_dir, "sum_diff_16_", year, ".tif"))
  
  non_na_count2 <- sum(!is.na(values(sum_diff_12)))
  print(paste("Year:", year, "- Non-NA pixels in sum_diff_12:", non_na_count2))
  
  ## 2 Extract pixels common across all six sum_diff layers
  # Read raster for average difference within 5 days after each phenophase, used as a common pixel mask
  aa <- rast(paste0("./NA_Results/0diff_result/0common_pixel/average_diff_1_", year, ".tif"))  
  sample_diff <- aa
  sample_diff[is.finite(sample_diff)] <- 1
  
  # Create mask sample layers for each sum_diff
  sample <- sum_diff_12
  sample[is.finite(sample)] <- 1
  
  sample2 <- sum_diff_23
  sample2[is.finite(sample2)] <- 1
  
  sample3 <- sum_diff_34
  sample3[is.finite(sample3)] <- 1
  
  sample4 <- sum_diff_45
  sample4[is.finite(sample4)] <- 1
  
  sample5 <- sum_diff_56
  sample5[is.finite(sample5)] <- 1
  
  sample6 <- sum_diff_16
  sample6[is.finite(sample6)] <- 1
  
  # Multiply all rasters by the common pixel mask
  common_mask <- sample[[1]] * sample2[[1]] * sample3[[1]] * sample4[[1]] * sample5[[1]] * sample6[[1]] * sample_diff[[1]]
  
  # Apply the mask to each sum_diff data layer
  sum_diff_12 <- sum_diff_12 * common_mask
  sum_diff_23 <- sum_diff_23 * common_mask
  sum_diff_34 <- sum_diff_34 * common_mask
  sum_diff_45 <- sum_diff_45 * common_mask
  sum_diff_56 <- sum_diff_56 * common_mask
  sum_diff_16 <- sum_diff_16 * common_mask
  
  # non_empty_pixels <- sum(!is.na(values(sum_diff_12)))
  # print(paste("Year:", year, "- Non-empty pixel count:", non_empty_pixels))
  
  # Save the processed raster files
  writeRaster(sum_diff_12, paste0(output_dir, "sum_diff_12_", year, ".tif"), overwrite = TRUE)
  writeRaster(sum_diff_23, paste0(output_dir, "sum_diff_23_", year, ".tif"), overwrite = TRUE)
  writeRaster(sum_diff_34, paste0(output_dir, "sum_diff_34_", year, ".tif"), overwrite = TRUE)
  writeRaster(sum_diff_45, paste0(output_dir, "sum_diff_45_", year, ".tif"), overwrite = TRUE)
  writeRaster(sum_diff_56, paste0(output_dir, "sum_diff_56_", year, ".tif"), overwrite = TRUE)
  writeRaster(sum_diff_16, paste0(output_dir, "sum_diff_16_", year, ".tif"), overwrite = TRUE)
}
