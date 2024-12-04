###### 0. Load Packages ####
library(terra)
library(tidyverse)
library(raster)

setwd("D:/VegetationImpact")

##################   01 Extract common pixel raster images (9 years)  #################################
####### Run this for different years, repeat 9 times * 2 (EA/NA)
####### 1 Read             ###2013/2014/2015/2016/2017/2018/2019/2020/2021

# Create output folder (if it does not exist)
new_folder <- "./EA_Results/0.actLSTmean_yr/0common_pixel/"
dir.create(new_folder, showWarnings = FALSE)

# Loop through the years 2013 to 2021
for (year in 2013:2021) {
  # Read the actLSTmean file
  actLSTmean <- rast(paste0("./EA_Results/0.actLSTmean_yr/", year, "_actLSTmean.tif"))
  
  # Read the file with common pixels for diff
  aa <- rast(paste0("./EA_Results/0.diff_result/0common_pixel/average_diff_1_", year, ".tif"))
  sample_diff <- aa
  sample_diff[is.finite(sample_diff)] <- 1
  
  # Get the pixels of actLSTmean
  sample <- actLSTmean
  sample[is.finite(sample)] <- 1
  
  # Retain only the intersecting pixels with the common region
  actLSTmean <- actLSTmean * sample[[1]] * sample_diff[[1]]
  
  # Calculate the number of non-NA pixels and print the result
  non_empty_pixels <- sum(!is.na(values(actLSTmean)))
  print(paste("Year:", year, "Number of non-empty pixels:", non_empty_pixels))
  
  # Write the result to the new folder
  output_filename <- paste0(new_folder, year, "_actLSTmean.tif")
  writeRaster(actLSTmean, output_filename, overwrite = TRUE)
}

