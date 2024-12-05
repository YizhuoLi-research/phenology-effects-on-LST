###### 0. Load Packages ####
# Purpose: Load necessary libraries for raster processing, data manipulation, and visualization.
library(terra)
library(tidyverse)
library(raster)

# Set the working directory
setwd("D:/VegetationImpact")

## Extract common pixels for each year with matching PHE (Phenological Events) and LST_diff images (9 years) 
##################   01 Extract Common Pixel Raster Images (9 years)  ################################# 

# Define the years to process
years <- 2013:2021

# Define the data directory path
input_dir  <- "./01 Download/03 PhenologicalEvents_download/EA_PHE/EA_PHE_merged/"
output_dir <- "./EA_Results/0.phe_DOY/0common_pixel/"
dir.create(output_dir, showWarnings = FALSE)

# Loop through each year to process the data
for (year in years) {
  
  # Dynamically construct file paths for phenological event rasters
  phe1 <- rast(paste0(input_dir, year, "/", year, "-Onset_Greenness_Increas_merged.tif"))  # SOS
  phe2 <- rast(paste0(input_dir, year, "/", year, "-Date_Mid_Greenup_Phase__merged.tif"))  # MGP
  phe3 <- rast(paste0(input_dir, year, "/", year, "-Onset_Greenness_Maximum_merged.tif"))  # GMO
  phe4 <- rast(paste0(input_dir, year, "/", year, "-Onset_Greenness_Decreas_merged.tif"))  # GDO
  phe5 <- rast(paste0(input_dir, year, "/", year, "-Date_Mid_Senescence_Pha_merged.tif"))  # MSP
  phe6 <- rast(paste0(input_dir, year, "/", year, "-Onset_Greenness_Minimum_merged.tif"))  # EOS
  
  # Load LST diff raster (temperature effect) for the current year
  # Identify common pixels between PHE and LST-diff rasters
  aa <- rast(paste0("./EA_Results/0.diff_result/0common_pixel/average_diff_1_", year, ".tif"))  
  sample_diff <- aa
  sample_diff[is.finite(sample_diff)] <- 1  # Assign 1 to valid (finite) pixels
  
  # Create a mask using the PHE rasters
  sample <- phe1
  sample[is.finite(sample)] <- 1  # Assign 1 to valid (finite) pixels
  
  # Construct the common pixel mask by multiplying PHE and LST-diff rasters
  common_mask <- sample[[1]] * sample_diff[[1]]
  
  # Multiply all PHE rasters by the common pixel mask to retain only common pixels
  phe1 <- phe1 * common_mask
  phe2 <- phe2 * common_mask
  phe3 <- phe3 * common_mask
  phe4 <- phe4 * common_mask
  phe5 <- phe5 * common_mask
  phe6 <- phe6 * common_mask
  
  # Print the number of non-NA pixels in the LST-diff raster
  non_empty_pixels <- sum(!is.na(values(aa)))
  print(paste("Year:", year, "- Number of non-empty pixels in LST-diff:", non_empty_pixels))
  
  # Print the number of non-NA pixels in the PHE raster for EOS (phe6)
  non_empty_pixels <- sum(!is.na(values(phe1)))
  print(paste("Year:", year, "- Number of non-empty pixels in phe6:", non_empty_pixels))
  
  ###### 3. Write the results #######
  # Save the processed rasters (PHE event DOY for the common pixels) for each year
  writeRaster(phe1, paste0(output_dir, "phe1_DOY_", year, ".tif"), overwrite = TRUE)
  writeRaster(phe2, paste0(output_dir, "phe2_DOY_", year, ".tif"), overwrite = TRUE)
  writeRaster(phe3, paste0(output_dir, "phe3_DOY_", year, ".tif"), overwrite = TRUE)
  writeRaster(phe4, paste0(output_dir, "phe4_DOY_", year, ".tif"), overwrite = TRUE)
  writeRaster(phe5, paste0(output_dir, "phe5_DOY_", year, ".tif"), overwrite = TRUE)
  writeRaster(phe6, paste0(output_dir, "phe6_DOY_", year, ".tif"), overwrite = TRUE)
}
