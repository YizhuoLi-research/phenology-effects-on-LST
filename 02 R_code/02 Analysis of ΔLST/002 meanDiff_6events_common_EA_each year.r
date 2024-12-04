library(terra)
library(raster)
setwd("D:/VegetationImpact")

# Define the years to process
years <- 2013:2021

for (year in years) {
  
  ####### 1. Read ####
  average_diff_21 <- rast(paste0("./EA_Results/0.diff_result/average_diff_1_", year, ".tif"))
  average_diff_22 <- rast(paste0("./EA_Results/0.diff_result/average_diff_2_", year, ".tif"))
  average_diff_23 <- rast(paste0("./EA_Results/0.diff_result/average_diff_3_", year, ".tif"))
  average_diff_24 <- rast(paste0("./EA_Results/0.diff_result/average_diff_4_", year, ".tif"))
  average_diff_25 <- rast(paste0("./EA_Results/0.diff_result/average_diff_5_", year, ".tif"))
  average_diff_26 <- rast(paste0("./EA_Results/0.diff_result/average_diff_6_", year, ".tif"))
  
  non_na_count2 <- sum(!is.na(values(average_diff_21)))
  print(paste("Non-NA pixel count for year", year, ":", non_na_count2))
  
  ###### 2. Select pixels with all 6 diffs ####
  
  sample = average_diff_21      
  sample[is.finite(sample)] = 1
  
  sample2 = average_diff_22
  sample2[is.finite(sample2)] = 1
  
  sample3 = average_diff_23
  sample3[is.finite(sample3)] = 1
  
  sample4 = average_diff_24
  sample4[is.finite(sample4)] = 1
  
  sample5 = average_diff_25
  sample5[is.finite(sample5)] = 1
  
  sample6 = average_diff_26
  sample6[is.finite(sample6)] = 1
  
  # Multiply all rasters by the common pixel mask
  common_mask <- sample[[1]] * sample2[[1]] * sample3[[1]] * sample4[[1]] * sample5[[1]] * sample6[[1]]
  
  average_diff_21 <- average_diff_21 * common_mask
  average_diff_22 <- average_diff_22 * common_mask
  average_diff_23 <- average_diff_23 * common_mask
  average_diff_24 <- average_diff_24 * common_mask
  average_diff_25 <- average_diff_25 * common_mask
  average_diff_26 <- average_diff_26 * common_mask
  
  # Ensure the output directory exists
  output_dir <- "./EA_Results/0.diff_result/0common_pixel"
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  ###### 3. Write the results #######
  writeRaster(average_diff_21, paste0("./EA_Results/0.diff_result/0common_pixel/average_diff_1_", year, ".tif"), overwrite=TRUE)
  writeRaster(average_diff_22, paste0("./EA_Results/0.diff_result/0common_pixel/average_diff_2_", year, ".tif"), overwrite=TRUE)
  writeRaster(average_diff_23, paste0("./EA_Results/0.diff_result/0common_pixel/average_diff_3_", year, ".tif"), overwrite=TRUE)
  writeRaster(average_diff_24, paste0("./EA_Results/0.diff_result/0common_pixel/average_diff_4_", year, ".tif"), overwrite=TRUE)
  writeRaster(average_diff_25, paste0("./EA_Results/0.diff_result/0common_pixel/average_diff_5_", year, ".tif"), overwrite=TRUE)
  writeRaster(average_diff_26, paste0("./EA_Results/0.diff_result/0common_pixel/average_diff_6_", year, ".tif"), overwrite=TRUE)
  
  print(paste("Finished processing year", year))
}

