###### 0. Load Libraries ####
library(terra)  # A package for raster data manipulation
library(tidyverse)  # A package collection for data manipulation and visualization
library(raster)  # A package for raster operations
setwd("D:/VegetationImpact")

#####################################   01 Calculation   ############################################
# Objective description: 
# LST_diff <- rast(c(file_list1)) contains 9 layers representing 2013-2022, one file per year (first data group)
# LST_act  <- rast(c(file_list2)) contains 9 layers representing 2013-2022, one file per year (second data group)
# After aligning the common pixels, for each pixel, there are two sets of data: LST_diff and LST_act.
# For each pixel, we perform a linear regression of the two datasets (y ~ kx + b).
# This regression does not consider years, so we use the data from 2013-2021 for fitting.
# The result: A map showing the k values (slope of the regression).
###############################################################################################      

# Create a vector representing all average_diff groups
average_diff_groups <- 1:6

# Loop to process each group
for (i in average_diff_groups) {
  
  # Define file paths for LST_diff and LST_act
  file_list1 <- list.files(paste0("./EA+NA_Results/merge_average_diff_years/merged_average_diff_", i, "/"), 
                           pattern = "\\.tif$", full.names = TRUE)
  file_list2 <- list.files("./EA+NA_Results/merged_actLSTmean_years/", pattern = "\\.tif$", full.names = TRUE)
  
  # Read raster data
  LST_diff <- rast(c(file_list1))  # Load the LST difference raster
  LST_act <- rast(c(file_list2))   # Load the actual LST raster
  
  # Align common pixels
  sample = LST_diff      
  sample[is.finite(sample)] = 1  # Mark valid pixels
  plot(sample)  # Visualize the valid pixels of LST_diff
  
  sample2 = LST_act
  sample2[is.finite(sample2)] = 1  # Mark valid pixels for LST_act
  
  # Apply common mask for both LST_diff and LST_act
  LST_diff = LST_diff * sample[[1]] * sample2[[1]]  # Mask LST_diff with valid pixels
  LST_act = LST_act * sample[[1]] * sample2[[1]]    # Mask LST_act with valid pixels
  
  plot(LST_diff)  # Visualize the masked LST_diff
  plot(LST_act)   # Visualize the masked LST_act
  
  
##########################   02 Build Linear Model   #####################################
  
  
  # Objective description:
  # This section builds a linear model between LST_diff and LST_act for each pixel.
  # For each pixel, the goal is to calculate the regression slope (k), R², and p-value between LST_diff and LST_act.
  # The results will be used to generate a map showing the regression slope (k).
  
  sr <- sds(LST_diff, LST_act)  # Compute the stack of LST_diff and LST_act for each pixel
  
  # Compute Pearson correlation coefficient and linear model for each pixel
  lm_result <- lapp(sr, \(y, x) {
    tryCatch({
      df = na.omit(data.frame(x=x, y=y))  # Create a data frame with valid (non-NA) values
      if (sum(!is.na(y)) < 3 || sum(!is.na(x)) < 3) { 
        return(c(NA, NA, NA))  # Return NA if there are fewer than 3 non-NA values
      } else {
        model <- lm(y ~ x, data = df)  # Fit a linear model to the data
        
        # Return the regression slope (coef), R², and p-value
        return(
          c(
            coef(model)[2],                               # Slope (k)
            summary(model)[["r.squared"]],                # R² (R-squared)
            summary(model)[["coefficients"]][2, 4]        # p-value
          )
        )
      }
    }, error = function(e) {
      return(c(NA, NA, NA))  # In case of an error, return NA for all values
    })
  })
  
  # Label the output of the linear model results
  names(lm_result) = c("coef_x", "r2", "p_value")
  picture_k <- rast(lm_result$coef_x)  # Create a raster of the regression slopes (k values)
  
  # Visualize the linear model results
  plot(lm_result)  # Plot the full results
  
  # Print the total number of pixels in the output raster
  num_cells <- ncell(lm_result$coef_x)
  print(num_cells)
  
  # Print the number of non-NA pixels
  num_cells_not_na <- sum(!is.na(values(lm_result$coef_x)))
  print(num_cells_not_na)
  
  # Extract the regression slope map (k values)
  k_map <- lm_result$coef_x
  class(k_map)  # Check the class of the k_map raster
  
  # Convert the k_map to a data frame for further analysis
  df <- as.data.frame(k_map, xy = TRUE, na.rm = TRUE)  
  colnames(df) <- c("long", "lat", "k_value")  # Rename the columns
  summary(df$k_value)  # Summarize the k values (number of non-zero pixels)
  
  # Calculate the mean and standard deviation of the k values
  mean_k_map <- mean(k_map[], na.rm = TRUE)
  sd_k_map <- sd(k_map[], na.rm = TRUE)
  
  # Set the threshold as three times the standard deviation
  threshold <- 3 * sd_k_map
  
  # Remove pixels with values outside the threshold
  k_map_no_outliers <- k_map
  # Identify outliers based on the threshold
  outliers <- k_map[] < (mean_k_map - threshold) | k_map[] > (mean_k_map + threshold)
  # Set outliers to NA
  k_map_no_outliers[outliers] <- NA
  
  # Mask out the outlier pixels
  k_map_no_outliers <- mask(k_map_no_outliers, is.na(k_map_no_outliers))
  
  # Print the number of pixels remaining after removing outliers
  num_cells_not_NA <- sum(!is.na(values(k_map_no_outliers)))
  print(num_cells_not_NA)
  
  # Define the output directory
  output_dir <- "./EA+NA_Results/merged_diffLST&actLST/"
  # Create the output directory if it doesn't exist
  if (!dir.exists(output_dir)) { dir.create(output_dir, recursive = TRUE) }
  
  # Define the output file path
  output_file <- paste0(output_dir, "merged_diffLST&actLST_", i, ".tif")
  
  # Write the output raster (with outliers removed) to file
  writeRaster(k_map_no_outliers, output_file, overwrite = TRUE)
  

########################   03 Extract Common Pixels for 6 Events   #####################################
  
  # Objective description:
  # This section extracts the common pixels between six rasters (representing 6 different event times).
  # The goal is to ensure that only the pixels which have valid data in all six layers (k_1 to k_6) are retained.
  # The resulting rasters will be saved in a new directory for further analysis.
  
  ####### 1 Read the raster files            
  k_1 <- rast("./EA+NA_Results/merged_diffLST&actLST/merged_diffLST&actLST_1.tif")
  k_2 <- rast("./EA+NA_Results/merged_diffLST&actLST/merged_diffLST&actLST_2.tif")
  k_3 <- rast("./EA+NA_Results/merged_diffLST&actLST/merged_diffLST&actLST_3.tif")
  k_4 <- rast("./EA+NA_Results/merged_diffLST&actLST/merged_diffLST&actLST_4.tif")
  k_5 <- rast("./EA+NA_Results/merged_diffLST&actLST/merged_diffLST&actLST_5.tif")
  k_6 <- rast("./EA+NA_Results/merged_diffLST&actLST/merged_diffLST&actLST_6.tif")
  
  # Count the number of non-NA pixels in the first raster (k_1)
  non_na_count2 <- sum(!is.na(values(k_1)))
  print(non_na_count2)
  
  ##2 Extract common pixels with valid data across all six rasters
  
  # For each raster, create a mask where valid (non-NA) values are set to 1
  sample = k_1      
  sample[is.finite(sample)] = 1
  
  sample2 = k_2
  sample2[is.finite(sample2)] = 1
  
  sample3 = k_3
  sample3[is.finite(sample3)] = 1
  
  sample4 = k_4
  sample4[is.finite(sample4)] = 1
  
  sample5 = k_5
  sample5[is.finite(sample5)] = 1
  
  sample6 = k_6
  sample6[is.finite(sample6)] = 1
  
  # Multiply the rasters by the corresponding sample mask to retain only the common valid pixels
  k_1 = k_1 * sample[[1]] * sample2[[1]] * sample3[[1]] * sample4[[1]] * sample5[[1]] * sample6[[1]]
  k_2 = k_2 * sample[[1]] * sample2[[1]] * sample3[[1]] * sample4[[1]] * sample5[[1]] * sample6[[1]]
  k_3 = k_3 * sample[[1]] * sample2[[1]] * sample3[[1]] * sample4[[1]] * sample5[[1]] * sample6[[1]]
  k_4 = k_4 * sample[[1]] * sample2[[1]] * sample3[[1]] * sample4[[1]] * sample5[[1]] * sample6[[1]]
  k_5 = k_5 * sample[[1]] * sample2[[1]] * sample3[[1]] * sample4[[1]] * sample5[[1]] * sample6[[1]]
  k_6 = k_6 * sample[[1]] * sample2[[1]] * sample3[[1]] * sample4[[1]] * sample5[[1]] * sample6[[1]]
  
  # Count the number of non-NA pixels in the first raster after extracting common pixels
  non_empty_pixels <- sum(!is.na(values(k_1)))
  print(paste("Number of non-empty pixels: ", non_empty_pixels))
  
  # Create a new folder for the output files if it doesn't exist
  new_folder <- "./EA+NA_Results/merged_diffLST&actLST/0common_pixel_k/"
  dir.create(new_folder, showWarnings = FALSE)
  
  # Write the resulting rasters to the new folder
  writeRaster(k_1, "./EA+NA_Results/merged_diffLST&actLST/0common_pixel_k/merged_diffLST&actLST_1.tif", overwrite = TRUE)
  writeRaster(k_2, "./EA+NA_Results/merged_diffLST&actLST/0common_pixel_k/merged_diffLST&actLST_2.tif", overwrite = TRUE)
  writeRaster(k_3, "./EA+NA_Results/merged_diffLST&actLST/0common_pixel_k/merged_diffLST&actLST_3.tif", overwrite = TRUE)
  writeRaster(k_4, "./EA+NA_Results/merged_diffLST&actLST/0common_pixel_k/merged_diffLST&actLST_4.tif", overwrite = TRUE)
  writeRaster(k_5, "./EA+NA_Results/merged_diffLST&actLST/0common_pixel_k/merged_diffLST&actLST_5.tif", overwrite = TRUE)
  writeRaster(k_6, "./EA+NA_Results/merged_diffLST&actLST/0common_pixel_k/merged_diffLST&actLST_6.tif", overwrite = TRUE)
  