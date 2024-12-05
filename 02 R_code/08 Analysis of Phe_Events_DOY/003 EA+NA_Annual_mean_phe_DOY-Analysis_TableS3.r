# This code calculates various statistical values for Day of Year (DOY) for North America (NA),
# Euraisa (EA) and the Northern Hemisphere (NH).
# specifically focusing on phenological events (from SOS to EOS). It computes the mean, maximum, minimum, 
# and standard deviation of the DOY rasters, then calculates a linear trend across the years 2013 to 2021 
# using a linear regression. The results are summarized and printed in a data frame for further analysis.

###### 0. 加载包 ####
library(terra)
library(tidyverse)
library(raster)

setwd("D:/VegetationImpact")

###### 0. Load Packages ####
# Purpose: Load necessary libraries for raster processing, data manipulation, and file handling.
library(terra)
library(tidyverse)
library(raster)

# Set the working directory
setwd("D:/VegetationImpact")

################################   01 Calculate DOY for North America  ##################################
###############################################################################################         
# NA (North America)
# SOS: phe1_DOY_;  MGP: phe2_DOY_;  GMO: phe3_DOY_;
# GDO: phe4_DOY_;  MSP: phe5_DOY_;  EOS: phe6_DOY_;

# The code modifies the pattern in list.files() to match all six phenological
# events (phe1_DOY_ to phe6_DOY_) in a single run for processing.

# List the files for the "phe6_DOY_" event in the North America directory
file_list <- list.files("./NA_Results/0.phe_DOY/0common_pixel/", 
                        pattern = "phe6_DOY_.*\\.tif$", full.names = TRUE)

# Read all the raster images for "phe6_DOY_"
rasters <- stack(file_list)
mean_raster <- calc(rasters, fun = mean, na.rm = TRUE)

# Calculate the mean, maximum, minimum, and 0.15 times the standard deviation
mean_value <- ceiling(cellStats(mean_raster, stat = 'mean', na.rm = TRUE))
max_value <- round(cellStats(mean_raster, stat = 'max', na.rm = TRUE),2)
min_value <- round(cellStats(mean_raster, stat = 'min', na.rm = TRUE),2)
sd_value <- round(cellStats(mean_raster, stat = 'sd', na.rm = TRUE),2)
spatial_sd_variation <- round(0.15 * sd_value,2)

# Print the results (commented out, but useful for debugging)
# cat("Mean:", mean_value, "\n",
#     "Max:", max_value, "\n",
#     "Min:", min_value, "\n",
#     "0.15 * SD (Spatial Variation):", spatial_sd_variation, "\n")

########## Calculate the trend over time ##########
# Define a function to calculate the trend for each pixel

calc_trend <- function(pixel_values, ...) {
  if (all(is.na(pixel_values))) {
    return(NA)  # Return NA if all pixel values are NA
  }
  
  # Remove NA values
  valid_values <- !is.na(pixel_values)
  if (sum(valid_values) < 3) {
    return(NA)  # Return NA if there are fewer than 3 valid data points
  }
  
  # Define the years corresponding to the data
  years <- 2013:2021  # Years corresponding to the data files
  
  # Perform linear regression to calculate the trend
  model <- lm(pixel_values ~ years)
  
  # Return the regression slope (trend)
  return(coef(model)[2])
}

# Use the calc function to calculate the trend for each pixel
trend_raster <- calc(rasters, fun = calc_trend, na.rm = TRUE)

# Summary of the trend raster
summary(trend_raster[])

# # Uncomment to view the trend raster plot
# plot(trend_raster, main = "DOY Trend (2013-2021)")
# summary(trend_raster)

# Calculate the average trend and ±0.15 SD
mean_trend <- mean(trend_raster[], na.rm = TRUE)
sd_trend <- sd(trend_raster[], na.rm = TRUE)
spatial_sd_trend <- round(0.15 * sd_trend,2)

## Summarize the results in a data frame
results_df <- data.frame(
  Metric = c("Mean_DOY", "Max_DOY", "Min_DOY", 
             "Mean_Trend", "Max_Trend", "Min_Trend"),
  Value = c(paste(mean_value, "±", spatial_sd_variation),  # Round the mean and spatial variation
            round(max_value, 2),                           # Round the max value to 2 decimal places
            round(min_value, 2),                           # Round the min value to 2 decimal places
            paste(round(mean_trend, 2), "±", round(spatial_sd_trend, 2)), # Round the trend mean and spatial variation
            round(max(trend_raster[], na.rm = TRUE), 2),   # Round the max trend value to 2 decimal places
            round(min(trend_raster[], na.rm = TRUE), 2) )  # Round the min trend value to 2 decimal places
)

# Print the summarized results in a table
print(results_df)

# #SOS  phe1_DOY_.
# 1   Mean_DOY   119 ± 3.18
# 2    Max_DOY          184
# 3    Min_DOY           15
# 4 Mean_Trend -0.21 ± 0.25
# 5  Max_Trend           19
# 6  Min_Trend        -20.5

##MGP  phe2_DOY_.
# 1   Mean_DOY  141 ± 2.78
# 2    Max_DOY         198
# 3    Min_DOY          40
# 4 Mean_Trend 0.01 ± 0.19
# 5  Max_Trend        16.5
# 6  Min_Trend         -17

##GMO  phe3_DOY_.
# 1   Mean_DOY 162 ± 2.48
# 2    Max_DOY        213
# 3    Min_DOY       88.5
# 4 Mean_Trend 0.21 ± 0.2
# 5  Max_Trend       13.5
# 6  Min_Trend      -15.5

##GDO  phe4_DOY_.
# 1   Mean_DOY  226 ± 1.25
# 2    Max_DOY         272
# 3    Min_DOY       125.5
# 4 Mean_Trend 0.09 ± 0.19
# 5  Max_Trend          15
# 6  Min_Trend      -13.25

##MSP  phe5_DOY_.
# 1   Mean_DOY   261 ± 1.83
# 2    Max_DOY       298.67
# 3    Min_DOY          167
# 4 Mean_Trend -0.05 ± 0.15
# 5  Max_Trend        14.25
# 6  Min_Trend         -9.5

##EOS  phe6_DOY_.
# 1   Mean_DOY  296 ± 3.28
# 2    Max_DOY      348.75
# 3    Min_DOY         195
# 4 Mean_Trend -0.19 ± 0.2
# 5  Max_Trend          21
# 6  Min_Trend       -17.1


###### 0. Load Packages ####
# Purpose: Load necessary libraries for raster processing, data manipulation, and file handling.
library(terra)
library(tidyverse)
library(raster)

# Set the working directory
setwd("D:/VegetationImpact")

################################   01 Calculate DOY for North America  ##################################
###############################################################################################         
# NA (North America)
# SOS: phe1_DOY_;  MGP: phe2_DOY_;  GMO: phe3_DOY_;
# GDO: phe4_DOY_;  MSP: phe5_DOY_;  EOS: phe6_DOY_;

# The code modifies the pattern in list.files() to match all six phenological
# events (phe1_DOY_ to phe6_DOY_) in a single run for processing.

# List the files for the "phe6_DOY_" event in the North America directory
file_list <- list.files("./NA_Results/0.phe_DOY/0common_pixel/", 
                        pattern = "phe6_DOY_.*\\.tif$", full.names = TRUE)

# Read all the raster images for "phe6_DOY_"
rasters <- stack(file_list)
mean_raster <- calc(rasters, fun = mean, na.rm = TRUE)

# Calculate the mean, maximum, minimum, and 0.15 times the standard deviation
mean_value <- ceiling(cellStats(mean_raster, stat = 'mean', na.rm = TRUE))
max_value <- round(cellStats(mean_raster, stat = 'max', na.rm = TRUE),2)
min_value <- round(cellStats(mean_raster, stat = 'min', na.rm = TRUE),2)
sd_value <- round(cellStats(mean_raster, stat = 'sd', na.rm = TRUE),2)
spatial_sd_variation <- round(0.15 * sd_value,2)

# Print the results (commented out, but useful for debugging)
# cat("Mean:", mean_value, "\n",
#     "Max:", max_value, "\n",
#     "Min:", min_value, "\n",
#     "0.15 * SD (Spatial Variation):", spatial_sd_variation, "\n")

########## Calculate the trend over time ##########
# Define a function to calculate the trend for each pixel

calc_trend <- function(pixel_values, ...) {
  if (all(is.na(pixel_values))) {
    return(NA)  # Return NA if all pixel values are NA
  }
  
  # Remove NA values
  valid_values <- !is.na(pixel_values)
  if (sum(valid_values) < 3) {
    return(NA)  # Return NA if there are fewer than 3 valid data points
  }
  
  # Define the years corresponding to the data
  years <- 2013:2021  # Years corresponding to the data files
  
  # Perform linear regression to calculate the trend
  model <- lm(pixel_values ~ years)
  
  # Return the regression slope (trend)
  return(coef(model)[2])
}

# Use the calc function to calculate the trend for each pixel
trend_raster <- calc(rasters, fun = calc_trend, na.rm = TRUE)

# Summary of the trend raster
summary(trend_raster[])

# # Uncomment to view the trend raster plot
# plot(trend_raster, main = "DOY Trend (2013-2021)")
# summary(trend_raster)

# Calculate the average trend and ±0.15 SD
mean_trend <- mean(trend_raster[], na.rm = TRUE)
sd_trend <- sd(trend_raster[], na.rm = TRUE)
spatial_sd_trend <- round(0.15 * sd_trend,2)

## Summarize the results in a data frame
results_df <- data.frame(
  Metric = c("Mean_DOY", "Max_DOY", "Min_DOY", 
             "Mean_Trend", "Max_Trend", "Min_Trend"),
  Value = c(paste(mean_value, "±", spatial_sd_variation),  # Round the mean and spatial variation
            round(max_value, 2),                           # Round the max value to 2 decimal places
            round(min_value, 2),                           # Round the min value to 2 decimal places
            paste(round(mean_trend, 2), "±", round(spatial_sd_trend, 2)), # Round the trend mean and spatial variation
            round(max(trend_raster[], na.rm = TRUE), 2),   # Round the max trend value to 2 decimal places
            round(min(trend_raster[], na.rm = TRUE), 2) )  # Round the min trend value to 2 decimal places
)

# Print the summarized results in a table
print(results_df)



##SOS  phe1_DOY_.
# 1   Mean_DOY  123 ± 3.04
# 2    Max_DOY         190
# 3    Min_DOY          21
# 4 Mean_Trend -0.1 ± 0.18
# 5  Max_Trend          32
# 6  Min_Trend       -19.5

##MGP  phe2_DOY_.
# 1   Mean_DOY  164 ± 2.08
# 2    Max_DOY         228
# 3    Min_DOY          95
# 4 Mean_Trend 0.06 ± 0.18
# 5  Max_Trend        17.5
# 6  Min_Trend         -18

##GMO  phe3_DOY_.
# 1   Mean_DOY 164 ± 2.1
# 2    Max_DOY       228
# 3    Min_DOY        95
# 4 Mean_Trend 0.1 ± 0.2
# 5  Max_Trend      17.5
# 6  Min_Trend       -18

##GDO   phe4_DOY_.
# 1   Mean_DOY  225 ± 1.03
# 2    Max_DOY         275
# 3    Min_DOY         122
# 4 Mean_Trend 0.01 ± 0.18
# 5  Max_Trend          20
# 6  Min_Trend       -20.5

##MSP  phe5_DOY_.
# 1   Mean_DOY  255 ± 1.92
# 2    Max_DOY         304
# 3    Min_DOY         150
# 4 Mean_Trend 0.12 ± 0.14
# 5  Max_Trend       11.25
# 6  Min_Trend         -28

##EOS  phe6_DOY_.
# 1   Mean_DOY  286 ± 3.42
# 2    Max_DOY         353
# 3    Min_DOY         178
# 4 Mean_Trend 0.24 ± 0.17
# 5  Max_Trend       16.25
# 6  Min_Trend       -37.5


############################   03  Calculate DOY for Northern Hemisphere  ###############################
###############################################################################################      
# List all phe6_DOY files for the Northern Hemisphere
file_list <- list.files("./EA+NA_Results/merge_Phe_DOY_years/merged_phe6_DOY/", 
                        pattern = "phe6_DOY_.*\\.tif$", full.names = TRUE)

# phe1_DOY_. phe2_DOY_. phe3_DOY_. phe4_DOY_. phe5_DOY_. phe6_DOY_.

# Read all raster images
rasters <- stack(file_list)
mean_raster <- calc(rasters, fun = mean, na.rm = TRUE)

# Calculate mean, maximum, minimum, and 0.15 times the standard deviation
mean_value <- ceiling(cellStats(mean_raster, stat = 'mean', na.rm = TRUE))
max_value <- cellStats(mean_raster, stat = 'max', na.rm = TRUE)
min_value <- cellStats(mean_raster, stat = 'min', na.rm = TRUE)
sd_value <- cellStats(mean_raster, stat = 'sd', na.rm = TRUE)
spatial_sd_variation <- round(0.15 * sd_value, 2)

# Print the results (commented out)
# cat("Mean:", mean_value, "\n",
#     "Max:", max_value, "\n",
#     "Min:", min_value, "\n",
#     "0.15 * SD (Spatial Variation):", spatial_sd_variation, "\n")

########## Calculate Temporal Trend
# Define a function to calculate trends for each pixel

calc_trend <- function(pixel_values, ...) {
  if (all(is.na(pixel_values))) {
    return(NA)  # Return NA if all pixel values are NA
  }
  
  # Remove NA values
  valid_values <- !is.na(pixel_values)
  if (sum(valid_values) < 3) {
    return(NA)  # Return NA if fewer than 3 valid data points
  }
  
  # Define years corresponding to the data
  years <- 2013:2021  # Years corresponding to the data files
  
  # Perform linear regression to calculate the trend
  model <- lm(pixel_values ~ years)
  
  # Return the regression slope (trend)
  return(coef(model)[2])
}

# Use the calc function to calculate the trend for each pixel
trend_raster <- calc(rasters, fun = calc_trend, na.rm = TRUE)

# Summary of the trend raster
summary(trend_raster[])

# # Uncomment to view the trend raster plot
# plot(trend_raster, main = "DOY Trend (2013-2021)")
# summary(trend_raster)

# Calculate the average trend and ±0.15 SD
mean_trend <- mean(trend_raster[], na.rm = TRUE)
sd_trend <- sd(trend_raster[], na.rm = TRUE)
spatial_sd_trend <- round(0.15 * sd_trend, 2)

## Summarize Results
results_df <- data.frame(
  Metric = c("Mean_DOY", "Max_DOY", "Min_DOY", 
             "Mean_Trend", "Max_Trend", "Min_Trend"),
  Value = c(paste(mean_value, "±", spatial_sd_variation),  # Round mean and spatial variation
            max_value,  # No rounding for max value
            min_value,  # No rounding for min value
            paste(round(mean_trend, 2), "±", round(spatial_sd_trend, 2)),  # Round trend mean and spatial variation
            round(max(trend_raster[], na.rm = TRUE), 2),  # Round max trend value
            round(min(trend_raster[], na.rm = TRUE), 2) )  # Round min trend value
)

# Print the summarized results
print(results_df)



##SOS  phe1_DOY_.
# 1   Mean_DOY  122 ± 3.09
# 2    Max_DOY         190
# 3    Min_DOY          15
# 4 Mean_Trend -0.13 ± 0.2
# 5  Max_Trend          32
# 6  Min_Trend       -20.5

# ##MGP  phe2_DOY_.
# 1   Mean_DOY   143 ± 2.55
# 2    Max_DOY          209
# 3    Min_DOY           40
# 4 Mean_Trend -0.02 ± 0.16
# 5  Max_Trend         16.5
# 6  Min_Trend          -17

##GMO  phe3_DOY_.
# 1   Mean_DOY 164 ± 2.19
# 2    Max_DOY        228
# 3    Min_DOY       88.5
# 4 Mean_Trend 0.1 ± 0.18
# 5  Max_Trend       17.5
# 6  Min_Trend        -18

##GDO  phe4_DOY_.
# 1   Mean_DOY  225 ± 1.09
# 2    Max_DOY         275
# 3    Min_DOY         122
# 4 Mean_Trend 0.03 ± 0.18
# 5  Max_Trend          20
# 6  Min_Trend       -20.5

##MSP  phe5_DOY_.
# 1   Mean_DOY  257 ± 1.93
# 2    Max_DOY         304
# 3    Min_DOY         150
# 4 Mean_Trend 0.08 ± 0.14
# 5  Max_Trend       14.25
# 6  Min_Trend         -28

##EOS  phe6_DOY_.
# 1   Mean_DOY  288 ± 3.45
# 2    Max_DOY         353
# 3    Min_DOY         178
# 4 Mean_Trend 0.13 ± 0.18
# 5  Max_Trend          21
# 6  Min_Trend       -37.5