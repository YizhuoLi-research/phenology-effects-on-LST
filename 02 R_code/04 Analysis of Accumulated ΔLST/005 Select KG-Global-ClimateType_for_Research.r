##################  00 Load Packages   ##########################################################################
# Load required libraries
library(terra)
library(tidyverse)
library(raster)

# Set the working directory
setwd("D:/VegetationImpact")

##################  01 Calculate Regression Slope for Pixels by Climate Type -- Add Attributes to Climate Type Data  ###################

# Load the climate raster dataset
r <- raster("./EA+NA_Results/EA+NA_koppen_30km_addClimate.tif")
r[1:30] <- seq(1,30,1)  
r0 <- r[1:30]

# Convert raster values to categorical data (factorized)
r <- ratify(r) # Converts raster field to categorical data
rat <- levels(r)[[1]]

# Legend in alphabetic order for climate types
rat$climate <- c('Af', 'Am', 'As', 'Aw',
                 'BSh', 'BSk', 'BWh', 'BWk',
                 'Cfa', 'Cfb','Cfc', 
                 'Csa', 'Csb','Csc', 
                 'Cwa','Cwb', 'Cwc', 
                 'Dfa', 'Dfb', 'Dfc','Dfd', 
                 'Dsa', 'Dsb', 'Dsc','Dsd',
                 'Dwa', 'Dwb', 'Dwc','Dwd', 
                 'EF',  'ET')

# Remove the placeholders from the raster
r[1:30] <- r0

# Assign the modified attribute table back to the raster
levels(r) <- rat
# library(rasterVis);levelplot(r)

# Define the boundary of each climate region in filter_values_list
classify_border <- as.polygons(rast(r))

# Create a list of climate types
climate_types <- rat$climate 

# Generate all combinations of climate types using expand.grid
all_climates <- expand.grid(climate_types, stringsAsFactors = FALSE)
colnames(all_climates) <- "Climate_Type"

# Store the climate types into a list
filter_values_list <- split(all_climates$Climate_Type, 1:nrow(all_climates))

###########################  02 Filter Climate Types in diff Files and Count Pixels for Each Type  #####################################################################
# As the diff data for years 1 to 6 are the same, we use diff_1 as a representative

file_paths <-"./EA+NA_Results/merge_average_diff_years/merged_average_diff_1/" 
file_list  <- list.files(file_paths, pattern = "\\.tif$", full.names = TRUE)

# Read the first file as the base raster
base_raster <- raster(file_list[1])

# Extract non-empty pixels from the base raster
base_non_empty <- ifelse(!is.na(values(base_raster)), 1, NA)

# Loop through the list of files and add non-empty pixels from each file to the base
for (file in file_list[-1]) {
  raster_data <- raster(file)
  non_empty_pixels <- ifelse(!is.na(values(raster_data)), 1, NA)
  base_non_empty <- base_non_empty | non_empty_pixels
}

# Create a raster containing the union of non-empty pixels from all files
final_raster <- base_raster
final_raster[] <- ifelse(base_non_empty == 1, 1, NA)
plot(final_raster)   # Plot the union of non-empty pixels from 2013-2021

# Create the sample raster for further analysis
sample = final_raster      
sample[is.finite(final_raster)] = 1
sample2 = r
sample2[is.finite(sample2)] = 1

# Perform the operation of multiplying rasters
r_common = r * sample[[1]] * sample2[[1]]
final_raster_common = final_raster * sample[[1]] * sample2[[1]] 

# Count the number of non-empty pixels in the common raster
non_empty_pixels <- sum(!is.na(values(r_common)))
plot(r)  # Plot the original raster
plot(r_common)  # Plot the raster after applying the common pixels filter

# Convert the filtered raster to a data frame for further processing
r_df <- as.data.frame(rasterToPoints(r_common), xy = TRUE, na.rm = TRUE)
colnames(r_df) <- c("long","lat","koppen_val")

# Create a correlation table to map Koppen climate values to climate types
climate_cor <- tibble(
  koppen_val = 0:30,
  climate_type = c('Af', 'Am', 'As', 'Aw',
                   'BSh', 'BSk', 'BWh', 'BWk',
                   'Cfa', 'Cfb','Cfc', 
                   'Csa', 'Csb','Csc', 
                   'Cwa','Cwb', 'Cwc', 
                   'Dfa', 'Dfb', 'Dfc','Dfd', 
                   'Dsa', 'Dsb', 'Dsc','Dsd',
                   'Dwa', 'Dwb', 'Dwc','Dwd', 
                   'EF',  'ET')
)

# Map the koppen_val to climate_type in the data frame
r_df <- r_df %>%
  left_join(climate_cor, by = "koppen_val")

# Create a group column based on the climate type (e.g., 'A' for 'Af')
r_df <- r_df %>%   
  mutate(group = paste0(substr(climate_type, 1, 1), "X", substr(climate_type, 3, 3)))

# Get the distinct climate types in the data
distinct_climate_types <- distinct(r_df, climate_type)  # Get unique climate types
cat("Different climate types are:", distinct_climate_types$climate_type, "\n")

# Count the number of pixels for each climate type
climate_type_counts <- count(r_df, climate_type)
cat("Number of pixels for each climate type:\n")
print(climate_type_counts)

# Example output of climate type counts
# climate_type     n
# 1            Af     2
# 2           BSh    15
# 3           BSk   374
# 4           BWk    30
# 5           Cfa  4235
# 6           Cfb  4656
# 7           Cfc   120
# 8           Csa   546
# 9           Csb   588
# 10          Cwa   523
# 11          Cwb   155
# 12          Dfa   579
# 13          Dfb 11782
# 14          Dfc 17605
# 15          Dfd   741
# 16          Dsa     3
# 17          Dsb   103
# 18          Dsc   735
# 19          Dsd    77
# 20          Dwa   456
# 21          Dwb  1663
# 22          Dwc  3838
# 23          Dwd   214
# 24           ET   753

# Select climate types C (Warm temperate) and D (Snow temperate) for further analysis
