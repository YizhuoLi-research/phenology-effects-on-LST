library(terra)
library(raster)
setwd("D:/VegetationImpact")

############### 01 Read tif files  #################################################
# Use pixels with complete average_diff_1.tif values as the mask to extract overlapping pixels across all parameters

# Read common_tif_file
common_tif_file <- raster("./NA_DIFF_9yearAverge/average_diff_1.tif")  # Use SOS (6 consistent diff layers) as base mask

# Get all tif files from parameter folder
parameter_tif_paths <- list.files(path = "./NA_Results/0.atc_evaluation/", 
                                  pattern = "\\.tif$", full.names = TRUE)

# Create a list to store the masked results
common_parameter_tifs <- list()

############### 02 Extract common pixels  #################################################

# Loop through each tif file
for (tif_path in parameter_tif_paths) {
  current_tif <- raster(tif_path)
  
  # Convert non-NA pixels in common_tif_file to 1
  common_tif_file[!is.finite(common_tif_file)] <- NA
  common_tif_file[is.finite(common_tif_file)] <- 1
  
  # Multiply current tif with mask to get common pixels
  common_parameter_tif <- current_tif * common_tif_file
  
  # Store result
  common_parameter_tifs[[tif_path]] <- common_parameter_tif
}

# Create output folder
output_folder <- "./NA_Results/0.atc_evaluation/0common_pixel/"
if (!dir.exists(output_folder)) {
  dir.create(output_folder)
}

# Write common-pixel result to output folder
for (i in seq_along(parameter_tif_paths)) {
  output_file <- basename(parameter_tif_paths[i])
  output_path <- file.path(output_folder, output_file)
  writeRaster(common_parameter_tifs[[i]], filename = output_path, overwrite = TRUE)
}

# Print number of non-NA pixels
non_empty_pixels <- sum(!is.na(values(common_parameter_tifs[1])))
print(paste("Number of non-NA pixels:", non_empty_pixels))

# Check pixel count in base mask file
tif_file <- rast("./NA_DIFF_9yearAverge/average_diff_1.tif")
non_na_count <- sum(!is.na(values(tif_file)))
print(non_na_count)
# NA: 12527; EA: 37302

# ########################## Check pixel count for result
tif_file2 <- rast("./NA_Results/0.atc_evaluation/0common_pixel/2013_rr_2.tif")
non_na_count2 <- sum(!is.na(values(tif_file2)))
print(non_na_count2)
# NA: 10369; EA: 32020

############### 03 Compute multi-year averages  #################################################
# Compute 9-year average for me, me_2, rr, rr_2, p, p2 
# Run separately for each parameter (total 8 runs)
# me: mean error for first iteration; me_2: second iteration
# rr: R value for first iteration; rr_2: second iteration

file_list <- list.files("./NA_Results/0.atc_evaluation/0_common_pixel", 
                        pattern = "\\.tif$", full.names = TRUE)

# Filter parameter files (change pattern accordingly)
files <- file_list[grep("me\\.tif$", file_list, ignore.case = TRUE)]  # Change to me_2, rr, rr_2 as needed

# Create new folder
new_folder <- "./NA_Results/0.atc_evaluation/NA_para_9yearAverge"
dir.create(new_folder, showWarnings = FALSE)

# Stack and average files
average_images <- list()
for (file_path in files) {
  current_image <- raster::raster(file_path)
  average_images[[basename(file_path)]] <- current_image
}

if (length(average_images) > 0) {
  average_stack <- stack(average_images)
  average_tif <- raster::mean(average_stack, na.rm = TRUE)
  
  # Save averaged result
  new_file_path <- file.path(new_folder, "me.tif")  # Change filename accordingly
  raster::writeRaster(average_tif, filename = new_file_path, overwrite = TRUE)
}

# Plot and summary
plot(average_tif)
summary(average_tif)

# Convert raster to data frame for inspection
df1 <- as.data.frame(average_tif, xy = TRUE, na.rm = TRUE)  
colnames(df1) <- c("long", "lat", "average") 
df1$average <- as.numeric(as.character(df1$average))
summary(df1$average)

#### Inspect data distribution
# For mean error
over_10_count <- sum(df1$average > 10, na.rm = TRUE)
total_count <- length(df1$average)
over_10_percentage <- over_10_count / total_count * 100
cat("Percentage > 10:", over_10_percentage, "%\n")

# For R
below_0.5_count <- sum(df1$average < 0.5, na.rm = TRUE)
total_count <- length(df1$average)
below_0.5_percentage <- below_0.5_count / total_count * 100
cat("Count < 0.5:", below_0.5_count, "%\n")

# For p-value
over_0.05_count <- sum(df1$average > 0.05, na.rm = TRUE)
total_count <- length(df1$average)
over_0.05_percentage <- over_0.05_count / total_count * 100
cat("Percentage > 0.05:", over_0.05_percentage, "%\n")

# Optional: Check NA count in averaged raster
# tif_file3 <- rast("./0.figure/pr_test/NA_para_9yearAverge/rr_2.tif")
# non_na_count3 <- sum(!is.na(values(average_tif)))
# print(non_na_count3)  # 12527 for NA; 37302 for EA
