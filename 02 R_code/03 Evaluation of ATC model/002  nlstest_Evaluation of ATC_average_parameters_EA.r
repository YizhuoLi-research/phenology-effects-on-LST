## Extract pixels that have diff123456 values
library(terra)
library(raster)
setwd("D:/VegetationImpact")

############### 01 Load TIF files #################################################
# Use pixels with common values in average_diff as the standard to obtain pixels with shared data across parameters

# Load the common_tif_file
common_tif_file <- raster("./EA_DIFF_9yearAverge/average_diff_1.tif")   # EA, using SOS (pixels with 6 common diff values) as the standard
# Get all TIF file paths in the parameter_tifs folder
parameter_tif_paths <- list.files(path = "./EA_Results/0.atc_evaluation/",      # EA
                                  pattern = "\\.tif$", full.names = TRUE)
# Create a list to store the result of shared pixels
common_parameter_tifs <- list()

############### 02 Extract shared pixels #################################################

# Loop through each TIF file in parameter_tifs
for (tif_path in parameter_tif_paths) {
  # Load the current TIF file
  current_tif <- raster(tif_path)
  
  # Set non-missing pixels in common_tif_file to 1, others to NA
  common_tif_file[!is.finite(common_tif_file)] <- NA
  common_tif_file[is.finite(common_tif_file)] <- 1
  
  # Multiply the current TIF file with common_tif_file to obtain shared pixels
  common_parameter_tif <- current_tif * common_tif_file
  
  # Add the result to the common_parameter_tifs list
  common_parameter_tifs[[tif_path]] <- common_parameter_tif
}

# Create output folder
output_folder <- "./EA_Results/0.atc_evaluation/0common_pixel/"               # EA
if (!dir.exists(output_folder)) {
  dir.create(output_folder)
}

# Write the result of shared pixels into the output folder
for (i in seq_along(parameter_tif_paths)) {
  output_file <- basename(parameter_tif_paths[i])
  output_path <- file.path(output_folder, output_file)
  writeRaster(common_parameter_tifs[[i]], filename = output_path, overwrite = TRUE)
}

# Print the number of non-empty pixels
non_empty_pixels <- sum(!is.na(values(common_parameter_tifs[1])))
print(paste("Number of non-empty pixels:", non_empty_pixels))

# Load the TIF file
tif_file <- rast("./EA_DIFF_9yearAverge/average_diff_1.tif")
non_na_count <- sum(!is.na(values(tif_file)))
print(non_na_count)
# NA: 12527; EA: 37302

# ########################## Check pixel count -- verification
tif_file2 <- rast("./EA_Results/0.atc_evaluation/0common_pixel/2013_rr_2.tif")
non_na_count2 <- sum(!is.na(values(tif_file2)))
# Print result
print(non_na_count2)
# NA   2013 - 2013 ncells: 10369 
# EA   2013 - 2013 ncells: 32020 

############### 03 Calculate multi-year averages #################################################
# Calculate 9-year average values me, me_2, rr, rr_2, p, p2 *NA EA, run 8 times in total
# me:mean error for the first time iteration; me_2: mean error for the second time iteration
# rr: R for the first time iteration; rr_2:  R for the second time iteration

file_list <- list.files("./EA_Results/0.atc_evaluation/0_common_pixel",             # EA
                        pattern = "\\.tif$", full.names = TRUE)
files <- file_list[grep("me\\.tif$", file_list, ignore.case = TRUE)]              # 4 parameters change to me_2 ,rr, rr_2

# Create new folder
new_folder <- "./EA_Results/0.atc_evaluation/EA_para_9yearAverge"                   # EA
dir.create(new_folder, showWarnings = FALSE)

# Calculate the average for each group of images and save
average_images <- list()
for (file_path in files) {
  # Load the current file
  current_image <- raster::raster(file_path)
  
  # Add the current image to the list
  average_images[[basename(file_path)]] <- current_image
}

# Calculate the average
if (length(average_images) > 0) {
  average_stack <- stack(average_images)
  average_tif <- raster::mean(average_stack, na.rm = TRUE)
  
  # Generate a new file name and save the image
  new_file_path <- file.path(new_folder, "me.tif")                     # 4 parameters
  raster::writeRaster(average_tif, filename = new_file_path, overwrite = TRUE)
}

plot(average_tif)
summary(average_tif)

df1 <- as.data.frame(average_tif, xy = TRUE, na.rm = TRUE)  
colnames(df1) <- c("long", "lat", "average") 
df1$average <- as.numeric(as.character(df1$average))
summary(df1$average)

#### Check data
# ME
over_10_count <- sum(df1$average > 10, na.rm = TRUE)
total_count <- length(df1$average)
over_10_percentage <- over_10_count /  total_count * 100
cat("over_10_percentage:", over_10_percentage, "%\n")

# R
below_0.5_count <- sum(df1$average < 0.5, na.rm = TRUE)
total_count <- length(df1$average)
below_0.5_percentage <- below_0.5_count /  total_count * 100
cat("below_0.5_count:", below_0.5_count, "%\n")

# p
over_0.05_count <- sum(df1$average > 0.05, na.rm = TRUE)
total_count <- length(df1$average)
over_0.05_percentage <- over_0.05_count / total_count * 100
cat("over_0.05_percentage:", over_0.05_percentage, "%\n")

# tif_file3 <- rast("./0.figure/pr_test/EA_para_9yearAverge/rr_2.tif")
# non_na_count3 <- sum(!is.na(values(average_tif)))
# # Print result
# print(non_na_count3)   #12527 NA No problem!!!  #37302 EA  Same thing
