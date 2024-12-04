library(raster)
# Set the working directory
setwd("D:/VegetationImpact/01 Download/03 PhenologicalEvents_download/EA_PHE")


#######################  1. Merging files by latitude band  ############################

# Get all .tif files in the current working directory
tif_files <- list.files(pattern = "\\.tif$", full.names = TRUE)

# Create a list to group files by the first 25 characters of the file name
file_groups <- list()
for (file in tif_files) {
  file_name <- basename(file)
  key <- substr(file_name, 1, 30)
  if (!key %in% names(file_groups)) {
    file_groups[[key]] <- list()
  }
  file_groups[[key]] <- c(file_groups[[key]], file)
}

for (key in names(file_groups)) {
  cat(paste("File group with the first 25 characters as '", key, "':\n", sep = ""))
  
  # Read all Raster files in the file list
  raster_list <- lapply(file_groups[[key]], raster)
  
  # Perform the merge
  raster_list$fun <- mean
  raster_list$na.rm <- TRUE
  merged_raster <- do.call(mosaic, raster_list)
  
  # Generate output file name # Convert parameters to a character vector
  first_file <- as.character(file_groups[[key]][1]) 
  prefix <- substr(first_file, 1, 30)  # Extract the first 30 characters
  
  # Set the output folder path
  output_folder <- "D:/VegetationImpact/01 Download/03 PhenologicalEvents_download/EA_PHE/EA_PHE_merged"
  if (!file.exists(output_folder)) {
    dir.create(output_folder, recursive = TRUE)
  }
  
  output_path <- file.path(output_folder, paste0(prefix, "_merged.tif"))
  
  # Save the merged file
  writeRaster(merged_raster, filename = output_path, format = "GTiff", 
              options = "COMPRESS=LZW", overwrite=TRUE)
  
  cat("Merging complete, file saved to", output_path, "\n")
}

#######################  2. Grouping files by year  ##############################

setwd("D:/VegetationImpact/01 Download/03 PhenologicalEvents_download/EA_PHE/EA_PHE_merged")

# Get a list of all TIFF files in the directory
merged_tif_files <- list.files(pattern = "\\.tif$", full.names = TRUE)

# Create folders based on the year and move files to respective folders
for (file in merged_tif_files) {
  # Extract the year from the file name (first four characters)
  year <- substr(basename(file), 1, 4)
  
  # Create a directory for the year if it doesn't exist
  year_folder <- file.path("D:/VegetationImpact/01 Download/03 PhenologicalEvents_download/EA_PHE/EA_PHE_merged", year)
  if (!dir.exists(year_folder)) {
    dir.create(year_folder)
  }
  
  # Move the file to the respective year folder
  file.rename(file, file.path(year_folder, basename(file)))
  cat("Moved", basename(file), "to", year, "folder.\n")
}




