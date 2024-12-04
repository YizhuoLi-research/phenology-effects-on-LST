# # library(raster)
# setwd("D:/VegetationImpact")
# file_path <- "./01 Download/02 LandSurfaceTemperature_download/NA_LST/_NA_30-40_2013.tif"
# # 打开影像文件
# raster_stack <- stack(file_path)
# # 获取波段数量
# band_count <- nlayers(raster_stack)
# print(paste("波段数量为:", band_count))
# # 获取并显示波段名
# for (i in 1:band_count) {
#   band_name <- names(raster_stack)[i]
#   if (is.null(band_name) || band_name == '') {
#     band_name <- paste("波段", i)
#   }
#   print(paste("波段", i, "名称:", band_name))
# }

library(terra)
library(progress)

folder_path <- "./01 Download/02 LandSurfaceTemperature_download/NA_LST/"
file_list <- list.files(folder_path, pattern = "\\.tif$", full.names = TRUE)
# Identify the years from the filenames and categorize
file_years <- substr(basename(file_list), 11, 14)
file_groups <- split(file_list, file_years)
pb <- progress_bar$new(format = "[:bar] :current/:total", total = length(file_groups), clear = FALSE)
for (i in 1:length(file_groups)) {
  year <- names(file_groups)[i]
  files <- file_groups[[year]]
  
  # Read the first file to get the number of bands
  first_raster <- rast(files[1])
  band_count <- nlyr(first_raster)
  
  # Create a new folder to store the merged files
  output_folder <- paste0(folder_path, year, "/")
  # Check if the directory exists; if not, create it
  if (!file.exists(output_folder)) {
    dir.create(output_folder, recursive = TRUE)
  }
  
  # Process and merge each band separately, and save as individual files
  for (j in 1:band_count) {
    print(j)
    
    # Retrieve the current band's raster data from each file in the four latitude bands
    band_rasters <- lapply(files, function(file) {
      a <- rast(file)[[j]]
      a[is.nan(a)] = NA
      return(a)
    })
    
    # Merge the raster data for the current band
    rsrc <- sprc(band_rasters)
    merged_raster <- merge(rsrc, na.rm = TRUE)
    # merged_raster <- do.call(terra::mosaic, band_rasters)
    
    # Get the name of the current band
    band_name <- names(rast(files[1]))[j]
    band_number <- gsub("\\D", "", substr(band_name, nchar(band_name) - 2, nchar(band_name)))
    
    # Generate the output filename for the current band
    output_file <- paste0(output_folder, band_number, ".tif")
    
    # Write the merged raster to the output file
    writeRaster(merged_raster, filename = output_file, overwrite = TRUE)
  }
  
  pb$tick()
  cat("\nGenerated merged files for the year", year, "\n")
}