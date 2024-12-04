###### 0. Load Packages ####
library(terra)
library(tidyverse)
library(raster)

setwd("D:/VegetationImpact")

##################   01 Merge Europe and North America actLSTmean_yr raster data ####################################

file_list2 <- list.files("./NA_Results/0.actLSTmean_yr/0common_pixel/", pattern = "\\.tif$", full.names = TRUE)
file_list22 <- list.files("./EA_Results/0.actLSTmean_yr/0common_pixel/", pattern = "\\.tif$", full.names = TRUE)

rasters2 <- sapply(file_list2, rast)
rasters22 <- sapply(file_list22, rast)

output_path <- "./EA+NA_Results/"
output_folder <- paste0(output_path, "merged_actLSTmean_years")
# Check and create the folder if it does not exist
if (!file.exists(output_folder)) { dir.create(output_folder) }

for (i in seq_along(rasters2)) {
  merged_raster <- merge(rasters2[[i]], rasters22[[i]])
  year <- substr(basename(file_list2[i]), 1, 4)  # Get the first four characters as the year
  filename <- file.path(output_folder, paste0("EA+NA_merged_actLSTmean_yr_", year, ".tif"))
  writeRaster(merged_raster, filename = filename, overwrite = TRUE)
}
