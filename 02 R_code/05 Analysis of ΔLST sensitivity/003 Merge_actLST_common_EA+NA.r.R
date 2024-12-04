###### 0. 加载包 ####
library(terra)
library(tidyverse)
library(raster)

setwd("D:/VegetationImpact")


##################   01 欧洲美洲actLSTmean_yr栅格数据 合并  ####################################

 file_list2 <- list.files("./NA_Results/0.actLSTmean_yr/0common_pixel/",pattern = "\\.tif$",full.names = TRUE)
 file_list22 <- list.files("./EA_Results/0.actLSTmean_yr/0common_pixel/",pattern = "\\.tif$",full.names = TRUE)

 rasters2 <- sapply(file_list2, rast)
 rasters22 <- sapply(file_list22, rast)

 output_path <- "./EA+NA_Results/"
 output_folder <- paste0(output_path, "merged_actLSTmean_years")
 # 检查并创建文件夹
 if (!file.exists(output_folder)) { dir.create(output_folder)  }

 for (i in seq_along(rasters2)) {
   merged_raster <- merge(rasters2[[i]], rasters22[[i]])
   year <- substr(basename(file_list2[i]), 1, 4)  # 获取前四个字符作为年份
   filename <- file.path(output_folder, paste0("EA+NA_merged_actLSTmean_yr_", year, ".tif"))
   writeRaster(merged_raster, filename = filename, overwrite = TRUE)
 }
 