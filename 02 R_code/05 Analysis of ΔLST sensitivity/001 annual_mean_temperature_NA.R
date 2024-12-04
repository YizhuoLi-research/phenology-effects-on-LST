###### 0. 加载包 ####
library(terra)
library(tidyverse)
library(raster)

setwd("D:/VegetationImpact")

##################  01 获取所有年份实际LST的平均值影像  NA#####################################
# 获取所有文件夹的路径
folder_path <- "./01 Download/02 LandSurfaceTemperature_download/NA_LST/" 
all_folders <- list.dirs(folder_path, full.names = TRUE, recursive = FALSE)
for (folder in all_folders) {
  file_list <- list.files(folder, pattern = "\\.tif$", full.names = TRUE)
  LST_year <- rast(file_list)     #读取影像到一个sds-多层数据集对象中
  mean_LST_year <- mean(LST_year,na.rm=T) #计算平均
  year <- basename(folder)        #获取年份
  output_folder <- paste0("./NA_Results/", "0.actLSTmean_yr")
  dir.create(output_folder, showWarnings = FALSE)
  output_file <- file.path(output_folder, paste0(year, "_actLSTmean.tif"))
  writeRaster(mean_LST_year, filename = output_file,overwrite=T)
}
